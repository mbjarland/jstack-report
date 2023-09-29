(ns ^{:doc "The central namespace for jstack-report

 This namespace contains utilities for dealing with
 thread dump output produced by the java jstack tool.

 The main entry point in this namespace is the parse-jstack
 function which takes as input a sequence of lines and
 returns as output a clojure map which contains the jstack
 output in a clojure data structure.

 A typical usage might look like this:

   ~> java -jar target/jstack-report-0.1.0-standalone.jar -f thread-dumps/some-thread-dump.txt

 or using babashka:

   (ns user (:require [jstack-tools :as jt]))

   (jt/parse-jstack *in*)

 where *in* is bound by babashka to a sequence of input
 lines."
      :author "Matias Bjarland"}

  jstack-report.core
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]
    ;[taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [jstack-report.ansi :as ansi])
  (:import [java.io BufferedReader File Reader]
           [java.time Duration LocalDateTime LocalTime ZoneOffset]
           [java.time.format DateTimeFormatter]))


; TODO:
;   -
;   - top blocker threads
;   - lock type counts (transaction etc)
;   - number of threads
;   - longest running threads
;   - graph extract extra info reaper | socketRead0
; elsewhere:
;   - connection counting servlet url -> [req count, conn count]
;   - long running query sql logging

(def trace-report-limit 250)
(def date-roll-fluff-seconds 5)

; the below two defs define a line based state machine to
; parse jstack output. A state machine approach is a robust
; and transparent way to parse the jstack output

;;; ***********************************************
;;; PARSING STATE MACHINE

;; define common block transitions
(def block-transitions
  ["\t- locked" :locked
   "\t- parking to wait for" :waiting-concurrent
   "\t- waiting on" :waiting-notify
   "\t- waiting to lock" :waiting-synchronized
   "\t- waiting to re-lock" :waiting-re-lock
   "\t- eliminated " :eliminated
   "\tat " :trace-element
   "   No compile task" :no-compile-task
   :empty :block-end])

;; define the main state machine
(def finite-state-machine
  {:start                [:any :prelude]                    ;; :start is a "virtual" state in that there is no matching line
   :prelude              ["\"" :block-start
                          :any :prelude]
   :block-start          ["   java.lang.Thread.State:" :block-second
                          :empty :block-end
                          "\"" :block-start]                ;; special case, a one line block without an empty line after
   :block-second         block-transitions
   :trace-element        block-transitions
   :locked               block-transitions
   :eliminated           block-transitions                  ;; a lock eliminated in the byte code
   :waiting-concurrent   block-transitions
   :waiting-notify       block-transitions
   :waiting-synchronized block-transitions
   :waiting-re-lock      block-transitions
   :no-compile-task      block-transitions
   :block-end            ["\"" :block-start
                          "   Locked ownable" :owned-locks-start
                          "JNI global" :epilogue
                          :empty :block-end]
   :owned-locks-start    ["\t- None" :no-owned
                          "\t- " :owned-lock]
   :no-owned             [:empty :block-end]
   :owned-lock           ["\t- " :owned-lock
                          :empty :block-end]
   :epilogue             [:any :end]
   :end                  nil})

; the finite state machine in a map format, i.e.
; :block-end ({:pattern "\"", :state :block-start}
;             {:pattern "   Locked ownable", :state :owned-locks-start}
;             {:pattern "JNI global", :state :epilogue}
;             {:pattern :empty, :state :block-end})}
(def fsm-mapped
  (reduce-kv
    (fn [a k v]
      (assoc a k (map (fn [[k v]] {:pattern k :state v}) (partition 2 v))))
    {}
    finite-state-machine))

;; we map the different types of lock / wait lines to maps with :type and :wait-type keys
(def dash-types
  {:locked               {:type :locked}                    ; \t- locked <0x000000066c425080>
   :eliminated           {:type :eliminated}                ; a redundant lock eliminated by the jvm
   :waiting-concurrent   {:type      :waiting               ; TIMED_WAITING (parking)
                          :wait-type :concurrent}           ; \t- parking to wait for  <0x0000000645e75218>
   :waiting-synchronized {:type      :waiting               ; BLOCKED (on object monitor)
                          :wait-type :synchronized}         ; \t- waiting to lock <0x00000006ee7545b0>
   :waiting-re-lock      {:type      :waiting
                          :wait-type :re-lock}              ; in notify-wait trying to re-enter the wait section
   :waiting-notify       {:type      :waiting               ; "TIMED_WAITING (on object monitor)"
                          :wait-type :notify}               ; \t- waiting on <0x000000066c425080>
   })

(def thread-states
  {"NEW"           :new                                     ; The thread has not yet started
   "RUNNABLE"      :runnable                                ; The thread is executing in the JVM
   "BLOCKED"       :blocked                                 ; The thread is blocked waiting for a monitor lock
   "WAITING"       :waiting                                 ; The thread is waiting indefinitely for another thread to perform a particular action
   "TIMED_WAITING" :timed-waiting                           ; The thread is waiting for another thread to perform an action for up to a specified waiting time.
   "TERMINATED"    :terminated                              ; The thread has exited
   })

(defn next-state [old-state ^String line]
  (let [transitions (old-state fsm-mapped)
        next-state  (loop [[transition & xs] transitions]
                      (if (not transition)
                        nil
                        (let [pattern (:pattern transition)
                              state   (:state transition)
                              found   (or
                                        (and (instance? String pattern) (str/starts-with? line pattern))
                                        (= :any pattern)
                                        (and (= :empty pattern) (= line "")))]
                          (if found
                            state
                            (recur xs)))))]
    (or next-state :undefined)))

;;; ***********************************************
;;; A few utility methods

(defn color [[& styles] & xs]
  (when (not-empty xs)
    (apply ansi/style (concat [(apply str xs)] styles))))

(defn parse-date-time [str date-pattern]
  (LocalDateTime/parse str (DateTimeFormatter/ofPattern date-pattern)))

(defn parse-time [str time-pattern]
  (LocalTime/parse str (DateTimeFormatter/ofPattern time-pattern)))

(defn date->seconds [^LocalDateTime date]
  (.toEpochSecond date ZoneOffset/UTC))

(defn seconds-between [^LocalDateTime new ^LocalDateTime old]
  (- (date->seconds new) (date->seconds old)))

(defn req-date [t]
  (-> t :request :date))

(defn assoc-if
  "called with two maps, it will assoc the entries from the second
  map into the first map only when the values are non-nil, called with
  one map, it will remove all nil valued entries. Uses sorted maps to
  make resulting structures more readable"
  ([m]
   (assoc-if (sorted-map) m))
  ([m kvs]
   (into (into (sorted-map) m) (filter val kvs))))

;; ***********************************************

(defn first-line-prop
  "a function to extract a property value from the first line
   of a thread block. Example properties: prio=6 tid=0x00007f4e0c0e9800"
  [line name]
  (let [re   (re-pattern (str " " name "=([^ ]+) "))
        vals (re-find re line)]
    (second vals)))

(defn thread-name
  "extract thread name from the first line of a thread block"
  [line]
  (second (re-find #"\"([^\"]+)\"" line)))                  ; first thing within quotes

(defn daemon?
  "extract daemon boolean from the first line of a thread block"
  [line]
  (str/includes? (second (re-find #"\"[^\"]+\" (.*)" line)) " daemon ")) ; not withing the initial quotes, line contains " daemon "

(defn currently
  "extract thread state from the end of the first line of a thread block
   example: ...nid=0x726b in Object.wait()  [0x00007f4e04dbb000] would
   return 'in Object.wait()' from this method"
  [line]
  (str/trim (second (re-find #".*nid=[^ ]+ ([^\[]+)" line))))

(defn id
  "extract the integer thread id (#<number>) from the first line
   of a thread block or nil if no thread id was present"
  [line]
  (some-> (second (re-find #"\"[^\"]+\".* #([0-9]+) " line)) Integer/parseInt))

; https://dzone.com/articles/how-to-read-a-thread-dump
; https://gist.github.com/rednaxelafx/843622
; "Reference Handler" #2 daemon prio=10 os_prio=0 cpu=48.14ms elapsed=268568.18s tid=0x00007f4e7c102000 nid=0x7248 waiting on condition  [0x00007f4e6818f000]
(defn parse-block-first-line
  "Parses the first line of the thread block. The first argument is a map
   which will be used to assoc parsed values into, typically an empty map.
   Example line: '\"RMI TCP Connection(idle)\" daemon prio=10 tid=0x0000000050977800 nid=0x34d waiting on condition [0x00002b7b25bab000]'"
  [line]
  (let [prop (partial first-line-prop line)]
    (assoc-if
      {:NAME      (thread-name line)
       :id        (id line)
       :daemon?   (daemon? line)
       :prio      (some-> (prop "prio") Integer/parseInt)
       :os-prio   (some-> (prop "os_prio") Integer/parseInt)
       :cpu       (prop "cpu")
       :elapsed   (prop "elapsed")
       :tid       (prop "tid")
       :nid       (prop "nid")
       :currently (currently line)
       :lines     [line]})))

;   java.lang.Thread.State:
(defn parse-block-second-line
  "parses the second line of a thread block. The first argument is a map
   which will be used to assoc parsed values into.
   Example line: '   java.lang.Thread.State: WAITING (on object monitor)'"
  [rec line]
  (assoc rec :thread-state (subs line 27)))

(defn parse-trace-element-line
  [line]
  (let [[_ class method file-and-line] (re-find #"\tat ([^(]+)[.]([^(]+)\(([^)]+)\)" line)
        file-and-line (if (#{"Native Method" "Unknown Source" "<generated>"} file-and-line)
                        {:file file-and-line}
                        (let [r (str/split file-and-line #":")]
                          {:file (first r) :line-# (second r)}))]
    {:class  class
     :method method
     :file   (:file file-and-line)
     :line-# (some-> (:line-# file-and-line) Integer/parseInt)}))

(defn parse-trace-element-line-delayed
  [rec line]
  (update rec :trace (fnil conj [])
          {:type    :stack-element
           :line    line
           :details (delay (parse-trace-element-line line))}))

; types
;- locked <0x0000000644ce4f50> (a java.lang.ref.ReferenceQueue$Lock)
;- parking to wait for  <0x00000000a0f42790> (a java.util.concurrent.SynchronousQueue$TransferStack)
;- parking to wait for  <0x00000007da611cb0> (a ext.iplocation.com.google.common.util.concurrent.SettableFuture)
;- waiting on <0x00000000a17769f0> (a java.lang.ref.Reference$Lock)
;- waiting on <no object reference available>
;- waiting to lock <0x0000000629bf6988> (a java.lang.Class for atg.servlet.SessionConfirmationNumberManager)
;- waiting to lock <0x0000000640381bb8> (a org.jboss.virtual.plugins.context.zip.ZipEntryContext)

(defn parse-dashed-line
  "parse one of the '\t- xxx' lines in the thread block stack trace"
  [m state line]
  (let [[_ oid class class-for] (re-find #".*<([^>]+)>(?: \(a ([^ )]+))?(?: for ([^ )]+))?" line)]
    (update m :trace (fnil conj []) (assoc-if
                                      (state dash-types)
                                      {:oid       oid
                                       :class     class
                                       :class-for class-for
                                       :line      line}))))

(defn parse-block-line [rec state line]
  (case state
        :block-second (parse-block-second-line rec line)
        :trace-element (parse-trace-element-line-delayed rec line)
        ;;:owned-locks-start (update rec :lines conj "")
        :locked (parse-dashed-line rec state line)
        (:waiting-concurrent
          :waiting-notify
          :waiting-synchronized
          :waiting-re-lock) (parse-dashed-line rec state line)
        rec))

(defn display-duration [seconds]
  (let [zf (fn [n u] (if (= n 0) "" (str n u)))
        h  (int (/ seconds 3600))
        m  (int (/ (mod seconds 3600) 60))
        s  (mod seconds 60)]
    (str (zf h "h") (zf m "m") (format "%02ds" s))))

(defn decorate-dump-date [dump first-line]
  (let [matches      (re-matches #"\d\d\d\d-\d\d-\d\d \d\d:\d\d:\d\d" first-line)
        date-pattern "yyyy-MM-dd HH:mm:ss"]
    (if matches
      (assoc dump :date (parse-date-time first-line date-pattern))
      dump)))

(defn parse-line [m state line line-#]
  (case state
        :prelude (cond-> m
                         (empty? (:prelude m)) (decorate-dump-date line)
                         true (update :prelude conj line))
        :epilogue (update m :epilogue conj line)
        :undefined (throw (ex-info (str "error on line "
                                        line-#
                                        " - no state transition defined for line:\n"
                                        line)
                                   {:line#         line-#
                                    :line          line
                                    :current-state state}))
        :block-start (update m :threads conj (parse-block-first-line line)) ;; add new thread in vec
        ;; default
        (update-in m
                   [:threads (-> m :threads count dec)]     ;; update last thread in vec
                   #(parse-block-line % state line))))

(defn parse-jstack-lines
  "main parsing method of this namespace, parses out the base
  structure and details from the jstack lines. The dump function
  will call this and decorate in a few extra properties for post
  processing"
  [lines]
  (loop [m          {:prelude [] :threads [] :epilogue []}
         prev-state :start
         line-#     1
         [line & xs] lines]
    (let [state (next-state prev-state line)]
      (if (not= state :end)
        (recur
          (parse-line m state line line-#)
          state
          (inc line-#)
          xs)
        m))))

(defn remove-waiting-on-lock [locks t]
  (let [result (filter (fn [lock] (not= (:oid lock) (:oid t))) locks)]
    (if (empty? result) nil result)))

(defn extract-locks-and-wait
  "based on the parsed trace data for a thread, determines two properties
   for the thread: what (if anything) it is waiting on and what locks it
   is holding on to."
  [thread]
  (reduce
    (fn [[locks wait-oid] te]
      (cond
        (and locks
             (= (:wait-type te) :notify)) [(remove-waiting-on-lock locks te) wait-oid]
        (#{:concurrent
           :synchronized
           :re-lock} (:wait-type te)) [locks {:oid (:oid te) :class (:class te) :wait-type (:wait-type te)}]
        (= (:type te) :locked) [((fnil conj []) locks {:oid   (:oid te)
                                                       :class (:class te)}) wait-oid]
        :else [locks wait-oid]))
    [nil nil]
    (-> thread :trace reverse)))

(defn reconcile-locks
  "post processing for a thread, walks through the trace of the thread and
   assocs in two extra keys :locked [{:oid oid :class fqcn} ...] and
   :waiting-on {:oid od :class name} where the :locked vector is in temporal
   lock order, i.e. the first element in the vector was locked first"
  [thread]
  (let [[locks waiting-on] (extract-locks-and-wait thread)]
    (assoc-if thread {:locked     locks
                      :waiting-on waiting-on})))

(defn thread-date
  "given a dump date as defined by the first line in the dump and
  a time string from the thread name (format 025902.625 - HHmmSS.SSS),
  parses the thread time into a LocalDateTime instance. Note that this
  involves figuring out what day the time string belongs to as the time
  string contains no date information"
  [dump-date time-str]
  (when time-str
    (let [one-day    (Duration/ofDays 1)
          fluff      (Duration/ofSeconds date-roll-fluff-seconds)
          fluff-date (.plus dump-date fluff)
          time       (parse-time time-str "HHmmss.SSS")
          date       (.atDate time (.toLocalDate dump-date))]
      (if (.isAfter date fluff-date)                        ;; we don't store dates in thread names, handle rolling
        (.minus date one-day)
        date))))

(defn thread-name-part [token]
  (fn [part]
    (let [[_ lhs rhs] (re-find #"([^=]+)=([^=]+)" part)]
      (when (= lhs token) rhs))))

(defn parse-thread-name [name]
  (let [parts   (str/split name #"\|")
        extract (fn [token] (first (keep (thread-name-part token) parts)))]
    (when (< 1 (count parts))
      {:pre  (first parts)
       :time (nth parts 1)
       :cid  (extract "cid")
       :rid  (extract "rid")
       :oip  (extract "oip")
       :url  (last parts)})))

; ("ajp"
; "125034.018"
; "cid=lU8ZNBjPdX"
; "rid=lY6aupnv3Z"
; "oip=47.39.173.80"
; "g/commerce/locations/StoreLocatorActor/locateItems")
(defn decorate-request-thread
  "for request threads with the new improved thread naming convention
   ajp|025902.625|cid=<x>|rid=<y>|<req url>, decorates in key :request
   into the thread map containing parsed out properties for the request"
  [dump-date thread]
  (let [{:keys [pre time cid rid oip url]} (parse-thread-name (:NAME thread))]
    (if (or (= pre "ajp") (= pre "http"))
      (assoc thread :request (into (sorted-map)
                                   {:time time
                                    :date (thread-date dump-date time)
                                    :cid  cid
                                    :rid  rid
                                    :oip  oip
                                    :url  url}))
      thread)))

(defn decorate-thread-age [newest-date threads]
  (let [age-secs (fn [t] (seconds-between newest-date (req-date t)))]
    (map
      (fn [t]
        (if (req-date t)
          (let [secs (age-secs t)]
            (-> t
                (assoc-in [:request :age-seconds] secs)
                (assoc-in [:request :display-age] (display-duration secs))))
          t))
      threads)))

(defn decorate-request-threads
  "decorates request threads with a few keys based on the information
  in the thread name"
  [dump]
  (let [req-decorator (partial decorate-request-thread (:date dump))
        dump          (update dump :threads #(map req-decorator %))
        req-threads   (sort-by req-date (filter req-date (:threads dump)))
        newest-date   (req-date (last req-threads))]
    (update dump :threads (partial decorate-thread-age newest-date))))

(defn extract-lines
  "given a line source (String, File, Reader, seq, ...) returns a lazy
  seq of lines which will be used to parse the jstack data"
  [line-source]
  (cond
    (seq? line-source) line-source
    (instance? File line-source) (str/split-lines (slurp line-source))
    (string? line-source) (str/split-lines (slurp (jio/file line-source)))
    (instance? Reader line-source) (line-seq (BufferedReader. line-source))
    :else (throw (ex-info (str "unknown line source: " line-source) {:class (class line-source)}))))

(defn source-type
  "dispatch function for the dump multi-method, returns a keyword
  indicating the type of line-source sent in to the dump function"
  [line-source]
  (cond
    (seq? line-source) :seq
    (vector? line-source) :seq
    (instance? File line-source) :file
    (string? line-source) :string
    (instance? Reader line-source) :reader
    :else (throw (ex-info (str "unknown line source: " line-source)
                          {:class (class line-source)}))))

(defmulti
  dump
  "main entry point to this namespace. Given a line source (sequence of lines,
   string which is either a path to a file or the data, File, or Reader) with a set
   of lines from a jstack thread dump, produces a clojure data structure representing
   the thread dump. Example usage: (def my-dump (dump (\"dump.txt\")))."
  {:arglists '[[line-source]]}
  source-type)

(defmethod dump :file
  [file]
  (with-open [rdr (jio/reader file)]
    (dump rdr)))

(defmethod dump :string
  [str]
  (with-open [rdr (jio/reader str)]
    (dump rdr)))

(defmethod dump :reader
  [reader]
  (if (instance? BufferedReader reader)
    (dump (line-seq reader))
    (dump (line-seq (BufferedReader. reader)))))

(defmethod dump :seq
  [lines]
  (-> lines
      (parse-jstack-lines)
      (update :threads #(map reconcile-locks %))
      (decorate-request-threads)))

;;
;; ANALYSIS FUNCTIONS
;; the below functions are used for analysing and working with
;; a dump as produced by the dump function above
;;
(defn threads-by-tid
  "returns a map {tid thread ...} where thread is the
  map structure representing a thread in the dump. The argument
  dump is a map as returned by the dump function"
  [dump]
  (reduce
    (fn [a t] (assoc a (:tid t) t))
    (sorted-map)
    (:threads dump)))

(defn threads-by-name
  "returns a map {thread-name thread ...} where thread is the
  map structure representing a thread in the dump. The argument
  dump is a map as returned by the dump function"
  [dump]
  (reduce
    (fn [a t] (assoc a (:NAME t) t))
    (sorted-map)
    (:threads dump)))

(defn lockers-by-oid
  "returns a map {<locked oid> <thread> ...} where oid is the locked object
  id and thread is the thread holding the lock. The argument dump is a map
  as returned by the dump function."
  [dump]
  (reduce
    (fn [a t]
      (if (:locked t)
        (reduce
          (fn [a2 {:keys [oid]}] (assoc a2 oid t))
          a
          (:locked t))
        a))
    {}
    (:threads dump)))

(defn waiters-by-tid
  "returns a map {tidA {:tid tidB :oid oidB} ...} where thread A is waiting
  on a lock on object oidB held by thread tidB. The argument dump is a map as
  returned by the dump function"
  [dump]
  (let [lockers (lockers-by-oid dump)]
    (reduce
      (fn [a t]
        (let [waiting-on-oid (-> t :waiting-on :oid)
              waiting-on-tid (:tid (get lockers waiting-on-oid))]
          (if waiting-on-tid
            (assoc a (:tid t) {:tid waiting-on-tid :oid waiting-on-oid})
            a)))
      (sorted-map)
      (filter :waiting-on (:threads dump)))))

(defn waiters-by-oid
  "returns a map {oidA #{tidA tidB} ...} where thread A and thread A are waiting
  on a lock on object oidA. The argument dump is a map as returned by the dump function"
  [dump]
  (reduce
    (fn [a t]
      (update a (-> t :waiting-on :oid) (fnil conj #{}) (:tid t)))
    (sorted-map)
    (filter :waiting-on (:threads dump))))

(defn transitive-path
  "given a map of waiters as returned by waiters-by-tid and a thread
  t (tidC), returns a path [tidA tidB tidC] where thread C is waiting
  for a lock held by thread B, thread B is waiting for a lock
  held by thread A etc exhaustively until the chain completes"
  [waiters t]
  (loop [tid (:tid t) p []]
    (let [locker (get waiters tid)]
      (if (not locker)
        (vec (reverse p))
        (recur (:tid locker) (conj p locker))))))

(defn keys-in [m]
  (if (map? m)
    (vec
      (mapcat (fn [[k v]]
                (let [sub    (keys-in v)
                      nested (map #(into [k] %) (filter (comp not empty?) sub))]
                  (if (seq nested)
                    nested
                    [[k]])))
              m))
    []))

(defn key-count-in [m]
  (if (map? m)
    (count (distinct (flatten (keys-in m))))
    0))

(defn sorted-map-by-subtree [m key-comp-fn]
  (into (sorted-map-by
          (fn [key1 key2]
            (compare [(key-count-in (get m key2)) (key-comp-fn key1)]
                     [(key-count-in (get m key1)) (key-comp-fn key2)])))
        m))

(defn transitive-lock-graph
  "returns a map {tidA {tidB {tidC nil tidD nil}}} where
  threads C and D are waiting for a lock held by thread B and
  thread B is waiting on a lock held by thread A. Note that only
  string tid values are present in the returned structure. To look
  up the thread structure for a tid use function threads-by-tid. To
  look up which oid tidD is waiting for from tidB, look up threadD and do
  (-> threadD :waiting-on :oid), this works as any thread can at most
  wait for one oid"
  [dump]
  (let [waiters-by-tid (waiters-by-tid dump)
        waiters-by-oid (waiters-by-oid dump)
        threads        (filter :waiting-on (:threads dump))
        paths          (distinct (map #(transitive-path waiters-by-tid %) threads))
        sort-graph     (fn [m] (sorted-map-by-subtree m #(vector (:tid %) (:oid %))))]
    (sort-graph
      (reduce
        (fn [a path]
          (reduce
            (fn [a2 waiter-tid]
              (let [keys        (keys (get-in a2 path))
                    tid-exists? (first (filter #(= (:tid %) waiter-tid) keys))]
                (if tid-exists?
                  a2
                  (assoc-in a2 (conj path {:tid waiter-tid}) nil))))
            a
            (get waiters-by-oid (:oid (last path)))))
        {}
        (reverse (sort-by count paths))))))

(defn render-tree
  ([key val]
   (render-tree str compare key val))
  ([render-fn key-comp-f key val]
   (let [graph-color [:magenta]
         I-short     (color graph-color "│ ")
         I-branch    (color graph-color "│   ")
         T-branch    (color graph-color "├── ")
         L-branch    (color graph-color "└── ")
         spacer      "    "
         pre         (if (pos? (count val)) I-short "")
         label       (render-fn key val)
         label       (cons (first label) (map #(str pre %) (rest label)))]
     (concat label
             (mapcat
               (fn [[c-key c-val] index]
                 (let [subtree      (render-tree render-fn key-comp-f c-key c-val)
                       last?        (= index (dec (count val)))
                       prefix-first (if last? L-branch T-branch)
                       prefix-rest  (if last? spacer I-branch)]
                   (cons (str prefix-first (first subtree))
                         (map
                           #(str prefix-rest %)
                           (next subtree)))))
               (into (sorted-map-by key-comp-f) val)
               (range))))))

(defn short-name [fqn]
  (last (re-seq #"[^.]+" fqn)))

(defn trace-has? [t [class method]]
  (let [exists? (fn [p coll] (seq (filter p coll)))
        match?  (fn [e] (and (= (some-> e :details deref :class) class)
                             (= (some-> e :details deref :method) method)))]
    (exists? match? (:trace t))))

(defn tx-reaper? [t]
  (let [reaper-run ["com.arjuna.ats.internal.arjuna.coordinator.ReaperWorkerThread" "run"]]
    (trace-has? t reaper-run)))

(defn db-socket-read?
  "returns true if a thread is in a socketRead0 call to the database"
  [t]
  (let [socket-read ["java.net.SocketInputStream" "socketRead0"]
        oracle-read ["oracle.jdbc.driver.T4CSocketInputStreamWrapper" "read"]]
    (and (trace-has? t socket-read)
         (trace-has? t oracle-read))))

(defn db-socket-read-is-valid?
  "returns true if a thread is in a socketRead0 call to the database
  due to a isValidConnection 'select 1 from dual' call"
  [t]
  (let [valid-connection ["org.jboss.resource.adapter.jdbc.CheckValidConnectionSQL" "isValidConnection"]]
    (and (db-socket-read? t)
         (trace-has? t valid-connection))))

(defn thread-display-age [t]
  (some->> t :request :display-age (str "age ")))

(defn thread-extra-info [t]
  (cond
    (tx-reaper? t) "[jboss tx reaper thread]"
    (db-socket-read-is-valid? t) "[db socketRead0 isValid]"
    (db-socket-read? t) "[db socketRead0]"

    (< trace-report-limit (count (:trace t))) (str "[" (count (:trace t)) " line trace]")
    :else nil))

(defn render-graph-node [threads-by-tid k m]
  (let [thread      (get threads-by-tid (:tid k))
        second?     (pos? (count m))
        class       (first (keep (fn [{:keys [oid class]}] (when (= oid (:oid k)) class)) (:locked thread)))
        b-count     (key-count-in m)
        fg-normal   [:green]
        fg-bright   [:bright :cyan]
        fg-extra    [:bright :black]
        extra       (color fg-extra (thread-extra-info thread))
        age         (color fg-normal (thread-display-age thread))
        second-line (when second?
                      (str
                        (color fg-normal "tid " (:tid thread) " locked ")
                        (color fg-bright (short-name class))
                        (color fg-normal " " (:oid k) " - ")
                        (color fg-bright "blocks " b-count " threads")))]
    (cond-> [(str/join " " [(:NAME thread) age extra])]
            second? (conj second-line))))

(defn render-lock-graph
  ""
  ([dump]
   (render-lock-graph dump (transitive-lock-graph dump)))
  ([dump graph]
   (let [threads-by-tid (threads-by-tid dump)
         render-fn      (partial render-graph-node threads-by-tid)
         name           (fn [tid] (:NAME (get threads-by-tid tid)))
         key-comp-f     (fn [a b]
                          (let [nc (compare (name (:tid a)) (name (:tid b)))]
                            (if (zero? nc) (compare (:oid a) (:oid b)) nc)))]
     (mapcat (fn [[k v]] (render-tree render-fn key-comp-f k v)) graph))))

(defn print-lock-graph [dump]
  (let [graph (transitive-lock-graph dump)
        lines (render-lock-graph dump)
        count (key-count-in graph)]
    (if (empty? graph)
      (println "No transitive lock chains detected")
      (do
        (println (color [:white] "TRANSITIVE LOCK GRAPH (" count " threads)"))
        (println "")
        (doseq [line lines]
          (println line))))))

(defn print-oldest-threads [dump threads-to-print]
  (let [threads (sort-by req-date (filter req-date (:threads dump)))]
    (when (not-empty threads)
      (println "")
      (println (color [:white] threads-to-print " OLDEST REQUEST THREADS"))
      (println "")
      (doseq [t (take threads-to-print threads)]
        (println (color [:green] (format "%10s" (-> t :request :display-age))) (:NAME t))))))

(defn print-youngest-threads [dump threads-to-print]
  (let [threads (sort-by req-date (filter req-date (:threads dump)))]
    (when (not-empty threads)
      (println "")
      (println (color [:white] threads-to-print " YOUNGEST REQUEST THREADS"))
      (println "")
      (doseq [t (reverse (take-last threads-to-print threads))]
        (println (color [:green] (format "%10s" (-> t :request :display-age))) (:NAME t))))))

(defn print-clients-with-most-requests [dump cids-to-print]
  (let [cid-f   (fn [t] (-> t :request :cid))
        threads (filter cid-f (:threads dump))
        groups  (group-by cid-f threads)
        sorted  (sort-by
                  (fn [[_ v]] (- (count v)))
                  (keep (fn [[k v]]
                          (when (< 1 (count v)) [k v]))
                        groups))
        top-x   (take cids-to-print sorted)]
    (when (not-empty top-x)
      (println "")
      (println (color [:white] "TOP " cids-to-print " CLIENT IDS WITH MOST REQUESTS"))
      (println "")
      (doseq [[cid threads] top-x]
        (println (color [:green] "  cid " cid " - " (count threads) " threads"))
        (doseq [t (sort-by (fn [t] (- (-> t :request :age-seconds))) threads)]
          (println "     "
                   (color [:cyan] (format "%-10s" (str "age " (format "%6s" (-> t :request :display-age)))))
                   (:NAME t)))))))

(defn print-longest-traces [dump threads-to-print]
  (let [threads (sort-by (fn [t] (- (count (:trace t)))) (:threads dump))
        top-x   (take threads-to-print threads)]
    (when (not-empty top-x)
      (println "")
      (println (color [:white] "TOP " threads-to-print " THREADS WITH LONGEST TRACES"))
      (println "")
      (doseq [t top-x]
        (println (color [:green] (format "    %4s lines" (count (:trace t)))) "  " (:NAME t))))))

(defn print-most-requested-urls [dump threads-to-print]
  (let [url-f   (fn [t] (-> t :request :url))
        threads (filter url-f (:threads dump))
        groups  (group-by url-f threads)
        sorted  (sort-by
                  (fn [[_ v]] (- (count v)))
                  (keep (fn [[k v]]
                          (when (< 1 (count v)) [k v]))
                        groups))
        top-x   (take threads-to-print sorted)]
    (when (not-empty top-x)
      (println "")
      (println (color [:white] "TOP " threads-to-print " REQUESTED URLS"))
      (println "")
      (doseq [[url threads] top-x]
        (println "    " (color [:green] (format "%3d" (count threads)) " threads ") url))
      (println ""))))

(defn print-threads-in-db-socket-read [dump]
  (let [threads (filter db-socket-read? (:threads dump))
        count   (count threads)]
    (println "")
    (println (color [:white] "THREADS WAITING ON DB IN SocketRead0 (" count " threads)"))
    (println "")
    (if (not-empty threads)
      (doseq [t (sort-by :NAME threads)]
        (let [age     (or (-> t :request :display-age) "")
              isValid (if (db-socket-read-is-valid? t)
                        (color [:bright :black] "[in isValid]")
                        "")]
          (println "     "
                   (color [:green] (format "%10s" age))
                   (:NAME t)
                   isValid)))
      (println "   No threads in db socketRead0 detected!"))
    (println "")))

(defn print-stats [dump]
  (let [threads      (:threads dump)
        count-by     (fn [p] (count (filter p threads)))
        trace-pred   (fn [t] (< trace-report-limit (count (:trace t))))
        display-date (fn [d] (.format (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss") d))
        lock-waits   (count-by (fn [t] (= (-> t :waiting-on :wait-type) :synchronized)))
        fg           [:green]]
    (println "")
    (println (ansi/style "STATISTICS" :white))
    (println "")
    (println "jstack dump date:          " (color fg (display-date (:date dump))))
    (println "total threads:             " (color fg (count threads)))
    (println "threads waiting for locks: " (color fg lock-waits))
    (println "request threads:           " (color fg (count-by :request)))
    (println "traces >" trace-report-limit "lines:        " (color fg (count-by trace-pred)))
    (println "")))

(defn longest-common-prefix
  "given two collections, returns a vector containing the
  common prefix of the collections, i.e. the values at the
  start of coll-a and coll-b which are equal"
  [coll-a coll-b]
  (reduce
    (fn [v [a b]]
      (if (= a b) (conj v a) (reduced v)))
    []
    (map vector coll-a coll-b)))

(defn threads-by-common-trace [threads]
  (loop [a {} [t & ts] threads]
    (prn :size (count a) :tid (:tid t))
    (if t
      (recur
        (reduce
          (fn [a2 t2]
            (let [c (longest-common-prefix (map :line (:trace t))
                                           (map :line (:trace t2)))]
              (if (not-empty c)
                (update a2 c (fnil conj #{}) (:tid t) (:tid t2))
                a2)))
          a
          ts)
        ts)
      a)))


(defn report [dump]
  (let [header-fg [:bright :white]]
    (println "")
    (println (color header-fg "************ THREAD DUMP REPORT ************"))
    (print-stats dump)
    (print-lock-graph dump)
    (print-oldest-threads dump 10)
    (print-youngest-threads dump 10)
    (print-clients-with-most-requests dump 5)
    (print-longest-traces dump 10)
    (print-most-requested-urls dump 10)
    (print-threads-in-db-socket-read dump)
    (println (color header-fg "********************************************"))))

(defn jstack-report [opts]
  (let [source (or (:file opts) *in*)]
    (with-open [reader (jio/reader source)]
      (binding [ansi/*use-ansi* (not (:no-color opts))]
        (if (.ready reader)
          (report (dump reader))
          (println "no lines - skipping report (-h for help)"))))))

;(tufte/add-basic-println-handler! {})
;(defn profiled-report [dump-file]
;  (profile {}
;    (let [dump   (p :dump (dump dump-file))
;          report (p :report (report dump))])))

;; transitive lock graph
;; dump time
;; total number of threads
;; number of blocked threads
;; number of request threads
;; oldest threads with age
;;
;;   - top blocker threads
;   - lock type counts (transaction etc)
;   - number of threads
;   - longest running threads
;   - graph extract extra info reaper | socketRead0
