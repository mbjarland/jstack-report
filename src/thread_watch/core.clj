(ns thread-watch.core
  (:require [clojure.string :as str]
            [clojure.java.io :as jio]
            [thread-watch.ansi :as ansi])
  (:import [java.time.format DateTimeFormatter]
           [java.time LocalTime LocalDateTime ZoneOffset Duration]
           [java.io File]))

; namespace: jstack-tools
;
; This namespace contains utilities for dealing with
; thread dump output produced by the java jstack tool.
;
; The main entry point in this namespace is the parse-jstack
; function which takes as input a sequence of lines and
; returns as output a clojure map which contains the jstack
; output in a clojure data structure.
;
; A typical usage (from babashka) might look like this:
;
;   #!/usr/bin/env bb -i -o --classpath "bb-common"
;   (ns user (:require [jstack-tools :as jt]))
;
;   (jt/parse-jstack *in*)
;
; where *in* is bound by babashka to a sequence of input
; lines.

;; the below two defs define a line based state machine to
;; parse jstack output. A state machine approach is a robust
;; and transparent way to parse the jstack output

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

(def trace-limit 250)
(def date-roll-fluff-seconds 5)

;; define common block transitions
(def block-transitions
  ["\t- locked" :locked
   "\t- parking to wait for" :waiting-concurrent
   "\t- waiting on" :waiting-notify
   "\t- waiting to lock" :waiting-synchronized
   "\t- waiting to re-lock" :waiting-re-lock
   "\tat " :trace-element
   "   No compile task" :no-compile-task
   :empty :block-end])

;; define the main state machine
(def finite-state-machine
  {:start                [:any :prelude]                    ;; :start is a "virtual" state in that there is no matching line
   :prelude              ["\"" :block-start
                          :any :prelude]
   :block-start          ["   java.lang.Thread.State:" :block-second
                          :empty :block-end]
   :block-second         block-transitions
   :trace-element        block-transitions
   :locked               block-transitions
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

;; we map the the different types of lock / wait lines to maps with :type and :wait-type keys
(def dash-types
  {:locked               {:type :locked}                    ; \t- locked <0x000000066c425080>
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

(defn next-state [state line]
  (let [transitions (state finite-state-machine)
        [new-state _] (keep
                       (fn [[pattern state]]
                         (cond
                           (= :any pattern) state
                           (and (= :empty pattern)
                                (= (count line) 0)) state
                           (and (string? pattern)
                                (str/starts-with? line pattern)) state
                           :else nil))
                       (partition 2 transitions))]
    (or new-state :undefined)))

(defn emphasis [[& styles] & xs]
  (apply ansi/style (concat [(apply str xs)] styles)))

(defn assoc-if
  "called with two maps, it will assoc the entries from the second
  map into the first map only when the values are non-nil, called with
  one map, it will remove all nil valued entries. Uses sorted maps to
  make resulting structures more readable"
  ([m]
   (assoc-if (sorted-map) m))
  ([m kvs]
   (into (into (sorted-map) m) (filter val kvs))))

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
(defn parse-block-first-line [line]
  "Parses the first line of the thread block. The first argument is a map
   which will be used to assoc parsed values into, typically an empty map.
   Example line: '\"RMI TCP Connection(idle)\" daemon prio=10 tid=0x0000000050977800 nid=0x34d waiting on condition [0x00002b7b25bab000]'"
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

(defn parse-block-second-line [rec line]
  "parses the second line of a thread block. The first argument is a map
   which will be used to assoc parsed values into.
   Example line: '   java.lang.Thread.State: WAITING (on object monitor)'"
  (let [[_ state] (re-find #"   java.lang.Thread.State: (.+)" line)]
    (assoc rec :thread-state state)))

(defn parse-trace-element-line
  [rec line]
  (let [[_ class method file-and-line] (re-find #"\tat ([^(]+)[.]([^(]+)\(([^)]+)\)" line)
        [file line-#] (if (#{"Native Method" "Unknown Source" "<generated>"} file-and-line)
                        [file-and-line nil]
                        (str/split file-and-line #":"))]
    (update rec :trace (fnil conj []) (assoc-if {:type   :stack-element
                                                 :class  class
                                                 :method method
                                                 :file   file
                                                 :line-# (some-> line-# Integer/parseInt)
                                                 :line   line}))))

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
    (update m :trace (fnil conj []) (assoc-if (state dash-types)
                                              {:oid       oid
                                               :class     class
                                               :class-for class-for
                                               :line      line}))))

(defn parse-block-line [rec state line]
  (case state
        :block-second (parse-block-second-line rec line)
        :trace-element (parse-trace-element-line rec line)
        ;;:owned-locks-start (update rec :lines conj "")
        :locked (parse-dashed-line rec state line)
        (:waiting-concurrent
         :waiting-notify
         :waiting-synchronized
         :waiting-re-lock) (parse-dashed-line rec state line)
        rec))

(defn parse-and-append [rec state line]
  (-> rec
      (parse-block-line state line)
      (update :lines conj line)))

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
      (assoc dump :date (LocalDateTime/parse first-line (DateTimeFormatter/ofPattern date-pattern)))
      dump)))

(defn parse-line [m state line line-#]
  (case state
        :prelude (cond-> m
                         (empty? (:prelude m)) (decorate-dump-date line)
                         true (update :prelude conj line))
        :epilogue (update m :epilogue conj line)
        :undefined (throw (ex-info "Encountered line with no defined state transition"
                                   {:line#         line-#
                                    :line          line
                                    :current-state state}))
        :block-start (update m :threads conj (parse-block-first-line line)) ;; add new thread in vec
        ;; default
        (update-in m
                   [:threads (-> m :threads count dec)]     ;; update last thread in vec
                   #(parse-and-append % state line))))

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
        (recur (parse-line m state line line-#)
               state
               (inc line-#)
               xs)
        m))))

(defn remove-waiting-on-lock [locks t]
  (let [result (filter (fn [lock] (not= (:oid lock) (:oid t))) locks)]
    (if (empty? result) nil result)))

(defn extract-locks-and-wait [thread]
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
  "post procesing for a thread, walks through the trace of the thread and
   assocs in two extra keys :locked [{:oid oid :class fqcn} ...] and
   :waiting-on {:oid od :class name} where the :locked vector is in temporal
   lock order, i.e. the first element in the vector was locked first"
  [thread]
  (let [[locks waiting-on] (extract-locks-and-wait thread)]
    (assoc-if thread {:locked     locks
                      :waiting-on waiting-on})))

(defn thread-date [dump-date time-str]
  (when time-str
    (let [one-day    (Duration/ofDays 1)
          fluff      (Duration/ofSeconds date-roll-fluff-seconds)
          fluff-date (.plus dump-date fluff)
          time       (LocalTime/parse time-str (DateTimeFormatter/ofPattern "HHmmss.SSS"))
          date       (.atDate time (.toLocalDate dump-date))]
      (if (.isAfter date fluff-date)                        ;; we don't store dates in thread names, handle rolling
        (.minus date one-day)
        date))))

(defn decorate-request-thread [dump-date thread]
  (let [pattern #"([^\\|]+)\|([^\\|]+)\|cid=([^\\|]+)\|rid=([^\\|]+)\|([^\\|]+)"
        [_ pre time cid rid url] (re-matches pattern (:NAME thread))]
    (if (or (= pre "ajp") (= pre "http"))
      (assoc thread :request (into (sorted-map)
                                   {:time time
                                    :date (thread-date dump-date time)
                                    :cid  cid
                                    :rid  rid
                                    :url  url}))
      thread)))

(defn date->seconds [^LocalDateTime date]
  (.toEpochSecond date ZoneOffset/UTC))

(defn seconds-between [^LocalDateTime new ^LocalDateTime old]
  (- (date->seconds new) (date->seconds old)))

(defn decorate-thread-age [newest-date threads]
  (let [age-secs (fn [t] (seconds-between newest-date (-> t :request :date)))]
    (map
     (fn [t]
       (if (-> t :request :date)
         (-> t
             (assoc-in [:request :age-seconds] (age-secs t))
             (assoc-in [:request :display-age] (display-duration (age-secs t))))
         t))
     threads)))

(defn decorate-request-threads [dump]
  (let [decorator   (partial decorate-request-thread (:date dump))
        dump        (update dump :threads #(map decorator %))
        req-date    (fn [t] (-> t :request :date))
        req-threads (sort-by req-date (filter req-date (:threads dump)))
        newest-date (req-date (last req-threads))]
    (update dump :threads (partial decorate-thread-age newest-date))))

(defn extract-lines [line-source]
  (cond
    (seq? line-source) line-source
    (instance? File line-source) (str/split-lines (slurp line-source))
    (string? line-source) (str/split-lines (slurp (jio/file line-source)))
    :else (throw (ex-info (str "unknown line source: " line-source) {:class (class line-source)}))))

(defn dump
  "main entry point to this namespace. Given a seq of lines from a jstack thread
  dump, produces a clojure data structure representing the thread dump. Example
  usage: (def my-dump (dump (str/split-lines (slurp \"dump.txt\"))))."
  [line-source]
  (-> (extract-lines line-source)
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
        (fn [a2 {:keys [oid]}] (assoc a2 oid t))            ;;@@@
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
        paths          (distinct (map #(transitive-path waiters-by-tid %) threads))]
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
     (reverse (sort-by count paths)))))

(defn render-tree
  ([key val]
   (render-tree str compare key val))
  ([render-fn key-comp-f key val]
   (let [graph-color [:magenta]
         I-short     (emphasis graph-color "│ ")
         I-branch    (emphasis graph-color "│   ")
         T-branch    (emphasis graph-color "├── ")
         L-branch    (emphasis graph-color "└── ")
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

(defn short-name [fqn]
  (last (re-seq #"[^.]+" fqn)))

(defn thread-extra-info [t]
  (let [exists?     (fn [p coll] (not (empty? (filter p coll))))
        match?      (fn [[c m]] (fn [e] (and (= (:class e) c) (= (:method e) m))))
        trace       (:trace t)
        trace-has?  (fn [v] (exists? (match? v) trace))
        reaper-run  ["com.arjuna.ats.internal.arjuna.coordinator.ReaperWorkerThread" "run"]
        socket-read ["java.net.SocketInputStream" "socketRead0"]
        oracle-read ["oracle.jdbc.driver.T4CSocketInputStreamWrapper" "read"]
        tx-reaper?  (trace-has? reaper-run)
        db-read?    (and (trace-has? socket-read) (trace-has? oracle-read))
        result      (cond
                      tx-reaper? "[jboss tx reaper thread]"
                      db-read? "[in db socketRead0]"
                      (< trace-limit (count (:trace t))) (str " [" (count (:trace t)) " line trace]")
                      :else nil)
        thread-age  (-> t :request :display-age)
        display-age (if thread-age (str "age " thread-age) "")]
    (if (or result display-age)
      (str " " (emphasis [:magenta] display-age) (emphasis [:bright :black] result))
      nil)))

(defn render-graph-node [threads-by-tid k m]
  (let [thread      (get threads-by-tid (:tid k))
        second?     (pos? (count m))
        class       (first (keep (fn [{:keys [oid class]}] (when (= oid (:oid k)) class)) (:locked thread)))
        b-count     (count (distinct (flatten (keys-in m))))
        extra       (thread-extra-info thread)
        normal      [:green]
        bright      [:bright :cyan]
        second-line (when second?
                      (str
                       (emphasis normal "tid " (:tid thread) " locked ")
                       (emphasis bright (short-name class))
                       (emphasis normal " " (:oid k) " - ")
                       (emphasis bright "blocks " b-count " threads")))]
    (cond-> [(str (:NAME thread) extra)]
            second? (conj second-line))))

(defn render-lock-graph
  ""
  [dump]
  (let [graph          (transitive-lock-graph dump)
        threads-by-tid (threads-by-tid dump)
        render-fn      (partial render-graph-node threads-by-tid)
        name           (fn [tid] (:NAME (get threads-by-tid tid)))
        key-comp-f     (fn [a b]
                         (let [nc (compare (name (:tid a)) (name (:tid b)))]
                           (if (zero? nc) (compare (:oid a) (:oid b)) nc)))]
    (mapcat (fn [[k v]] (render-tree render-fn key-comp-f k v)) graph)))

(defn print-lock-graph
  [dump]
  (let [graph (render-lock-graph dump)]
    (if (empty? graph)
      (println "No transitive lock chains detected")
      (doseq [line (render-lock-graph dump)]
        (println line)))))

(defn print-oldest-threads [dump threads-to-print]
  (let [thread-date (fn [t] (-> t :request :date))
        threads     (sort-by thread-date (filter thread-date (:threads dump)))]
    (when (not-empty threads)
      (println "")
      (println (emphasis [:white] threads-to-print " OLDEST REQUEST THREADS"))
      (println "")
      (doseq [t (take threads-to-print threads)]
        (println (emphasis [:green] (format "%10s" (-> t :request :display-age))) (:NAME t))))))

(defn print-cids-with-multiple-requests [dump cids-to-print]
  (let [cid-f   (fn [t] (-> t :request :cid))
        threads (filter cid-f (:threads dump))
        groups  (group-by cid-f threads)
        sorted  (sort-by
                 (fn [[k v]] (- (count v)))
                 (keep (fn [[k v]]
                         (when (< 1 (count v)) [k v]))
                       groups))
        top-x   (take cids-to-print sorted)]
    (when (not-empty top-x)
      (println (emphasis [:white] "TOP " cids-to-print " CLIENT IDS WITH MULTIPLE REQUESTS"))
      (println "")
      (doseq [[cid threads] top-x]
        (println (emphasis [:green] "  cid " cid " - " (count threads) " threads"))
        (doseq [t (sort-by (fn [t] (- (-> t :request :age-seconds))) threads)]
          (println "     "
                   (emphasis [:cyan] (format "%-10s" (str "age " (format "%6s" (-> t :request :display-age)))))
                   (:NAME t)))))))

(defn print-threads-with-longest-traces [dump threads-to-print]
  (let [threads (sort-by (fn [t] (- (count (:trace t)))) (:threads dump))
        top-x   (take threads-to-print threads)]
    (when (not-empty top-x)
      (println (emphasis [:white] "TOP " threads-to-print " THREADS WITH LONGEST TRACES"))
      (println "")
      (doseq [t top-x]
        (println (emphasis [:green] (format "    %4s lines" (count (:trace t)))) "  " (:NAME t))))))

(defn print-urls-with-most-threads [dump threads-to-print]
  (let [url-f   (fn [t] (-> t :request :url))
        threads (filter url-f (:threads dump))
        groups  (group-by url-f threads)
        sorted  (sort-by
                 (fn [[k v]] (- (count v)))
                 (keep (fn [[k v]]
                         (when (< 1 (count v)) [k v]))
                       groups))
        top-x   (take threads-to-print sorted)]
    (when (not-empty top-x)
      (println (emphasis [:white] "TOP " threads-to-print " REQUESTED URLS"))
      (println "")
      (doseq [[url threads] top-x]
        (println "    " (emphasis [:green] (format "%3d" (count threads)) " threads ") url)))))

(defn display-date [date]
  (.format (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss") date))

(defn report [dump]
  (let [threads    (:threads dump)
        count-by   (fn [p] (count (filter p threads)))
        trace-pred (fn [t] (< trace-limit (count (:trace t))))]
    (println "")
    (println (ansi/style "************ THREAD DUMP REPORT ************" :bright :white))
    (println "")
    (println (ansi/style "STATISTICS" :white))
    (println "")
    (println "jstack dump date:          " (display-date (:date dump)))
    (println "total threads:             " (count threads))
    (println "threads waiting for locks: " (count-by (fn [t] (= (-> t :waiting-on :wait-type) :synchronized))))
    (println "request threads:           " (count-by :request))
    (println "traces >" trace-limit "lines:        " (count-by trace-pred))
    (println "")
    (println (ansi/style "TRANSITIVE LOCK GRAPH" :white))
    (println "")
    (print-lock-graph dump)
    (print-oldest-threads dump 10)
    (println "")
    (print-cids-with-multiple-requests dump 10)
    (println "")
    (print-threads-with-longest-traces dump 10)
    (println "")
    (print-urls-with-most-threads dump 10)
    (println "")

    (println (ansi/style "********************************************" :bright :white))))

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
