(ns thread-watch.babashka
  (:require [clojure.string :as str]))

; TODO:
;   - top blocker threads
;   - lock type counts (transaction etc)
;   - number of threads
; elsewhere:
;   - connection counting servlet url -> [req count, conn count]
;   - long running query sql logging


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
;;

; #!/usr/bin/env bb

; "AWT-XAWT" #21 daemon prio=6 os_prio=0 cpu=54648.82ms elapsed=270369.39s tid=0x00007f4e0c0e1000 nid=0x7268 runnable  [0x00007f4e050be000]
; "AWT-Shutdown" #25 prio=4 os_prio=0 cpu=4.89ms elapsed=270369.39s tid=0x00007f4e0c0e9800 nid=0x726b in Object.wait()  [0x00007f4e04dbb000]
; \"AWT-Shutdown\" #25 prio=4 os_prio=0 cpu=4.89ms elapsed=270369.39s tid=0x00007f4e0c0e9800 nid=0x726b in Object.wait()  [0x00007f4e04dbb000]

(defn assoc-if
  "called with two maps, it will assoc the entries from the second
  map into the first map only when the values are non-nil, called with
  one map, it will remove all nil valued entries"
  ([m]
   (assoc-if (sorted-map) m))
  ([m kvs]
   (into m (filter val kvs))))

(defn append [m path x]
  (update-in m path #((fnil conj []) % x)))

(defn append-if [m path rec kvs]
  (let [rec (assoc-if rec kvs)]
    (append m path rec)))

(defn split-lines-by [pred lines]
  (reduce
   (fn [res line]
     (if (pred line)
       (conj res [line])
       (conj (if (not-empty res) (pop res) [])
             (conj (or (peek res) []) line))))
   []
   lines))


;; the below two defs define a line based state machine to
;; parse jstack output. A state machine approach is a robust
;; and transparent way to parse the jstack output

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
      :lines     [line]
      :trace     []})))

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
    (update rec :trace conj (assoc-if {:type   :stack-element
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
(comment
 ;; testing the parsing of various
 (map (fn [line]
        (let [[_ oid class class-for] (re-find #".*<([^>]+)>(?: \(a ([^ )]+))?(?: for ([^ )]+))?" line)]
          {:oid oid :class class :class-for class-for}))
      ["\t- locked <0x0000000644ce4f50> (a java.lang.ref.ReferenceQueue$Lock)"
       "\t- parking to wait for  <0x00000000a0f42790> (a java.util.concurrent.SynchronousQueue$TransferStack)"
       "\t- parking to wait for  <0x00000007da611cb0> (a ext.iplocation.com.google.common.util.concurrent.SettableFuture)"
       "\t- waiting on <0x00000000a17769f0> (a java.lang.ref.Reference$Lock)"
       "\t- waiting on <no object reference available>"
       "\t- waiting to lock <0x0000000629bf6988> (a java.lang.Class for atg.servlet.SessionConfirmationNumberManager)"
       "\t- waiting to lock <0x0000000640381bb8> (a org.jboss.virtual.plugins.context.zip.ZipEntryContext)])"])
 )
(defn parse-dashed-line
  "parse one of the '\t- xxx' lines in the thread block stack trace"
  [m state line]
  (let [[_ oid class class-for] (re-find #".*<([^>]+)>(?: \(a ([^ )]+))?(?: for ([^ )]+))?" line)]
    (update m :trace conj (assoc-if (state dash-types)
                                    {:oid       oid
                                     :class     class
                                     :class-for class-for}))))
(defn add-locked [rec]
  (update rec :locked (fnil conj #{}) (-> rec :trace last :oid)))

(defn add-waiting-on [rec]
  (assoc rec :waiting-on (-> rec :trace last :oid)))

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

;(:owned-locks-start
; :owned-lock
; :no-compile-task
; :block-end)

(defn parse-line [m state line line-#]
  (case state
        :prelude (update m :prelude conj line)
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

;; main entry point, parses jstack output lines
;; into clojure data structure
(defn parse-jstack-lines [lines]
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
  (let [result (dissoc locks (:oid t))]
    (if (empty? result) nil result)))

; types
;- locked <0x0000000644ce4f50> (a java.lang.ref.ReferenceQueue$Lock)
;- parking to wait for  <0x00000000a0f42790> (a java.util.concurrent.SynchronousQueue$TransferStack)
;- parking to wait for  <0x00000007da611cb0> (a ext.iplocation.com.google.common.util.concurrent.SettableFuture)
;- waiting on <0x00000000a17769f0> (a java.lang.ref.Reference$Lock)
;- waiting on <no object reference available>
;- waiting to lock <0x0000000629bf6988> (a java.lang.Class for atg.servlet.SessionConfirmationNumberManager)
;- waiting to lock <0x0000000640381bb8> (a org.jboss.virtual.plugins.context.zip.ZipEntryContext)
;- waiting to re-lock in wait() <0x00000004429f3b08> (a java.util.TaskQueue)

(defn extract-locks-and-wait [rec]
  (reduce
   (fn [[locks wait-oid] t]
     (cond
       (and locks
            (= (:wait-type t) :notify)) [(remove-waiting-on-lock locks t) wait-oid]
       (#{:concurrent
          :synchronized
          :re-lock} (:wait-type t)) [locks {(:oid t) (:class t)}]
       (= (:type t) :locked) [(merge locks {(:oid t) (:class t)}) wait-oid]
       :else [locks wait-oid]))
   [nil nil]
   (-> rec :trace reverse)))

(defn reconcile-locks [rec]
  (let [[locks waiting-on] (extract-locks-and-wait rec)]
    (assoc-if rec {:locked     locks
                   :waiting-on waiting-on})))

(defn dump
  "main method to parse jstack output lines to a thread dump
  in clojure data format"
  [lines]
  (-> (parse-jstack-lines lines)
      (update :threads #(map reconcile-locks %))))

(defn thread-by-name [dump name]
  (first (filter #(= (:name %) name) (-> dump :threads))))

; types
;- locked <0x0000000644ce4f50> (a java.lang.ref.ReferenceQueue$Lock)
;- parking to wait for  <0x00000000a0f42790> (a java.util.concurrent.SynchronousQueue$TransferStack)
;- parking to wait for  <0x00000007da611cb0> (a ext.iplocation.com.google.common.util.concurrent.SettableFuture)
;- waiting on <0x00000000a17769f0> (a java.lang.ref.Reference$Lock)
;- waiting to lock <0x0000000629bf6988> (a java.lang.Class for atg.servlet.SessionConfirmationNumberManager)
;- waiting to lock <0x0000000640381bb8> (a org.jboss.virtual.plugins.context.zip.ZipEntryContext)

; the "synchronized(x) { x.wait() }" pattern leads to a sequence of
; '- waiting on x' (x.wait)
; ...
; '- locked x' (synchronized)
; in the trace where the thread is now placed in the wait set for object x.
;
;; [{:type :trace :class "atg.vfs.VirtualFileImpl" :method "getChild" :file "VirtualFileImpl.java" :line 177}
;;  {:type :locked :oid "0x000000064906aac8" :class "java.util.zip.ZipFile"}
;;  {:type :waiting
;;
;; "parking to wait for" "waiting to lock" "waiting on" thread dump

