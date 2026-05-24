(ns ^{:doc "Line-based finite state machine that turns a sequence of
jstack output lines into a parsed dump map of the shape

  {:prelude  [<lines>]
   :threads  [<thread> ...]
   :epilogue [<lines>]}

The parser knows nothing about colors, indexes, or reporting — it is
purely lines in, data out. Downstream namespaces (model, analyze,
render, report) layer enrichment, derivation, and presentation on top."
      :author "Matias Bjarland"}
  jstack-report.parser
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; State machine definition

(def block-transitions
  "Patterns that may appear inside a thread block. The parser walks
  this list top-to-bottom and takes the first match — so the most
  common patterns (stack-trace entries, blank line terminators) come
  first. Patterns are mutually exclusive, so order is a performance
  knob only."
  [:empty                     :block-end           ; ends every block
   "\tat "                    :trace-element       ; ~95% of in-block lines
   "\t- locked"               :locked
   "\t- waiting to lock"      :waiting-synchronized
   "\t- parking to wait for"  :waiting-concurrent
   "\t- waiting on"           :waiting-notify
   "\t- waiting to re-lock"   :waiting-re-lock
   "\t- eliminated "          :eliminated
   "   No compile task"       :no-compile-task])

(def finite-state-machine
  "Allowed state transitions keyed by current state. `:start` is a
  virtual state with no matching line. `:any` matches any line and
  `:empty` matches the empty string."
  {:start                [:any :prelude]
   :prelude              ["\"" :block-start
                          :any :prelude]
   :block-start          ["   java.lang.Thread.State:" :block-second
                          :empty :block-end
                          "\"" :block-start]  ; one-line block, no trailing blank
   :block-second         block-transitions
   :trace-element        block-transitions
   :locked               block-transitions
   :eliminated           block-transitions
   :waiting-concurrent   block-transitions
   :waiting-notify       block-transitions
   :waiting-synchronized block-transitions
   :waiting-re-lock      block-transitions
   :no-compile-task      block-transitions
   :block-end            ["\""                 :block-start
                          "   Locked ownable"  :owned-locks-start
                          "JNI global"         :epilogue
                          :empty               :block-end]
   :owned-locks-start    ["\t- None" :no-owned
                          "\t- "     :owned-lock]
   :no-owned             [:empty :block-end]
   :owned-lock           ["\t- "  :owned-lock
                          :empty  :block-end]
   :epilogue             [:any :end]
   :end                  nil})

(def ^:private fsm-mapped
  (reduce-kv
    (fn [a k v]
      (assoc a k (map (fn [[k v]] {:pattern k :state v}) (partition 2 v))))
    {}
    finite-state-machine))

(def dash-types
  "Maps a dashed-line state to the `:type` (and optional `:wait-type`)
  recorded for the resulting trace element."
  {:locked               {:type :locked}                         ; - locked <0x...>
   :eliminated           {:type :eliminated}                     ; lock eliminated by JIT
   :waiting-concurrent   {:type :waiting :wait-type :concurrent} ; - parking to wait for <0x...>
   :waiting-synchronized {:type :waiting :wait-type :synchronized} ; - waiting to lock <0x...>
   :waiting-re-lock      {:type :waiting :wait-type :re-lock}    ; re-enter wait section
   :waiting-notify       {:type :waiting :wait-type :notify}})   ; - waiting on <0x...>

(def thread-states
  "JVM Thread.State name to keyword."
  {"NEW"           :new
   "RUNNABLE"      :runnable
   "BLOCKED"       :blocked
   "WAITING"       :waiting
   "TIMED_WAITING" :timed-waiting
   "TERMINATED"    :terminated})

(defn next-state [old-state ^String line]
  (let [transitions (get fsm-mapped old-state)
        match?      (fn [{:keys [pattern]}]
                      (or (and (string? pattern) (str/starts-with? line pattern))
                          (= :any pattern)
                          (and (= :empty pattern) (= line ""))))]
    (or (some #(when (match? %) (:state %)) transitions)
        :undefined)))

;; ---------------------------------------------------------------------------
;; First line of a thread block

(def ^:private prop-re-cache
  "Memoized regex per property name. We see the same handful of names
  (prio, os_prio, tid, nid, cpu, elapsed) thousands of times per dump,
  so compiling the pattern once is worth it."
  (memoize (fn [name] (re-pattern (str " " name "=([^ ]+) ")))))

(defn first-line-prop
  "Extract a `name=value` property value from the first line of a
  thread block (e.g. `prio=6`, `tid=0x00007f4e0c0e9800`)."
  [line name]
  (second (re-find (prop-re-cache name) line)))

(defn thread-name
  "Extract a thread name (the leading quoted segment) from the first
  line of a thread block."
  [line]
  (second (re-find #"\"([^\"]+)\"" line)))

(defn daemon?
  "True when the first line of a thread block marks the thread daemon."
  [line]
  (str/includes? (second (re-find #"\"[^\"]+\" (.*)" line)) " daemon "))

(defn currently
  "Extract the trailing state-summary phrase from the first line of a
  thread block, e.g. `in Object.wait()` or `runnable`."
  [line]
  (str/trim (second (re-find #".*nid=[^ ]+ ([^\[]+)" line))))

(defn id
  "Extract the integer thread id (the `#N` token) from the first line
  of a thread block, or nil when no id is present."
  [line]
  (some-> (second (re-find #"\"[^\"]+\".* #([0-9]+) " line)) Integer/parseInt))

;; ---------------------------------------------------------------------------
;; Block line parsers

(defn ^:private assoc-non-nil
  "Drop entries with nil values. Hash-map output — there's no reason
  to pay the red-black-tree allocation cost on a 4000-thread parse."
  [m]
  (persistent! (reduce-kv (fn [a k v] (if (some? v) (assoc! a k v) a))
                          (transient {})
                          m)))

(defn parse-block-first-line
  "Parse the quoted-name + flags line that opens a thread block.

  Example: `\"RMI TCP Connection(idle)\" daemon prio=10 tid=... nid=...
            waiting on condition [0x00002b7b25bab000]`"
  [line]
  (let [prop (partial first-line-prop line)]
    (assoc-non-nil
      {:name      (thread-name line)
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

(defn parse-block-second-line
  "Parse `   java.lang.Thread.State: WAITING (on object monitor)`."
  [rec line]
  (assoc rec :thread-state (subs line 27)))

(defn parse-trace-element-line
  "Parse one `\\tat fully.qualified.Class.method(File.java:42)` line
  into `{:class :method :file :line-#}`. Native methods, unknown source,
  and `<generated>` files have no line number."
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
  "Append a stack-trace element to the thread's :trace vector. Keeps
  only the raw `:line` and an element `:type` so the parser stays
  cheap; callers that need the parsed class/method/file/line can run
  `parse-trace-element-line` on the `:line` themselves."
  [rec line]
  (update rec :trace (fnil conj [])
          {:type :stack-element :line line}))

(defn parse-dashed-line
  "Parse one of the `\\t- ...` annotation lines emitted by jstack, e.g.

      - locked <0x00000007d39893e0> (a atg.nucleus.ConfigurationLock)
      - waiting to lock <0x00000006492773a0> (a java.lang.Object)
      - parking to wait for  <0x0000000645e75218>
      - waiting on <0x000000066c425080> (a java.lang.ref.Reference$Lock)
      - waiting on <no object reference available>
      - waiting to lock <0x...> (a java.lang.Class for some.pkg.Klass)"
  [m state line]
  (let [[_ oid class class-for] (re-find
                                  #".*<([^>]+)>(?: \(a ([^ )]+))?(?: for ([^ )]+))?"
                                  line)]
    (update m :trace (fnil conj [])
            (assoc-non-nil
              (merge (get dash-types state)
                     {:oid       oid
                      :class     class
                      :class-for class-for
                      :line      line})))))

(defn parse-block-line [rec state line]
  (case state
        :block-second  (parse-block-second-line rec line)
        :trace-element (parse-trace-element-line-delayed rec line)
        (:locked
         :eliminated
         :waiting-concurrent
         :waiting-notify
         :waiting-synchronized
         :waiting-re-lock) (parse-dashed-line rec state line)
        rec))

;; ---------------------------------------------------------------------------
;; Top-level line loop

(defn decorate-dump-date
  "If the first line of the dump matches `yyyy-MM-dd HH:mm:ss`, parse
  and stash it on the dump map under :date."
  [dump first-line]
  (if (re-matches #"\d\d\d\d-\d\d-\d\d \d\d:\d\d:\d\d" first-line)
    (assoc dump :date (java.time.LocalDateTime/parse
                        first-line
                        (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss")))
    dump))

(defn parse-jstack-lines
  "Walk a sequence of jstack lines and return the structural skeleton:
  prelude, vector of threads with raw `:trace`, and epilogue. Use
  jstack-report.model/dump for the fully enriched result.

  The in-progress thread block is tracked as a local `building` and
  only conj'd into the threads vector at block boundaries. For a
  500k-line dump that saves hundreds of thousands of intermediate
  outer-map allocations the previous `update-in [:threads idx] ...`
  approach was paying."
  [lines]
  (loop [m          {:prelude [] :threads [] :epilogue []}
         building   nil
         prev-state :start
         line-#     1
         [line & xs] lines]
    (let [state (next-state prev-state line)]
      (cond
        (= state :end)
        (if building (update m :threads conj building) m)

        (= state :undefined)
        (throw (ex-info (str "error on line " line-#
                             " - no state transition defined for line:\n" line)
                        {:line#         line-#
                         :line          line
                         :current-state state}))

        (= state :prelude)
        (recur (cond-> m
                 (empty? (:prelude m)) (decorate-dump-date line)
                 true                  (update :prelude conj line))
               building state (inc line-#) xs)

        (= state :epilogue)
        (recur (update m :epilogue conj line)
               building state (inc line-#) xs)

        (= state :block-start)
        (recur (if building (update m :threads conj building) m)
               (parse-block-first-line line)
               state (inc line-#) xs)

        :else
        (recur m
               (parse-block-line building state line)
               state (inc line-#) xs)))))
