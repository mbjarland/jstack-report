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
      [:threads (-> m :threads count dec)]                  ;; update last thread in vec
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

;; main entrypoint to this namespace, give
(defn dump
  "main entrypoint to this namespace. Given a seq of lines from a jstack thread
  dump, produces a clojure data structure representing the thread dump. Example
  usage: (def my-dump (dump (str/split-lines (slurp \"dump.txt\"))))."
  [lines]
  (-> (parse-jstack-lines lines)
    (update :threads #(map reconcile-locks %))))

;;
;; ANALYSIS FUNCTIONS
;; the below functions are used for analysing and working with
;; a dump as produced by the dump function above
;;

(defn threads-by-ti
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
        (reduce-kv
          (fn [a2 oid _] (assoc a2 oid t))
          a
          (:locked t))
        a))
    {}
    (:threads dump)))

(defn waiters-by-tid
  "returns a map {tidA tidB ...} where thread A is waiting
  on a lock held by thread B. The argument dump is a map as
  returned by the dump function"
  [dump]
  (let [lockers (lockers-by-oid dump)]
    (reduce
      (fn [a t]
        (let [waiting-on-oid (first (keys (:waiting-on t)))
              waiting-on-tid (:tid (get lockers waiting-on-oid))]
          (if waiting-on-tid
            (assoc a (:tid t) waiting-on-tid)
            a)))
      {}
      (filter :waiting-on (:threads dump)))))

(defn transitive-path
  "given a map of waiters as returned by waiters-by-tid and a thread
  t (tidC), returns a path [tidA tidB tidC] where thread C is waiting
  for a lock held by thread B, thread B is waiting for a lock
  held by thread A etc exhaustively until the chain completes"
  [waiters t]
  (loop [tid (:tid t) p [tid]]
    (let [locker-tid (get waiters tid)]
      ;(prn :locker locker-tid :tid tid)
      (if (not locker-tid)
        (reverse p)
        (recur locker-tid (conj p locker-tid))))))

(defn transitive-lock-graph
  "returns a map {tidA {tidB {tidC nil tidD nil}}} where
  threads C and D are waiting for a lock held by thread B and
  thread B is waiting on a lock held by thread A. Note that only
  string tid values are present in the returned structure. To look
  up the thread structure for a tid use function threads-by-tid. To
  look up which oid tidD is waiting for from tidB, look up threadD and do
  (-> threadD :waiting-on keys first), this works as any thread can at most
  wait for one oid"
  [dump]
  (let [waiters (waiters-by-tid dump)
        threads (filter :waiting-on (:threads dump))
        paths   (map (partial transitive-path waiters) threads)]
    (reduce
      (fn [a p]
        (if (< 1 (count p))
          (assoc-in a p nil)
          a))
      (sorted-map)
      (sort-by count paths))))

(defn render-tree
  ([key val]
   (render-tree str compare key val))
  ([render-fn key-compare-fn key val]
   (let [I-branch "│   "
         T-branch "├── "
         L-branch "└── "
         spacer   "    "]
     (cons (render-fn key val)
       (mapcat
         (fn [[c-key c-val] index]
           (prn :c-val c-val)
           (let [subtree      (render-tree render-fn key-compare-fn c-key c-val)
                 last?        (= index (dec (count val)))
                 prefix-first (if last? L-branch T-branch)
                 prefix-rest  (if last? spacer I-branch)]
             (cons (str prefix-first (first subtree))
               (map #(str prefix-rest %) (next subtree)))))
         (into (sorted-map-by key-compare-fn) val)
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

(defn display [threads-by-tid tid m]
  (let [t        (get threads-by-tid tid)
        blocking (count (distinct (flatten (keys-in m))))]
    (cond-> (:NAME t)
      (pos? blocking) (str " - blocks " blocking " threads")
      (:waiting-on t) (str " - waiting on " (str/join " " (flatten (vec (:waiting-on t))))))))

(defn render-lock-graph
  [dump]
  (let [graph          (transitive-lock-graph dump)
        threads-by-tid (threads-by-ti dump)
        render-fn      (partial display threads-by-tid)]
    (mapcat (fn [[k v]] (render-tree render-fn compare k v)) graph)))
; "ajp|024721.012|cid=xjWPV4WJhU|rid=MJm3lyXjlo|/us/browse/_/N-1z0tidcZ1z0vrsaZ1z11u3u" - blocks 87 threads, locked 0x00000007231f7420 GSATransaction
; └── "Thread-5987" - blocks 86 threads, waiting on 0x00000007231f7420, locked 0x00000006ee7545b0 GSAContentItem
;     ├── "ajp|025730.050|cid=2CCmLyEivf|rid=rWjXA7jKAT|/us/" - waiting on 0x00000006ee7545b0
;     ├── "ajp|025704.641|cid=Y0KzRJi1Qy|rid=WFtQ5Je7E4|/us/campaign/thank-you.jsp" - waiting on 0x00000006ee7545b0
;     └── "ajp|025725.745|cid=COS56IPjv2|rid=FxLDDpMVGu|ever-handles/productDetail/sink-faucets/870355.htm" - waiting on 0x00000006ee7545b0
;
;   type tree =
;       | T of string * tree list
;
;   let prefMid = seq { yield "├─"; while true do yield "│ " }
;   let prefEnd = seq { yield "└─"; while true do yield "  " }
;   let prefNone = seq { while true do yield "" }
;
;   let c2 x y = Seq.map2 (fun u v -> String.concat "" [u; v]) x y
;
;   let rec visualize (T(label, children)) pre =
;       seq {
;           yield (Seq.head pre) + label
;           if children <> [] then
;               let preRest = Seq.skip 1 pre
;               let last = Seq.last (List.toSeq children)
;               for e in children do
;                   if e = last then yield! visualize e (c2 preRest prefEnd)
;                   else yield! visualize e (c2 preRest prefMid)
;       }
;
;   let example =
;       T ("root",
;               [T ("a",
;                       [T ("a1",
;                               [T ("a11", []);
;                               T ("a12", []) ]) ]);
;               T ("b",
;                       [T ("b1", []) ]) ])
;
;   visualize example prefNone
;   |> Seq.iter (printfn "%s")
;   Output:
;   root
;   ├─a
;   │ └─a1
;   │   ├─a11
;   │   └─a12
;   └─b
;     └─b1


(defn ascii-tree
  "given a map m
      {a {b {c nil}
          d nil}}
  and a function which can take a key a, b, c etc and produce a display
  string for a node, produces a ascii tree similar to the linux tree command.
  The example map given above would produce:
  (leaf-fn a)
  ├── (leaf-fn b)
  │   └── (leaf-fn c)
  └── (leaf-fn d)"
  [m leaf-fn]
  (let [pref-mid  (lazy-seq (cons "├─" (repeat "│ ")))
        pref-end  (lazy-seq (cons "└─" (repeat "  ")))
        pref-none (repeat "")
        c2        (fn [xs ys] (map #(str %1 %2) xs ys))]
    (loop [r [] m2 m pre pref-none]

      (conj r (str (first pre) (if (empty? r) "root" (leaf-fn))))

      )

    ))


(defn transitive-lock-report

  [dump]
  (let [graph (transitive-lock-graph dump)]
    ))



; waiters
;
; {tidB tidA        ;; B waiting on lock oidA held by tidA
;  tidC tidB
;  tidD tidB
;  tidE tidB
;  tidF oidC }
; ->
; {tidA {tidB {tidC {tidF nil}
;              tidD nil
;              tidE nil}}}
;
;
;
;
;
;
; {tidA {tidB {tidC {tidF nil}
;              tidD nil
;              tidE nil}}}
;
; {tidA {{:tid tidB :oid oidB} {{:tid tidC :oid oidC} {{:tid tidF} nil}
;                               {:tid tidD} nil
;                               {:tid tidE} nil}}}
;
; root node: is not waiting on anything (not in any set)
; report ->
; tidA locked X - 5 transitive threads waiting
;   tidB waiting on X, locked Y - 4 transitive threads waiting
;     tidC waiting on Y, locked Z - 1 transitive threads waiting
;     tidD waiting on Y
;     tidE waiting on Y
; {oidA {tid #{oidB oidC}}   ;; oidA - transaction
;  oidB {tidB #{}            ;; oidB - ContentItem
;        tidC #{}
;        tidD #{}}
;  oidC {tidE #{}}
; t:
;   {:locked {oid class
;             oid class}
;    :waiting-on oid}
;
;  {oidA {oidB {oidC {:tid x :NAME y :}
;               oidD {:tid x :NAME y :}}}}
;  ->
;  {oidA {oidB {oidC {:tid x :NAME y :}
;               oidD {oidE {:tid x :NAME y :}}}}}
;
;  {oidA {:tid x :NAME y}}
;  ->
;  {oidA {:tid x :NAME y}
;   oidB {:tid x :NAME y}}
;
;
;







(defn thread-by-name [dump name]
  (first (filter #(= (:name %) name) (-> dump :threads))))
