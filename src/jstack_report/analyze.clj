(ns ^{:doc "Builds indexes and derived views over an enriched dump:
look-up tables (by tid / by locked oid), wait-for relationships, the
transitive lock graph, and a handful of trace-content predicates."
      :author "Matias Bjarland"}
  jstack-report.analyze)

(def trace-report-limit
  "Threads with stack traces longer than this many lines are flagged
  as `[N line trace]` in the lock graph rendering."
  250)

;; ---------------------------------------------------------------------------
;; Basic indexes

(defn threads-by-tid
  "{tid thread, ...}. tid is the JVM-assigned hex tid string."
  [dump]
  (reduce (fn [a t] (assoc a (:tid t) t))
          (sorted-map)
          (:threads dump)))

(defn lockers-by-oid
  "{<locked oid> <thread that holds it>, ...}."
  [dump]
  (reduce
    (fn [a t]
      (if (:locked t)
        (reduce (fn [a2 {:keys [oid]}] (assoc a2 oid t)) a (:locked t))
        a))
    {}
    (:threads dump)))

(defn waiters-by-tid
  "{waiter-tid {:tid owner-tid :oid contested-oid}, ...}."
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
  "{contested-oid #{waiter-tid ...}, ...}."
  [dump]
  (reduce
    (fn [a t]
      (update a (-> t :waiting-on :oid) (fnil conj #{}) (:tid t)))
    (sorted-map)
    (filter :waiting-on (:threads dump))))

;; ---------------------------------------------------------------------------
;; Transitive lock graph

(defn transitive-path
  "Walk from a leaf thread upward through `waiters` to its root locker
  and return the path as a vector of `{:tid :oid}` ascend-order entries."
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
                      nested (map #(into [k] %) (filter seq sub))]
                  (if (seq nested) nested [[k]])))
              m))
    []))

(defn key-count-in [m]
  (if (map? m)
    (count (distinct (flatten (keys-in m))))
    0))

(defn ^:private sorted-map-by-subtree [m subtree-size key-comp-fn]
  (into (sorted-map-by
          (fn [k1 k2]
            (compare [(subtree-size k2) (key-comp-fn k1)]
                     [(subtree-size k1) (key-comp-fn k2)])))
        m))

(defn ^:private subtree-sizes
  "Precompute {root-key subtree-key-count} for the top-level entries of
  `m`. Lets the comparator look up the size in O(1) instead of
  recomputing it on every call."
  [m]
  (into {} (map (fn [[k v]] [k (key-count-in v)])) m))

(defn transitive-lock-graph
  "Returns a nested map {tidA {tidB {tidC nil ...}}} where threads C
  are waiting for a lock held by B, which is waiting on A, and so on.
  Top-level keys are sorted by subtree size (biggest blockers first)
  then by `(tid, oid)` for stable output."
  [dump]
  (let [waiters-by-tid (waiters-by-tid dump)
        waiters-by-oid (waiters-by-oid dump)
        threads        (filter :waiting-on (:threads dump))
        paths          (distinct (map #(transitive-path waiters-by-tid %) threads))
        graph          (reduce
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
                         (sort-by (comp - count) paths))
        sizes          (subtree-sizes graph)]
    (sorted-map-by-subtree graph sizes #(vector (:tid %) (:oid %)))))

;; ---------------------------------------------------------------------------
;; Trace-content predicates

(defn trace-has? [t [class method]]
  (let [match? (fn [e] (and (= (some-> e :details deref :class) class)
                            (= (some-> e :details deref :method) method)))]
    (boolean (some match? (:trace t)))))

(defn tx-reaper? [t]
  (trace-has? t ["com.arjuna.ats.internal.arjuna.coordinator.ReaperWorkerThread"
                 "run"]))

(defn db-socket-read?
  "True when the thread is parked in a socketRead0 call to an Oracle
  JDBC stream — the canonical 'waiting on the database' fingerprint."
  [t]
  (and (trace-has? t ["java.net.SocketInputStream" "socketRead0"])
       (trace-has? t ["oracle.jdbc.driver.T4CSocketInputStreamWrapper" "read"])))

(defn db-socket-read-is-valid?
  "True when the socketRead0 is inside a `isValidConnection` `select 1
  from dual` check rather than a real query."
  [t]
  (and (db-socket-read? t)
       (trace-has? t ["org.jboss.resource.adapter.jdbc.CheckValidConnectionSQL"
                      "isValidConnection"])))

;; ---------------------------------------------------------------------------
;; Display helpers used by the renderer

(defn thread-display-age [t]
  (some->> t :request :display-age (str "age ")))

(defn thread-extra-info [t]
  (cond
    (tx-reaper? t)               "[jboss tx reaper thread]"
    (db-socket-read-is-valid? t) "[db socketRead0 isValid]"
    (db-socket-read? t)          "[db socketRead0]"
    (< trace-report-limit (count (:trace t)))
    (str "[" (count (:trace t)) " line trace]")
    :else nil))
