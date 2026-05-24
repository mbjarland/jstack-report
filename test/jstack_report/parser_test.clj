(ns jstack-report.parser-test
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jstack-report.core :as core]))

(defn fixture-lines [name]
  (str/split-lines (slurp (jio/resource (str "dumps/" name)))))

;; ---------------------------------------------------------------------------
;; State machine

(deftest next-state-prelude-to-block
  (testing "any non-quote line in :start stays in :prelude"
    (is (= :prelude (core/next-state :start "2024-01-15 09:30:45"))))
  (testing "quote opens a block"
    (is (= :block-start (core/next-state :prelude "\"main\" #1 ..."))))
  (testing "thread state line moves from :block-start to :block-second"
    (is (= :block-second
           (core/next-state :block-start
                            "   java.lang.Thread.State: RUNNABLE")))))

(deftest next-state-trace-and-dashed-lines
  (is (= :trace-element  (core/next-state :block-second "\tat java.lang.Thread.run(Thread.java:833)")))
  (is (= :locked         (core/next-state :trace-element "\t- locked <0x000000000000aaaa> (a example.Mutex)")))
  (is (= :waiting-concurrent (core/next-state :trace-element "\t- parking to wait for  <0x0> (a x)")))
  (is (= :waiting-notify     (core/next-state :trace-element "\t- waiting on <0x0> (a x)")))
  (is (= :waiting-synchronized (core/next-state :trace-element "\t- waiting to lock <0x0> (a x)")))
  (is (= :waiting-re-lock    (core/next-state :trace-element "\t- waiting to re-lock in wait() <0x0> (a x)")))
  (is (= :eliminated         (core/next-state :trace-element "\t- eliminated <0x0> (a x)"))))

(deftest next-state-block-end-and-epilogue
  (is (= :block-end          (core/next-state :trace-element "")))
  (is (= :owned-locks-start  (core/next-state :block-end "   Locked ownable synchronizers:")))
  (is (= :no-owned           (core/next-state :owned-locks-start "\t- None")))
  (is (= :owned-lock         (core/next-state :owned-locks-start "\t- <0x000000000000aaaa>")))
  (is (= :epilogue           (core/next-state :block-end "JNI global refs: 17"))))

(deftest next-state-undefined-on-unknown-line
  (testing "the FSM signals :undefined for lines it can't classify"
    (is (= :undefined
           (core/next-state :block-second "totally bogus content")))))

;; ---------------------------------------------------------------------------
;; First-line property extraction

(def sample-first-line
  (str "\"Reference Handler\" #2 daemon prio=10 os_prio=0 cpu=48.14ms "
       "elapsed=268568.18s tid=0x00007f4e7c102000 nid=0x7248 "
       "waiting on condition  [0x00007f4e6818f000]"))

(deftest first-line-prop-extracts-values
  (is (= "10"                 (core/first-line-prop sample-first-line "prio")))
  (is (= "0"                  (core/first-line-prop sample-first-line "os_prio")))
  (is (= "48.14ms"            (core/first-line-prop sample-first-line "cpu")))
  (is (= "268568.18s"         (core/first-line-prop sample-first-line "elapsed")))
  (is (= "0x00007f4e7c102000" (core/first-line-prop sample-first-line "tid")))
  (is (= "0x7248"             (core/first-line-prop sample-first-line "nid"))))

(deftest first-line-prop-returns-nil-for-missing
  (is (nil? (core/first-line-prop sample-first-line "nope"))))

(deftest thread-name-extracts-quoted-prefix
  (is (= "Reference Handler" (core/thread-name sample-first-line)))
  (is (= "ajp|093041|cid=abc|rid=xyz|/api/x"
         (core/thread-name
           "\"ajp|093041|cid=abc|rid=xyz|/api/x\" daemon prio=5 tid=0x1 nid=0x1 runnable"))))

(deftest daemon-extraction
  (is (true?  (core/daemon? sample-first-line)))
  (is (false? (core/daemon? "\"main\" #1 prio=5 tid=0x1 nid=0x1 runnable"))))

(deftest currently-pulls-state-summary
  (is (= "waiting on condition"  (core/currently sample-first-line)))
  (is (= "runnable"              (core/currently "\"x\" #1 daemon prio=5 tid=0x1 nid=0x2 runnable"))))

(deftest id-pulls-integer-thread-id-when-present
  (is (= 2 (core/id sample-first-line)))
  (testing "no #N present returns nil"
    (is (nil? (core/id "\"x\" daemon prio=5 tid=0x1 nid=0x2 runnable")))))

;; ---------------------------------------------------------------------------
;; Trace and dashed lines

(deftest parse-trace-element-line-extracts-class-method-file-line
  (let [r (core/parse-trace-element-line
            "\tat java.util.concurrent.LinkedBlockingQueue.take(LinkedBlockingQueue.java:399)")]
    (is (= "java.util.concurrent.LinkedBlockingQueue" (:class r)))
    (is (= "take"                                      (:method r)))
    (is (= "LinkedBlockingQueue.java"                  (:file r)))
    (is (= 399                                         (:line-# r)))))

(deftest parse-trace-element-line-handles-native-method
  (let [r (core/parse-trace-element-line
            "\tat sun.misc.Unsafe.park(Native Method)")]
    (is (= "sun.misc.Unsafe" (:class r)))
    (is (= "park"            (:method r)))
    (is (= "Native Method"   (:file r)))
    (is (nil? (:line-# r)))))

(deftest parse-dashed-line-locked-form
  (let [m (core/parse-dashed-line {} :locked
            "\t- locked <0x00000007d39893e0> (a atg.nucleus.ConfigurationLock)")
        e (first (:trace m))]
    (is (= :locked                       (:type e)))
    (is (= "0x00000007d39893e0"          (:oid e)))
    (is (= "atg.nucleus.ConfigurationLock" (:class e)))))

(deftest parse-dashed-line-class-for-form
  (let [m (core/parse-dashed-line {} :waiting-synchronized
            "\t- waiting to lock <0x000000000000ffff> (a java.lang.Class for example.SessionManager)")
        e (first (:trace m))]
    (is (= :waiting              (:type e)))
    (is (= :synchronized         (:wait-type e)))
    (is (= "0x000000000000ffff"  (:oid e)))
    (is (= "example.SessionManager" (:class-for e)))))

;; ---------------------------------------------------------------------------
;; End-to-end parsing of synthetic dumps

(deftest minimal-dump-parses-cleanly
  (let [d (core/dump (fixture-lines "minimal.txt"))]
    (is (= 1 (count (:threads d))))
    (let [t (first (:threads d))]
      (is (= "main" (:NAME t)))
      (is (= 1      (:id t)))
      (is (= "RUNNABLE" (:thread-state t))))))

(deftest apple-orange-banana-shape
  (let [d (core/dump (fixture-lines "apple-orange-banana.txt"))]
    (is (= 4 (count (:threads d))))
    (is (= #{"thread-X" "thread-A" "thread-B" "thread-C"}
           (set (map :NAME (:threads d)))))))

(deftest request-threads-decorated
  (let [d  (core/dump (fixture-lines "request-threads.txt"))
        ts (:threads d)
        by (fn [n] (first (filter #(= n (:NAME %)) ts)))]
    (testing "ajp/http threads pick up :request map"
      (is (= "clientAAA" (-> (by "ajp|093041.250|cid=clientAAA|rid=req001|oip=10.0.0.1|/api/orders")
                             :request :cid)))
      (is (= "req002"    (-> (by "ajp|093040.000|cid=clientBBB|rid=req002|oip=10.0.0.2|/api/cart")
                             :request :rid)))
      (is (= "/api/profile" (-> (by "http|093042.500|cid=clientAAA|rid=req003|oip=10.0.0.1|/api/profile")
                                :request :url))))
    (testing "non-request threads have no :request key"
      (is (nil? (:request (by "Reference Handler")))))))

(deftest edge-cases-trace-types-roundtrip
  (let [d  (core/dump (fixture-lines "edge-cases.txt"))
        by (fn [n] (first (filter #(= n (:NAME %)) (:threads d))))]
    (testing "eliminated locks appear in trace with type :eliminated"
      (is (some #(= :eliminated (:type %)) (:trace (by "eliminated-locks")))))
    (testing "parking concurrent shows wait-type :concurrent"
      (is (some #(and (= :waiting (:type %)) (= :concurrent (:wait-type %)))
                (:trace (by "parking-concurrent")))))
    (testing "Object.wait shows :notify wait-type"
      (is (some #(and (= :waiting (:type %)) (= :notify (:wait-type %)))
                (:trace (by "notify-wait-pair")))))
    (testing "class-for form is parsed into :class-for"
      (is (some #(= "example.SessionManager" (:class-for %))
                (:trace (by "class-for-form")))))))
