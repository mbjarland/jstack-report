(ns jstack-report.parser-test
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jstack-report.model :as model]
            [jstack-report.parser :as parser]))

(defn fixture-lines [name]
  (str/split-lines (slurp (jio/resource (str "dumps/" name)))))

;; ---------------------------------------------------------------------------
;; State machine

(deftest next-state-prelude-to-block
  (testing "any non-quote line in :start stays in :prelude"
    (is (= :prelude (parser/next-state :start "2024-01-15 09:30:45"))))
  (testing "quote opens a block"
    (is (= :block-start (parser/next-state :prelude "\"main\" #1 ..."))))
  (testing "thread state line moves from :block-start to :block-second"
    (is (= :block-second
           (parser/next-state :block-start
                              "   java.lang.Thread.State: RUNNABLE")))))

(deftest next-state-trace-and-dashed-lines
  (is (= :trace-element        (parser/next-state :block-second "\tat java.lang.Thread.run(Thread.java:833)")))
  (is (= :locked               (parser/next-state :trace-element "\t- locked <0x000000000000aaaa> (a example.Mutex)")))
  (is (= :waiting-concurrent   (parser/next-state :trace-element "\t- parking to wait for  <0x0> (a x)")))
  (is (= :waiting-notify       (parser/next-state :trace-element "\t- waiting on <0x0> (a x)")))
  (is (= :waiting-synchronized (parser/next-state :trace-element "\t- waiting to lock <0x0> (a x)")))
  (is (= :waiting-re-lock      (parser/next-state :trace-element "\t- waiting to re-lock in wait() <0x0> (a x)")))
  (is (= :eliminated           (parser/next-state :trace-element "\t- eliminated <0x0> (a x)"))))

(deftest next-state-block-end-and-epilogue
  (is (= :block-end          (parser/next-state :trace-element "")))
  (is (= :owned-locks-start  (parser/next-state :block-end "   Locked ownable synchronizers:")))
  (is (= :no-owned           (parser/next-state :owned-locks-start "\t- None")))
  (is (= :owned-lock         (parser/next-state :owned-locks-start "\t- <0x000000000000aaaa>")))
  (is (= :epilogue           (parser/next-state :block-end "JNI global refs: 17"))))

(deftest next-state-undefined-on-unknown-line
  (testing "the FSM signals :undefined for lines it can't classify"
    (is (= :undefined
           (parser/next-state :block-second "totally bogus content")))))

;; ---------------------------------------------------------------------------
;; First-line property extraction

(def sample-first-line
  (str "\"Reference Handler\" #2 daemon prio=10 os_prio=0 cpu=48.14ms "
       "elapsed=268568.18s tid=0x00007f4e7c102000 nid=0x7248 "
       "waiting on condition  [0x00007f4e6818f000]"))

(deftest first-line-prop-extracts-values
  (is (= "10"                 (parser/first-line-prop sample-first-line "prio")))
  (is (= "0"                  (parser/first-line-prop sample-first-line "os_prio")))
  (is (= "48.14ms"            (parser/first-line-prop sample-first-line "cpu")))
  (is (= "268568.18s"         (parser/first-line-prop sample-first-line "elapsed")))
  (is (= "0x00007f4e7c102000" (parser/first-line-prop sample-first-line "tid")))
  (is (= "0x7248"             (parser/first-line-prop sample-first-line "nid"))))

(deftest first-line-prop-returns-nil-for-missing
  (is (nil? (parser/first-line-prop sample-first-line "nope"))))

(deftest thread-name-extracts-quoted-prefix
  (is (= "Reference Handler" (parser/thread-name sample-first-line)))
  (is (= "ajp|093041|cid=abc|rid=xyz|/api/x"
         (parser/thread-name
           "\"ajp|093041|cid=abc|rid=xyz|/api/x\" daemon prio=5 tid=0x1 nid=0x1 runnable"))))

(deftest daemon-extraction
  (is (true?  (parser/daemon? sample-first-line)))
  (is (false? (parser/daemon? "\"main\" #1 prio=5 tid=0x1 nid=0x1 runnable"))))

(deftest currently-pulls-state-summary
  (is (= "waiting on condition"  (parser/currently sample-first-line)))
  (is (= "runnable"              (parser/currently "\"x\" #1 daemon prio=5 tid=0x1 nid=0x2 runnable"))))

(deftest id-pulls-integer-thread-id-when-present
  (is (= 2 (parser/id sample-first-line)))
  (testing "no #N present returns nil"
    (is (nil? (parser/id "\"x\" daemon prio=5 tid=0x1 nid=0x2 runnable")))))

;; ---------------------------------------------------------------------------
;; Trace and dashed lines

(deftest parse-trace-element-line-extracts-class-method-file-line
  (let [r (parser/parse-trace-element-line
            "\tat java.util.concurrent.LinkedBlockingQueue.take(LinkedBlockingQueue.java:399)")]
    (is (= "java.util.concurrent.LinkedBlockingQueue" (:class r)))
    (is (= "take"                                      (:method r)))
    (is (= "LinkedBlockingQueue.java"                  (:file r)))
    (is (= 399                                         (:line-# r)))))

(deftest parse-trace-element-line-handles-native-method
  (let [r (parser/parse-trace-element-line
            "\tat sun.misc.Unsafe.park(Native Method)")]
    (is (= "sun.misc.Unsafe" (:class r)))
    (is (= "park"            (:method r)))
    (is (= "Native Method"   (:file r)))
    (is (nil? (:line-# r)))))

(deftest parse-dashed-line-locked-form
  (let [m (parser/parse-dashed-line {} :locked
            "\t- locked <0x00000007d39893e0> (a atg.nucleus.ConfigurationLock)")
        e (first (:trace m))]
    (is (= :locked                       (:type e)))
    (is (= "0x00000007d39893e0"          (:oid e)))
    (is (= "atg.nucleus.ConfigurationLock" (:class e)))))

(deftest parse-dashed-line-class-for-form
  (let [m (parser/parse-dashed-line {} :waiting-synchronized
            "\t- waiting to lock <0x000000000000ffff> (a java.lang.Class for example.SessionManager)")
        e (first (:trace m))]
    (is (= :waiting              (:type e)))
    (is (= :synchronized         (:wait-type e)))
    (is (= "0x000000000000ffff"  (:oid e)))
    (is (= "example.SessionManager" (:class-for e)))))

;; ---------------------------------------------------------------------------
;; End-to-end parsing of synthetic dumps

(deftest minimal-dump-parses-cleanly
  (let [d (model/dump (fixture-lines "minimal.txt"))]
    (is (= 1 (count (:threads d))))
    (let [t (first (:threads d))]
      (is (= "main" (:name t)))
      (is (= 1      (:id t)))
      (is (= "RUNNABLE" (:thread-state t))))))

(deftest apple-orange-banana-shape
  (let [d (model/dump (fixture-lines "apple-orange-banana.txt"))]
    (is (= 4 (count (:threads d))))
    (is (= #{"thread-X" "thread-A" "thread-B" "thread-C"}
           (set (map :name (:threads d)))))))

(deftest request-threads-decorated
  (let [d  (model/dump (fixture-lines "request-threads.txt"))
        ts (:threads d)
        by (fn [n] (first (filter #(= n (:name %)) ts)))]
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
  (let [d  (model/dump (fixture-lines "edge-cases.txt"))
        by (fn [n] (first (filter #(= n (:name %)) (:threads d))))]
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
