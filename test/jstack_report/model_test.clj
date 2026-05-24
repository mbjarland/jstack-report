(ns jstack-report.model-test
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jstack-report.core :as core])
  (:import [java.time LocalDateTime]))

(defn fixture-lines [name]
  (str/split-lines (slurp (jio/resource (str "dumps/" name)))))

;; ---------------------------------------------------------------------------
;; Lock reconciliation

(deftest reconcile-locks-extracts-locked-set-and-wait
  (let [d  (core/dump (fixture-lines "apple-orange-banana.txt"))
        by (fn [n] (first (filter #(= n (:NAME %)) (:threads d))))
        a  (by "thread-A")
        x  (by "thread-X")
        c  (by "thread-C")]
    (testing "thread-A holds Apple, waiting on Orange"
      (is (= [{:oid "0x0000000000000002" :class "example.Apple"}]
             (:locked a)))
      (is (= {:oid "0x0000000000000001"
              :class "example.Orange"
              :wait-type :synchronized}
             (:waiting-on a))))
    (testing "thread-X holds Orange, not waiting on anything"
      (is (= "0x0000000000000001" (-> x :locked first :oid)))
      (is (nil? (:waiting-on x))))
    (testing "thread-C is at the tail — no locks held"
      (is (nil? (:locked c)))
      (is (= "0x0000000000000003" (-> c :waiting-on :oid))))))

(deftest reconcile-locks-notify-wait-self-lock-is-stripped
  (testing "Object.wait pattern: the lock the thread holds AND is waiting on
            is removed from both :locked and :waiting-on — the thread has
            temporarily released the monitor and is not blocking anyone"
    (let [d  (core/dump (fixture-lines "request-threads.txt"))
          rh (first (filter #(= "Reference Handler" (:NAME %)) (:threads d)))]
      (is (nil? (:locked rh)))
      (is (nil? (:waiting-on rh))))))

;; ---------------------------------------------------------------------------
;; Request thread enrichment

(deftest parse-thread-name-handles-token-equals-value-parts
  (is (= {:pre "ajp"
          :time "093041.250"
          :cid "abc"
          :rid "xyz"
          :oip "10.0.0.1"
          :url "/api/x"}
         (core/parse-thread-name
           "ajp|093041.250|cid=abc|rid=xyz|oip=10.0.0.1|/api/x")))
  (testing "thread names without pipes return nil"
    (is (nil? (core/parse-thread-name "main")))))

(deftest thread-date-parses-and-handles-rollover
  (let [dump-date (LocalDateTime/of 2024 1 15 9 30 45)]
    (testing "earlier in the same day stays on dump date"
      (let [d (core/thread-date dump-date "093000.000")]
        (is (= 2024 (.getYear d)))
        (is (= 9    (.getHour d)))
        (is (= 30   (.getMinute d)))))
    (testing "a time that's significantly after dump time is treated as previous day"
      (let [d (core/thread-date dump-date "235959.999")]
        (is (= 14 (.getDayOfMonth d)))))
    (testing "nil time-str returns nil"
      (is (nil? (core/thread-date dump-date nil))))))

(deftest decorate-thread-age-uses-newest-date-as-reference
  (let [d         (core/dump (fixture-lines "request-threads.txt"))
        with-req  (filter :request (:threads d))]
    (is (every? (fn [t] (-> t :request :age-seconds (>= 0))) with-req))
    (is (every? (fn [t] (-> t :request :display-age string?)) with-req))))

(deftest display-duration-formats-h-m-s
  (testing "zero-valued units higher than the largest non-zero unit are suppressed"
    (is (= "00s"    (core/display-duration 0)))
    (is (= "59s"    (core/display-duration 59)))
    (is (= "1m00s"  (core/display-duration 60)))
    (is (= "1h00s"  (core/display-duration 3600)))
    (is (= "1h1m30s" (core/display-duration 3690)))))

;; ---------------------------------------------------------------------------
;; Indexes built from a parsed dump

(deftest threads-by-tid-keyed-by-thread-id
  (let [d  (core/dump (fixture-lines "apple-orange-banana.txt"))
        bt (core/threads-by-tid d)]
    (is (= "thread-X" (-> bt (get "0x000000000000000a") :NAME)))
    (is (= "thread-C" (-> bt (get "0x000000000000000d") :NAME)))))

(deftest lockers-by-oid-finds-owning-thread
  (let [d  (core/dump (fixture-lines "apple-orange-banana.txt"))
        lo (core/lockers-by-oid d)]
    (is (= "thread-X" (-> lo (get "0x0000000000000001") :NAME)))
    (is (= "thread-A" (-> lo (get "0x0000000000000002") :NAME)))
    (is (= "thread-B" (-> lo (get "0x0000000000000003") :NAME)))))

(deftest waiters-by-tid-chains-tids-to-owners
  (let [d  (core/dump (fixture-lines "apple-orange-banana.txt"))
        wt (core/waiters-by-tid d)]
    (is (= "0x000000000000000a" (-> wt (get "0x000000000000000b") :tid)))
    (is (= "0x000000000000000b" (-> wt (get "0x000000000000000c") :tid)))
    (is (= "0x000000000000000c" (-> wt (get "0x000000000000000d") :tid)))))

(deftest waiters-by-oid-groups-waiters-on-the-same-lock
  (let [d  (core/dump (fixture-lines "wide-graph.txt"))
        wo (core/waiters-by-oid d)]
    (is (= 5 (count (get wo "0x00000000000000aa"))))))
