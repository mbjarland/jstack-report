(ns jstack-report.analyze-test
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jstack-report.analyze :as analyze]
            [jstack-report.model :as model]))

(defn fixture-lines [name]
  (str/split-lines (slurp (jio/resource (str "dumps/" name)))))

;; ---------------------------------------------------------------------------
;; Transitive path & graph

(deftest transitive-path-walks-from-leaf-to-root
  (let [d  (model/dump (fixture-lines "apple-orange-banana.txt"))
        wt (analyze/waiters-by-tid d)
        c  (first (filter #(= "thread-C" (:name %)) (:threads d)))
        p  (analyze/transitive-path wt c)]
    (is (= ["0x000000000000000a" "0x000000000000000b" "0x000000000000000c"]
           (map :tid p)))))

(deftest transitive-lock-graph-has-single-root
  (let [d (model/dump (fixture-lines "apple-orange-banana.txt"))
        g (analyze/transitive-lock-graph d)]
    (testing "exactly one root tid"
      (is (= 1 (count g))))
    (testing "root is thread-X"
      (is (= "0x000000000000000a" (-> g first key :tid))))))

(deftest transitive-lock-graph-wide-fanout
  (let [d            (model/dump (fixture-lines "wide-graph.txt"))
        g            (analyze/transitive-lock-graph d)
        root         (first g)
        [_ children] root]
    (is (= 1 (count g)))
    (is (= "0x0000000000000100" (-> root key :tid)))
    (is (= 5 (count children)))
    (is (every? nil? (vals children)))))

(deftest empty-graph-when-no-blocking
  (let [d (model/dump (fixture-lines "minimal.txt"))]
    (is (empty? (analyze/transitive-lock-graph d)))))

;; ---------------------------------------------------------------------------
;; key-count-in / keys-in tree utilities

(deftest keys-in-walks-nested-map
  (is (= [[:a :b] [:a :c :d]]
         (analyze/keys-in {:a {:b 1 :c {:d 2}}}))))

(deftest key-count-in-counts-distinct-keys-in-tree
  (is (= 4 (analyze/key-count-in {:a {:b 1 :c {:d 2}}})))
  (is (= 0 (analyze/key-count-in nil)))
  (is (= 0 (analyze/key-count-in :not-a-map))))

;; ---------------------------------------------------------------------------
;; Trace-based predicates

(deftest db-socket-read-detection
  (let [d (model/dump (fixture-lines "db-socket-read.txt"))
        by (fn [n] (first (filter #(= n (:name %)) (:threads d))))]
    (testing "matches socketRead0 + oracle wrapper"
      (is (analyze/db-socket-read? (by "db-read-regular")))
      (is (analyze/db-socket-read? (by "db-read-validating"))))
    (testing "does not match plain workers"
      (is (not (analyze/db-socket-read? (by "plain-worker")))))
    (testing "isValid variant detection"
      (is (analyze/db-socket-read-is-valid? (by "db-read-validating")))
      (is (not (analyze/db-socket-read-is-valid? (by "db-read-regular")))))))

(deftest tx-reaper-detection
  (let [d  (model/dump (fixture-lines "db-socket-read.txt"))
        by (fn [n] (first (filter #(= n (:name %)) (:threads d))))]
    (is (analyze/tx-reaper? (by "tx-reaper")))
    (is (not (analyze/tx-reaper? (by "plain-worker"))))))
