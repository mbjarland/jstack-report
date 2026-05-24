(ns jstack-report.analyze-test
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jstack-report.core :as core]))

(defn fixture-lines [name]
  (str/split-lines (slurp (jio/resource (str "dumps/" name)))))

;; ---------------------------------------------------------------------------
;; Transitive path & graph

(deftest transitive-path-walks-from-leaf-to-root
  (let [d  (core/dump (fixture-lines "apple-orange-banana.txt"))
        wt (core/waiters-by-tid d)
        c  (first (filter #(= "thread-C" (:NAME %)) (:threads d)))
        p  (core/transitive-path wt c)]
    (is (= ["0x000000000000000a" "0x000000000000000b" "0x000000000000000c"]
           (map :tid p)))))

(deftest transitive-lock-graph-has-single-root
  (let [d (core/dump (fixture-lines "apple-orange-banana.txt"))
        g (core/transitive-lock-graph d)]
    (testing "exactly one root tid"
      (is (= 1 (count g))))
    (testing "root is thread-X"
      (is (= "0x000000000000000a" (-> g first key :tid))))))

(deftest transitive-lock-graph-wide-fanout
  (let [d (core/dump (fixture-lines "wide-graph.txt"))
        g (core/transitive-lock-graph d)
        root (first g)
        [_ children] root]
    (is (= 1 (count g)))
    (is (= "0x0000000000000100" (-> root key :tid)))
    (is (= 5 (count children)))
    (is (every? nil? (vals children)))))

(deftest empty-graph-when-no-blocking
  (let [d (core/dump (fixture-lines "minimal.txt"))]
    (is (empty? (core/transitive-lock-graph d)))))

;; ---------------------------------------------------------------------------
;; key-count-in / keys-in tree utilities

(deftest keys-in-walks-nested-map
  (is (= [[:a :b] [:a :c :d]]
         (core/keys-in {:a {:b 1 :c {:d 2}}}))))

(deftest key-count-in-counts-distinct-keys-in-tree
  (is (= 4 (core/key-count-in {:a {:b 1 :c {:d 2}}})))
  (is (= 0 (core/key-count-in nil)))
  (is (= 0 (core/key-count-in :not-a-map))))

;; ---------------------------------------------------------------------------
;; Trace-based predicates

(deftest db-socket-read-detection
  (let [d (core/dump (fixture-lines "db-socket-read.txt"))
        by (fn [n] (first (filter #(= n (:NAME %)) (:threads d))))]
    (testing "matches socketRead0 + oracle wrapper"
      (is (boolean (core/db-socket-read? (by "db-read-regular"))))
      (is (boolean (core/db-socket-read? (by "db-read-validating")))))
    (testing "does not match plain workers"
      (is (not (core/db-socket-read? (by "plain-worker")))))
    (testing "isValid variant detection"
      (is (boolean (core/db-socket-read-is-valid? (by "db-read-validating"))))
      (is (not (core/db-socket-read-is-valid? (by "db-read-regular")))))))

(deftest tx-reaper-detection
  (let [d  (core/dump (fixture-lines "db-socket-read.txt"))
        by (fn [n] (first (filter #(= n (:NAME %)) (:threads d))))]
    (is (boolean (core/tx-reaper? (by "tx-reaper"))))
    (is (not (core/tx-reaper? (by "plain-worker"))))))

;; ---------------------------------------------------------------------------
;; Longest common prefix helper

(deftest longest-common-prefix-vector-out
  (is (= [1 2 3]   (core/longest-common-prefix [1 2 3 4] [1 2 3 9])))
  (is (= []        (core/longest-common-prefix [1] [2])))
  (is (= [:a]      (core/longest-common-prefix [:a :b] [:a :c]))))
