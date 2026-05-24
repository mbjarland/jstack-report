(ns jstack-report.render-test
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jstack-report.analyze :as analyze]
            [jstack-report.ansi :as ansi]
            [jstack-report.model :as model]
            [jstack-report.render :as render]))

(defn fixture-lines [name]
  (str/split-lines (slurp (jio/resource (str "dumps/" name)))))

;; ---------------------------------------------------------------------------
;; ANSI gating

(deftest style-emits-codes-by-default
  (is (str/includes? (ansi/style "x" :red) "[31m")))

(deftest without-ansi-suppresses-codes
  (ansi/without-ansi
    (let [s (ansi/style "hello" :red :bg-blue)]
      (is (= "hello" s)))))

;; ---------------------------------------------------------------------------
;; Tree rendering

(deftest render-lock-graph-produces-tree-shape
  (let [d     (model/dump (fixture-lines "apple-orange-banana.txt"))
        lines (ansi/without-ansi
                (doall (render/render-lock-graph d)))]
    (testing "rendering produces at least one line"
      (is (seq lines)))
    (testing "tree contains box-drawing characters somewhere"
      (is (some #(re-find #"[├└│]" %) lines)))
    (testing "deepest leaf is in the output"
      (is (some #(str/includes? % "thread-C") lines)))))

(deftest render-graph-node-shows-blocker-count
  (let [d              (model/dump (fixture-lines "wide-graph.txt"))
        threads-by-tid (analyze/threads-by-tid d)
        graph          (analyze/transitive-lock-graph d)
        [k v]          (first graph)
        rendered       (ansi/without-ansi
                         (render/render-graph-node threads-by-tid k v))]
    (is (some #(re-find #"blocks 5 threads" %) rendered))))

(deftest render-graph-node-leaf-has-one-line-only
  (let [d              (model/dump (fixture-lines "wide-graph.txt"))
        threads-by-tid (analyze/threads-by-tid d)
        graph          (analyze/transitive-lock-graph d)
        [_ children]   (first graph)
        [k v]          (first children)
        rendered       (ansi/without-ansi
                         (render/render-graph-node threads-by-tid k v))]
    (is (= 1 (count rendered)))))

(deftest short-name-strips-fqn
  (is (= "Object" (render/short-name "java.lang.Object")))
  (is (= "Apple"  (render/short-name "example.deep.nested.Apple"))))
