(ns jstack-report.report-test
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jstack-report.ansi :as ansi]
            [jstack-report.model :as model]
            [jstack-report.report :as report])
  (:import [java.io BufferedReader StringReader]))

(defn fixture-lines [name]
  (str/split-lines (slurp (jio/resource (str "dumps/" name)))))

(defn capture-report [dump-name]
  (let [lines (fixture-lines dump-name)
        out   (java.io.StringWriter.)]
    (binding [*out* out]
      (ansi/without-ansi
        (report/report (model/dump lines))))
    (str out)))

;; ---------------------------------------------------------------------------
;; End-to-end report output

(deftest report-renders-statistics-section
  (let [out (capture-report "apple-orange-banana.txt")]
    (is (str/includes? out "STATISTICS"))
    (is (str/includes? out "total threads"))
    (is (str/includes? out "4"))))

(deftest report-renders-transitive-lock-graph
  (let [out (capture-report "apple-orange-banana.txt")]
    (is (str/includes? out "TRANSITIVE LOCK GRAPH"))
    (is (str/includes? out "thread-X"))
    (is (str/includes? out "thread-A"))
    (is (str/includes? out "thread-B"))
    (is (str/includes? out "thread-C"))))

(deftest report-shows-no-chains-headline-when-graph-empty
  (let [out (capture-report "minimal.txt")]
    (is (str/includes? out "No transitive lock chains"))))

(deftest report-shows-root-blocker-headline-when-graph-non-empty
  (let [out (capture-report "wide-graph.txt")]
    (is (str/includes? out "Root blocker"))
    (is (str/includes? out "root-blocker"))
    (is (str/includes? out "blocks 5"))))

(deftest report-handles-request-threads
  (let [out (capture-report "request-threads.txt")]
    (is (str/includes? out "OLDEST REQUEST THREADS"))
    (is (str/includes? out "YOUNGEST REQUEST THREADS"))))

;; ---------------------------------------------------------------------------
;; Multimethod dispatch

(deftest dump-accepts-seq-of-lines
  (let [lines (fixture-lines "minimal.txt")
        d     (model/dump lines)]
    (is (= 1 (count (:threads d))))))

(deftest dump-accepts-reader
  (let [r (BufferedReader.
            (StringReader.
              (slurp (jio/resource "dumps/minimal.txt"))))]
    (let [d (model/dump r)]
      (is (= 1 (count (:threads d)))))))

(deftest dump-accepts-file
  (let [src (jio/resource "dumps/minimal.txt")
        tmp (java.io.File/createTempFile "jstack-test" ".txt")]
    (try
      (spit tmp (slurp src))
      (let [d (model/dump tmp)]
        (is (= 1 (count (:threads d)))))
      (finally
        (.delete tmp)))))

;; ---------------------------------------------------------------------------
;; Parsed dump structure invariants

(deftest dump-preserves-prelude-and-epilogue
  (let [d (model/dump (fixture-lines "minimal.txt"))]
    (is (seq (:prelude d)))
    (is (seq (:epilogue d)))
    (is (= "2024-01-15 09:30:45" (first (:prelude d))))))

(deftest dump-captures-date-when-present
  (let [d (model/dump (fixture-lines "minimal.txt"))]
    (is (some? (:date d)))))

;; ---------------------------------------------------------------------------
;; Backwards-compatible facade

(deftest core-facade-re-exports-dump-and-report
  (require 'jstack-report.core)
  (let [dump-fn   (ns-resolve 'jstack-report.core 'dump)
        report-fn (ns-resolve 'jstack-report.core 'report)]
    (is (some? dump-fn))
    (is (some? report-fn))
    (is (= 1 (count (:threads (@dump-fn (fixture-lines "minimal.txt"))))))))
