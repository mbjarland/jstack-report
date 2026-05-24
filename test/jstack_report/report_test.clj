(ns jstack-report.report-test
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jstack-report.ansi :as ansi]
            [jstack-report.core :as core])
  (:import [java.io BufferedReader StringReader]))

(defn fixture-lines [name]
  (str/split-lines (slurp (jio/resource (str "dumps/" name)))))

(defn capture-report [dump-name]
  (let [lines (fixture-lines dump-name)
        out   (java.io.StringWriter.)]
    (binding [*out* out]
      (ansi/without-ansi
        (core/report (core/dump lines))))
    (str out)))

;; ---------------------------------------------------------------------------
;; End-to-end report output

(deftest report-renders-statistics-section
  (let [out (capture-report "apple-orange-banana.txt")]
    (is (str/includes? out "STATISTICS"))
    (is (str/includes? out "total threads:"))
    (is (str/includes? out "4"))))

(deftest report-renders-transitive-lock-graph
  (let [out (capture-report "apple-orange-banana.txt")]
    (is (str/includes? out "TRANSITIVE LOCK GRAPH"))
    (is (str/includes? out "thread-X"))
    (is (str/includes? out "thread-A"))
    (is (str/includes? out "thread-B"))
    (is (str/includes? out "thread-C"))))

(deftest report-omits-graph-when-no-blocking
  (let [out (capture-report "minimal.txt")]
    (is (str/includes? out "No transitive lock chains"))))

(deftest report-handles-request-threads
  (let [out (capture-report "request-threads.txt")]
    (is (str/includes? out "OLDEST REQUEST THREADS"))
    (is (str/includes? out "YOUNGEST REQUEST THREADS"))))

;; ---------------------------------------------------------------------------
;; Multimethod dispatch

(deftest dump-accepts-seq-of-lines
  (let [lines (fixture-lines "minimal.txt")
        d     (core/dump lines)]
    (is (= 1 (count (:threads d))))))

(deftest dump-accepts-reader
  (let [r (BufferedReader.
            (StringReader.
              (slurp (jio/resource "dumps/minimal.txt"))))]
    (let [d (core/dump r)]
      (is (= 1 (count (:threads d)))))))

(deftest dump-accepts-file
  (let [src  (jio/resource "dumps/minimal.txt")
        tmp  (java.io.File/createTempFile "jstack-test" ".txt")]
    (try
      (spit tmp (slurp src))
      (let [d (core/dump tmp)]
        (is (= 1 (count (:threads d)))))
      (finally
        (.delete tmp)))))

;; ---------------------------------------------------------------------------
;; Parsed dump structure invariants

(deftest dump-preserves-prelude-and-epilogue
  (let [d (core/dump (fixture-lines "minimal.txt"))]
    (is (seq (:prelude d)))
    (is (seq (:epilogue d)))
    (is (= "2024-01-15 09:30:45" (first (:prelude d))))))

(deftest dump-captures-date-when-present
  (let [d (core/dump (fixture-lines "minimal.txt"))]
    (is (some? (:date d)))))
