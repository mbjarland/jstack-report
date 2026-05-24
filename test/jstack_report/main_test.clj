(ns jstack-report.main-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jstack-report.main :as main]))

(deftest version-string-renders-without-exploding
  (is (string? (main/version-string))))

(deftest help-flag-produces-usage
  (let [{:keys [exit-message ok?]} (#'main/validate-args ["-h"])]
    (is ok?)
    (is (string? exit-message))
    (is (str/includes? exit-message "jstack-report"))))

(deftest unknown-flag-produces-error
  (let [{:keys [exit-message ok?]} (#'main/validate-args ["--nonsense"])]
    (is (not ok?))
    (is (str/includes? exit-message "Errors"))))

(deftest file-flag-validates-existence
  (let [{:keys [exit-message]} (#'main/validate-args
                                 ["-f" "/does/not/exist/jstack.txt"])]
    (is (str/includes? exit-message "Must be a readable file"))))
