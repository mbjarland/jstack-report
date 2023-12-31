(ns jstack-report.main
  (:require [clojure.edn :as edn]
            [clojure.java.io :as jio]
            [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]]
            [jstack-report.core :as core])
  (:import [java.io File PushbackReader Reader]
           [java.text SimpleDateFormat]
           [java.util Date])
  (:gen-class))

(defn version-string []
  (with-open [io-reader (jio/reader (or (jio/resource "build/version.edn")
                                        (jio/file "gen-resources/build/version.edn")))
              pb-reader (PushbackReader. io-reader)]
    (let [{:keys [timestamp ref-short version dirty?]} (edn/read pb-reader)
          dev-timestamp (str (Math/round ^Double (/ (System/currentTimeMillis) 1000.0)))
          timestamp     (parse-long (or timestamp dev-timestamp))
          format        (SimpleDateFormat. "yyyy.MM.dd HH:mm:ss")
          date          (.format format (Date. ^Long (* timestamp 1000)))]
      (str version " - " ref-short " - " date (if dirty? " +" "")))))

(def cli-options
  ;; An option with a required argument
  [["-f" "--file <jstack dump file>" "A thread dump file as produced by jstack"
    :parse-fn #(jio/as-file %)
    :validate [#(.isFile ^File %) "Must be a readable file on the local file system"]]
   ;; A non-idempotent option (:default is applied first)
   ["-n" "--no-color" "Disables ansi coloring of output"]
   ;; A boolean option defaulting to nil
   ["-h" "--help"]])

;; The :default values are applied first to options. Sometimes you might want
;; to apply default values after parsing is complete, or specifically to
;; compute a default value based on other option values in the map. For those
;; situations, you can use :default-fn to specify a function that is called
;; for any options that do not have a value after parsing is complete, and
;; which is passed the complete, parsed option map as it's single argument.
;; :default-fn (constantly 42) is effectively the same as :default 42 unless
;; you have a non-idempotent option (with :update-fn or :assoc-fn) -- in which
;; case any :default value is used as the initial option value rather than nil,
;; and :default-fn will be called to compute the final option value if none was
;; given on the command-line (thus, :default-fn can override :default)

(defn usage
  ([options-summary]
   (usage options-summary []))
  ([options-summary extra]
   (str/join
     \newline
     (concat
       [""
        (str "jstack-report - a tool to analyze lock chains in jvm thread dumps")
        ""
        "usage: java -jar jstack-report-1.0.0.jar -f <jstack-thread-dump-file>"
        ""
        "or"
        ""
        "usage: cat thread_dump.txt | java -jar jstack-report-1.0.0.jar"
        ""
        "Defaults to reading from stdin, i.e. the second usage example"
        "above."
        ""
        "Options:"
        options-summary
        ""
        ""
        "Author: Matias Bjarland / mbjarland@gmail.com"
        ""
        (str "jstack-report " (version-string))
        ""]
       extra))))

(defn error-msg [errors]
  (str "Errors:\n\n"
       (str/join \newline errors)))

(defn validate-args
  "Validate command line arguments. Either return a map indicating the program
  should exit (with a error message, and optional ok status), or a map
  indicating the action the program should take and the options provided."
  [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options)                                       ; help => exit OK with usage summary
      {:exit-message (usage summary) :ok? true}

      errors                                                ; errors => exit with description of errors
      {:exit-message (error-msg errors)}
      ;; custom validation on arguments

      (and (not (:file options))
           (not (.ready ^Reader *in*)))                     ; no indata
      {:exit-message (usage summary
                            ["no thread dump provided - doing nothing!" ""]) :ok? true}

      :else                                                 ; failed custom validation => exit with usage summary
      {:options options :arguments arguments})))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn main-entrypoint [hard-exit-on-errors? [& args]]
  (let [{:keys [action options exit-message ok?]} (validate-args args)]
    (if exit-message
      (if hard-exit-on-errors?
        (exit (if ok? 0 1) exit-message)
        (println "would exit with code " (if ok? 0 1) "msg," exit-message))
      (core/jstack-report options))))

(defn -main [& args]
  (main-entrypoint true args))

(defn repl-main [& args]
  (main-entrypoint false args))


(comment
  (repl-main "-h")

  )


