(ns thread-watch.cli
  (:require [clojure.tools.cli :only [parse-opts]]
            [clojure.string :refer [join split]]))

(defn exit [status msg]
  (println msg)
  (System/exit status))


(defn this-jar
  "utility function to get the name of jar in which this function is invoked"
  [& [ns]]
  (as-> (or ns (class *ns*)) s
        (.getProtectionDomain s)
        (.getCodeSource s)
        (.getLocation s)
        (.getPath s)
        (split s #"/")
        (last s)))

(defn get-usage [options-summary]
  (->> ["PURPOSE"
        ""
        "  This script will allow you to monitor specific JVM threads for activity"
        ""
        "  You would typically invoke this script using something like:"
        ""
        "    watch -n 1 \"jstack <pid> | java -jar thread-watch.jar -t <tid>\""
        ""
        "  which gives you a live updating stack trace view of the "
        "  thread with the specified tid."
        ""
        "  You can find the tid of a thread by running jstack once"
        "  on the pid and looking at the first lines of every thread"
        "  stack trace. "
        ""
        (str "Usage: java -jar " (this-jar) " [options]")
        ""
        "Options:"
        options-summary
        ""
        "Author: Matias Bjarland / matias@iteego.com - 2017.Apr.28"]
       (join \newline)))

(defn error-msg [prefix errors postfix]
  (str prefix postfix
       (join "\n       " errors)))

(def option-def
  [
   ["-t" "--tid <tid>" "the 0x00002b7ae4090000 part of tid=0x00002b7ae4090000 in traces"]
   ;["-p" "--pid <pid>" "PID of java process to analyze"]
   ;["-n" "--interval <seconds>" "Refresh interval. Defaul: 1"]
   ;["-s" "--jstac-executable <path>" "path to jstack exexutable. Defaults to assuming it's on the path"]
   ["-h" "--help" "Prints this help screen"]])

(defn missing-required-opts [opts]
  (as->
    (filter #(not (% opts)) [:tid]) s
    (map #(str "--" (name %)) s)
    (if (empty? s) nil [(join ", " s)])))

(defn validate-cmd-line [opts errors summary]
  (let [missing (missing-required-opts opts)
        usage (str (get-usage summary) "\n")]
    (cond
      (:help opts) (exit 0 usage)
      errors (exit 1 (error-msg usage errors "\nERROR: "))
      missing (exit 1 (error-msg usage missing "\nERROR missing required options: ")))))






















