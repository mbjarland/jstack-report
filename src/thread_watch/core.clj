(ns thread-watch.core
  (:require [thread-watch.cli :refer [validate-cmd-line option-def]]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :refer [join split]]
            [clojure.java.shell :refer [sh]]
            [clojure.string :refer [starts-with?]]
            [lanterna.terminal :as t])
  (:gen-class)
  (:import (java.io BufferedReader)))

(defn get-file-lines [file-name]
  (with-open [rdr (clojure.java.io/reader file-name)]
    (into [] (doall (line-seq rdr)))))

; https://gist.github.com/rednaxelafx/843622
(defn parse-first-line [line]
  "parses lines of type '\"RMI TCP Connection(idle)\" daemon prio=10 tid=0x0000000050977800 nid=0x34d waiting on condition [0x00002b7b25bab000]'"
  (let [re #"\"([^\"]+)\"(?: ([^ ]*))? prio=([0-9]+) tid=(0x[0-9a-f]+) nid=(0x[0-9a-f]+) (.+)[ ]*"
        parts (rest (first (re-seq re line)))]
    {
     :name       (nth parts 0)
     :is-daemon  (not (nil? (nth parts 1)))
     :prio       (read-string (nth parts 2))
     :tid        (nth parts 3)
     :nid        (nth parts 4)
     :state      (nth parts 5)
     :first-seen (System/currentTimeMillis)
     :lines      [line]
     }))

(defn parse-second-line [stack line]
  "parses lines of type '   java.lang.Thread.State: WAITING (on object monitor)'"
  (let [[_ thread-state]
        (first (re-seq #"   java.lang.Thread.State: (.*+)" line))]
    (assoc
      (update-in stack [:lines] #(conj % line))
      :thread-state thread-state)))

(defn parse-jstack-output [lines]
  (let [[res current]
        (reduce
          (fn [[res stack] line]
            (cond
              (starts-with? line "\"") [(if (nil? stack) res (conj res stack))
                                        (parse-first-line line)]
              (starts-with? line "   ") [res (parse-second-line stack line)]
              (starts-with? line "\t") [res (update-in stack [:lines] #(conj % line))]
              :else [res stack]))
          [[] nil]
          lines)]
    (if (not-empty current)
      (conj res current)
      res)))

; main entry points
(defn get-std-in-lines []
  (doall (line-seq (BufferedReader. *in*))))


(defn print-stack [options line-provider]
  (let [tids (into #{} (split (:tid options) #","))
        stacks (parse-jstack-output (line-provider))
        hits (filter #(tids (:tid %)) stacks)]
    (doseq [stack hits]
      (doseq [line (:lines stack)]
        (println line)))))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]}
        (parse-opts args option-def)]
    (validate-cmd-line options errors summary)
    (print-stack options get-std-in-lines)))

(defn get-sample-lines []
  (get-file-lines "dumps/stack_only_aux_target.txt"))

(defn test-app []
  (print-stack {:tid "0x000000005072d800,0x000000005bac6000"}
             get-sample-lines))