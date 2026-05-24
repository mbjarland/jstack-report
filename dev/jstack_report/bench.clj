(ns jstack-report.bench
  "Phase-by-phase wall-clock benchmark for a thread-dump file. Run with

       lein with-profiles +bench run -m jstack-report.bench <path/to/dump.txt> [runs]

  Reports timings for each layer in the pipeline (parse, reconcile,
  decorate, index, transitive graph, render, full report) so we can
  see where time actually goes rather than guessing."
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]
            [jstack-report.analyze :as analyze]
            [jstack-report.ansi :as ansi]
            [jstack-report.model :as model]
            [jstack-report.parser :as parser]
            [jstack-report.render :as render]
            [jstack-report.report :as report])
  (:gen-class))

(defn ^:private bench-once [label f]
  (let [start (System/nanoTime)
        v     (f)
        end   (System/nanoTime)]
    {:label label
     :ms    (/ (- end start) 1e6)
     :value v}))

(defn ^:private bench-n [n label f]
  (let [runs (doall (repeatedly n #(bench-once label f)))
        mss  (map :ms runs)]
    {:label  label
     :runs   n
     :min-ms (apply min mss)
     :max-ms (apply max mss)
     :avg-ms (/ (reduce + mss) n)
     :value  (:value (first runs))}))

(defn ^:private fmt-ms [ms]
  (cond
    (< ms 1)    (format "%8.3f ms" (double ms))
    (< ms 1000) (format "%8.1f ms" (double ms))
    :else       (format "%8.2f s " (double (/ ms 1000)))))

(defn ^:private print-table
  "Print a sequence of bench-n result maps as an aligned table. Label
  column is sized to the longest label in the batch so every row's
  timing columns line up."
  [rows]
  (let [width (apply max (map (comp count :label) rows))
        fmt   (str "  %-" width "s   %s   (min %s, max %s%s)")]
    (doseq [{:keys [label avg-ms min-ms max-ms pct]} rows]
      (println (format fmt
                       label
                       (fmt-ms avg-ms)
                       (fmt-ms min-ms)
                       (fmt-ms max-ms)
                       (if pct (format ", %4.1f%% of total" pct) ""))))))

(defn ^:private discard
  "Force lazy seqs and Java collections so we time the actual work
  rather than thunk allocation."
  [x]
  (cond
    (seq? x)    (count x)
    (map? x)    (count x)
    (coll? x)   (count x)
    :else       (str x)))

(defn run-bench [path runs]
  (let [lines (vec (str/split-lines (slurp (jio/file path))))]
    (println)
    (println (format "Benchmark: %s" path))
    (println (format "  %d lines, file size %s"
                     (count lines)
                     (let [b (.length (jio/file path))]
                       (cond
                         (> b 1e6) (format "%.1f MB" (/ b 1e6))
                         (> b 1e3) (format "%.1f KB" (/ b 1e3))
                         :else     (str b " B")))))
    (println (format "  %d runs each, JVM warmed once" runs))
    (println)

    ;; --- warm-up
    (let [d (model/dump lines)]
      (discard (analyze/transitive-lock-graph d))
      (binding [ansi/*use-ansi* false]
        (discard (with-out-str (report/report d)))))

    ;; --- per-phase
    (let [d         (model/dump lines)
          thr-count (count (:threads d))
          graph     (analyze/transitive-lock-graph d)
          parse-r   (bench-n runs "parser/parse-jstack-lines"
                             #(discard (parser/parse-jstack-lines lines)))
          dump-r    (bench-n runs "model/dump (parse + reconcile + decorate)"
                             #(discard (model/dump lines)))
          idx-r     (bench-n runs "analyze/indexes (tid + oid + waiters)"
                             #(do (discard (analyze/threads-by-tid d))
                                  (discard (analyze/lockers-by-oid d))
                                  (discard (analyze/waiters-by-tid d))))
          graph-r   (bench-n runs "analyze/transitive-lock-graph"
                             #(discard (analyze/transitive-lock-graph d)))
          render-r  (bench-n runs "render/render-lock-graph"
                             #(binding [ansi/*use-ansi* false]
                                (discard (render/render-lock-graph d graph))))
          report-r  (bench-n runs "report/report (full text output)"
                             #(binding [ansi/*use-ansi* false]
                                (discard (with-out-str (report/report d)))))
          full-r    (bench-n runs "end-to-end (model/dump + report/report)"
                             #(binding [ansi/*use-ansi* false]
                                (discard (with-out-str (report/report (model/dump lines))))))
          rows      [parse-r dump-r idx-r graph-r render-r report-r full-r]
          total     (:avg-ms full-r)]
      (println (format "  threads parsed: %d  ·  graph roots: %d  ·  blocked threads: %d"
                       thr-count
                       (count graph)
                       (analyze/key-count-in graph)))
      (println)
      (print-table
        (for [r rows]
          (assoc r :pct (* 100.0 (/ (:avg-ms r) total))))))))

(defn -main [& [path runs]]
  (let [runs (Integer/parseInt (or runs "5"))]
    (run-bench path runs)
    (shutdown-agents)))
