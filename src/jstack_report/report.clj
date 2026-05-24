(ns ^{:doc "Orchestrates printing of a full report: stats, headline,
transitive lock graph, oldest/youngest requests, top clients, longest
traces, top URLs, and DB socketRead0 callouts."
      :author "Matias Bjarland"}
  jstack-report.report
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]
            [jstack-report.analyze :as analyze]
            [jstack-report.ansi :as ansi]
            [jstack-report.model :as model]
            [jstack-report.render :as render])
  (:import [java.time.format DateTimeFormatter]))

(def ^:private color render/color)

;; ---------------------------------------------------------------------------
;; Section headers

(defn ^:private header
  "Render a section header in a uniform style: bold-white title, an
  optional muted count after a middle-dot separator, blank line above
  and below."
  ([title]
   (println)
   (println (color [:bright :white] title))
   (println))
  ([title detail]
   (println)
   (println (str (color [:bright :white] title)
                 " "
                 (color [:bright :black] (str "· " detail))))
   (println)))

;; ---------------------------------------------------------------------------
;; Headline (the "what should I look at first?" line)

(defn ^:private headline [dump]
  (let [graph (analyze/transitive-lock-graph dump)]
    (println)
    (if (empty? graph)
      (println (color [:bright :green] "✓ No transitive lock chains detected"))
      (let [[root children] (first graph)
            threads-by-tid  (analyze/threads-by-tid dump)
            root-thread     (get threads-by-tid (:tid root))
            blocked         (analyze/key-count-in children)]
        (println (color [:bright :red] "⚠ Root blocker:")
                 (color [:bright :white] (:NAME root-thread))
                 (color [:bright :black] (str "(tid " (:tid root) ")"))
                 (color [:bright :red] (str "blocks " blocked " other threads")))
        (println (color [:bright :black]
                        "  see TRANSITIVE LOCK GRAPH below"))))))

;; ---------------------------------------------------------------------------
;; Sections

(defn ^:private print-stats [dump]
  (let [threads      (:threads dump)
        count-by     (fn [p] (count (filter p threads)))
        trace-pred   #(< analyze/trace-report-limit (count (:trace %)))
        display-date #(.format (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss") %)
        lock-waits   (count-by #(= (-> % :waiting-on :wait-type) :synchronized))
        labels       [["jstack dump date" (when (:date dump) (display-date (:date dump)))]
                      ["total threads"             (count threads)]
                      ["threads waiting for locks" lock-waits]
                      ["request threads"           (count-by :request)]
                      [(str "traces > " analyze/trace-report-limit " lines") (count-by trace-pred)]]
        width        (apply max (map (comp count first) labels))
        fg           [:green]]
    (header "STATISTICS")
    (doseq [[label value] labels]
      (when (some? value)
        (println (format (str "  %-" width "s  ") label)
                 (color fg value))))))

(defn ^:private print-lock-graph [dump]
  (let [graph (analyze/transitive-lock-graph dump)
        lines (render/render-lock-graph dump graph)
        count (analyze/key-count-in graph)]
    (when (seq graph)
      (header "TRANSITIVE LOCK GRAPH" (str count " threads"))
      (doseq [line lines]
        (println line)))))

(defn ^:private print-oldest-threads [dump n]
  (let [threads (sort-by model/req-date (filter model/req-date (:threads dump)))]
    (when (seq threads)
      (header (str n " OLDEST REQUEST THREADS"))
      (doseq [t (take n threads)]
        (println (color [:green] (format "%10s" (-> t :request :display-age)))
                 (:NAME t))))))

(defn ^:private print-youngest-threads [dump n]
  (let [threads (sort-by model/req-date (filter model/req-date (:threads dump)))]
    (when (seq threads)
      (header (str n " YOUNGEST REQUEST THREADS"))
      (doseq [t (reverse (take-last n threads))]
        (println (color [:green] (format "%10s" (-> t :request :display-age)))
                 (:NAME t))))))

(defn ^:private grouped-tops [threads key-fn n]
  (let [groups (group-by key-fn (filter key-fn threads))
        sorted (sort-by (fn [[_ v]] (- (count v)))
                        (keep (fn [[k v]] (when (< 1 (count v)) [k v])) groups))]
    (take n sorted)))

(defn ^:private print-clients-with-most-requests [dump n]
  (let [top-x (grouped-tops (:threads dump) #(-> % :request :cid) n)]
    (when (seq top-x)
      (header (str "TOP " n " CLIENT IDS WITH MOST REQUESTS"))
      (doseq [[cid threads] top-x]
        (println (color [:green] "  cid " cid " - " (count threads) " threads"))
        (doseq [t (sort-by (fn [t] (- (-> t :request :age-seconds))) threads)]
          (println "     "
                   (color [:cyan] (format "%-10s" (str "age " (format "%6s" (-> t :request :display-age)))))
                   (:NAME t)))))))

(defn ^:private print-longest-traces [dump n]
  (let [threads (sort-by #(- (count (:trace %))) (:threads dump))
        top-x   (take n threads)]
    (when (seq top-x)
      (header (str "TOP " n " THREADS WITH LONGEST TRACES"))
      (doseq [t top-x]
        (println (color [:green] (format "    %4s lines" (count (:trace t))))
                 "  " (:NAME t))))))

(defn ^:private print-most-requested-urls [dump n]
  (let [top-x (grouped-tops (:threads dump) #(-> % :request :url) n)]
    (when (seq top-x)
      (header (str "TOP " n " REQUESTED URLS"))
      (doseq [[url threads] top-x]
        (println "    " (color [:green] (format "%3d" (count threads)) " threads ") url)))))

(defn ^:private print-threads-in-db-socket-read [dump]
  (let [threads (filter analyze/db-socket-read? (:threads dump))]
    (when (seq threads)
      (header "THREADS WAITING ON DB IN SocketRead0" (str (count threads) " threads"))
      (doseq [t (sort-by :NAME threads)]
        (let [age     (or (-> t :request :display-age) "")
              isValid (if (analyze/db-socket-read-is-valid? t)
                        (color [:bright :black] "[in isValid]")
                        "")]
          (println "     "
                   (color [:green] (format "%10s" age))
                   (:NAME t)
                   isValid))))))

;; ---------------------------------------------------------------------------
;; Top-level report

(defn report [dump]
  (println)
  (println (color [:bright :white] "THREAD DUMP REPORT"))
  (headline dump)
  (print-stats dump)
  (print-lock-graph dump)
  (print-oldest-threads dump 10)
  (print-youngest-threads dump 10)
  (print-clients-with-most-requests dump 5)
  (print-longest-traces dump 10)
  (print-most-requested-urls dump 10)
  (print-threads-in-db-socket-read dump)
  (println))

(defn jstack-report [opts]
  (let [source (or (:file opts) *in*)]
    (with-open [reader (jio/reader source)]
      (binding [ansi/*use-ansi* (not (:no-color opts))]
        (if (.ready reader)
          (report (model/dump reader))
          (println "no lines - skipping report (-h for help)"))))))
