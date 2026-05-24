(ns ^{:doc "Turns the structural skeleton produced by the parser into
an enriched dump suitable for analysis: lock/wait reconciliation,
request-thread decoration from the `ajp|HHmmss|cid=...|rid=...|url`
naming convention, and per-thread age computation."
      :author "Matias Bjarland"}
  jstack-report.model
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]
            [jstack-report.parser :as parser])
  (:import [java.io BufferedReader File Reader]
           [java.time Duration LocalDateTime LocalTime ZoneOffset]
           [java.time.format DateTimeFormatter]))

(def date-roll-fluff-seconds
  "Slack used to decide whether a thread's HH:mm:ss timestamp belongs
  to the same day as the dump (or the previous one). A thread named
  with a time strictly later than dump-time + this many seconds is
  treated as having been started yesterday — jstack does not put dates
  in thread names so we have to infer the rollover."
  5)

;; ---------------------------------------------------------------------------
;; Lock reconciliation

(defn remove-waiting-on-lock [locks t]
  (let [result (filter #(not= (:oid %) (:oid t)) locks)]
    (if (empty? result) nil result)))

(defn extract-locks-and-wait
  "Given a thread's trace, return `[locks waiting-on]` where `locks`
  is the set of held lock entries and `waiting-on` describes the lock
  the thread is blocked on (if any). Walks the trace bottom-to-top so
  earlier-acquired locks land first in the resulting vector."
  [thread]
  (reduce
    (fn [[locks wait-oid] te]
      (cond
        (and locks (= (:wait-type te) :notify))
        [(remove-waiting-on-lock locks te) wait-oid]

        (#{:concurrent :synchronized :re-lock} (:wait-type te))
        [locks {:oid (:oid te) :class (:class te) :wait-type (:wait-type te)}]

        (= (:type te) :locked)
        [((fnil conj []) locks {:oid (:oid te) :class (:class te)}) wait-oid]

        :else
        [locks wait-oid]))
    [nil nil]
    (-> thread :trace reverse)))

(defn reconcile-locks
  "Post-process a parsed thread, assoc'ing :locked (in acquisition
  order) and :waiting-on derived from the trace."
  [thread]
  (let [[locks waiting-on] (extract-locks-and-wait thread)]
    (cond-> thread
            locks      (assoc :locked locks)
            waiting-on (assoc :waiting-on waiting-on))))

;; ---------------------------------------------------------------------------
;; Time / age

(defn date->seconds [^LocalDateTime date]
  (.toEpochSecond date ZoneOffset/UTC))

(defn seconds-between [^LocalDateTime new ^LocalDateTime old]
  (- (date->seconds new) (date->seconds old)))

(defn req-date [t]
  (-> t :request :date))

(defn display-duration
  "Format an integer seconds count as `[Hh][Mm]Ss` with the leading
  units suppressed when zero."
  [seconds]
  (let [zf (fn [n u] (if (= n 0) "" (str n u)))
        h  (int (/ seconds 3600))
        m  (int (/ (mod seconds 3600) 60))
        s  (mod seconds 60)]
    (str (zf h "h") (zf m "m") (format "%02ds" s))))

(defn thread-date
  "Combine a dump's wall-clock date and a `HHmmss.SSS` time pulled out
  of a thread name into an absolute LocalDateTime. If the time is
  strictly after `dump-date + date-roll-fluff-seconds` the thread is
  assumed to have started the previous day."
  [dump-date time-str]
  (when time-str
    (let [one-day    (Duration/ofDays 1)
          fluff      (Duration/ofSeconds date-roll-fluff-seconds)
          fluff-date (.plus dump-date fluff)
          time       (LocalTime/parse time-str
                                      (DateTimeFormatter/ofPattern "HHmmss.SSS"))
          date       (.atDate time (.toLocalDate dump-date))]
      (if (.isAfter date fluff-date)
        (.minus date one-day)
        date))))

;; ---------------------------------------------------------------------------
;; Thread name decoration

(defn ^:private name-part [token]
  (fn [part]
    (let [[_ lhs rhs] (re-find #"([^=]+)=([^=]+)" part)]
      (when (= lhs token) rhs))))

(defn parse-thread-name
  "Parse a thread name of the form

      ajp|093041.250|cid=clientA|rid=req001|oip=10.0.0.1|/api/x

  into `{:pre :time :cid :rid :oip :url}`. Returns nil when the name
  has no pipe-separated parts."
  [name]
  (let [parts   (str/split name #"\|")
        extract (fn [token] (first (keep (name-part token) parts)))]
    (when (< 1 (count parts))
      {:pre  (first parts)
       :time (nth parts 1)
       :cid  (extract "cid")
       :rid  (extract "rid")
       :oip  (extract "oip")
       :url  (last parts)})))

(defn decorate-request-thread
  "If a thread's name follows the request-thread convention (ajp/http
  prefix), assoc a :request map describing the request."
  [dump-date thread]
  (let [{:keys [pre time cid rid oip url]} (parse-thread-name (:name thread))]
    (if (or (= pre "ajp") (= pre "http"))
      (assoc thread :request (into (sorted-map)
                                   {:time time
                                    :date (thread-date dump-date time)
                                    :cid  cid
                                    :rid  rid
                                    :oip  oip
                                    :url  url}))
      thread)))

(defn decorate-thread-age [newest-date threads]
  (let [age-secs (fn [t] (seconds-between newest-date (req-date t)))]
    (map (fn [t]
           (if (req-date t)
             (let [secs (age-secs t)]
               (-> t
                   (assoc-in [:request :age-seconds] secs)
                   (assoc-in [:request :display-age] (display-duration secs))))
             t))
         threads)))

(defn decorate-request-threads
  "Walk the dump's threads, attach :request to ajp/http threads, then
  attach :request/:age-seconds and :request/:display-age relative to
  the newest request thread in the dump."
  [dump]
  (let [req-decorator (partial decorate-request-thread (:date dump))
        dump          (update dump :threads #(map req-decorator %))
        req-threads   (sort-by req-date (filter req-date (:threads dump)))
        newest-date   (req-date (last req-threads))]
    (update dump :threads (partial decorate-thread-age newest-date))))

;; ---------------------------------------------------------------------------
;; Dump entry point

(defn ^:private source-type [src]
  (cond
    (seq? src)                :seq
    (vector? src)             :seq
    (instance? File src)      :file
    (string? src)             :string
    (instance? Reader src)    :reader
    :else                     (throw (ex-info (str "unknown line source: " src)
                                              {:class (class src)}))))

(defmulti dump
  "Parse a jstack thread dump and return an enriched dump map. Accepts
  a seq of lines, a vector of lines, a File, a path-like String, or a
  Reader."
  {:arglists '[[line-source]]}
  #'source-type)

(defmethod dump :seq [lines]
  (-> lines
      parser/parse-jstack-lines
      (update :threads #(map reconcile-locks %))
      decorate-request-threads))

(defmethod dump :reader [reader]
  (if (instance? BufferedReader reader)
    (dump (line-seq reader))
    (dump (line-seq (BufferedReader. reader)))))

(defmethod dump :file [file]
  (with-open [rdr (jio/reader file)]
    (dump rdr)))

(defmethod dump :string [s]
  (with-open [rdr (jio/reader (jio/file s))]
    (dump rdr)))
