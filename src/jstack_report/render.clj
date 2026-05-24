(ns ^{:doc "Pure presentation: ASCII tree drawing for the lock graph,
plus the small helpers (`color`, `short-name`) shared by report
sections. No I/O — everything returns lines/strings that the report
namespace prints."
      :author "Matias Bjarland"}
  jstack-report.render
  (:require [clojure.string :as str]
            [jstack-report.analyze :as analyze]
            [jstack-report.ansi :as ansi]))

(defn color
  "Apply a vector of ANSI styles to one or more text fragments. Returns
  nil when every fragment is nil/empty so that callers can splice
  without emitting empty escape sequences."
  [styles & xs]
  (let [s (apply str xs)]
    (when (seq s)
      (apply ansi/style s styles))))

(defn short-name
  "Strip a fully-qualified Java class name down to its last segment."
  [fqn]
  (last (re-seq #"[^.]+" fqn)))

;; ---------------------------------------------------------------------------
;; Generic tree rendering

(defn render-tree
  "Render a {key children} tree into a sequence of strings with Unicode
  box-drawing prefixes. `render-fn` is called as `(render-fn key
  children)` and must return the seq of lines for a node (the first
  is the label, the rest are continuation lines indented under it)."
  ([key val]
   (render-tree str compare key val))
  ([render-fn key-comp-f key val]
   (let [graph-color [:magenta]
         I-short     (color graph-color "│ ")
         I-branch    (color graph-color "│   ")
         T-branch    (color graph-color "├── ")
         L-branch    (color graph-color "└── ")
         spacer      "    "
         pre         (if (pos? (count val)) I-short "")
         label       (render-fn key val)
         label       (cons (first label) (map #(str pre %) (rest label)))]
     (concat label
             (mapcat
               (fn [[c-key c-val] index]
                 (let [subtree      (render-tree render-fn key-comp-f c-key c-val)
                       last?        (= index (dec (count val)))
                       prefix-first (if last? L-branch T-branch)
                       prefix-rest  (if last? spacer I-branch)]
                   (cons (str prefix-first (first subtree))
                         (map #(str prefix-rest %) (next subtree)))))
               (into (sorted-map-by key-comp-f) val)
               (range))))))

;; ---------------------------------------------------------------------------
;; Lock-graph node rendering

(defn render-graph-node [threads-by-tid k m]
  (let [thread      (get threads-by-tid (:tid k))
        has-kids?   (pos? (count m))
        class       (first (keep (fn [{:keys [oid class]}] (when (= oid (:oid k)) class))
                                 (:locked thread)))
        b-count     (analyze/key-count-in m)
        fg-normal   [:green]
        fg-bright   [:bright :cyan]
        fg-extra    [:bright :black]
        extra       (color fg-extra (analyze/thread-extra-info thread))
        age         (color fg-normal (analyze/thread-display-age thread))
        second-line (when has-kids?
                      (str
                        (color fg-normal "tid " (:tid thread) " locked ")
                        (color fg-bright (short-name class))
                        (color fg-normal " " (:oid k) " - ")
                        (color fg-bright "blocks " b-count " threads")))]
    (cond-> [(str/join " " [(:name thread) age extra])]
            has-kids? (conj second-line))))

(defn render-lock-graph
  "Render the full transitive lock graph as a flat sequence of lines."
  ([dump]
   (render-lock-graph dump (analyze/transitive-lock-graph dump)))
  ([dump graph]
   (let [threads-by-tid (analyze/threads-by-tid dump)
         render-fn      (partial render-graph-node threads-by-tid)
         name           (fn [tid] (:name (get threads-by-tid tid)))
         key-comp-f     (fn [a b]
                          (let [nc (compare (name (:tid a)) (name (:tid b)))]
                            (if (zero? nc) (compare (:oid a) (:oid b)) nc)))]
     (mapcat (fn [[k v]] (render-tree render-fn key-comp-f k v)) graph))))
