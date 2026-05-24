(ns build
  "Build script for jstack-report. Invoke via the :build alias:

       clojure -T:build clean
       clojure -T:build test
       clojure -T:build uber

  Replaces the previous lein uberjar workflow. Writes a build/version.edn
  before AOT-compiling jstack-report.main so the --help output picks up
  the current git ref."
  (:refer-clojure :exclude [test])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.build.api :as b]))

(def lib       'jstack-report)
(def version   "1.3.1")
(def class-dir "target/classes")
(def uber-file (format "target/%s-%s-standalone.jar" (name lib) version))
(def basis     (delay (b/create-basis {:project "deps.edn"})))

;; ---------------------------------------------------------------------------
;; Helpers

(defn ^:private git
  "Run a git command and return the trimmed stdout, or an empty string
  if the command fails (e.g. building outside a checkout)."
  [args]
  (try
    (str/trim (b/git-process {:git-args args}))
    (catch Exception _ "")))

(defn ^:private write-version-edn []
  (let [ref       (git "rev-parse HEAD")
        ref-short (git "rev-parse --short HEAD")
        status    (git "status --porcelain")
        v         {:ref       ref
                   :version   version
                   :timestamp (str (quot (System/currentTimeMillis) 1000))
                   :dirty?    (not (str/blank? status))
                   :ref-short ref-short}
        path      "gen-resources/build/version.edn"]
    (io/make-parents path)
    (spit path (pr-str v))
    (println "Wrote" path "->" v)))

;; ---------------------------------------------------------------------------
;; Public tasks

(defn clean
  "Remove all build outputs."
  [_]
  (b/delete {:path "target"})
  (b/delete {:path "gen-resources/build"})
  (println "Cleaned target/ and gen-resources/build/"))

(defn test
  "Run the test suite. Equivalent to `clojure -M:test`."
  [_]
  (let [{:keys [exit]} (b/process {:command-args
                                   ["clojure" "-M:test"]})]
    (when-not (zero? exit)
      (System/exit exit))))

(defn uber
  "Build the standalone uberjar at target/jstack-report-<version>-standalone.jar.

  Steps: clean, write gen-resources/build/version.edn from git, copy
  sources + resources, AOT-compile jstack-report.main with direct
  linking, then package the uber."
  [_]
  (clean nil)
  (write-version-edn)
  (b/copy-dir {:src-dirs   ["src" "gen-resources"]
               :target-dir class-dir})
  (b/compile-clj {:basis      @basis
                  :ns-compile '[jstack-report.main]
                  :class-dir  class-dir
                  :java-opts  ["-Dclojure.compiler.direct-linking=true"
                               "-Dclojure.spec.skip-macros=true"]})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis     @basis
           :main      'jstack-report.main})
  (println "Created" uber-file))
