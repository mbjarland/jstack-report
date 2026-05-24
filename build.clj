(ns build
  "Build script for jstack-report. Invoke via the :build alias:

       clojure -T:build clean
       clojure -T:build test
       clojure -T:build uber       ; standalone CLI distribution (GitHub release)
       clojure -T:build jar        ; library jar for Clojars
       clojure -T:build install    ; install library jar into local ~/.m2
       clojure -T:build deploy     ; push library jar to Clojars

  Deploy reads CLOJARS_USERNAME and CLOJARS_PASSWORD (a deploy token
  from https://clojars.org/tokens) from the environment.

  Writes gen-resources/build/version.edn from the git ref so the
  --help footer in the uberjar identifies the build."
  (:refer-clojure :exclude [test])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]))

;; ---------------------------------------------------------------------------
;; Coordinates

(def lib       'io.github.mbjarland/jstack-report)
(def main-ns   'jstack-report.main)
(def version   "1.3.1")

(def class-dir "target/classes")
(def jar-file  (format "target/%s-%s.jar"            (name lib) version))
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

(defn ^:private write-pom []
  (b/write-pom
    {:basis     @basis
     :class-dir class-dir
     :lib       lib
     :version   version
     :src-pom   "pom-template.xml"
     :src-dirs  ["src"]}))

;; ---------------------------------------------------------------------------
;; Tasks

(defn clean
  "Remove all build outputs."
  [_]
  (b/delete {:path "target"})
  (b/delete {:path "gen-resources/build"})
  (println "Cleaned target/ and gen-resources/build/"))

(defn test
  "Run the test suite. Equivalent to `clojure -M:test`."
  [_]
  (let [{:keys [exit]} (b/process {:command-args ["clojure" "-M:test"]})]
    (when-not (zero? exit)
      (System/exit exit))))

(defn uber
  "Build the standalone CLI uberjar at
  target/jstack-report-<version>-standalone.jar.

  This is the artifact attached to GitHub releases. Embeds Clojure
  and all dependencies; runnable with `java -jar`."
  [_]
  (clean nil)
  (write-version-edn)
  (b/copy-dir {:src-dirs   ["src" "gen-resources"]
               :target-dir class-dir})
  (b/compile-clj {:basis      @basis
                  :ns-compile [main-ns]
                  :class-dir  class-dir
                  :java-opts  ["-Dclojure.compiler.direct-linking=true"
                               "-Dclojure.spec.skip-macros=true"]})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis     @basis
           :main      main-ns})
  (println "Created" uber-file))

(defn jar
  "Build the library jar at target/jstack-report-<version>.jar.

  Plain (non-AOT, no embedded deps) — this is the artifact published
  to Clojars so downstream projects can resolve transitive deps via
  their own deps.edn / project.clj."
  [_]
  (clean nil)
  (write-version-edn)
  (write-pom)
  (b/copy-dir {:src-dirs   ["src" "gen-resources"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file  jar-file})
  (println "Created" jar-file))

(defn install
  "Build the library jar and install it into the local ~/.m2/repository
  so other projects on this machine can depend on it without a
  Clojars round-trip."
  [_]
  (jar nil)
  (b/install {:basis     @basis
              :lib       lib
              :version   version
              :jar-file  jar-file
              :class-dir class-dir})
  (println "Installed" lib version "to ~/.m2"))

(defn deploy
  "Build the library jar and push it to Clojars. Requires the
  CLOJARS_USERNAME and CLOJARS_PASSWORD (deploy token) environment
  variables to be set."
  [_]
  (jar nil)
  (dd/deploy {:installer :remote
              :artifact  jar-file
              :pom-file  (b/pom-path {:lib lib :class-dir class-dir})}))
