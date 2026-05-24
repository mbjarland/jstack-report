(defproject jstack-report "1.0.0"
  :description "jstack-report - a tool for analyzing jstack thread dumps"
  :url "https://github.com/mbjarland/jstack-report"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/tools.cli "1.0.219"]
                 [cheshire "5.12.0"]]                       ;; required for graalvm generation of reflection.json

  :repl-options {:init-ns jstack-report.core}
  :main jstack-report.main
  :profiles {:uberjar    {:global-vars {*assert* false}
                          :jvm-opts    ["-Dclojure.compiler.direct-linking=true"
                                        "-Dclojure.spec.skip-macros=true"]
                          :main        jstack-report.main
                          :aot         :all}
             :reflection {:main jstack-report.classes/generate-reflection-file}
             :test       {:resource-paths ["test/resources"]}}
  :plugins [[me.arrdem/lein-git-version "2.0.8"]]
  :git-version {:version-file      "gen-resources/build/version.edn"
                :version-file-keys [:ref :version :timestamp :dirty? :ref-short]}
  :resource-paths ["gen-resources"])
