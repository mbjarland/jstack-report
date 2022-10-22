(defproject jstack-report "1.0.0"
  :description "jstack-report - a tool for analyzing jstack thread dumps"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 ;[com.taoensso/tufte "2.1.0"]
                 [org.clojure/tools.cli "1.0.214"]
                 [cheshire "5.11.0"]                        ;; required for graalvm generation of reflection.json
                 [say-cheez "0.2.0"]]                       ;;build information

  :repl-options {:init-ns jstack-report.core}
  ;:main ^:skip-aot thread-watch.core
  :main jstack-report.main
  :profiles {:uberjar    {:global-vars {*assert* false}
                          :jvm-opts    ["-Dclojure.compiler.direct-linking=true"
                                        "-Dclojure.spec.skip-macros=true"]
                          :main        jstack-report.main
                          :aot         :all}
             :reflection {:main jstack-report.classes/generate-reflection-file}}
  :plugins [[me.arrdem/lein-git-version "2.0.8"]]
  ;:clean-targets ^{:protect false} [:target-path "gen-resources"]
  :git-version {:version-file      "gen-resources/build/version.edn"
                :version-file-keys [:ref :version :timestamp :dirty? :ref-short]}
  :java-source-paths ["java"])
