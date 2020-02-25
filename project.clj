(defproject jstack-report "0.1.0"
  :description "jstack-report - a tool for analyzing jstack thread dumps"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 ;[com.taoensso/tufte "2.1.0"]
                 [org.clojure/tools.cli "1.0.194"]
                 [cheshire "5.10.0"]  ;; required for graalvm generation of reflection.json
                 [say-cheez "0.2.0"]] ;;build information

  :repl-options {:init-ns jstack-report.core}
  ;:main ^:skip-aot thread-watch.core
  :main jstack-report.main
  :profiles {:uberjar {:global-vars {*assert* false}
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"
                                  "-Dclojure.spec.skip-macros=true"]
                       :main jstack-report.main
                       :aot :all}
             :reflection {:main jstack-report.classes/generate-reflection-file}}
  :java-source-paths ["java"])
