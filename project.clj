(defproject thread-watch "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/tools.cli "0.3.5"]
                 [org.clojure/core.async "0.3.442"]
                 [clojure-lanterna "0.9.7"]]
  :repl-options {:init-ns thread-watch.babashka}
  :main ^:skip-aot thread-watch.core
  :profiles {:uberjar {:aot :all}})
