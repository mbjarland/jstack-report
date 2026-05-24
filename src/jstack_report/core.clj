(ns ^{:doc "Backwards-compatible facade. Historically every public
function lived in this namespace; it has since been split into
jstack-report.{parser,model,analyze,render,report}. This namespace
re-exports the two entry points most users (REPL, babashka scripts)
actually call. Reach for the focused namespaces directly when
working inside the project."
      :author "Matias Bjarland"}
  jstack-report.core
  (:require [jstack-report.model :as model]
            [jstack-report.report :as report]))

(def dump   model/dump)
(def report report/report)
