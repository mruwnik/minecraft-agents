(ns ^:dev/always engine.registry
  "The job registry: every job namespace under engine/src/jobs, found at
  compile time. See README.md, Jobs."
  (:require-macros [engine.registry :refer [job-registry settings-registry]]))

(def jobs
  "{ns-symbol {:check fn :round fn :doc string-or-nil :args map-or-nil}}"
  (job-registry))

(def settings-by-ns
  "{ns-symbol settings-map}: the `settings` of every namespace under jobs/ and triggers/ that declares one."
  (settings-registry))

(def settings
  "{key spec} of every declared setting (see engine.settings)."
  (into {} (mapcat val) settings-by-ns))
