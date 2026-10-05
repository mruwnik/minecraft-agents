(ns ^:dev/always engine.registry
  "The job registry: every job namespace under engine/src/jobs, found at
  compile time. See README.md, Jobs."
  (:require-macros [engine.registry :refer [job-registry]]))

(def jobs
  "{ns-symbol {:check fn :round fn :doc string-or-nil :args map-or-nil
  :backoff map-false-or-nil :hold true-or-nil}}"
  (job-registry))
