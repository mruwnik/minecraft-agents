(ns ^:dev/always engine.hooks
  "The job-side fns the engine calls, named in the resource jobs/hooks.edn and built at compile time (see
  engine.registry): the world store (:world/open, :world/blank).
  The engine never requires a job namespace; it calls these."
  (:require-macros [engine.registry :refer [hook-table]]))

(def all
  "{hook-key fn}"
  (hook-table))
