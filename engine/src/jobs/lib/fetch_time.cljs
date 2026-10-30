(ns jobs.lib.fetch-time
  "The seconds fetching one item may take: the base of the default :spend :job-time of a job that fetches (items.obtain, animals.leash)."
  (:require [engine.args :as a]
            [engine.settings :as settings]))

(a/defargs settings
  {::one-unit-s {:default 600 :doc "Seconds a fetch of one unit may run." :spec (a/num-in 0 nil)}})

(defn one-unit-s [] (settings/get settings ::one-unit-s))
