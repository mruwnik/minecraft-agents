(ns engine.triggers.night-unsafe
  "The night-unsafe trigger. It holds when it is night, the body is awake and
  nothing solid is within :roof-height blocks straight above it. It never
  holds by day."
  (:require [engine.jobs.shelter :as sh]))

(def trigger
  {:name :night-unsafe
   :when (fn [world _memory args]
           (sh/unsafe-night? world (:roof-height args sh/default-roof-height)))
   :job '(jobs.survival.shelter)
   :args {:roof-height sh/default-roof-height}
   :persistence :cooldown
   :cooldown-s 10})
