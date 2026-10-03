(ns engine.triggers.night-unsafe
  "The night-unsafe trigger. It holds when it is night, the body is awake and
  nothing solid is within :roof-height blocks straight above it; and also at
  day while the latest :shelter entry is built and the body is within two
  blocks of it, so that the shelter job, dropped at night because it had
  nothing to do, comes back at dawn to open the shelter."
  (:require [engine.jobs.shelter :as sh]
            [engine.jobs.util :as u]))

(def trigger
  {:name :night-unsafe
   :when (fn [world memory args]
           (or (sh/unsafe-night? world (:roof-height args sh/default-roof-height))
               (and (sh/day? world)
                    (some? (sh/built-shelter-here memory (u/self-pos {:primitives world}))))))
   :job '(jobs.survival.shelter)
   :args {:roof-height sh/default-roof-height}
   :persistence :cooldown
   :cooldown-s 10})
