(ns engine.triggers.night-unsafe
  "The night-unsafe trigger. It holds when it is night, the body is awake and either nothing solid is within
  :roof-height blocks straight above it, or it is shut in its own latest :shelter (the night's work is not done:
  a shelter cut by a higher reflex after it dug in is fired again and holds the body until day, eating as it holds,
  instead of leaving the night to the reflexes below it). It never holds by day; shut-in-by-day takes over then."
  (:require [engine.jobs.shelter :as sh]
            [engine.memory :as mem]))

(defn holds?
  "Night, awake, and unroofed or shut in the body's own latest :shelter."
  [p view roof-height]
  (boolean (or (sh/unsafe-night? p roof-height)
               (and (sh/night? p)
                    (not (sh/sleeping? p))
                    (sh/in-own-shelter p (:data (mem/latest view :shelter)))))))

(def trigger
  {:name :night-unsafe
   :when (fn [world memory args]
           (holds? world memory (:roof-height args sh/default-roof-height)))
   :job '(jobs.survival.shelter)
   :args {:roof-height sh/default-roof-height}
   :persistence :cooldown
   :cooldown-s 10})
