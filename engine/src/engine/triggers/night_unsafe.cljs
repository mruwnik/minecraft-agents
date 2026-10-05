(ns engine.triggers.night-unsafe
  "The night-unsafe trigger. It holds at night while the body is awake and one of these is so:
    nothing solid is within :roof-height blocks straight above it (and it is not buried)
    it is shut in its own latest :shelter (a shelter cut by a higher reflex after it dug in fires again and holds
      the body until day, eating as it holds)
    it is roofed (its hut), a remembered :bed lies within :bed-radius and it has not slept tonight,
      so the shelter job can sleep in it
    it is roofed with a bed item carried and no bed in reach (the shelter job puts it down and sleeps)
  It never holds by day; shut-in-by-day takes over then."
  (:require [engine.jobs.shelter :as sh]
            [engine.memory :as mem]))

(defn holds?
  "Whether the trigger holds: see the namespace doc for the four cases."
  ([p view roof-height] (holds? p view roof-height sh/default-bed-radius))
  ([p view roof-height bed-radius]
  (boolean (or (sh/unsafe-night? p roof-height)
               (some? (sh/sleep-wanted p view bed-radius))
               (sh/bed-place-wanted? p view roof-height bed-radius)
               (and (sh/night? p)
                    (not (sh/sleeping? p))
                    (sh/in-own-shelter p (:data (mem/latest view :shelter))))))))

(def trigger
  {:name :night-unsafe
   :when (fn [world memory args]
           (holds? world memory (:roof-height args sh/default-roof-height) (:bed-radius args sh/default-bed-radius)))
   :job '(jobs.survival.shelter)
   :args {:roof-height sh/default-roof-height :bed-radius sh/default-bed-radius}
   :persistence :cooldown
   :cooldown-s 10})
