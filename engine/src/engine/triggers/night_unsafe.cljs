(ns engine.triggers.night-unsafe
  "The night-unsafe trigger. It holds when it is night, the body is awake and either nothing solid is within
  :roof-height blocks straight above it, or it is shut in its own latest :shelter (the night's work is not done:
  a shelter cut by a higher reflex after it dug in is fired again and holds the body until day, eating as it holds,
  instead of leaving the night to the reflexes below it). A body that is roofed (its hut) at night holds too while a remembered :bed lies within :bed-radius and it has
  not slept tonight, so the shelter job can sleep in it. It never holds by day; shut-in-by-day takes over then."
  (:require [engine.jobs.shelter :as sh]
            [engine.memory :as mem]))

(defn holds?
  "Night, awake, and unroofed, shut in the body's own latest :shelter, or sheltered with a bed in reach it has not slept
  in tonight (the shelter job sleeps in it, which sets the respawn point), or with no bed in reach but a bed item
  carried (the shelter job puts it down and sleeps in it)."
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
