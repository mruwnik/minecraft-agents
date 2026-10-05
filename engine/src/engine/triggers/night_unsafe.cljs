(ns engine.triggers.night-unsafe
  "The night-unsafe trigger. It holds at night while the body is awake and one of these is so:
    nothing solid is within :roof-height blocks straight above it (and it is not buried)
    it is shut in its own latest :shelter (a shelter cut by a higher reflex after it dug in fires again and holds
      the body until day, eating as it holds)
    it is roofed in a room (a hut, not a one-wide tunnel) with a bed of the room (remembered within 12 blocks, or seen
      in the room; not an occupied one), no sleep tonight and no :sleep-failed entry, so the shelter job can sleep in it
    it is roofed in a room with a bed item carried and no bed of the room (the shelter job puts it down and sleeps)
  It never holds by day; shut-in-by-day takes over then."
  (:require [engine.jobs.shelter :as sh]
            [engine.memory :as mem]))

(defn holds?
  "Whether the trigger holds: see the namespace doc for the four cases."
  ([p view roof-height] (holds? p view roof-height (constantly true)))
  ([p view roof-height permit?]
  (boolean (or (sh/unsafe-night? p roof-height)
               (some? (sh/sleep-wanted p view roof-height permit?))
               (sh/bed-place-wanted? p view roof-height permit?)
               (and (sh/night? p)
                    (not (sh/sleeping? p))
                    (sh/in-own-shelter p (:data (mem/latest view :shelter))))))))

(def trigger
  {:name :night-unsafe
   :when (fn [world memory args kn]
           (holds? world memory (:roof-height args sh/default-roof-height) (sh/bed-permit world kn (:now memory))))
   :job '(jobs.survival.shelter)
   :args {:roof-height sh/default-roof-height :bed-radius sh/default-bed-radius}
   :persistence :cooldown
   :cooldown-s 10})
