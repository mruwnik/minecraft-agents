(ns triggers.survival.night
  "The night trigger: the one night handler. At night, while the body is awake, it holds when any of these is so:
    a bed to sleep in (sh-beds/bed-to-use: seen, or the nearest remembered :bed or live :spawn-bed within :bed-radius under any roof; not occupied, nor one whose
      walk would leave an underground, dug-in or closed-room shelter)
    a bed item carried and no bed to use (sh-beds/bed-place-wanted?)
    a look round first (sh/unlooked-for-bed?): a bed known or carried and cells beside the feet never seen, or a closed hut
      not looked round from tonight while it could still sleep
    someone else on the server asleep (sh-sleepers/log-out-for-sleepers?)
    nothing solid within :roof-height blocks straight above, not buried and not in a closed room, or buried in a dark open cave (sh-sense/open-cave?: 16+ standable cells under a ceiling within 3 blocks, under 8 block light; its own 1-wide tunnel is not one) (sh-sense/unsafe-night?)
    an open door in the closed room the body stands in (sh/open-room-doors), also one the night could not shut
  By day it holds while the body is shut in its own latest :shelter (no :shelter-trapped entry for the cell), or a bed
  it put down outside its own zone (:bed-placed) still stands. Roofed, buried or shut in (its dug-in shelter too) with no bed and no sleeper it does not
  hold: the queue runs. An agent that wants the night for itself mutes it."
  (:require [jobs.lib.body :as body]
            [jobs.lib.shelter :as sh]
            [jobs.lib.shelter.beds :as sh-beds]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.shelter.sleepers :as sh-sleepers]))

(def defaults {:roof-height sh-sense/default-roof-height :bed-radius sh-beds/default-bed-radius})

(defn night
  [world memory args kn]
  (and (not (body/recovery-pending? memory :night))
       (sh/night-holds? world memory args (sh-beds/bed-permit world kn))))
