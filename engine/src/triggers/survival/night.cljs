(ns triggers.survival.night
  "The night trigger: the one night handler. At night, while the body is awake, it holds when any of these is so:
    a bed to sleep in (sh-beds/bed-to-use: seen, or the nearest remembered :bed or live :spawn-bed within the default-bed-radius setting under any roof; not occupied, nor (unless another player sleeps near) one whose
      walk would leave an underground, dug-in or closed-room shelter)
    a bed item carried and no bed to use (sh-beds/bed-place-wanted?)
    a look round first (sh/unlooked-for-bed?): a bed known or carried and cells beside the feet never seen, or a closed hut
      not looked round from tonight while it could still sleep
    someone else on the server asleep (sh-sleepers/log-out-for-sleepers?)
    not safe from the night (sh-inside/safe-from-night?: shut in a closed, roofed room with no dark spawnable cell beyond the 24-block spawn-free radius; a roof with open sides or a cave is not)
    an open door in the closed room the body stands in (sh/open-room-doors), also one the night could not shut
  By day it holds while the body is shut in its own latest :shelter (no :shelter-trapped entry for the cell), or a bed
  it put down outside its own zone, or a doorway block it filled (scaffold ledger :bed, :doorway), still stands. Safe from the night (its dug-in shelter too) with no bed and no sleeper it does not
  hold: the queue runs. An agent that wants the night for itself mutes it."
  (:require [jobs.lib.body :as body]
            [jobs.lib.shelter :as sh]
            [jobs.lib.shelter.beds :as sh-beds]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.shelter.sleepers :as sh-sleepers]))

(def defaults {:roof-height sh-sense/default-roof-height})

(defn night
  [world memory args kn]
  (and (not (body/recovery-pending? memory :night))
       (sh/night-holds? world memory args (sh-beds/bed-permit world kn))))
