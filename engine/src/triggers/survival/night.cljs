(ns triggers.survival.night
  "The night trigger: the one night handler. At night, while the body is awake, it holds when any of these is so:
    a bed to sleep in (sh/bed-to-use: seen, or remembered within :bed-radius under any roof; not occupied)
    a bed item carried and no bed to use (sh/bed-place-wanted?)
    someone else on the server asleep (sh/log-out-for-sleepers?)
    nothing solid within :roof-height blocks straight above and not buried (sh/unsafe-night?)
    shut in its own latest :shelter
  By day it holds while the body is shut in its own latest :shelter (no :shelter-trapped entry for the cell), or a bed
  it put down outside its own zone (:bed-placed) still stands. Roofed or buried with no bed and no sleeper it does not
  hold: the queue runs. An agent that wants the night for itself mutes it."
  (:require [jobs.lib.shelter :as sh]))

(def defaults {:roof-height sh/default-roof-height :bed-radius sh/default-bed-radius})

(defn night
  [world memory args kn]
  (sh/night-holds? world memory args (sh/bed-permit world kn)))
