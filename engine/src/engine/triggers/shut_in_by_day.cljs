(ns engine.triggers.shut-in-by-day
  "The shut-in-by-day trigger. It holds by day while the body stands in its own latest :shelter entry's cell and is
  still shut in it (a pit not climbed out of, a roofed shaft, a walled cell), so a body restarted while sealed, or sealed by
  a dig-in an agent submitted directly, is let out: it fires the shelter job, whose day round calls dig-in's leave!. It
  does not hold once a :shelter-trapped entry for this cell says leave! found no way out, so a trapped body is tried once
  and not again every cooldown. It never holds at night."
  (:require [engine.jobs.shelter :as sh]
            [engine.memory :as mem]))

(def trigger
  {:name :shut-in-by-day
   :when (fn [world memory _args]
           (sh/shut-in-by-day? world (:data (mem/latest memory :shelter)) (:data (mem/latest memory :shelter-trapped))))
   :job '(jobs.survival.shelter)
   :args {}
   :persistence :cooldown
   :cooldown-s 10})
