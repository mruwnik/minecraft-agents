(ns triggers.survival.dawn
  "The dawn trigger: by day, while the body is awake and holds one of these, it leaves its shelter (jobs.survival.leave-shelter):
    shut in its own latest :shelter (no :shelter-trapped entry for the cell)
    a doorway the night filled and still holding a block it put there (sh-beds/doorway-to-open)
    a bed it put down outside its own zone (ledger :bed) still standing (sh-beds/bed-to-collect)
  An agent that wants the shelter for itself mutes it."
  (:require [jobs.lib.body :as body]
            [jobs.lib.shelter :as sh]))

(defn dawn
  [world memory _args _kn]
  (and (not (body/recovery-pending? memory :dawn))
       (sh/leave-wanted? world memory)))
