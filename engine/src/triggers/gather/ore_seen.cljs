(ns triggers.gather.ore-seen
  "The ore-seen trigger: a mining job is live and a seen ore lies exposed within its reach.
  A mining job marks body memory (jobs.lib.ore-pick/mark!, :ore-mark) with what it digs itself; the trigger holds while a mark
  of a listed job exists, the body is on the ground and jobs.lib.ore-pick/candidates finds a block outside the marks' leave and except
  and outside the cells a run left (:ore-left). It sees only ore the body has seen; the zone verdict is the job's, which
  remembers what it may not dig. Runs jobs.gather.pick-ores."
  (:require [jobs.lib.body :as body]
            [jobs.lib.ore-pick :as ore-pick]))

(def defaults {})

(defn ore-seen
  [p view _args _kn live]
  (boolean
   (when-let [ms (seq (ore-pick/marks view live))]
     (and (not (body/recovery-pending? view :ore-seen))
          (not (false? (.-onGround (.self p))))
          (seq (ore-pick/candidates {:primitives p}
                                    (assoc (ore-pick/merged ms) :skipped (ore-pick/left-cells view))))))))
