(ns jobs.lib.waits
  "The world's waits a parent may sit out: which :later reasons mean waiting, and passing a called child's ending through them."
  (:require [engine.contract :as contract]))

(def wait-reasons
  "The :reason of every :later these checkers give: a job that declines for one is waiting out the world, not failing."
  #{:exposed :dark :hostile-near :low-health :poisoned :unarmed})

(defn waiting-out
  "r, a called child's answer, but :continue (the caller waits with the reason) when the child ended condition-failed for
  a reason in wait-reasons: the world's wait,
  not a failed child. A caller opts in by passing the answer through here."
  [c r]
  (let [cause (when (= :condition-failed (:kind r)) (:cause r))]
    (if (contains? wait-reasons (:reason cause))
      (do (some-> (:child-wait c) (reset! cause)) :continue)
      r)))

(defn reflex-wait-end
  "For a reflex whose child waits on the world (child-wait set): the condition-failed ending with that reason, else nil.
  A reflex may not :continue, and stepping on while the child waits would spin."
  [c]
  (when-let [cause (and (:reflex c) (some-> (:child-wait c) deref))]
    (contract/ending :condition-failed cause nil)))
