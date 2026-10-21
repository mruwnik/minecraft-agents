(ns jobs.lib.waits
  "The world's waits a parent may sit out: which :later reasons mean waiting, and passing a called child's ending through them."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]))

(def wait-reasons
  "The :reason of every :later these checkers give: a job that declines for one is waiting out the world, not failing."
  #{:exposed :dark :hostile-near :low-health :poisoned :unarmed :no-free-slot})

(def full-pack-limit-ms
  "How long a caller waits for a full pack to clear before it ends; a wait that went unasked for full-pack-gap-ms starts over."
  300000)

(def full-pack-gap-ms 120000)

(defn full-pack-ending
  "Books this wait on c's memory; the :inventory-full ending once the pack has stayed full for full-pack-limit-ms, else nil."
  [c]
  (let [now (ctx/now c)
        {:keys [since last]} (:pack-full (ctx/mem c))
        since (if (and last (< (- now last) full-pack-gap-ms)) since now)]
    (ctx/update-mem! c assoc :pack-full {:since since :last now})
    (when (>= (- now since) full-pack-limit-ms)
      (contract/ending :condition-failed
                       {:reason :inventory-full
                        :text (str "the pack stayed full for " (quot (- now since) 1000) " s: nothing could be put away or tossed to make room")}
                       nil))))

(defn waiting-out
  "r, a called child's answer, but :continue (the caller waits with the reason) when the child ended condition-failed for
  a reason in wait-reasons: the world's wait,
  not a failed child. A caller opts in by passing the answer through here."
  [c r]
  (let [cause (when (= :condition-failed (:kind r)) (:cause r))
        ended (when (= :no-free-slot (:reason cause)) (full-pack-ending c))]
    (cond
      (not (contains? wait-reasons (:reason cause))) r
      ended ended
      :else (do (some-> (:child-wait c) (reset! cause)) :continue))))

(defn reflex-wait-end
  "For a reflex whose child waits on the world (child-wait set): the condition-failed ending with that reason, else nil.
  A reflex may not :continue, and stepping on while the child waits would spin."
  [c]
  (when-let [cause (and (:reflex c) (some-> (:child-wait c) deref))]
    (contract/ending :condition-failed cause nil)))
