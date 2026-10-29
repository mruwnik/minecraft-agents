(ns jobs.movement.walk-look
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.cost.health :as cost-health]
            [jobs.lib.look-edge :as look-edge]
            [jobs.lib.pace :as pace]
            [jobs.lib.ready :as ready]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]))

(def doc
  "Walk to look, as a player judging a dark tunnel does: the unseen side of the region round the feet a walk shows
  (look-edge/far-side, within look-edge/max-edge-walk of where the call began) is looked at, walked to (go-to, on foot)
  and looked at again (look-edge/look!), then the next such side the walk opened, until none is left; then the body
  walks back to where it began, and writes the sides it looked at to body memory (:walk-looked {:cells}, walked-policy),
  which no later walk-look is offered for while they last (jobs.lib.ready/walk-look). Readiness runs it as a check's :look-first child (jobs.lib.ready/walk-look, priced) when
  that walk costs less than the stay it judges. Needs nothing; ends :done (also with no side to show), or
  condition-failed :no-way-back {:from} when the walk back fails. Info walk-look.side {:side :to :from} for each side it
  walks to, walk-look.ended {:looked} at the end.")

(a/defargs args {})

(def target [])

(def safe? ready/threatened)
(def fit? ready/ok)

(def walked-policy "A side looked at from close by and still unseen stays so (a hidden corner), until the world may have changed." {:cap 20 :ttl 600000})

(defn ended
  "The ending of kind with cause, once the sides looked at are written (:walk-looked)."
  [c kind cause]
  (let [cells (vec (:edge-looked (ctx/mem c)))]
    (when (seq cells) (ctx/remember! c :walk-looked {:cells cells} walked-policy))
    (ctx/emit! c :walk-look.ended :info {:looked cells :text (str "walked to look at " (count cells) " unseen sides")}))
  (ctx/end c kind cause))

(defn ^:async step [c]
  (let [{:keys [from side]} (ctx/mem c)
        looked (:edge-looked (ctx/mem c) #{})
        side (if (and side (not (looked (:side side)))) side (look-edge/far-side (:primitives c) from looked))]
    (when (and side (not= side (:side (ctx/mem c))))
      (ctx/emit! c :walk-look.side :info {:side (:side side) :to (:to side) :from from}))
    (ctx/update-mem! c assoc :side side)
    (if side
      (await (look-edge/look! c [(:side side)]))
      (let [w (if (u/within? (sh-sense/feet (:primitives c)) from 0)
                {:kind :done}
                (await (ctx/call c :back 'jobs.movement.go-to {:pos from :range 0 :ways #{:walk} :floor cost-health/spend-down})))]
        (cond
          (= :continue w) :again
          (= :done (:kind w)) (ended c :done nil)
          :else (ended c :condition-failed {:reason :no-way-back :from from}))))))

(defn ^:async round [c]
  (when-not (:from (ctx/mem c))
    (ctx/update-mem! c assoc :from (update-vals (sh-sense/feet (:primitives c)) js/Math.floor)))
  (await (pace/steps! c #(step c))))
