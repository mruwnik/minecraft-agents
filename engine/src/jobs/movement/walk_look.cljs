(ns jobs.movement.walk-look
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.cost.health :as cost-health]
            [jobs.lib.look :as look]
            [jobs.lib.look-edge :as look-edge]
            [jobs.lib.pace :as pace]
            [jobs.lib.ready :as ready]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]))

(def doc
  "Walk to look, as a player judging a dark tunnel does, one leg: the unseen `:side` cell is looked at, walked to at `:to`
  (go-to, on foot) and looked at again (look-edge/look!), then the body walks back to where the call began. A side still
  unseen then is written to body memory (:walk-looked {:cells}, walked-policy): from close by it stayed unseen, so no later
  walk-look is offered for it (jobs.lib.ready/walk-look). Readiness runs it as a check's :look-first child, the leg priced
  (walk-risk/look-way there and back); the check after it offers the next leg the walk opened. Needs nothing; ends :done
  (also when the side shows before the walk), condition-failed :no-way-back {:from} when the walk back fails. Info
  walk-look.ended {:side :unseen} at the end.")

(a/defargs args
  {:side {:doc "the unseen side cell to look at" :spec ::a/pos :default nil}
   :to {:doc "the cell to walk to and look from" :spec ::a/pos :default nil}})

(def target [:side])

(def defaults {:spend {:job-time ::job-time-s}})

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds a look-and-back walk may take." :spec (a/num-in 0 nil)}})

(defn can-start? [c]
  (if (and (:side (:args c)) (:to (:args c)))
    (contract/ok :args "a side and a cell to look from")
    (contract/never :bad-args "no :side or no :to")))

(def safe? ready/threatened)
(def fit? ready/ok)

(def walked-policy
  "A side looked at from close by and still unseen stays so: a remembered solid cell does not change (jobs.lib.memory-age).
  The cap bounds the memory only."
  {:cap 20 :ttl :forever})

(defn cell-of [{:keys [x y z]}] [x y z])

(defn ended
  "The ending of kind with cause, once the side, when still unseen, is written (:walk-looked)."
  [c kind cause]
  (let [side (:side (:args c))
        unseen? (look/unseen? (:primitives c) [side])]
    (when unseen? (ctx/remember! c :walk-looked {:cells [(cell-of side)]} walked-policy))
    (ctx/emit! c :walk-look.ended :info {:side side :unseen unseen?
                                         :text (if unseen? "walked to look: the side stayed unseen" "walked to look: the side shows")}))
  (ctx/end c kind cause))

(defn ^:async step [c]
  (let [{:keys [side to]} (:args c)
        from (:from (ctx/mem c))
        cell (cell-of side)]
    (if (and (look/unseen? (:primitives c) [side]) (not (contains? (:edge-looked (ctx/mem c)) cell)))
      (await (look-edge/look! c [cell] to))
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
