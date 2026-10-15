(ns engine.core.legacy
  "The adapter between old callers (call-child, child-result) and contract jobs: a contract job called through
  call-child answers as an old one, and an old :done result reads as an ending."
  (:require [engine.contract :as contract]))

(def reason-kinds
  "The kind of ending an old job's :stopped reason reads as; any other reason is :nothing-possible. Reasons that
  name a missing target or fact, or a danger present, are condition-failed; a goal that already holds is done."
  (merge (zipmap [:refused :need :no-tool :not-loaded :zone :no-zones :hazard :not-allowed
                  :inventory-full :bag-full :fluid :fluid-adjacent :no-chest :no-bed
                  :unarmed :declined :not-ready :danger :hostile-near :tool-spent
                  :gone :no-target :no-targets :no-source :no-tree :no-sapling :no-composter :no-boat :no-vehicle
                  :not-found :nothing-found :no-food-found :no-box :no-home
                  :no-base :no-block :no-item :no-items :no-mob-or-box :chest-lacks :no-food]
                 (repeat :condition-failed))
         (zipmap [:bad-args :bad-pos :unsupported :no-pen] (repeat :impossible))
         (zipmap [:timeout :low-health :hungry] (repeat :limit-reached))
         (zipmap [:not-mounted :no-hostile] (repeat :done))))

(def reason-lines
  "The limit line ({:line ..}, as the contract gives it) of an old limit reason; :reason keeps what an old parent reads."
  {:timeout :job-time :low-health :health :hungry :food})

(defn reason-cause [reason]
  (if-let [line (reason-lines reason)] {:line line :reason reason} reason))

(defn ending-of
  "The ending of an old job's :done round with the result it handed over (result!): :stopped results read by reason-kinds."
  [result]
  (let [data (dissoc result :status :reason)]
    (if (and (= :stopped (:status result)) (not= :done (reason-kinds (:reason result))))
      (contract/ending (get reason-kinds (:reason result) :nothing-possible) (reason-cause (:reason result)) data)
      (contract/ending :done nil data))))

(defn result-of
  "The result an old parent reads (child-result) of a contract child's ending e; :already? when its goal held before it
  started."
  [e]
  (if (= :done (:kind e))
    (cond-> (assoc (:data e) :status :done) (= :already (:cause e)) (assoc :already? true))
    (assoc (:data e) :status :stopped :reason (contract/cause-key e) :ending e)))
