(ns jobs.movement.go-to.way-steps
  "go-to's ways, the methods a call may use to reach its goal, and the escalation steps each allows."
  (:require [engine.ctx :as ctx]))

(def ways
  "The ways in order; :clear-seal takes the body's own ledger blocks back from round a goal it cannot reach."
  [:walk :clear-seal :clear-path :pillar :stair :dig-out])

(def way-steps
  "The escalation steps (jobs.lib.escape/choose and go-to's own) of each way that changes the world. A walk to the
  nearest wall (:approach) belongs to none: it only readies the others."
  {:pillar #{:pillar :fill-gap} :clear-path #{:clear-path :dig-route} :stair #{:stair} :dig-out #{:leave-shelter}})

(defn way?
  "Whether the call allows way k: its frozen call's :ways; before the call starts (a condition), the args' (all when
  none given)."
  [c k]
  (let [ways (if-let [call (ctx/frozen-call c)] (:ways call) (:ways (:args c)))]
    (or (nil? ways) (contains? (set ways) k))))

(defn escalating?
  "Whether the call allows any way that changes the world."
  [c]
  (boolean (some #(way? c %) (keys way-steps))))

(defn barred-steps
  "The escalation steps of the ways the call does not allow."
  [c]
  (into #{} (mapcat (fn [[w steps]] (when-not (way? c w) steps))) way-steps))
