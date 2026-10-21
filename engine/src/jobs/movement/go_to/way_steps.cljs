(ns jobs.movement.go-to.way-steps
  "go-to's ways, the methods a call may use to reach its goal, and the escalation steps each allows."
  (:require [engine.ctx :as ctx]))

(def ways
  "The ways in order. :dig takes the body's own ledger blocks back from round a goal it cannot reach and, when the
  call may dig natural blocks (:may-dig), digs a door, a route or its way out of its shelter."
  [:walk :dig :pillar :stair :bridge])

(def way-steps
  "The escalation steps (jobs.lib.escape/choose and go-to's own) of each way that changes the world. A walk to the
  nearest wall (:approach) belongs to none: it only readies the others."
  {:pillar #{:pillar :fill-gap} :dig #{:clear-path :dig-route :leave-shelter} :stair #{:stair} :bridge #{:bridge}})

(def natural-steps
  "The steps that cut natural blocks: barred unless the call's :may-dig has :natural."
  #{:clear-path :dig-route :leave-shelter :stair})

(defn way?
  "Whether the call allows way k: its frozen call's :ways; before the call starts (a condition), the args' (all when
  none given)."
  [c k]
  (let [ways (if-let [call (ctx/frozen-call c)] (:ways call) (:ways (:args c)))]
    (or (nil? ways) (contains? (set ways) k))))

(defn may-dig-natural?
  "Whether the call's :may-dig (default #{:own :natural}) lets it cut natural blocks."
  [c]
  (contains? (set (or (:may-dig (:args c)) #{:own :natural})) :natural))

(defn barred-steps
  "The escalation steps of the ways the call does not allow, and the natural-cutting ones when :may-dig has no :natural."
  [c]
  (into (if (may-dig-natural? c) #{} natural-steps)
        (mapcat (fn [[w steps]] (when-not (way? c w) steps))) way-steps))

(defn escalating?
  "Whether the call allows any way that changes the world by escalating."
  [c]
  (boolean (seq (remove (barred-steps c) (mapcat val way-steps)))))
