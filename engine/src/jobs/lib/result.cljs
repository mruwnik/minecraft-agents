(ns jobs.lib.result
  "Reads of go-to's ending: whether a walk's give-up is a verdict on the cell."
  (:require [engine.contract :as contract]))

(def permanent-why "go-to's :why of a give-up that is a verdict on the cell." #{:abilities :goal-enclosed :goal-cut-off :one-way :goal-not-standable :goal-on-magma :exhausted})

(defn of
  "A child's ending e as the map a parent reads: its data with :status (:done or :stopped) and, for a stop, :reason (the
  cause's key); a done ending keeps the :reason its data gives."
  [e]
  (cond-> (assoc (:data e) :status (if (= :done (:kind e)) :done :stopped))
    (not= :done (:kind e)) (assoc :reason (contract/cause-key e))))

(defn why
  "The :why go-to's ending e names (its cause's, else its data's), nil without one."
  [e]
  (when (map? e)
    (or (when (map? (:cause e)) (:why (:cause e))) (:why (:data e)))))

(defn permanent-walk?
  "Whether go-to's ending e is a verdict on the cell (held :unreachable), not something that may pass: a refused call
  (:bad-pos, :unsupported) or an :unreachable whose :why is permanent (a door stuck, a stuck step, no progress, a move
  while searching are not)."
  [e]
  (and (map? e)
       (not= :done (:kind e))
       (let [reason (contract/cause-key e)]
         (or (contains? #{:bad-pos :unsupported} reason)
             (and (= :unreachable reason) (contains? permanent-why (why e)))))))

(defn unreachable-walk?
  "Whether a blocked walk to a target is no reason to count a failure: a permanent verdict, or a way go-to will not take
  (:off-plan on a long drop, :needs-health: too costly to walk). The run digs toward other ore or tunnels instead."
  [e]
  (boolean (or (permanent-walk? e)
               (and (map? e) (not= :done (:kind e)) (contains? #{:off-plan :needs-health} (why e))))))
