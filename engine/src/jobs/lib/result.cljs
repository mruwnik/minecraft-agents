(ns jobs.lib.result
  "Job result constructors: a give-up cannot be mistaken for success.
  stop! ends the round :stopped with a reason; finish! hands success data.")

(defn cause-of
  "The nested cause for a parent's stop!, from the stopped result of the child in slot:
  {:child slot :reason .. :text .. :cause ..} (keys the child left out are left out)."
  [slot child-result]
  (merge {:child slot :reason (or (:reason child-result) :unknown)}
         (select-keys child-result [:text :cause])))

(defn stop!
  "Give up: set {:status :stopped :reason :text} (plus :cause, :cell and any other
  option keys, nil values dropped) as the round's result and return :done."
  [c reason text & {:as extra}]
  ((:result c) (into {:status :stopped :reason reason :text text}
                     (remove (comp nil? val)) extra))
  :done)

(defn finish!
  "Succeed: set {:status :done ..data} as the round's result and return :done."
  ([c] (finish! c {}))
  ([c data]
   ((:result c) (assoc data :status :done))
   :done))

(def permanent-why "go-to's :why of a give-up that is a verdict on the cell." #{:abilities :goal-enclosed :goal-cut-off :one-way :goal-not-standable :goal-on-magma :exhausted})

(defn permanent-walk?
  "Whether a go-to result is a verdict on the cell (held :unreachable), not something that may pass: a refused call
  (:bad-pos, :unsupported) or an :unreachable whose :why is permanent (a door stuck, a stuck step, no progress, a move
  while searching are not)."
  [{:keys [status reason why]}]
  (and (= :stopped status)
       (or (contains? #{:bad-pos :unsupported} reason)
           (and (= :unreachable reason) (contains? permanent-why why)))))

(defn unreachable-walk?
  "Whether a blocked walk to a target is no reason to count a failure: a permanent verdict, or a way go-to will not take
  (:off-plan on a long drop, :needs-health: too costly to walk). The run digs toward other ore or tunnels instead."
  [{:keys [why] :as result}]
  (boolean (or (permanent-walk? result)
               (and (= :stopped (:status result)) (contains? #{:off-plan :needs-health} why)))))
