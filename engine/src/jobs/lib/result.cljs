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
