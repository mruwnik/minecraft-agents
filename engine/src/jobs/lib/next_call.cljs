(ns jobs.lib.next-call
  "The calls a goal job's ending names under :next for the agent to submit (the agent tools print them): what would
  unblock it, then the job itself again ({:job :args :again true}), last. Each call is {:job \"jobs.x\" :args {..}}."
  (:require [engine.contract :as contract]
            [jobs.lib.need :as need]))

(def max-fixes "Unblocking calls an ending names at most: the first ones are the nearest to the cause." 3)

(defn call [job args] {:job (str job) :args (or args {})})

(defn given
  "The args of a call that differ from the defaults of defs (a job's defargs map): what a resubmit repeats."
  [defs args]
  (into {} (filter (fn [[k v]] (and (contains? defs k) (not= v (get-in defs [k :default]))))) args))

(defn wait-reason
  "A cause map as the wait reason jobs.lib.need/plan-for reads: a failed tool fetch's {:reason :tool ..} (need/tool!) as :no-tool."
  [cause]
  (cond-> cause
    (= :tool (:reason cause)) (assoc :reason :no-tool)
    (and (= :tool (:reason cause)) (:item cause)) (assoc :needs (:item cause))))

(defn chain-fix
  "The fetch (jobs.lib.need/plan-for) that solves the first cause along ending e's :because chain that names a missing
  item or tool, as a call, or nil."
  [e]
  (some #(when-let [pl (and (map? (:cause %)) (need/plan-for (wait-reason (:cause %)) {:explore true}))]
           (call (:job pl) (:args pl)))
        (take-while map? (iterate :because e))))

(defn of
  "The calls ending e names: its data's :next, else its cause's (a start check's refusal)."
  [e]
  (or (get-in e [:data :next]) (when (map? (:cause e)) (:next (:cause e)))))

;; A seal is offered only where the ending says the body's own passage opened into water, lava or a cave.
(defn seal-fix
  "The call to jobs.blocks.seal-opening at `at` when ending e's cause is one of causes, else nil. With the optional
  pred, the cause map must also satisfy it (a cause shared by breaches and other stops)."
  ([e causes at] (seal-fix e causes at any?))
  ([e causes at pred]
   (when (and (contract/ending? e) (contains? causes (contract/cause-key e)) (pred (:cause e)))
    (call 'jobs.blocks.seal-opening {:at at}))))

(defn with-next
  "data with :next: the calls a child passed up (in data, else named by its ending e; its resubmit becomes a plain call),
  else the fetch e's chain names (e may be nil), then fixes, then job again with the args of c's call that differ from
  defs' defaults."
  [data c job defs e & fixes]
  (let [passed (map #(dissoc % :again) (or (:next data) (of e)))
        unblock (concat (if (seq passed) passed (some-> e chain-fix vector)) (keep identity fixes))]
    (assoc data :next (conj (vec (take max-fixes (distinct unblock)))
                            (assoc (call job (given defs (:args c))) :again true)))))

(defn on-ending
  "A round's result e, with :next in its data (with-next over e and its :because chain) when it is an ending other
  than done or cut-off."
  [c e job defs & fixes]
  (if (and (contract/ending? e) (not (#{:done :cut-off} (:kind e))))
    (assoc e :data (apply with-next (:data e) c job defs e fixes))
    e))
