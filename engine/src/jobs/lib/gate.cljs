(ns jobs.lib.gate
  "The zone rule as a block-changing job asks it: keep the cells it may act on, warn once when some were refused.
  Only social refusals (:zone :claim :footprint :no-zones) count here (see jobs.lib.access/may?). A physical
  refusal (not loaded, own body) is left to the act itself. A job given :ignore-zones? acts regardless."
  (:require [jobs.lib.access :as access]))

(def social-reasons #{:zone :claim :footprint :no-zones})

(defn refused? [v]
  (and (not (:ok v)) (contains? social-reasons (:reason v))))

(defn judge
  "As allowed, as {:allowed [pos ..] :refusal nil|:refused|:no-zones}: what the decline warn named, for a job that
  ends on it."
  ([c kind job-name action poss] (judge c kind job-name action poss {}))
  ([c kind job-name action poss opts]
   (let [in (access/rules-input c opts)
         judged (mapv (fn [pos] [pos (access/may? in action pos)]) poss)
         refusals (filterv refused? (map second judged))
         refusal (when (seq refusals)
                   (if (some #(= :no-zones (:reason %)) refusals) :no-zones :refused))]
     (when refusal
       (access/decline! c kind job-name (assoc (access/refusal-fields refusals) :reason refusal)))
     {:allowed (into [] (comp (remove (comp refused? second)) (map first)) judged)
      :refusal refusal})))

(defn allowed
  "The positions ({:x :y :z}) of poss the job may act on with action (:dig :place :sow :harvest :take :put), in
  order. When some are refused, one decline warn of kind (for job-name) names the zones, claims and plans that
  refuse, or :no-zones. opts: :except is the plan the job works (its own footprint does not refuse)."
  ([c kind job-name action poss] (allowed c kind job-name action poss {}))
  ([c kind job-name action poss opts] (:allowed (judge c kind job-name action poss opts))))

(defn allowed?
  "Whether the job may act on pos; see allowed."
  ([c kind job-name action pos] (allowed? c kind job-name action pos {}))
  ([c kind job-name action pos opts] (boolean (seq (allowed c kind job-name action [pos] opts)))))
