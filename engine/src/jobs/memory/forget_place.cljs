(ns jobs.memory.forget-place
  (:require [engine.args :as a]
            [jobs.lib.checks :as checks]
            [jobs.lib.ready :as ready]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.places :as places]))

(def doc
  "Forget a named place in body memory, in one round. The place (or the retraction of a gone one) is removed.
  Ends :done with data {:name :was}; the memory layer emits info place.forgotten with :was.
  A refusal ends :condition-failed with cause {:reason :text}. :reason is
  :bad-name, :reserved-name or :not-a-place (the name holds other memory, which is left alone).
  A name no place can have is refused before any round: ends :impossible {:reason :bad-args :why :bad-name|:reserved-name :text}.
  Needs nothing; uses no time to speak of. A name with no place recorded is already done.")

(a/defargs args
  {:name {:doc "the place's name: a keyword or string" :spec (a/or-of keyword? string?) :default nil}})

(def target [:name])

(a/defargs settings
  {::job-time-s {:default 5 :doc "Seconds one call may take." :spec (a/int-in 1 nil)}})

(def defaults {:spend {:job-time ::job-time-s}})

(def arg-refusals #{:bad-name :reserved-name :not-a-place})

(defn bad-args
  "The refusal cause when the name is not one a place can have (a name with no place recorded is for the round)."
  [c]
  (let [r (places/resolve-forget (ctx/view c) (:name (:args c)))]
    (when (arg-refusals (:reason r))
      {:reason (:reason r) :text (:message r)})))

(defn can-start?
  "Never for a name no place can have; later for a name that is no place yet."
  [c]
  (let [cause (bad-args c)]
    (if (= :not-a-place (:reason cause))
      (checks/cause-status :later cause)
      (checks/cause-status :never (some-> cause (assoc :reason :bad-args :why (:reason cause)))))))

(defn done?
  "Done when nothing is recorded under the name."
  [c]
  (let [gone? (= :no-such-place (:reason (places/resolve-forget (ctx/view c) (:name (:args c)))))]
    (if gone?
      (contract/ok :forgotten (str "no place " (:name (:args c)) " recorded") :units :places :done 1 :left 0 :result {:name (:name (:args c))})
      (contract/later :recorded (str "place " (:name (:args c)) " recorded") :units :places :done 0 :left 1 :result {:name (:name (:args c))}))))

(defn quote-of [_c] {:uses {:job-time [0 0 1]}})

(def safe? ready/ok)
(def fit? ready/ok)

(defn round [c]
  (let [r (places/resolve-forget (ctx/view c) (:name (:args c)))]
    (if (:reason r)
      (ctx/end c :condition-failed {:reason (:reason r) :text (:message r)})
      (do (ctx/forget-where! c (:name r) (constantly true))
          (ctx/end c :done nil {:name (:name r) :was (:was r)})))))
