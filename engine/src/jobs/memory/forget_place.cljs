(ns jobs.memory.forget-place
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.places :as places]))

(def doc
  "Forget a named place in body memory, in one round. The place (or the retraction of a gone one) is removed.
  Emits info place.forgotten with :was; ends :done with data {:name :was}.
  A refusal gives warn place.refused and ends :condition-failed with cause {:reason :text}. :reason is
  :bad-name, :reserved-name, :no-such-place (nothing recorded under that name) or :not-a-place (the name holds
  other memory, which is left alone).
  Needs nothing; uses no time to speak of.")

(a/defargs args
  {:name {:doc "the place's name: a keyword or string" :spec (a/or-of keyword? string?) :default nil}})

(def target [:name])

(def arg-refusals #{:bad-name :reserved-name :not-a-place})

(defn bad-args
  "The refusal cause when the name is not one a place can have (a name with no place recorded is for the round)."
  [c]
  (let [r (places/resolve-forget (ctx/view c) (:name (:args c)))]
    (when (arg-refusals (:reason r))
      {:reason (:reason r) :text (:message r)})))

(def conditions [[:fn bad-args]])

(defn quote-of [_c] {:uses {:time [0 0 1]}})

(defn round [c]
  (let [r (places/resolve-forget (ctx/view c) (:name (:args c)))]
    (if (:reason r)
      (let [text (:message r)]
        (ctx/emit! c :place.refused :warn (-> r (dissoc :message) (assoc :text text)))
        (ctx/end c :condition-failed {:reason (:reason r) :text text}))
      (do (ctx/forget-where! c (:name r) (constantly true))
          (ctx/emit! c :place.forgotten :info {:name (:name r) :was (:was r)
                                               :text (str (name (:name r)) " forgotten")})
          (ctx/end c :done nil {:name (:name r) :was (:was r)})))))
