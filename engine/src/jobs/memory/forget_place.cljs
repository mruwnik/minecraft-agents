(ns jobs.memory.forget-place
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.places :as places]))

(def doc
  "Forget a named place in body memory, in one round. The place (or the retraction of a gone one) is removed.
  Emits info place.forgotten with :was. Result {:ok true :name :was}.
  A refusal gives warn place.refused with :reason and :text, and the result {:ok false :reason}. :reason is
  :bad-name, :reserved-name, :no-such-place (nothing recorded under that name) or :not-a-place (the name holds
  other memory, which is left alone).")

(a/defargs args
  {:name {:doc "the place's name: a keyword or string" :spec (a/or-of keyword? string?) :default nil}})

(defn check [_c] true)

(defn round [c]
  (let [r (places/resolve-forget (ctx/view c) (:name (:args c)))]
    (if (:reason r)
      (do (ctx/emit! c :place.refused :warn (-> r (dissoc :message) (assoc :text (:message r))))
          (ctx/result! c {:ok false :reason (:reason r)}))
      (do (ctx/forget-where! c (:name r) (constantly true))
          (ctx/emit! c :place.forgotten :info {:name (:name r) :was (:was r)
                                               :text (str (name (:name r)) " forgotten")})
          (ctx/result! c {:ok true :name (:name r) :was (:was r)})))
    :done))
