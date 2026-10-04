(ns jobs.memory.forget-place
  (:require [engine.ctx :as ctx]
            [engine.places :as places]))

(def doc
  "Forget a named place in body memory, one round: the place (or the retraction of a gone one) is removed, so the
  readers see none. One place.forgotten info event with :was and a result {:ok true :name :was}. Refused, as one
  place.refused warn event with :reason and :text and a result {:ok false :reason}: :bad-name, :reserved-name,
  :no-such-place (nothing recorded under that name), :not-a-place (the name holds other memory, which is left alone).")

(def args
  {:name {:doc "the place's name: a keyword or string" :default nil}})

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
