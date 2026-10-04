(ns jobs.memory.set-place
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [engine.memory :as mem]
            [engine.places :as places]))

(def doc
  "Record a named place (:bed, :chest, :home, any name) in body memory, one round. :pos [x y z] or {:x :y :z} (whole or
  fractional, floored to its cell); without it the cell the body stands in. With :block (a block name such as
  \"white_bed\" or \"chest\"; \"bed\" is any colour of bed) the block must stand at the position or within 1 of it,
  and the place is recorded at the block's own cell. Nothing is recorded when two such blocks lie within 1 and
  neither at the position (:ambiguous: give the exact one), when none does (:no-such-block) or the position is not
  loaded (:not-loaded). A recorded place of that name is moved. Places are this body's own memory; the readers
  (sleep, deposit, the (place :name) condition fact) find them by name. Refused, as one place.refused warn event
  with :reason and :text and a result {:ok false :reason}: :bad-name, :reserved-name (a memory kind the engine
  uses), :not-a-place (the name holds other memory), :bad-pos, :bad-block, :no-such-block, :ambiguous, :not-loaded.
  Success: one place.set info event and a result {:ok true :name :pos}. Submit it on a running body with
  {:op :submit :front? true :spec (jobs.memory.set-place {...})}; the outcome is in the event stream.")

(def args
  {:name {:doc "the place's name: a keyword or string of 1 to 32 lowercase letters, digits and dashes" :default nil}
   :pos {:doc "[x y z] or {:x :y :z}; nil: where the body stands" :default nil}
   :block {:doc "block name that must stand at or within 1 of :pos (\"bed\" matches any *_bed); the place is recorded at it" :default nil}})

(defn check [_c] true)

(defn standing
  "The cell the body stands in."
  [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    {:x (js/Math.floor x) :y (js/Math.floor y) :z (js/Math.floor z)}))

(defn round [c]
  (let [p (:primitives c)
        r (places/resolve-set (ctx/view c) #(u/block-name p %) (standing c) (:args c))]
    (if (:reason r)
      (do (ctx/emit! c :place.refused :warn (-> r (dissoc :message) (assoc :text (:message r))))
          (ctx/result! c {:ok false :reason (:reason r)}))
      (let [{place-name :name :keys [pos block]} r
            was (mem/place (ctx/view c) place-name)]
        (ctx/remember! c place-name {:pos pos} mem/place-policy)
        (ctx/emit! c :place.set :info {:name place-name :pos pos :block block :was was
                                       :text (str (name place-name) " recorded at " (pr-str pos))})
        (ctx/result! c {:ok true :name place-name :pos pos})))
    :done))
