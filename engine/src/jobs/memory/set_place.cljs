(ns jobs.memory.set-place
  (:require [engine.ctx :as ctx]
            [jobs.lib.util :as u]
            [engine.memory :as mem]
            [jobs.lib.places :as places]))

(def doc
  "Record a named place (:bed, :chest, :home, any name) in body memory, in one round.
  :pos is [x y z] or {:x :y :z}, floored to its cell. Without it the cell the body stands in is used.
  With :block (a block name such as \"white_bed\" or \"chest\"; \"bed\" is any colour of bed) the block must stand
  at the position or within 1 of it. The place is recorded at the block's own cell.
  A place already recorded under that name is moved. Places are this body's own memory. Readers (sleep, deposit,
  the (place :name) condition fact) find them by name.
  Refused (warn place.refused with :reason and :text, result {:ok false :reason}): :bad-name, :reserved-name (a
  memory kind the engine uses), :not-a-place (the name holds other memory), :bad-pos, :bad-block,
  :no-such-block, :ambiguous (two such blocks lie within 1 and neither at the position: give the exact one),
  :not-loaded.
  Success: info place.set and result {:ok true :name :pos}.
  To run it on a body that is running: {:op :submit :front? true :spec (jobs.memory.set-place {...})}. The
  outcome is in the event stream.")

(def args
  {:name {:doc "the place's name: a keyword or string of 1 to 32 lowercase letters, digits and dashes" :default nil}
   :pos {:doc "[x y z] or {:x :y :z}; nil: where the body stands" :type :pos :default nil}
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
