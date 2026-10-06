(ns jobs.lib.util
  "Helpers the library jobs share: reading positions off JS, distances, the
  inventory and bounded failure counting. Walking in reach is jobs.lib.near/walk-near!."
  (:require [engine.ctx :as ctx]))

(def max-failures 3)

(defn block-name
  "The block name at cell pos, or nil when the chunk is not loaded."
  [p pos]
  (some-> (.blockAt p (clj->js pos)) .-name))

(defn block-facts
  "What a cell holds as cljs facts {:name :full-cube? :waterlogged?} (full-cube?: its collision shape fills the cell), or
  nil when the chunk is not loaded."
  [p pos]
  (when-let [b (.blockAt p (clj->js pos))]
    (let [logged (some-> b .-properties .-waterlogged)]
      {:name (.-name b)
       :full-cube? (boolean (.-fullCube b))
       :waterlogged? (or (true? logged) (= "true" logged))})))

(defn pos-of
  "A JS {x y z} object as a cljs map."
  [o]
  {:x (.-x o) :y (.-y o) :z (.-z o)})

(defn self-pos [c]
  (pos-of (.-pos (.self (:primitives c)))))

(defn dist [a b]
  (js/Math.hypot (- (:x a) (:x b)) (- (:y a) (:y b)) (- (:z a) (:z b))))

(defn within?
  "True when the floored cells of a and b are within range, the measure moveTo's arrival uses."
  [a b range]
  (let [d (fn [k] (- (js/Math.floor (k a)) (js/Math.floor (k b))))]
    (<= (+ (* (d :x) (d :x)) (* (d :y) (d :y)) (* (d :z) (d :z))) (* range range))))

(defn inventory
  "The carried items as cljs maps {:name :count :slot}, with :durability (left) and :max for tools."
  [p]
  (mapv (fn [i] (cond-> {:name (.-name i) :count (.-count i) :slot (.-slot i)}
                  (some? (.-durability i)) (assoc :durability (.-durability i) :max (.-maxDurability i))))
        (array-seq (.-inventory (.self p)))))

(def inventory-slots
  "Main and hotbar slots: what the inventory list holds (armour and off-hand are not in it)."
  36)

(defn free-slots
  "Empty main and hotbar slots of primitives p, at least 0."
  [p]
  (max 0 (- inventory-slots (.-length (.-inventory (.self p))))))

(defn fail!
  "Count a failed round in job memory. Returns :continue until max-failures, then emits a warn of kind and returns :done."
  [c kind text]
  (let [tries (inc (:failures (ctx/mem c) 0))]
    (ctx/update-mem! c assoc :failures tries)
    (if (< tries max-failures)
      :continue
      (do (ctx/emit! c kind :warn {:tries tries :text text})
          :done))))
