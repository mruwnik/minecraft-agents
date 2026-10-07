(ns jobs.lib.util
  "Helpers the library jobs share: reading positions off JS, distances, the
  inventory and bounded failure counting. Walking in reach is jobs.lib.near/go-near!."
  (:require [engine.ctx :as ctx]))

(def max-failures 3)

(defn ^:async untimed!
  "Await (thunk), then move mem :started (the job's :timeout-s start) on by the time it took: :timeout-s bounds the
  work the job does, not a fetch inside it. Returns the thunk's result."
  [c thunk]
  (let [t0 (ctx/now c)
        r (await (thunk))]
    (ctx/update-mem! c update :started #(when % (+ % (- (ctx/now c) t0))))
    r))

(defn block-at
  "The block at cell pos (a cljs or JS {x y z}) as the JS object, or nil when the chunk is not loaded."
  [p pos]
  (.blockAt p (clj->js pos)))

(defn block-name
  "The block name at cell pos, or nil when the chunk is not loaded."
  [p pos]
  (some-> (block-at p pos) .-name))

(defn feel
  "The block at a cell the body touches (its feet, head and the cell under the feet), as the JS object, in any light;
  nil for a cell it does not touch. Primitives that are not wrapped by perception (a bare fake) have no feel and read blockAt."
  [p pos]
  (if (some? (.-feel p))
    (.feel p (clj->js pos))
    (block-at p pos)))

(defn feel-name
  "The block name at a cell the body touches, or nil (see feel)."
  [p pos]
  (some-> (feel p pos) .-name))

(defn block-facts
  "What a cell holds as cljs facts {:name :full-cube? :waterlogged?} (full-cube?: its collision shape fills the cell), or
  nil when the chunk is not loaded."
  [p pos]
  (when-let [b (block-at p pos)]
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

(def eye-height 1.62)

(def eye-reach
  "Eye-to-centre distance within which a block is dug or placed without walking: a margin under the primitives' 4.5."
  4.2)

(def bucket-reach "Eye-to-centre distance the game accepts for a click, a bucket included." 4.5)

(defn eye-dist
  "Distance from the eye of a body at feet position here to the centre of cell (a [x y z] vector or an {:x :y :z} map)."
  [here cell]
  (let [[x y z] (if (vector? cell) cell [(:x cell) (:y cell) (:z cell)])]
    (dist {:x (:x here) :y (+ (:y here) eye-height) :z (:z here)} {:x (+ x 0.5) :y (+ y 0.5) :z (+ z 0.5)})))

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
        (some-> (.self p) .-inventory array-seq)))

(def inventory-slots
  "Main and hotbar slots: what the inventory list holds (armour and off-hand are not in it)."
  36)

(defn free-slots
  "Empty main and hotbar slots of primitives p, at least 0."
  [p]
  (max 0 (- inventory-slots (.-length (.-inventory (.self p))))))

(defn progress!
  "The act the failures counted against succeeded: the failures in a row start again from none."
  [c]
  (ctx/update-mem! c dissoc :failures)
  nil)

(defn count-fail!
  "Count one more failure in a row in job memory. True when that makes max-failures."
  [c]
  (let [tries (inc (:failures (ctx/mem c) 0))]
    (ctx/update-mem! c assoc :failures tries)
    (>= tries max-failures)))

(defn fail!
  "Count a failed round in a row (progress! resets). Returns :continue until max-failures in a row, then emits a warn of kind and returns :done."
  [c kind text]
  (if-not (count-fail! c)
    :continue
    (do (ctx/emit! c kind :warn {:tries max-failures :text text})
        :done)))
