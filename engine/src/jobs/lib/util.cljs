(ns jobs.lib.util
  "Helpers the library jobs share: reading positions off JS, distances, the
  inventory and bounded failure counting. Walking in reach is jobs.lib.near/go-near!."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [engine.game :as game]
            [engine.ctx :as ctx]
            [engine.perception.rays :as rays]))

(a/defargs settings
  {::max-failures {:default 3 :doc "Failures in a row a retrying helper takes before it gives up." :spec (a/int-in 1 nil)}
   ::max-reads {:default 256 :doc "Unknown cells *reads* records per decision." :spec (a/int-in 1 nil)}})

(defn max-failures [] (settings/get settings ::max-failures))

(def ^:dynamic *reads*
  "Bound by jobs.lib.sense/decide! around a sync decision to a volatile of what it read and the body does not know:
  {:cells #{[x y z]} :guessed #{[x y z]} :area #{query} :entities #{query}}; nil otherwise."
  nil)

(defn note-read!
  "Record v under k in *reads* when it is bound (at most ::max-reads cells under :cells and :guessed)."
  [k v]
  (when-let [r *reads*]
    (vswap! r (fn [m] (if (and (#{:cells :guessed} k) (>= (count (get m k)) (settings/get settings ::max-reads)))
                        m
                        (update m k (fnil conj #{}) v))))))

(defn cell-vec
  "[x y z] of the cell holding pos (a cljs map or a JS {x y z})."
  [pos]
  (mapv js/Math.floor (if (map? pos) [(:x pos) (:y pos) (:z pos)] [(.-x pos) (.-y pos) (.-z pos)])))

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

(def touches?
  "Whether a body with its feet at (fx fy fz) touches the cell [x y z] whose collision top is `top`: the rule perception's
  feel answers by (engine.perception.rays/touches?)."
  rays/touches?)

(defn feel
  "The block at a cell the body touches (its feet, head and the cell under the feet), as the JS object, in any light;
  nil for a cell it does not touch. Without perception (no feel on p) the raw block is answered only for a cell touches?
  accepts: a block without a collision top reaches the feet only as a full cube."
  [p pos]
  (if (some? (.-feel p))
    (.feel p (clj->js pos))
    (let [^js at (.-pos ^js (.self p)) ^js q (clj->js pos)
          ^js b (block-at p q)
          top (or (some-> b .-top) (if (some-> b .-fullCube) 1 0))]
      (when (and b (touches? (.-x at) (.-y at) (.-z at) [(.-x q) (.-y q) (.-z q)] top))
        b))))

(defn feel-name
  "The block name at a cell the body touches, or nil (see feel)."
  [p pos]
  (some-> (feel p pos) .-name))

(defn sensed
  "What the body knows of a cell as the JS answer (blockAt's keys plus ageMs, felt, visible, unknown), or nil when the chunk
  is not loaded. Primitives that are not wrapped by perception (no sensedAt) know nothing: nil, never a blockAt read."
  [p pos]
  (when (some? (.-sensedAt p))
    (let [b (.sensedAt p (clj->js pos))]
      (when (and *reads* b (true? (.-unknown b))) (note-read! :cells (cell-vec pos)))
      b)))

(defn seen-block
  "The block at a cell in view or remembered, as the JS object; nil when unloaded or never seen (unknown). Look at the
  cell or skip it, never guess."
  [p pos]
  (let [b (sensed p pos)]
    (when-not (or (nil? b) (true? (.-unknown b))) b)))

(defn seen-name
  "The block name at a cell in view or remembered, or nil when unloaded or unknown (see seen-block)."
  [p pos]
  (some-> (seen-block p pos) .-name))

(defn block-name-or
  "The seen block name at a cell, or guess when it is unknown: the caller states its policy for what it cannot see.
  A guess is noted in *reads* (:guessed)."
  [p pos guess]
  (or (seen-name p pos)
      (do (when (and *reads* (true? (some-> (sensed p pos) .-unknown))) (note-read! :guessed (cell-vec pos)))
          guess)))

(defn seen-facts
  "seen-block as cljs facts {:name :full-cube? :waterlogged?}, or nil when the cell is unloaded or unknown."
  [p pos]
  (when-let [b (seen-block p pos)]
    (let [logged (some-> b .-properties .-waterlogged)]
      {:name (.-name b)
       :full-cube? (boolean (.-fullCube b))
       :waterlogged? (or (true? logged) (= "true" logged))})))

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


(def eye-reach
  "Eye-to-centre distance within which a block is dug or placed without walking: a margin under the primitives' 4.5."
  4.2)

(def bucket-reach "Eye-to-centre distance the game accepts for a click, a bucket included." 4.5)

(defn eye-dist
  "Distance from the eye of a body at feet position here to the centre of cell (a [x y z] vector or an {:x :y :z} map)."
  [here cell]
  (let [[x y z] (if (vector? cell) cell [(:x cell) (:y cell) (:z cell)])]
    (dist {:x (:x here) :y (+ (:y here) game/eye-height) :z (:z here)} {:x (+ x 0.5) :y (+ y 0.5) :z (+ z 0.5)})))

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
    (>= tries (max-failures))))

(defn fail!
  "Count a failed round in a row (progress! resets). Returns :continue until max-failures in a row, then emits a warn of kind and returns :done."
  [c kind text]
  (if-not (count-fail! c)
    :continue
    (do (ctx/emit! c kind :warn {:tries (max-failures) :text text})
        :done)))
