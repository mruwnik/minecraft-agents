(ns jobs.forestry.collect-drops
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.look :as look]
            [jobs.lib.reach :as reach]
            [jobs.lib.sense :as sense]
            [jobs.lib.util :as u]
            [jobs.lib.trees :refer [default-radius]]))

(def doc
  "Collect the nearest matching dropped item, in one call, until none is left within :radius of where the job began (the work area: a walk to an item
  does not move it, so drops far away, e.g. other bodies', are never chased).
  An item is tried once; one that is gone, cannot be reached or picked up is skipped.
  An item more than a block lower than the feet is walked to first (a jobs.movement.go-to child, range 1, which prices
  falls and dangers); one it cannot reach is skipped.
  An item lower than the feet in a hole the caller dug (:holes) is walked to only when it lies one block down at most and
  the body has seen the floor under it solid and safe and the cells round it (no lava, fluid or cave opening, nothing
  hazardous beside it), the walk dropping one block at most; else it is left, with a debug collect-drops.left
  {:pos :reason :unseen-floor|:unseen-cell|:unsafe-floor|:too-deep}.
  Result: {:collected n}, the number of items that entered the inventory, with :left [{:pos :reason}] for those.")

(a/defargs args
  {:radius {:doc "search radius in blocks" :spec (a/num-in 0 nil) :default default-radius}
   :filter {:doc "item names to collect; everything when nil" :spec (a/coll-of a/item?) :default nil}
   :near {:doc "{:x :y :z} the work area is centred on, instead of where the body stands when the job begins" :spec ::a/pos :default nil}
   :ids {:doc "entity ids to collect (only those); any item when nil" :spec (a/coll-of number?) :default nil}
   :holes {:doc "cells the caller dug: an item lower than the feet at or under one of them is in that hole"
           :spec (a/coll-of a/position?) :default nil}})

(defn check [_c] true)

(defn gained-count
  "The items an act result says entered the inventory."
  [r]
  (->> (array-seq (or (.-gained r) #js []))
       (map #(.-count %))
       (reduce + 0)))

(defn nearest-item
  "The nearest wanted item within :radius of the anchor that is not skipped, or nil."
  [c anchor]
  (let [{:keys [radius ids]} (:args c)
        only-ids (some-> ids set)
        wanted (some-> (:filter (:args c)) set)
        skipped (set (:skipped (ctx/mem c)))]
    (->> (look/seen-items (:primitives c) {:radius (+ radius (u/dist anchor (u/self-pos c))) :max (if (or only-ids wanted) 1024 32)})
         (filter #(if-let [p (.-pos %)] (<= (u/dist anchor (u/pos-of p)) radius) true))
         (remove #(skipped (.-id %)))
         (filter #(or (nil? only-ids) (only-ids (.-id %))))
         (filter #(or (nil? wanted) (wanted (some-> (.-item %) .-name))))
         first)))

(def fluids #{"water" "lava" "bubble_column"})

(defn item-cell [item] (let [{:keys [x y z]} (u/pos-of (.-pos item))] [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]))

(defn feet-y [c] (js/Math.floor (:y (u/self-pos c))))

(defn below-feet? [c item] (boolean (and (.-pos item) (< (second (item-cell item)) (feet-y c)))))

(defn in-hole?
  "Whether item lies lower than the feet in the column of a cell of :holes, at or under it."
  [c item]
  (let [[x y z] (when (.-pos item) (item-cell item))]
    (boolean (and (below-feet? c item)
                  (some #(let [{hx :x hy :y hz :z} (a/cell %)]
                           (and (= x (js/Math.floor hx)) (= z (js/Math.floor hz)) (<= y (js/Math.floor hy))))
                        (:holes (:args c)))))))

(defn deep? [c item] (boolean (and (.-pos item) (> (- (feet-y c) (second (item-cell item))) 1))))

(defn hole-problem
  "Why the body must not walk down to item, in a dug hole: :too-deep (more than one block down), :unseen-floor (the
  floor under it not seen), :unseen-cell (its cell, the head cell over it or a cell beside not seen) or :unsafe-floor
  (no seen solid floor to stand on, a hazard at feet or head, or a fluid beside); nil when it may."
  [c item]
  (let [p (:primitives c)
        [x y z] (item-cell item)
        at (fn [dx dy dz] {:x (+ x dx) :y (+ y dy) :z (+ z dz)})
        sides (map #(apply at %) [[1 0 0] [-1 0 0] [0 0 1] [0 0 -1]])]
    (if (> (- (feet-y c) y) 1)
      :too-deep
      (let [unseen (filterv #(dig-look/unknown? p (u/cell-vec %)) (concat [(at 0 -1 0) (at 0 0 0) (at 0 1 0)] sides))]
        (cond
          (some #{(at 0 -1 0)} unseen) :unseen-floor
          (seq unseen) :unseen-cell
          (or (not (reach/standable-cell? p (at 0 0 0)))
              (some #(reach/hazard-blocks (u/seen-name p %)) [(at 0 0 0) (at 0 1 0)])
              (some #(fluids (u/seen-name p %)) (concat [(at 0 0 0) (at 0 -1 0)] sides)))
          :unsafe-floor)))))

(defn ^:async hole-problem!
  "hole-problem of item, decided again after a look at each cell it found unseen (jobs.lib.sense/decide!)."
  [c item]
  (await (sense/decide! c :collect-drops.hole #(hole-problem c item) {:retry? some?})))

(defn ^:async walk-down!
  "Walk within a block of item, more than a block lower than the feet: one go-to call (child/run!, slot :walk).
  :arrived, :continue (go-to waits on the world) or :unreachable."
  [c item]
  (let [r (await (child/run! c :walk 'jobs.movement.go-to {:pos (item-cell item) :range 1 :escalate false}))]
    (cond
      (= :continue r) :continue
      (and (= :done r) (:arrived (ctx/child-result c :walk))) :arrived
      :else :unreachable)))

(defn leave!
  "Leave item where it lies: why is the hole-problem."
  [c item why]
  (let [[x y z] (item-cell item)
        cell {:x x :y y :z z}]
    (ctx/emit! c :collect-drops.left :debug {:pos cell :reason why :item (some-> (.-item item) .-name)
                                             :text (str "left a drop at " x "," y "," z ": " (name why))})
    (ctx/update-mem! c update :left (fnil conj []) {:pos cell :reason why})))

(defn ^:async round
  "One whole attempt: collects the nearest matching dropped item again and again until none is left in radius.
  Each item is tried once (picked up, gone, unreachable or not): its id is remembered in job memory and skipped,
  and :collected counts the items gained, also on a round that gave up.
  :ids limits it to those entity ids (a dig's own drops)."
  [c]
  (let [anchor (or (:anchor (ctx/mem c))
                   (let [a (or (:near (:args c)) (u/self-pos c))] (ctx/update-mem! c assoc :anchor a) a))]
    (loop []
      (let [item (when (ctx/alive? c) (nearest-item c anchor))]
        (if-not item
          (do (ctx/result! c (cond-> {:collected (:collected (ctx/mem c) 0)}
                               (seq (:left (ctx/mem c))) (assoc :left (:left (ctx/mem c)))
                               (not (ctx/alive? c)) (assoc :reason :cut)))
              :done)
          (let [hole? (in-hole? c item)
                why (when hole? (await (hole-problem! c item)))
                walk (when (and (not hole?) (deep? c item)) (await (walk-down! c item)))]
            (if (= :continue walk)
              :continue
              (do (cond
                    why (leave! c item why)
                    (not= :unreachable walk)
                    (let [r (await (ctx/act c :collect (clj->js (cond-> {:id (.-id item)} hole? (assoc :maxDropDown 1)))))]
                      (ctx/update-mem! c update :collected (fnil + 0) (gained-count r))))
                  (ctx/update-mem! c update :skipped (fnil conj []) (.-id item))
                  (recur)))))))))
