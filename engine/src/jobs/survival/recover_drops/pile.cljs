(ns jobs.survival.recover-drops.pile
  "A pile in the inventory and on the ground: what of it is back, what is left, which of its items are in sight."
  (:require [engine.ctx :as ctx]
            [engine.memory :as mem]
            [jobs.lib.storage :as storage]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]
            [jobs.survival.recover-drops.config :as cfg]))

(defn recovered-items
  "How many of the pile's items entered the inventory since the baseline (taken when the
  trip was decided): per item name the rise, capped by what the pile held. Items picked up
  on the walk count as well as those the collect step picks up."
  [c pile]
  (let [baseline (:baseline (ctx/mem c))
        now (storage/carried-totals (u/inventory (:primitives c)))
        wanted (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} pile)]
    (reduce + 0 (map (fn [[name n]] (min n (max 0 (- (get now name 0) (get baseline name 0))))) wanted))))

(defn baseline
  "The carried totals the pile's return is counted from: what the body holds less what it picked up since the death at
  death-t (the :picked-up memory), so drops it woke beside and took before the trip count as back."
  [c death-t]
  (let [picked (reduce (fn [m {:keys [data]}] (update m (:item data) (fnil + 0) (:count data))) {}
                       (filter #(>= (:t %) death-t) (mem/entries (ctx/view c) :picked-up)))]
    (reduce-kv (fn [m k n] (assoc m k (max 0 (- n (get picked k 0))))) {} (storage/carried-totals (u/inventory (:primitives c))))))

(defn pile-ids
  "Ids of the item entities in sight lying within radius of pos whose name is one of names."
  [c pos names radius]
  (let [wanted (set names)]
    (->> (look/seen-items (:primitives c) {:radius (+ radius (cfg/stray-range) 4) :max 64})
         (filter #(wanted (some-> (.-item %) .-name)))
         (filter #(<= (u/dist pos (u/pos-of (.-pos %))) radius))
         (mapv #(.-id %)))))

(defn left-over
  "{item name count} of the pile not yet back in the inventory."
  [c pile]
  (let [baseline (:baseline (ctx/mem c))
        now (storage/carried-totals (u/inventory (:primitives c)))
        wanted (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} pile)]
    (into {} (keep (fn [[name n]] (let [l (- n (max 0 (- (get now name 0) (get baseline name 0))))] (when (pos? l) [name l]))) wanted))))

(defn look-cell
  "The cell [x y z] the first unseen drop was last seen in, from the :left of a collect pass's ending data, or nil."
  [swept]
  (->> (:left swept)
       (filter #(= :unseen (:reason %)))
       (mapcat (comp vals :last))
       first))
