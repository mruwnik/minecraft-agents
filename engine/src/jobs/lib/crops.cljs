(ns jobs.lib.crops
  (:require [engine.constants :as constants]
            [jobs.lib.look :as look]))

(defn seen-crops
  "The crops of the given names the body has seen within radius, still standing, nearest first, as {:name :pos :age}
  (the age as last seen)."
  [p names radius max]
  (->> (look/seen-blocks p {:names names :radius radius :max max :live? true :properties? true})
       (map (fn [b] (assoc b :age (some-> (get-in b [:properties :age]) js/Number))))))

(def ripe-age
  "The field crops: farm jobs plant, tend and cut these; nether wart, berries and cocoa need other handling."
  (select-keys constants/crop-ripe-age ["wheat" "carrots" "potatoes" "beetroots"]))

(def field-items
  "Per field crop: the seed that sows it and the items its harvest drops (the one place these lists live)."
  {"wheat" {:seed "wheat_seeds" :produce ["wheat" "wheat_seeds"]}
   "carrots" {:seed "carrot" :produce ["carrot"]}
   "potatoes" {:seed "potato" :produce ["potato" "poisonous_potato"]}
   "beetroots" {:seed "beetroot_seeds" :produce ["beetroot" "beetroot_seeds"]}})

(def seed-of (update-vals field-items :seed))

(def field-seeds "The items that sow a field crop." (vec (vals seed-of)))

(def other-seeds "Seeds of crops the farm jobs do not sow (they stay carried or are composted)." ["melon_seeds" "pumpkin_seeds"])

(def produce-items "Everything a field harvest drops." (vec (distinct (mapcat :produce (vals field-items)))))

(def waste-seeds "Every seed: the field crops' and the others." (into (filterv #(.endsWith % "_seeds") field-seeds) other-seeds))

(def farm-goods
  "Seeds, crops, melon, pumpkin, hay and cocoa: what a farm yields and may be put in a chest."
  (vec (distinct (concat waste-seeds produce-items ["melon_slice" "melon" "pumpkin" "hay_block" "cocoa_beans"]))))

(defn seed-item?
  "Whether an item name is a seed or a seed-like crop item (carrot, potato) a body keeps to sow."
  [name]
  (boolean (or (.endsWith name "_seeds") (some #{name} field-seeds))))

(defn inc-in [m k] (update m k (fnil inc 0)))

(defn count-entry
  "Count a fail on pos in the list m[k] of {:pos :n}; at max fails the entry goes and (on-max m pos) is the result."
  [m k pos max on-max]
  (let [n (inc (or (:n (first (filter #(= pos (:pos %)) (k m)))) 0))]
    (if (>= n max)
      (on-max (update m k (fn [entries] (vec (remove #(= pos (:pos %)) entries)))) pos)
      (update m k (fn [entries] (conj (vec (remove #(= pos (:pos %)) entries)) {:pos pos :n n}))))))

(defn count-debt
  "Count a fail (field f) on the debt at pos in m[:replant]; at max fails (on-max m pos) is the result."
  [m f pos max on-max]
  (let [n (inc (or (f (first (filter #(= pos (:pos %)) (:replant m)))) 0))]
    (if (>= n max)
      (on-max m pos)
      (update m :replant (fn [debts] (mapv #(if (= pos (:pos %)) (assoc % f n) %) debts))))))
