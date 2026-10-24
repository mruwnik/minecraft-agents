(ns jobs.lib.crops
  (:require [engine.constants :as constants]
            [jobs.lib.look :as look]))

(defn seen-crops
  "The crops of the given names the body has seen within radius, still standing, nearest first, as {:name :pos :age}
  (the age as last seen)."
  [p names radius max]
  (->> (look/seen-blocks p {:names names :radius radius :max max :live? true :properties? true})
       (map (fn [b] (assoc b :age (some-> (get-in b [:properties :age]) js/Number))))))

(def cut-radius "Blocks round a ripe crop that a harvest called for it works (harvest alone works 12 round the body)." 16)

(def ripe-age
  "The field crops: farm jobs plant, tend and cut these; nether wart, berries and cocoa need other handling."
  (select-keys constants/crop-ripe-age ["wheat" "carrots" "potatoes" "beetroots"]))

(def stages-per-s
  "Mean growth stages a second of a crop in a field of its own kind on wet farmland, as the game draws them: 3 random
  ticks a game tick among the 4096 blocks of a section, 20 game ticks a second, and a tick grows the crop a stage with
  chance 1/(floor(25/5)+1) (growth speed 5: wet farmland round it, halved by neighbours of its kind). Under light 9 it
  does not grow; that is not counted."
  (/ (* 3 20) 4096 6))

(defn ripe-chance
  "The chance a crop seen at stage `age` `secs` seconds ago is ripe now: at least the stages left to its ripe-age grew
  in that time (Poisson at stages-per-s)."
  [crop age secs]
  (let [left (- (get ripe-age crop 7) age)
        mu (* stages-per-s (max 0 secs))]
    (if (<= left 0)
      1
      (- 1 (reduce + (map (fn [k] (* (js/Math.exp (- mu)) (/ (js/Math.pow mu k) (reduce * 1 (range 1 (inc k)))))) (range left)))))))

(def food-ripe-age
  "The growth stage at which each crop block can be harvested for food. Wheat is left out: only the :field way cuts it, in a field of its own."
  (dissoc ripe-age "wheat"))

(def wild-food
  "Blocks dug for food besides the crops (a melon has no growth stage: it is always ripe)."
  (merge (select-keys constants/crop-ripe-age ["sweet_berry_bush"]) {"melon" 0}))

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
