(ns jobs.lib.crops
  (:require [engine.constants :as constants]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]))

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

(def starter-field "The :w x :h of the field jobs.farm.grow sows when no crop stands: its args' defaults, and what obtain prices growing by." {:w 3 :h 3})

(def grow-light "Light (the brighter of block and sky light, the sky's before the night's darkening) under which a crop does not grow." 9)

(def random-ticks-per-s "Random ticks a second a block gets: 3 a game tick among the 4096 blocks of a section, 20 game ticks a second." (/ (* 3 20) 4096))

(defn stages-per-s
  "Mean growth stages a second of a crop in a field of its own kind, as the game draws them: random-ticks-per-s, and a tick grows the crop a stage with chance
  1/(floor(25/f)+1), f its growth points, halved by neighbours of its kind: wet farmland under it and round it 10, so 5;
  dry 4, so 2. light: the cell's (nil: unseen, guessed open sky); under grow-light it does not grow."
  [wet? light]
  (if (and light (< light grow-light))
    0
    (/ random-ticks-per-s (inc (js/Math.floor (/ 25 (if wet? 5 2)))))))

(def food-ripe-age
  "The growth stage at which each crop block can be harvested for food. Wheat is left out: only the :field way cuts it, in a field of its own."
  (dissoc ripe-age "wheat"))

(def fruit-items
  "Per fruit block: the stems that bear it beside them, the item its block drops and whether that is eaten. A fruit has no
  growth stage: beside a stem of its kind it is ripe, and cutting it leaves the stem to bear again. :mean-drops is the
  game's loot (3 to 7 slices) where minecraft-data's blockLoot gives none that is right."
  {"melon" {:stems #{"melon_stem" "attached_melon_stem"} :drop "melon_slice" :food? true :mean-drops 5}
   "pumpkin" {:stems #{"pumpkin_stem" "attached_pumpkin_stem"} :drop "pumpkin"}})

(def fruits (vec (keys fruit-items)))

(def fruit-drops (mapv :drop (vals fruit-items)))

(def stem-fruit "The fruit each stem block bears." (into {} (for [[f {:keys [stems]}] fruit-items s stems] [s f])))

(defn beside [{:keys [x y z]}] (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]]] {:x (+ x dx) :y y :z (+ z dz)}))

(defn borne?
  "Whether p shows a fruit block at pos with a stem of its kind beside it."
  [p pos]
  (boolean (when-let [stems (:stems (fruit-items (u/seen-name p pos)))]
             (some #(stems (u/seen-name p %)) (beside pos)))))

(defn fruits-beside
  "The cells beside a stem at pos that p shows holding its fruit."
  [p pos]
  (let [fruit (stem-fruit (u/seen-name p pos))]
    (filterv #(and fruit (= fruit (u/seen-name p %))) (beside pos))))

(def ripe-names "The blocks a harvest cuts when ripe: the field crops and the fruits." (into (vec (keys ripe-age)) fruits))

(defn ripe?
  "Whether a seen crop {:name :pos :age} is ripe: a field crop at its ripe age, a fruit beside its stem."
  [p {:keys [name pos age]}]
  (if (fruit-items name)
    (borne? p pos)
    (boolean (some-> age (>= (ripe-age name))))))

(def wild-food
  "Blocks dug for food besides the crops: berries, and the fruits that are eaten (wherever they grow)."
  (merge (select-keys constants/crop-ripe-age ["sweet_berry_bush"])
         (into {} (for [[f {:keys [food?]}] fruit-items :when food?] [f 0]))))

(def field-items
  "Per field crop: the seed that sows it and the items its harvest drops (the one place these lists live)."
  {"wheat" {:seed "wheat_seeds" :produce ["wheat" "wheat_seeds"]}
   "carrots" {:seed "carrot" :produce ["carrot"]}
   "potatoes" {:seed "potato" :produce ["potato" "poisonous_potato"]}
   "beetroots" {:seed "beetroot_seeds" :produce ["beetroot" "beetroot_seeds"]}})

(def field-seed-of "Seed per box-field crop." (update-vals field-items :seed))

(def stem-seed-of
  "Seed per stem crop: a plan may plant and tend them, but they have no growth stage to harvest (the fruit is the harvest)."
  {"melon_stem" "melon_seeds" "pumpkin_stem" "pumpkin_seeds"})

(def seed-of "Seed per crop a plan can want: the field crops and the stems." (merge field-seed-of stem-seed-of))

(def field-seeds "The items that sow a field crop." (vec (vals field-seed-of)))

(def other-seeds "Seeds of crops the farm jobs do not sow (they stay carried or are composted)." ["melon_seeds" "pumpkin_seeds"])

(def produce-items "Everything a field harvest drops." (vec (distinct (mapcat :produce (vals field-items)))))

(def waste-seeds "Every seed: the field crops' and the others." (into (filterv #(.endsWith % "_seeds") field-seeds) other-seeds))

(def stalk-items "Sugar cane and bamboo: the stalks farm.maintain cuts." ["sugar_cane" "bamboo"])

(def farm-goods
  "Seeds, crops, fruit, stalks, hay, cocoa, bread and baked potatoes: what a farm yields and may be put in a chest."
  (vec (distinct (concat waste-seeds produce-items fruits fruit-drops stalk-items ["hay_block" "cocoa_beans" "bread" "baked_potato"]))))

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
