(ns engine.value
  "Pure estimates for deciding whether to go back for the drops after a
  death: what the items are worth and what fetching them costs. Both are in
  the same arbitrary units and are meant to be refined later. The item worth
  (item-worth) is the shared worth table: recover-drops sums it over a death's
  inventory, make-room reads it to decide what may be tossed.

  Item value, per stack (an inventory entry), first matching rule wins:

    enchanted  25  the item has a non-empty :enchants or :nbt key (the
                   primitives do not report either yet, so this only fires
                   once they do)
    high       25  diamond and netherite anything, ancient debris, elytra,
                   totem of undying, nether star, beacon, shulker boxes
    medium      5  iron tools and armour, bow, crossbow, shield, redstone,
                   ender pearls, golden apples
    metal       2  each raw iron, iron ingot, gold ingot, coal block and iron
                   block: it takes mining, smelting and time to replace, so a
                   stack is worth its count times 2
    low         1  food, logs, planks, wood, stone/wooden/golden tools,
                   leather gear
    junk        0  everything else (seeds, dirt, cobblestone, saplings,
                   sticks, flowers, unknown names)

  inventory-value adds the stacks of one name together before it prices them
  (a pile split over slots is one pile). Experience adds 0.5 per level: the game drops only a few levels' worth."
  (:require [clojure.string :as str]))

(def tier-values {:junk 0 :low 1 :medium 5 :high 25})

(def metal-each 2)

(def rules
  "Name rules in priority order: {:match regex} plus :tier (the stack's worth is the tier value) or
  :each (the stack's worth is :each per item)."
  [{:tier :high :match #"^(diamond|netherite)(_|$)|^(ancient_debris|elytra|totem_of_undying|nether_star|beacon)$|shulker_box$"}
   {:tier :medium :match #"^iron_(pickaxe|axe|shovel|hoe|sword|helmet|chestplate|leggings|boots)$"}
   {:tier :medium :match #"^(bow|crossbow|shield|redstone|ender_pearl|golden_apple)$"}
   {:each metal-each :match #"^(coal_block|iron_block|iron_ingot|gold_ingot|raw_iron)$"}
   {:tier :low :match #"^(cooked_.*|bread|apple|carrot|baked_potato|golden_carrot|melon_slice|sweet_berries)$"}
   {:tier :low :match #"_(log|planks|wood)$"}
   {:tier :low :match #"^(stone|wooden|golden)_(pickaxe|axe|shovel|hoe|sword)$"}
   {:tier :low :match #"^leather"}])

(defn enchanted?
  "Whether the item carries a non-empty :enchants or :nbt."
  [item]
  (boolean (some #(seq (get item %)) [:enchants :nbt])))

(defn rule-for
  "The first rule matching the entry's name, or nil."
  [{:keys [name]}]
  (some #(when (re-find (:match %) name) %) rules))

(defn item-worth
  "The worth of one inventory entry ({:name :count? ...}); a missing :count counts as 1."
  [item]
  (cond
    (enchanted? item) (tier-values :high)
    :else (let [r (rule-for item)]
            (cond (nil? r) 0
                  (:each r) (* (:each r) (or (:count item) 1))
                  :else (tier-values (:tier r))))))

(defn merge-stacks
  "inventory with the plain stacks of one name added into one entry; enchanted stacks stay apart."
  [inventory]
  (let [[special plain] ((juxt filter remove) enchanted? inventory)]
    (concat special
            (map (fn [[name stacks]] {:name name :count (transduce (map #(or (:count %) 1)) + 0 stacks)})
                 (group-by :name plain)))))

(defn inventory-value
  "The worth of carrying inventory ([{:name :count ...}]) with experience
  level: the worth of each name's stacks together plus 0.5 per level."
  [inventory level]
  (+ (transduce (map item-worth) + 0 (merge-stacks inventory))
     (* 0.5 (or level 0))))

;; ---------------------------------------------------------------- the cost

(def despawn-ms (* 5 60 1000))
(def per-block 0.1)
(def per-hostile 10)
(def hostile-radius 16)
(def lethal-causes ["lava" "fire" "burn" "void" "out_of_world"])

(defn dist [a b]
  (js/Math.hypot (- (:x a) (:x b)) (- (:y a) (:y b)) (- (:z a) (:z b))))

(defn lethal-cause? [cause]
  (let [c (str/lower-case (if (keyword? cause) (name cause) (str cause)))]
    (boolean (some #(str/includes? c %) lethal-causes))))

(defn retrieval-cost
  "What fetching the drops costs, or js/Infinity when it should not be tried:
    0.1 per block between now-pos and death-pos
    + 10 per hostile (a seq of {:x :y :z}) within 16 blocks of death-pos
    + 5 * elapsed / (remaining window), which is 0 at the death, 5 halfway
      and unbounded as the 5 minute despawn window closes
  Infinite at or beyond 300000 ms elapsed, when cause (a string or keyword)
  mentions lava, fire, burning or the void, and when the cause is unknown
  (nil) and the death-pos is too (without a position there is nothing to
  walk to, so a nil death-pos is always infinite)."
  [death-pos now-pos hostiles cause elapsed-ms]
  (cond
    (>= elapsed-ms despawn-ms) js/Infinity
    (lethal-cause? cause) js/Infinity
    (nil? death-pos) js/Infinity
    :else (+ (* per-block (dist death-pos now-pos))
             (* per-hostile (count (filter #(<= (dist death-pos %) hostile-radius) hostiles)))
             (/ (* 5 elapsed-ms) (- despawn-ms elapsed-ms)))))
