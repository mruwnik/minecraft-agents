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
                   ender pearls, golden apples; coal and iron blocks or
                   ingots, gold ingots and raw iron at 8 or more in the stack
    low         1  food, logs, planks, wood, stone/wooden/golden tools,
                   leather gear, and the bulk metals below 8 in the stack
    junk        0  everything else (seeds, dirt, cobblestone, saplings,
                   sticks, flowers, unknown names)

  Experience adds 0.5 per level: the game drops only a few levels' worth."
  (:require [clojure.string :as str]))

(def tier-values {:junk 0 :low 1 :medium 5 :high 25})

(def bulk-count 8)

(def rules
  "Name rules in priority order: {:tier :match regex :min-count n}."
  [{:tier :high :match #"^(diamond|netherite)(_|$)|^(ancient_debris|elytra|totem_of_undying|nether_star|beacon)$|shulker_box$"}
   {:tier :medium :match #"^iron_(pickaxe|axe|shovel|hoe|sword|helmet|chestplate|leggings|boots)$"}
   {:tier :medium :match #"^(bow|crossbow|shield|redstone|ender_pearl|golden_apple)$"}
   {:tier :medium :match #"^(coal_block|iron_block|iron_ingot|gold_ingot|raw_iron)$" :min-count bulk-count}
   {:tier :low :match #"^(coal_block|iron_block|iron_ingot|gold_ingot|raw_iron)$"}
   {:tier :low :match #"^(cooked_.*|bread|apple|carrot|baked_potato|golden_carrot|melon_slice|sweet_berries)$"}
   {:tier :low :match #"_(log|planks|wood)$"}
   {:tier :low :match #"^(stone|wooden|golden)_(pickaxe|axe|shovel|hoe|sword)$"}
   {:tier :low :match #"^leather"}])

(defn enchanted?
  "Whether the item carries a non-empty :enchants or :nbt."
  [item]
  (boolean (some #(seq (get item %)) [:enchants :nbt])))

(defn item-tier
  "The tier of an inventory entry; a missing :count counts as 1."
  [{:keys [name count] :as item}]
  (let [count (or count 1)]
    (if (enchanted? item)
      :high
      (or (some (fn [r] (when (and (re-find (:match r) name) (>= count (:min-count r 0))) (:tier r)))
                rules)
          :junk))))

(defn item-worth
  "The worth of one inventory entry ({:name :count? ...}): the value of its tier."
  [item]
  (tier-values (item-tier item)))

(defn inventory-value
  "The worth of carrying inventory ([{:name :count ...}]) with experience
  level: the tier value of each stack plus 0.5 per level."
  [inventory level]
  (+ (transduce (map item-worth) + 0 inventory)
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
