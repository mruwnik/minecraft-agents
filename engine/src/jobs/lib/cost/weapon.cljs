(ns jobs.lib.cost.weapon
  "The fight inputs of the cost calculators, pure: what a weapon does and how fast, what is left of a mob, and the one
  table of tool materials (harvest tier, weapon rank, cheapness) every tool ranking reads."
  (:require [clojure.string :as str]))

(def ranged-mobs
  "Hostiles that shoot or throw from a distance, so count from further away."
  #{"skeleton" "stray" "parched" "bogged" "breeze" "blaze" "ghast" "pillager" "witch"})

(def tool-material
  "Per tool material: :tier harvest tier, :speed mining speed (apart from the tier: gold digs fastest and harvests as wood does),
  :rank weapon strength (higher is better), :cheap order to make the tool in (0 first; gold last)."
  {"wooden" {:tier 1 :speed 2 :rank 0 :cheap 0}
   "stone" {:tier 2 :speed 4 :rank 2 :cheap 1}
   "copper" {:tier 2 :speed 5 :rank 2 :cheap 2}
   "iron" {:tier 3 :speed 6 :rank 3 :cheap 3}
   "diamond" {:tier 4 :speed 8 :rank 4 :cheap 4}
   "netherite" {:tier 4 :speed 9 :rank 5 :cheap 5}
   "golden" {:tier 1 :speed 12 :rank 1 :cheap 6}})

(def cheapness
  "Tool materials, cheapest to make first."
  (mapv key (sort-by (comp :cheap val) tool-material)))

(defn material-of
  "The material prefix of a tool or armour name (\"iron_pickaxe\" -> \"iron\")."
  [item-name]
  (first (str/split item-name #"_")))

(defn tool-tier
  "Harvest tier of a tool by its material (1 for an unknown one)."
  [item-name]
  (get-in tool-material [(material-of item-name) :tier] 1))

(defn tool-speed
  "Mining speed of a tool by its material (1, the bare hand's, for an unknown one)."
  [item-name]
  (get-in tool-material [(material-of item-name) :speed] 1))

(defn weapon-rank
  "Weapon strength of a tool by its material (0 for an unknown one)."
  [item-name]
  (get-in tool-material [(material-of item-name) :rank] 0))


(defn cheap-rank
  "Position of the tool's material in cheapness (past the end for an unknown one)."
  [item-name]
  (let [i (.indexOf cheapness (material-of item-name))]
    (if (neg? i) (count cheapness) i)))

(def axe-attack-speed
  {"wooden" 0.8 "stone" 0.8 "copper" 0.8 "iron" 0.9 "golden" 1 "diamond" 1 "netherite" 1})

(defn attack-speed
  "Vanilla attack speed of the held item (nil: a fist; 4 for an unknown one)."
  [item-name]
  (cond
    (nil? item-name) 4
    (str/ends-with? item-name "_sword") 1.6
    (str/ends-with? item-name "_axe") (get axe-attack-speed (material-of item-name) 4)
    (str/ends-with? item-name "_pickaxe") 1.2
    (str/ends-with? item-name "_shovel") 1
    :else 4))

(def min-gap-ms
  "Mobs ignore damage for 0.5 s after a hit, so a swing sooner than this is wasted."
  500)

(defn attack-gap-ms
  "The full-strength attack cooldown in ms of the held item (nil: a fist), at least min-gap-ms."
  [item-name]
  (max min-gap-ms (js/Math.ceil (/ 1000 (attack-speed item-name)))))

(def mob-max-health
  "Full health of the common hostiles; unknown ones count as 20."
  {"zombie" 20 "husk" 20 "drowned" 20 "zombie_villager" 20 "skeleton" 20 "stray" 20 "parched" 20 "bogged" 16
   "spider" 16 "cave_spider" 12 "creeper" 20 "witch" 26 "pillager" 24 "slime" 16 "silverfish" 8
   "endermite" 8 "phantom" 20 "vindicator" 24 "enderman" 40})

(def passive-max-health
  "Full health of the common passive animals (what a hunt costs in the item value base)."
  {"cow" 10 "pig" 10 "sheep" 8 "chicken" 4 "rabbit" 3 "cod" 3 "salmon" 3 "squid" 10})

(def weapon-damage-by-name
  "Vanilla damage of one hit with each item that hits harder than a fist (hoes hit as a fist)."
  {"wooden_sword" 4 "golden_sword" 4 "stone_sword" 5 "copper_sword" 5 "iron_sword" 6 "diamond_sword" 7 "netherite_sword" 8
   "wooden_axe" 7 "golden_axe" 7 "stone_axe" 9 "copper_axe" 9 "iron_axe" 9 "diamond_axe" 9 "netherite_axe" 10
   "wooden_pickaxe" 2 "golden_pickaxe" 2 "stone_pickaxe" 3 "copper_pickaxe" 3 "iron_pickaxe" 4 "diamond_pickaxe" 5 "netherite_pickaxe" 6
   "wooden_shovel" 2.5 "golden_shovel" 2.5 "stone_shovel" 3.5 "copper_shovel" 3.5 "iron_shovel" 4.5 "diamond_shovel" 5.5 "netherite_shovel" 6.5})

(defn weapon-damage
  "Damage of one hit with item-name (a fist, 1, for nil or an unknown item)."
  [item-name]
  (get weapon-damage-by-name item-name 1))

(defn weapon-score
  "Higher is better: the damage a second of item-name's hits (weapon-damage over attack-gap-ms)."
  [item-name]
  (/ (weapon-damage item-name) (attack-gap-ms item-name)))

(defn digging-tool?
  "Whether item-name is a pickaxe or shovel: a hit wears it twice as a sword's does, and it is dear."
  [item-name]
  (boolean (some #(str/ends-with? (or item-name "") %) ["_pickaxe" "_shovel"])))

(defn weapon-order
  "Sort key, best first: a sword or axe before any digging tool, then the higher weapon-score."
  [item-name]
  [(if (digging-tool? item-name) 1 0) (- (weapon-score item-name))])

(defn kill-seconds
  "Seconds the hits of item-name (a fist for nil) take to kill a mob of hp, one every attack gap."
  [item-name hp]
  (* (/ (attack-gap-ms item-name) 1000) (js/Math.ceil (/ hp (weapon-damage item-name)))))

(defn base-name
  "The mob name of kind k: the part before the held item of an armed kind (\"zombie/iron_sword\"); the tables keyed by mob name read it."
  [k]
  (if (string? k) (first (str/split k #"/")) k))

(defn remaining-health
  "What is left of a mob: its reported :health, else full health less :hits times :damage (an estimate)."
  [{:keys [name hits damage health]}]
  (if (number? health)
    health
    (- (get mob-max-health (base-name name) 20) (* (or hits 0) damage))))
