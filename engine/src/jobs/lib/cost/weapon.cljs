(ns jobs.lib.cost.weapon
  "The fight inputs of the cost calculators, pure: what a weapon does and how fast, what is left of a mob, and the one
  table of tool materials (harvest tier, weapon rank, cheapness) every tool ranking reads."
  (:require [clojure.string :as str]))

(def ranged-mobs
  "Hostiles that shoot or throw from a distance, so count from further away."
  #{"skeleton" "stray" "bogged" "pillager" "witch"})

(def tool-material
  "Per tool material: :tier harvest tier, :rank weapon strength (higher is better), :cheap order to make the tool in (0 first;
  gold last: it harvests as wood does)."
  {"wooden" {:tier 1 :rank 0 :cheap 0}
   "stone" {:tier 2 :rank 2 :cheap 1}
   "copper" {:tier 2 :rank 2 :cheap 2}
   "iron" {:tier 3 :rank 3 :cheap 3}
   "diamond" {:tier 4 :rank 4 :cheap 4}
   "netherite" {:tier 4 :rank 5 :cheap 5}
   "golden" {:tier 1 :rank 1 :cheap 6}})

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

(defn weapon-rank
  "Weapon strength of a tool by its material (0 for an unknown one)."
  [item-name]
  (get-in tool-material [(material-of item-name) :rank] 0))

(defn cheap-rank
  "Position of the tool's material in cheapness (past the end for an unknown one)."
  [item-name]
  (let [i (.indexOf cheapness (material-of item-name))]
    (if (neg? i) (count cheapness) i)))

(def axe-gap-ms
  {"wooden_axe" 1250 "stone_axe" 1250 "copper_axe" 1250 "iron_axe" 1112 "golden_axe" 1000 "diamond_axe" 1000 "netherite_axe" 1000})

(def min-gap-ms
  "Mobs ignore damage for 0.5 s after a hit, so a swing sooner than this is wasted."
  500)

(defn attack-gap-ms
  "The full-strength attack cooldown in ms of the held item (nil: a fist), at least min-gap-ms."
  [item-name]
  (max min-gap-ms
       (cond
         (and item-name (str/ends-with? item-name "_sword")) 625
         :else (get axe-gap-ms item-name min-gap-ms))))

(def mob-max-health
  "Full health of the common hostiles; unknown ones count as 20."
  {"zombie" 20 "husk" 20 "drowned" 20 "zombie_villager" 20 "skeleton" 20 "stray" 20 "bogged" 16
   "spider" 16 "cave_spider" 12 "creeper" 20 "witch" 26 "pillager" 24 "slime" 16 "silverfish" 8
   "endermite" 8 "phantom" 20 "vindicator" 24 "enderman" 40})

(def weapon-damage-by-name
  {"wooden_sword" 4 "golden_sword" 4 "stone_sword" 5 "copper_sword" 5 "iron_sword" 6 "diamond_sword" 7 "netherite_sword" 8
   "wooden_axe" 7 "golden_axe" 7 "stone_axe" 9 "copper_axe" 9 "iron_axe" 9 "diamond_axe" 9 "netherite_axe" 10})

(defn weapon-damage
  "Damage of one hit with item-name (a fist, 1, for nil or an unknown item)."
  [item-name]
  (get weapon-damage-by-name item-name 1))

(defn remaining-health
  "What is left of a mob: its reported :health, else full health less :hits times :damage (an estimate)."
  [{:keys [name hits damage health]}]
  (if (number? health)
    health
    (- (get mob-max-health name 20) (* (or hits 0) damage))))
