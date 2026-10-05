(ns engine.jobs.tools
  "Which carried tool suits a block."
  (:require [clojure.string :as str]
            [engine.jobs.combat :as combat]))

(def shovel-blocks
  #{"dirt" "grass_block" "sand" "red_sand" "gravel" "clay" "soul_sand" "soul_soil" "mud" "snow_block"
    "coarse_dirt" "rooted_dirt" "podzol" "mycelium" "farmland" "dirt_path"})

(defn tool-kind
  "\"shovel\", \"axe\" or \"pickaxe\": the tool that digs block-name fastest."
  [block-name]
  (cond
    (shovel-blocks block-name) "shovel"
    (some #(str/ends-with? block-name %) ["_log" "_wood" "_planks"]) "axe"
    :else "pickaxe"))

(defn best-tool
  "The carried tool (of item-names) of the kind that suits block-name with the
  highest material, or nil."
  [item-names block-name]
  (let [suffix (str "_" (tool-kind block-name))
        rank #(get combat/material-rank (first (str/split % #"_")) 0)]
    (->> item-names
         (filter #(str/ends-with? % suffix))
         (sort-by rank >)
         first)))

(def cheapness
  "Pickaxe materials, cheapest first, as a player would pick the tool to make: gold last (it harvests as wood does)."
  ["wooden" "stone" "copper" "iron" "diamond" "netherite" "golden"])

(defn cheapest-tool
  "The cheapest of item-names by material (cheapness order; unknown materials last), or nil for none."
  [item-names]
  (let [rank (fn [n] (let [i (.indexOf cheapness (first (str/split n #"_")))] (if (neg? i) (count cheapness) i)))]
    (first (sort-by rank item-names))))

(defn harvest-need
  "nil when the carried items (names) can harvest a block whose minecraft-data harvestTools are harvest-tools (item
  names; nil when any tool or the hand harvests it), else the cheapest tool that does."
  [item-names harvest-tools]
  (when (and (seq harvest-tools) (not-any? (set harvest-tools) item-names))
    (cheapest-tool harvest-tools)))
