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

(def tool-tier
  "Harvest tier of a tool material: wood and gold 0, stone 1, iron 2, diamond and netherite 3."
  {"wooden" 0 "golden" 0 "stone" 1 "iron" 2 "diamond" 3 "netherite" 3})

(defn item-tier [item-name] (get tool-tier (first (str/split item-name #"_")) 0))

(def pickaxe-tiers
  "[tier regex] rows, highest first: the lowest pickaxe tier that harvests a block whose name matches. A block that
  matches none needs no pickaxe by hand (leaves, glass, torches...)."
  [[3 #"^(obsidian|crying_obsidian|ancient_debris|netherite_block|respawn_anchor)$"]
   [2 #"^(deepslate_)?(diamond|gold|emerald|redstone)_ore$|^(nether_gold_ore|gold_block|diamond_block|emerald_block|redstone_block|raw_gold_block)$"]
   [1 #"^(deepslate_)?(iron|lapis|copper)_ore$|^(iron_block|lapis_block|copper_block|raw_iron_block|raw_copper_block)$"]
   [0 #"stone|cobble|deepslate|andesite|diorite|granite|tuff|calcite|dripstone|netherrack|brick|blackstone|basalt|terracotta|concrete$|prismarine|purpur|coal_ore|coal_block|quartz|furnace|smoker|anvil|iron_bars|hopper|cauldron|spawner|_ore$"]])

(defn pickaxe-tier
  "The lowest pickaxe tier (0 wood/gold .. 3 diamond) that harvests block-name, or nil when it needs none."
  [block-name]
  (some (fn [[tier re]] (when (re-find re block-name) tier)) pickaxe-tiers))

(def tier-pickaxe ["wooden_pickaxe" "stone_pickaxe" "iron_pickaxe" "diamond_pickaxe"])

(defn harvest-need
  "nil when the carried items (names) can harvest block-name, else the pickaxe needed (\"pickaxe\" for any,
  \"stone_pickaxe\" and up as named: the minimum tier, or better)."
  [item-names block-name]
  (let [need (pickaxe-tier block-name)
        best (->> item-names (filter #(str/ends-with? % "_pickaxe")) (map item-tier) (reduce max -1))]
    (when (and need (< best need))
      (if (zero? need) "pickaxe" (get tier-pickaxe need)))))
