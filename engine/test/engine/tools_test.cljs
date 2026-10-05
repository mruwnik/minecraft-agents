(ns engine.tools-test
  (:require [cljs.test :refer [deftest is are]]
            [engine.jobs.tools :as tools]))

(deftest tool-kind-by-block
  (are [block kind] (= kind (tools/tool-kind block))
    "dirt" "shovel"
    "sand" "shovel"
    "dirt_path" "shovel"
    "oak_log" "axe"
    "birch_planks" "axe"
    "oak_wood" "axe"
    "stone" "pickaxe"
    "cobblestone" "pickaxe"
    "iron_ore" "pickaxe"))

(deftest best-tool-picks-the-highest-material-of-the-kind
  (are [items block expected] (= expected (tools/best-tool items block))
    ["wooden_shovel" "iron_shovel" "diamond_pickaxe"] "dirt" "iron_shovel"
    ["stone_pickaxe" "diamond_pickaxe" "iron_shovel"] "stone" "diamond_pickaxe"
    ["golden_axe" "stone_axe"] "oak_log" "stone_axe"
    ["iron_shovel"] "stone" nil
    [] "dirt" nil
    ["stone" "dirt"] "dirt" nil))

(deftest harvest-need-is-the-cheapest-listed-tool-no-carried-item-matches
  (are [carried harvest expected] (= expected (tools/harvest-need carried harvest))
    [] nil nil
    ["stone_pickaxe"] nil nil
    [] ["wooden_pickaxe" "stone_pickaxe" "iron_pickaxe"] "wooden_pickaxe"
    ["wooden_pickaxe"] ["copper_pickaxe" "stone_pickaxe" "iron_pickaxe"] "stone_pickaxe"
    ["stone_pickaxe"] ["copper_pickaxe" "stone_pickaxe" "iron_pickaxe"] nil
    ["iron_pickaxe"] ["diamond_pickaxe" "netherite_pickaxe"] "diamond_pickaxe"
    ["golden_pickaxe"] ["iron_pickaxe" "golden_pickaxe"] nil
    ["golden_pickaxe"] ["iron_pickaxe" "diamond_pickaxe"] "iron_pickaxe"))
