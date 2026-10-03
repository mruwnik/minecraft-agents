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
