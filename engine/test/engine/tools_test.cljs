(ns engine.tools-test
  (:require [cljs.test :refer [deftest is are]]
            [engine.fake]
            [engine.test-util :as tu]
            [jobs.lib.tool-kind :as kind]
            [jobs.lib.tools :as tools]))

(deftest tool-kind-by-block
  (are [block kind] (= kind (kind/tool-kind block))
    "dirt" "shovel"
    "sand" "shovel"
    "dirt_path" "shovel"
    "oak_log" "axe"
    "birch_planks" "axe"
    "oak_wood" "axe"
    "stone" "pickaxe"
    "cobblestone" "pickaxe"
    "iron_ore" "pickaxe"
    "oak_leaves" "hoe"
    "azalea_leaves" "hoe"
    "oak_fence" "axe"
    "dark_oak_fence_gate" "axe"
    "dark_oak_planks" "axe"
    "nether_brick_fence" "pickaxe"
    "rail" "pickaxe"
    "chest_minecart" "pickaxe"
    "cobweb" "shears"))

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

(deftest wear-events
  (are [prev now expected] (= expected (tools/wear-event prev now))
    nil {:name "stone_pickaxe" :durability 100 :max 131 :n 1} nil
    {:name "stone_pickaxe" :durability 100 :max 131 :n 1} {:name "stone_pickaxe" :durability 90 :max 131 :n 1} nil
    {:name "stone_pickaxe" :durability 14 :max 131 :n 1} {:name "stone_pickaxe" :durability 13 :max 131 :n 1} :tool-low
    {:name "stone_pickaxe" :durability 2 :max 131 :n 2} {:name "stone_pickaxe" :durability 100 :max 131 :n 1} :tool-broke
    {:name "stone_pickaxe" :durability 2 :max 131 :n 1} {:name "stone_pickaxe" :n 0} :tool-broke
    {:name "stone_pickaxe" :durability 60 :max 131 :n 2} {:name "stone_pickaxe" :n 1} nil
    ;; already low at the first hold: told once; not again once :low-seen
    {:name "stone_pickaxe" :durability 10 :max 131 :n 1} {:name "stone_pickaxe" :durability 9 :max 131 :n 1} :tool-low
    {:name "stone_pickaxe" :durability 10 :max 131 :n 1 :low-seen true} {:name "stone_pickaxe" :durability 9 :max 131 :n 1} nil))

(deftest need-kind-strips-the-material
  (are [tool kind] (= kind (tools/need-kind tool))
    "wooden_pickaxe" "pickaxe"
    "iron_sword" "sword"
    "shears" "shears"
    nil "pickaxe"))

(deftest clearing-needs-a-tool-only-when-nothing-carried-breaks-the-block-quickly
  (are [block carried needs?] (= needs? (tools/needs-tool-to-clear? (tu/fake-on-floor {:inventory (mapv #(hash-map :name % :count 1) carried)}) block))
    "snow_block" [] false
    "snow_block" ["iron_shovel"] false
    "oak_leaves" [] false
    "glass" [] false
    "dirt" [] false
    "cobweb" [] true
    "cobweb" ["iron_pickaxe"] true
    "cobweb" ["iron_sword"] false
    "cobweb" ["shears"] false
    "stone" [] true
    "stone" ["wooden_pickaxe"] false
    "iron_ore" [] true
    "iron_ore" ["wooden_pickaxe"] true
    "iron_ore" ["stone_pickaxe"] false
    "obsidian" ["iron_pickaxe"] true
    "obsidian" ["diamond_pickaxe"] false))
