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

(def stone-harvest ["wooden_pickaxe" "stone_pickaxe" "copper_pickaxe" "iron_pickaxe" "diamond_pickaxe" "golden_pickaxe"])
(def diamond-harvest ["iron_pickaxe" "diamond_pickaxe" "netherite_pickaxe"])

(defn items [& names] (mapv #(if (string? %) {:name % :count 1} %) names))

(deftest suited-tool-is-the-cheapest-that-harvests
  (are [carried block harvest expected] (= expected (tools/suited-tool carried block harvest))
    (items "iron_pickaxe" "stone_pickaxe") "stone" stone-harvest "stone_pickaxe"
    (items "diamond_pickaxe" "iron_pickaxe") "stone" stone-harvest "iron_pickaxe"
    (items "iron_pickaxe" "stone_pickaxe") "diamond_ore" diamond-harvest "iron_pickaxe"
    (items "stone_pickaxe") "diamond_ore" diamond-harvest nil
    (items "iron_shovel" "wooden_shovel") "dirt" nil "wooden_shovel"
    (items "iron_axe" "stone_axe") "oak_log" nil "stone_axe"))

(deftest suited-tool-uses-the-most-worn-of-a-tier-first
  (is (= {:name "stone_pickaxe" :durability 20}
         (tools/suited-item [{:name "stone_pickaxe" :durability 100}
                             {:name "stone_pickaxe" :durability 20}
                             {:name "iron_pickaxe" :durability 5}]
                            "stone" stone-harvest))))

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
