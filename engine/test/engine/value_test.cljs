(ns engine.value-test
  "make-room's item worth (jobs.lib.cost.value/item-value per item) against its :toss-below default."
  (:require [cljs.test :refer [deftest is are]]
            [jobs.storage.make-room :as mr]))

(def toss-below (get-in mr/args [:toss-below :default]))

(deftest everyday-junk-is-below-the-toss-line
  (are [item] (< (mr/item-worth item) toss-below)
    {:name "dirt" :count 64}
    {:name "cobblestone" :count 64}
    {:name "gravel" :count 10}
    {:name "rotten_flesh" :count 5}
    {:name "wheat_seeds" :count 20}
    {:name "oak_sapling" :count 2}
    {:name "stick" :count 8}
    {:name "mystery_thing" :count 1}))

(deftest ores-metals-tools-and-food-are-kept
  (are [item] (>= (mr/item-worth item) toss-below)
    {:name "coal" :count 30}
    {:name "charcoal" :count 30}
    {:name "raw_copper" :count 12}
    {:name "raw_gold" :count 3}
    {:name "lapis_lazuli" :count 5}
    {:name "raw_iron" :count 3}
    {:name "iron_ingot" :count 3}
    {:name "emerald" :count 3}
    {:name "diamond" :count 1}
    {:name "iron_pickaxe" :count 1}
    {:name "stone_pickaxe" :count 1}
    {:name "wooden_pickaxe" :count 1}
    {:name "bread" :count 2}
    {:name "cooked_beef" :count 4}
    {:name "apple" :count 1}
    {:name "oak_log" :count 20}
    {:name "oak_planks" :count 20}
    {:name "crimson_stem" :count 8}
    {:name "stick" :count 1 :enchants [{:id "sharpness" :lvl 1}]}))

(deftest worth-is-per-item-and-a-missing-count-is-one
  (is (= (mr/item-worth {:name "iron_ingot" :count 8}) (mr/item-worth {:name "iron_ingot"})))
  (is (> (mr/item-worth {:name "diamond"}) (mr/item-worth {:name "iron_ingot"}) (mr/item-worth {:name "dirt"}))))
