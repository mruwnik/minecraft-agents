(ns engine.recipes-test
  "jobs.items.recipes: crafting chains planned from minecraft-data recipes."
  (:require [cljs.test :refer [deftest is]]
            [jobs.items.recipes :as recipes]
            [engine.game :as game]))

(defn plan [have item n & [opts]] (recipes/plan game/default-version have item n (or opts {})))
(defn shape [p] (mapv (juxt :op :item :count) (:steps p)))

(deftest logs-only-make-planks-table-sticks-and-the-pickaxe
  (is (= [[:craft "oak_planks" 4] [:craft "crafting_table" 1] [:place "crafting_table" nil]
          [:craft "oak_planks" 4] [:craft "oak_planks" 4] [:craft "stick" 4] [:craft "wooden_pickaxe" 1]]
         (shape (plan {"oak_log" 12} "wooden_pickaxe" 1)))))

(deftest a-table-at-hand-leaves-out-the-table-steps
  (is (= [[:craft "oak_planks" 4] [:craft "oak_planks" 4] [:craft "stick" 4] [:craft "wooden_pickaxe" 1]]
         (shape (plan {"oak_log" 2} "wooden_pickaxe" 1 {:table? true})))))

(deftest sticks-and-planks-carried-need-only-the-table-and-pickaxe
  (is (= [[:craft "oak_planks" 4] [:craft "crafting_table" 1] [:place "crafting_table" nil] [:craft "wooden_pickaxe" 1]]
         (shape (plan {"oak_log" 1 "oak_planks" 3 "stick" 2} "wooden_pickaxe" 1)))))

(deftest another-wood-works-too
  (is (= "birch_planks" (:item (first (:steps (plan {"birch_log" 3} "stick" 1)))))))

(deftest the-table-and-the-tool-share-the-same-logs
  (is (nil? (plan {"oak_log" 2} "wooden_pickaxe" 1)) "a table (4 planks) and 5 planks for the pick need 3 logs")
  (is (some? (plan {"oak_log" 3} "wooden_pickaxe" 1))))

(deftest nothing-usable-is-no-plan
  (is (nil? (plan {} "wooden_pickaxe" 1)))
  (is (nil? (plan {"dirt" 5} "wooden_pickaxe" 1)))
  (is (nil? (plan {"oak_log" 9} "stone_pickaxe" 1)) "cobblestone is mined, not crafted")
  (is (nil? (plan {"oak_log" 9} "dirt" 1))))

(deftest a-stone-pickaxe-from-cobblestone-and-sticks
  (is (= [[:craft "stone_pickaxe" 1]]
         (shape (plan {"cobblestone" 3 "stick" 2} "stone_pickaxe" 1 {:table? true})))))
