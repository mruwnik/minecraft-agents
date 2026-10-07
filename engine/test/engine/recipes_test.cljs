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

(deftest gather-plans-name-the-raw-items-the-chain-lacks
  (let [p (plan {} "stone_pickaxe" 1 {:gather? true})]
    (is (= 3 (get-in p [:gather "cobblestone"])))
    (is (= 2 (reduce + (vals (filter (fn [[k _]] (re-find #"_log$" k)) (:gather p))))) "a table and two sticks: 2 logs")
    (is (some #(= "stone_pickaxe" (:item %)) (:steps p))))
  (is (nil? (plan {} "stone_pickaxe" 1)) "without :gather? nothing is assumed")
  (is (nil? (:gather (plan {"oak_log" 3 "cobblestone" 3} "stone_pickaxe" 1 {:gather? true}))) "nothing lacks")
  (is (= {"cobblestone" 2} (:gather (plan {"oak_planks" 4 "stick" 2 "cobblestone" 1 "crafting_table" 1} "stone_pickaxe" 1 {:gather? true :table? true})))))

(deftest stone-tool-materials-come-from-the-recipe-data
  (is (= #{"cobblestone" "cobbled_deepslate" "blackstone"} (recipes/stone-materials game/default-version))))

(deftest a-stone-pickaxe-accepts-cobbled-deepslate
  (is (= [[:craft "stone_pickaxe" 1]]
         (shape (plan {"cobbled_deepslate" 3 "stick" 2} "stone_pickaxe" 1 {:table? true}))))
  (is (= {"cobbled_deepslate" 3}
         (:gather (plan {"stick" 2 "crafting_table" 1} "stone_pickaxe" 1 {:gather? true :table? true :materials #{"cobbled_deepslate"}})))
      "only the seen material is gathered")
  (is (= {"cobblestone" 3}
         (:gather (plan {"stick" 2} "stone_pickaxe" 1 {:gather? true :table? true})))
      "no :materials: the first recipe's"))
