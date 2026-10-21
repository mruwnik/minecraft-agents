(ns engine.recipes-test
  "jobs.items.recipes: crafting chains planned from minecraft-data recipes."
  (:require [cljs.test :refer [deftest is]]
            [jobs.items.recipes :as recipes]
            [engine.game :as game]))

(defn plan [have item n & [opts]] (recipes/plan game/default-version have item n (or opts {})))
(defn shape [p] (mapv (juxt :op :item :count) (:steps p)))

(deftest logs-only-make-planks-table-sticks-and-the-pickaxe
  (is (= [[:craft "oak_planks" 4] [:craft "crafting_table" 1]
          [:craft "oak_planks" 4] [:craft "oak_planks" 4] [:craft "stick" 4] [:craft "wooden_pickaxe" 1]]
         (shape (plan {"oak_log" 12} "wooden_pickaxe" 1)))))

(deftest a-table-at-hand-leaves-out-the-table-steps
  (is (= [[:craft "oak_planks" 4] [:craft "oak_planks" 4] [:craft "stick" 4] [:craft "wooden_pickaxe" 1]]
         (shape (plan {"oak_log" 2} "wooden_pickaxe" 1 {:table? true})))))

(deftest sticks-and-planks-carried-need-only-the-table-and-pickaxe
  (is (= [[:craft "oak_planks" 4] [:craft "crafting_table" 1] [:craft "wooden_pickaxe" 1]]
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

(deftest a-plan-names-the-carried-units-its-crafts-take
  (is (= {"cobblestone" 3} (:uses (plan {"cobblestone" 3} "stone_axe" 1 {:gather? true})))))

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

(deftest a-lead-is-made-from-five-string
  (is (= [[:craft "lead" 2]] (shape (plan {"string" 5} "lead" 2 {:table? true})))))

(deftest lacking-names-the-material-a-player-looks-for
  (is (re-find #"needs 3 cobblestone, have 0" (recipes/lacking game/default-version {} "stone_pickaxe" 1))))

(deftest lacking-names-the-table-when-every-ingredient-is-carried
  (is (re-find #"^crafting iron_pickaxe needs a crafting_table, none carried; crafting crafting_table needs 4 \w+_planks, have 0$"
         (recipes/lacking game/default-version {"iron_ingot" 5 "stick" 2} "iron_pickaxe" 1))))

(deftest wool-is-gathered-for-a-bed
  (is (= {"white_wool" 1} (:gather (plan {"white_wool" 2 "oak_planks" 3 "crafting_table" 1} "white_bed" 1 {:gather? true :table? true})))))

(def smelt-opts {:smelts {"iron_ingot" ["raw_iron"]} :raw #{"raw_iron"}})

(deftest a-smelt-product-is-a-smelt-step-from-its-input
  (is (= [[:smelt "iron_ingot" 3]] (shape (plan {"raw_iron" 3} "iron_ingot" 3 smelt-opts))))
  (is (nil? (plan {"raw_iron" 3} "iron_ingot" 3)) "without :smelts no furnace step is planned")
  (is (nil? (plan {} "iron_ingot" 3 smelt-opts)) "no raw iron carried and no :gather?: no plan"))

(deftest an-iron-chestplate-from-nothing-gathers-raw-iron-then-smelts-and-crafts
  (let [p (plan {} "iron_chestplate" 1 (assoc smelt-opts :gather? true))]
    (is (= 8 (get-in p [:gather "raw_iron"])))
    (is (= [:smelt "iron_ingot" 8] (first (filter #(= :smelt (first %)) (shape p)))))
    (is (= [:craft "iron_chestplate" 1] (last (shape p))))))

(deftest a-gathered-chain-prefers-no-furnace
  (is (= {"coal" 1} (:gather (plan {"stick" 1} "torch" 4 {:gather? true :smelts {"charcoal" ["oak_log"]}})))
      "coal is gathered rather than a log smelted to charcoal"))

(deftest a-needed-item-is-never-crafted-down-to-be-crafted-back
  (is (nil? (plan {"iron_ingot" 3} "iron_ingot" 3 {:table? true})) "ingots are not turned into nuggets to make more ingots"))

(deftest nuggets-are-combined-into-ingots
  (is (= [[:craft "iron_ingot" 3]] (shape (plan {"iron_nugget" 27} "iron_ingot" 3 {:table? true}))))
  (is (= [:craft "iron_ingot" 3] (last (shape (plan {"iron_nugget" 27 "oak_log" 2} "iron_ingot" 3))))))

(deftest lacking-asks-for-the-torch-fuel-carried
  (is (= "crafting torch needs 3 stick, have 2" (recipes/lacking game/default-version {"coal" 15 "stick" 2} "torch" 12)) "coal carried: not charcoal")
  (is (= "crafting torch needs 3 stick, have 2" (recipes/lacking game/default-version {"charcoal" 15 "stick" 2} "torch" 12)) "charcoal carried: it is the fuel"))

(deftest planks-of-any-wood_pool_for_a_recipe_that_names_one
  (let [pooled (recipes/pooled game/default-version {"cherry_planks" 7 "oak_planks" 1 "cobblestone" 3})]
    (is (= 8 (get pooled "cherry_planks") (get pooled "oak_planks")))
    (is (= 3 (get pooled "cobblestone")))))
