(ns engine.get-tool-test
  "jobs.items.get-tool: which tools will do for a :block, :item or :kind."
  (:require [cljs.test :refer [deftest is]]
            [jobs.items.get-tool :as get-tool]))

(def p #js {:harvestTools (fn [b] (when (= b "stone") #js {"1" true "2" true}))})

(deftest an-unknown-block-is-an-error-not-a-hand-block
  (is (= {:error "unknown block not_a_block"} (get-tool/tools-for p {:block "not_a_block"}))))

(deftest a-known-block-lists-tools-cheapest-first-and-none-for-the-hand
  (is (= [] (get-tool/tools-for #js {:harvestTools (fn [_] nil)} {:block "dirt"})))
  (is (= ["wooden_pickaxe"] (take 1 (get-tool/tools-for #js {:harvestTools (fn [_] #js ["iron_pickaxe" "wooden_pickaxe"])} {:block "stone"})))))

(deftest kinds-and-items
  (is (= "wooden_axe" (first (get-tool/tools-for p {:kind "axe"}))))
  (is (= ["shears"] (get-tool/tools-for p {:kind "shears"})))
  (is (= ["bucket"] (get-tool/tools-for p {:item "bucket"})))
  (is (map? (get-tool/tools-for p {}))))
