(ns engine.value-test
  "jobs.lib.worth/item-worth: the shared item worth used by recover-drops and make-room."
  (:require [cljs.test :refer [deftest is are]]
            [jobs.lib.worth :as value]))

(deftest item-worth-by-tier
  (are [item expected] (= expected (value/item-worth item))
    {:name "mystery_thing" :count 1} 0
    {:name "cobblestone" :count 64} 0
    {:name "dirt" :count 3} 0
    {:name "bread" :count 2} 1
    {:name "oak_log" :count 5} 1
    {:name "iron_ingot" :count 3} 6
    {:name "iron_ingot" :count 8} 16
    {:name "iron_pickaxe" :count 1} 5
    {:name "diamond" :count 1} 25
    {:name "netherite_sword" :count 1} 25
    {:name "stick" :count 1 :enchants [{}]} 25
    {:name "stick" :count 1 :enchants [{:id "sharpness"}]} 25))

(deftest item-worth-treats-a-missing-count-as-one
  (are [item expected] (= expected (value/item-worth item))
    {:name "bread"} 1
    {:name "iron_ingot"} 2
    {:name "diamond"} 25
    {:name "cobblestone"} 0
    {:name "stick" :enchants [{}]} 25))
