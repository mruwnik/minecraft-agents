(ns engine.trees-test
  "jobs.lib.trees: the sapling item of each species."
  (:require [cljs.test :refer [deftest is are]]
            [jobs.lib.trees :as trees]))

(deftest sapling-of-names-the-item-each-species-is-planted-with
  (are [species item] (= item (trees/sapling-of species))
    "oak" "oak_sapling"
    "mangrove" "mangrove_propagule"
    "crimson" "crimson_fungus"
    "warped" "warped_fungus"))

(deftest sapling-for-finds-the-carried-item-of-the-species
  (are [species names found] (= found (trees/sapling-for (map (fn [n] {:name n}) names) species))
    "mangrove" ["stick" "mangrove_propagule"] "mangrove_propagule"
    "mangrove" ["oak_sapling"] nil
    "oak" ["mangrove_propagule" "oak_sapling"] "oak_sapling"
    nil ["stick" "mangrove_propagule"] "mangrove_propagule"))

(deftest drop-filter-keeps-the-planting-item-of-the-species
  (is (= ["mangrove_log" "mangrove_propagule" "stick" "apple"] (trees/drop-filter "mangrove")))
  (is (nil? (trees/drop-filter nil))))
