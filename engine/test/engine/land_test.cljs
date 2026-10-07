(ns engine.land-test
  "jobs.lib.land/land-cell?: a cell a body can stand on dry."
  (:require [cljs.test :refer [deftest is]]
            [jobs.lib.land :as land]))

(defn land?
  "land-cell? at 0,65,0 over a world of {y name}; a missing y is unloaded."
  [column]
  (land/land-cell? (fn [{:keys [y]}] (get column y)) {:x 0 :y 65 :z 0}))

(deftest plain-bank-is-land
  (is (land? {64 "grass_block" 65 "air" 66 "air"})))

(deftest plants-in-the-cell-are-passable
  (doseq [plant ["short_grass" "fern" "poppy" "dandelion" "snow"]]
    (is (land? {64 "grass_block" 65 plant 66 "air"}) (str "feet in " plant)))
  (is (land? {64 "grass_block" 65 "tall_grass" 66 "tall_grass"}) "tall grass, both cells"))

(deftest cell-above-the-plant-is-not-a-stand-cell
  (is (not (land? {64 "short_grass" 65 "air" 66 "air"}))))

(deftest unsafe-or-missing-ground-is-refused
  (doseq [below ["campfire" "soul_campfire" "magma_block" "fire" "water" "lava" "bubble_column" "seagrass"
                 "tall_seagrass" "kelp" "air"]]
    (is (not (land? {64 below 65 "air" 66 "air"})) (str "over " below))))

(deftest solid-feet-head-or-unloaded-is-refused
  (is (not (land? {64 "stone" 65 "stone" 66 "air"})))
  (is (not (land? {64 "stone" 65 "air" 66 "stone"})))
  (is (not (land? {64 "stone" 65 "water" 66 "air"})))
  (is (not (land? {65 "air" 66 "air"})) "ground unloaded")
  (is (not (land? {64 "stone" 66 "air"})) "feet unloaded"))
