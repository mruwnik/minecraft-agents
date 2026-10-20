(ns engine.crop-lists-test
  "The crop, seed and produce lists the farm jobs share all derive from jobs.lib.crops."
  (:require [cljs.test :refer [deftest is are]]
            [jobs.farm.compost :as compost]
            [jobs.farm.harvest :as harvest]
            [jobs.farm.tend-box :as tend-box]
            [jobs.lib.strays :as strays]
            [jobs.lib.crops :as crops]
            [jobs.lib.reserve :as reserve]))

(deftest every-field-crop-is-known-to-each-job
  (doseq [crop (keys crops/ripe-age)
          :when (crops/seed-of crop)]
    (is (strays/crop-blocks crop) (str crop " is a crop block to tidy")))
  (is (= (set crops/field-seeds) (set (vals crops/seed-of))))
  (is (every? (set harvest/collect-items) crops/field-seeds) "a harvest picks up every seed that sows a field crop")
  (is (= crops/field-seeds tend-box/seed-items)))

(deftest seeds-are-kept-and-composted-alike
  (doseq [seed (concat crops/field-seeds crops/other-seeds)]
    (is (compost/compostable seed) seed)
    (is (compost/held-back seed) seed)
    (is ((:seeds reserve/tags) seed) seed))
  (is (every? (set crops/farm-goods) (concat crops/waste-seeds crops/produce-items))))

(deftest only-seeds-are-seeds
  (are [item expected] (= expected ((:seeds reserve/tags) item))
    "carrot" true "potato" true "melon_seeds" true "torchflower_seeds" true
    "wheat" false "poisonous_potato" false "beetroot" false))
