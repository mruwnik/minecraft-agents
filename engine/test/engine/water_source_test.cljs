(ns engine.water-source-test
  (:require [cljs.test :refer [deftest is]]
            [jobs.lib.cost.ways :as ways]
            [jobs.lib.walk-pick :as pick]
            [jobs.lib.water-source :as ws]))

(def from {:x 0 :y 64 :z 0})

(defn s [z covered?] (ws/source-s from {:pos {:x 0 :y 64 :z z} :covered? covered?}))

(deftest a-near-covered-source-beats-a-far-open-one
  (is (< (s 3 true) (s 30 false))))

(deftest a-covered-source-costs-the-failed-click-a-side-step-and-a-second-click
  (is (= (+ (ws/source-s from {:pos {:x 0 :y 64 :z 10}}) (ways/place-s)
            (pick/walk-s {:x 0 :y 64 :z 10} {:pos {:x 1 :y 64 :z 10} :range 0}))
         (s 10 true))))
