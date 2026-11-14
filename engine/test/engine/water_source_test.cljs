(ns engine.water-source-test
  (:require [cljs.test :refer [deftest is]]
            [jobs.lib.water-source :as ws]))

(def from {:x 0 :y 64 :z 0})

(defn s [z covered?] (ws/source-s from {:pos {:x 0 :y 64 :z z} :covered? covered?}))

(deftest a-near-covered-source-beats-a-far-open-one
  (is (< (s 3 true) (s 30 false))))

(deftest a-covered-source-costs-more-than-an-open-one-as-far
  (is (> (s 10 true) (s 10 false))))
