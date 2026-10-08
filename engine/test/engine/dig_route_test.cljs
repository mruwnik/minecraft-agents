(ns engine.dig-route-test
  (:require [cljs.test :refer [deftest is testing]]
            [jobs.lib.dig-route :as dr]))

(defn world
  "block-at over a stone mass below y 10 with the given cells overridden."
  [overrides]
  (fn [[x y z :as c]] (or (overrides c) (if (< y 10) "stone" "air"))))

(deftest a-pit-beside-the-body-needs-no-dig-when-open
  (is (= {:dig []} (dr/route (world {[1 9 0] "air" [1 8 0] "air"}) [0 10 0] [1 8 0] {}))))

(deftest a-sealed-pocket-is-dug-down-in-walking-order
  (let [r (dr/route (world {[3 8 0] "air"}) [0 10 0] [3 8 0] {})]
    (is (seq (:dig r)))
    (is (not (some #{[3 8 0]} (:dig r))) "the drop's own cell is open already")
    (is (<= (count (:dig r)) 12))))

(deftest lava-on-the-way-is-not-dug-through
  (let [lava (into {} (for [x (range -5 10) z (range -5 5) y [8 9]] [[x y z] "lava"]))]
    (is (= {:problem :no-route} (dr/route (world (assoc lava [3 8 0] "air")) [0 10 0] [3 8 0] {})))))

(deftest too-many-digs-is-no-route
  (let [deep (fn [c] (or ({[30 8 0] "air" [0 20 0] "air" [0 21 0] "air"} c) (if (< (c 1) 20) "stone" "air")))]
    (is (= {:problem :no-route} (dr/route deep [0 20 0] [30 8 0] {:max-digs 5})))))
