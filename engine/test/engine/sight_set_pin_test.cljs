(ns engine.sight-set-pin-test
  "The set of cells one full sight pass records, pinned on a few worlds: a speed-up of the ray loop must not change it."
  (:require [cljs.test :refer [deftest is]]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.test-util :as tu]))

(defn seen-set
  "[count checksum] of the (pos, name) pairs of every seen cell within 24 blocks after a forced pass."
  [blocks light yaw]
  (let [p (tu/blind (tu/fake {:blocks blocks}))
        per (perception/create (fake-raw/create p) {:rand (constantly 0.5)})]
    (swap! (fake/state p) merge {:yaw yaw} light)
    (perception/pass! per)
    (reduce (fn [[n sum] pos]
              (let [b (perception/seen-block per pos)]
                (if (or (nil? b) (:unknown b))
                  [n sum]
                  [(inc n) (mod (+ (* 31 sum) (hash [pos (:name b)])) 1000000007)])))
            [0 0]
            (for [x (range -24 25) y (range 56 80) z (range -24 25)] [x y z]))))

(def pillars
  (merge (tu/box -24 56 -24 24 63 24 "stone")
         (into {} (for [x (range -20 21 5) z (range -20 21 7) y (range 64 68)] [(str x "," y "," z) "stone"]))
         (tu/box -6 64 8 6 70 9 "stone")
         {"0,65,8" "air" "0,64,8" "air" "0,65,9" "air" "0,64,9" "air" "0,66,14" "gold_block" "3,65,12" "glass"}))

(def cave
  (merge (apply dissoc (tu/box -10 60 -10 10 72 10 "stone") (keys (tu/box -4 64 -4 4 67 4 "stone")))
         {"2,65,3" "gold_block"}))

(deftest pillared-field-lit
  (is (= [1858 200791081] (seen-set pillars {} 0))))

(deftest pillared-field-dark-facing-east
  (is (= [101 679848052] (seen-set pillars {:light-default [0 0] :light {[3 65 3] [0 14]}} 270))))

(deftest sealed-room-dark
  (is (= [96 711454558] (seen-set cave {:light-default [0 0]} 45))))
