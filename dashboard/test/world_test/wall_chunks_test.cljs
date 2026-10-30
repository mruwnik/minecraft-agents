(ns world-test.wall-chunks-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.wall-chunks :as w]))

(defn hut
  "Stone below y 0; a room x 0..2 z 0 roofed (stone) at y 2 over x -1..3 z -1..1, walled at y 0 and 1 on that ring but for a door gap at [1 z -1]."
  [x y z]
  (let [ring? (and (<= -1 x 3) (<= -1 z 1))
        inside? (and (<= 0 x 2) (= z 0))]
    (cond (neg? y) "stone"
          (= y 2) (if ring? "stone_bricks" "air")
          (and ring? (not inside?) (not (and (= x 1) (= z -1)))) "stone"
          :else "air")))

(deftest a-roofed-space-counts-its-closed-and-open-sides
  (is (= [{:closed 7 :open 1}] (w/spaces hut -1 3 0 0 -1 1))
      "the room is one space; of its 8 side cells the door gap is passable (open), the rest blocked"))

(deftest the-prior-is-fitted-over-spaces
  (let [r (w/report [{:closed 9 :open 1} {:closed 5 :open 5}] 4)]
    (is (= 14 (:closed r)))
    (is (= 2 (:spaces r)))
    (is (> 1e-3 (js/Math.abs (- 0.7 (/ (get-in r [:setting :k]) (get-in r [:setting :n])))))  "the fit keeps the mean"))
  (is (= {:k 3 :n 4} (:setting (w/report [{:closed 3 :open 1}] 1))) "one space: pooled counts"))
