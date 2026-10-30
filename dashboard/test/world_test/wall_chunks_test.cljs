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
  (is (= [{:closed 7 :open 1 :roof "stone_bricks" :sky true}] (w/spaces hut -1 3 0 0 -1 1))
      "the room is one space; of its 8 side cells the door gap is passable (open), the rest blocked")
  (is (= [false] (map :sky (w/spaces #(if (= %2 10) "stone" (hut %1 %2 %3)) -1 3 0 0 -1 1))) "rock above the roof: no sky"))

(defn rows [closed open n] (repeat n {:closed closed :open open :roof "oak_planks" :sky true}))

(deftest the-prior-weighs-a-space-by-its-sides
  (let [r (w/report (concat (rows 1 3 9) (rows 36 4 1)) 4)
        {:keys [k n]} (:setting r)]
    (is (< 0.35 (/ k n) 0.45) "between the share of the average space (0.315) and the pooled sides (0.59): the big space counts by its sides")
    (is (= 10 (:spaces (:pooled r)))))
  (is (= {:k 3 :n 4 :sealed 0} (:setting (w/report (rows 3 1 1) 1))) "one space: pooled counts"))

(deftest sealed-spaces-beyond-the-beta-binomial-are-a-share-of-their-own
  (let [r (w/report (concat (rows 8 0 12) (rows 4 4 6) (rows 5 3 6) (rows 3 5 6) (rows 6 2 6)) 4)]
    (is (< 0.1 (get-in r [:setting :sealed]) 0.5) "12 of 36 spaces fully closed where the same sides alone would seal about 1 in 25")
    (is (= [36 12] (take 2 (get-in r [:by-size 8]))) "by size: spaces and sealed")))

(deftest natural-structures-and-spaces-under-rock-are-left-out-of-the-fit
  (let [r (w/report (concat (rows 8 0 3) (rows 4 4 3) (map #(assoc % :roof "tuff_bricks") (rows 3 5 5)) (map #(assoc % :sky false) (rows 2 6 5))) 4)]
    (is (= 6 (:spaces (:pooled r))) "the trial chamber's tuff and the rock-covered rooms are no population")
    (is (= [5 0 15 25 5] (get-in r [:by-roof "tuff_bricks"])) "yet the roof tally keeps them")))
