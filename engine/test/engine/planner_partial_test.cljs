(ns engine.planner-partial-test
  "engine/js/path/planner-partial.test.mjs against the ClojureScript planner: the partial end of a plan, the node nearest
  the goal among those reached without a step the body cannot undo, and result :oneWay when a nearer node lies behind one."
  (:require [cljs.test :refer [deftest is are]]
            [engine.planner-fixture :as pf :refer [near cells moves last-cell]]))

(def DROP (:drop pf/MOVE))
(def GAP (:gap pf/MOVE))

(defn run
  "plan over a hand-built world (no stone floor) with the goal flood off."
  ([fill from goal] (run fill from goal {}))
  ([fill from goal options]
   (pf/plan (pf/snapshot {:fill fill}) {:from from :goal goal} (assoc options :goalFlood 0))))

(defn stone
  ([x0 z0 x1 z1 feet] (stone x0 z0 x1 z1 feet 59))
  ([x0 z0 x1 z1 feet to] [x0 to z0 x1 (dec feet) z1 "stone"]))
(defn slab [x0 z0 x1 z1 feet] [x0 feet z0 x1 feet z1 "stone_slab" {:type "bottom"}])
(defn one-way-of [{:keys [oneWay]}] (select-keys oneWay [:move :x :y :z]))

(def island
  [(stone -2 -2 5 6 70)
   [6 60 -2 13 65 6 "water"] (stone 6 -2 13 6 60 55)
   (stone 14 -2 17 6 66)
   (stone 26 -2 33 6 66)])
(def plateau {:x 2 :y 70 :z 2})

(deftest island-reachable-only-by-a-drop-into-water
  (let [r (run island plateau (near 28 66 2))]
    (is (= "partial" (:status r)))
    (is (= [5 70 2] (last-cell r)))
    (is (not-any? #(= DROP %) (moves r)))
    (is (= {:move DROP :x 6 :y 65 :z 2} (one-way-of r)))
    (is (< (get-in r [:oneWay :distance]) 12))))

(deftest no-way-to-a-nearer-cell-by-the-drop-oneway-is-null
  (let [r (run [(stone -2 -2 5 6 70) (stone 26 -2 33 6 66)] plateau (near 28 66 2))]
    (is (= "partial" (:status r)))
    (is (nil? (:oneWay r)))))

(def ledge [(stone -2 -2 10 6 70) (stone 11 -2 12 6 69) (stone 13 -2 20 6 66) (stone 26 -2 33 6 66)])

(deftest goal-behind-a-3-drop-ends-on-the-returnable-ledge
  (let [r (run ledge plateau (near 28 66 2))]
    (is (= "partial" (:status r)))
    (is (= [12 69 2] (last-cell r)))
    (is (= [DROP] (filterv #(= DROP %) (moves r))))
    (is (= {:move DROP :x 13 :y 66 :z 2} (one-way-of r)))))

(def two-ways
  [(stone -2 -2 5 6 70)
   (stone -2 7 5 7 69) (stone -2 8 5 8 68) (stone -2 9 5 12 67)
   (stone 6 -2 20 12 67)
   (stone 26 -2 33 6 67)])

(deftest node-reached-by-a-cheap-3-drop-and-a-dearer-flight-is-the-partial-end-by-the-flight
  (let [r (run two-ways plateau (near 28 67 2))]
    (is (= "partial" (:status r)))
    (is (= [20 67 2] (last-cell r)))
    (is (= 1 (get-in r [:path :cost :maxDrop])))
    (is (nil? (:oneWay r)))))

(defn ledge-below [platform ledge-feet] (into (vec platform) [(stone 11 -2 12 6 ledge-feet) (stone 26 -2 33 6 70)]))
(def flat [(stone -2 -2 10 6 70)])

(deftest a-drop-of-exactly-1-is-undone-by-a-jump-up
  (let [r (run (ledge-below flat 69) plateau (near 28 70 2))]
    (is (= "partial" (:status r)))
    (is (= [12 69 2] (last-cell r)))
    (is (nil? (:oneWay r)))))

(deftest a-drop-of-1-5-off-a-slab-is-not-undone
  (let [r (run (ledge-below (conj flat (slab -2 -2 10 6 70)) 69) plateau (near 28 70 2))]
    (is (= "partial" (:status r)))
    (is (= [10 70 2] (last-cell r)))
    (is (= {:move DROP :x 11 :y 69 :z 2} (one-way-of r)))))

(defn pit [landing] [(stone -2 -2 5 6 70) (stone 8 -2 14 6 landing) (stone 26 -2 33 6 70)])

(deftest a-gap-jump-down-is-one-way-a-level-gap-jump-is-not
  (let [down (run (pit 69) plateau (near 28 70 2))
        level (run (pit 70) plateau (near 28 70 2))]
    (is (= [5 70 2] (last-cell down)))
    (is (= {:move GAP :x 9 :y 69 :z 2} (one-way-of down)))
    (is (= [14 70 2] (last-cell level)))
    (is (nil? (:oneWay level)))))

(deftest complete-plans-are-unchanged-a-goal-past-a-3-drop-is-found
  (let [r (run [(stone -2 -2 10 6 70) (stone 11 -2 20 6 67)] plateau (near 18 67 2))]
    (is (= "found" (:status r)))
    (is (some #(= DROP %) (moves r)))
    (is (nil? (:oneWay r)))))

(deftest no-returnable-node-2-blocks-nearer-is-none-with-the-one-way-step-reported
  (let [r (run [(stone 0 -2 3 6 70) [4 60 -2 13 65 6 "water"] (stone 4 -2 13 6 60 55) (stone 14 -2 17 6 66) (stone 26 -2 33 6 66)]
               plateau (near 28 66 2))]
    (is (= "none" (:status r)))
    (is (nil? (:path r)))
    (is (= {:move DROP :x 4 :y 65 :z 2} (one-way-of r)))))

(defn gap-of [cells] [(stone -2 -2 5 6 70) (stone (+ 6 cells) -2 20 6 70)])

(deftest a-gap-wider-than-any-jump-is-exhausted
  (are [cells status reason end]
       (let [r (run (gap-of cells) plateau (near 12 70 2))]
         (is (= [status reason (last-cell r)] [(:status r) (:reason r) end]))
         (is (nil? (:oneWay r))))
    3 "found" nil [12 70 2]
    4 "partial" "exhausted" [5 70 2]))
