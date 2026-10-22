(ns engine.gap-physics-test
  "A level gap jump over 2 and 3 cells walked in prismarine-physics by the executor, with a body that sprints (food above
  6) and one that cannot (policy :sprint false): the walking jump falls short, so a body that cannot sprint gets no plan
  over such a gap."
  (:require [cljs.test :refer [deftest is are]]
            [engine.path.executor :as ex]
            [engine.stairs-physics-test :as sp]))

(defn course
  "A one-block-wide path along +x at y 63 (top at 64) with n empty cells after x 9; nothing under the gap."
  [n]
  [[0 63 0 9 63 0 "stone" {}] [(+ 10 n) 63 0 (+ 20 n) 63 0 "stone" {}]])

(defn walk-with
  "sp/walk with policy: the executor's done status."
  [policy n at]
  (with-redefs [ex/policy policy]
    (:status (:done (sp/walk (course n) [2 64 0] [(+ 15 n) 64 0] at)))))

(def hungry (assoc ex/policy :sprint false))

(deftest a-sprinting-body-crosses-a-gap-of-2-or-3
  (are [n] (= :arrived (walk-with ex/policy n nil))
    2 3))

(deftest a-walking-jump-over-3-cells-does-not-arrive
  (is (not-any? #(= :arrived %)
                (map (fn [off] (walk-with hungry 3 [(+ 2.1 off) 0.5])) [0 0.2 0.4 0.6 0.8]))))

(deftest a-body-that-cannot-sprint-has-no-plan-over-a-gap-of-2-or-3
  (are [n] (= :no-plan (walk-with hungry n nil))
    2 3))

(def canopy
  "Leaf platforms at y 94 (feet 95) either side of a 2-cell gap, the far one one block lower."
  [[0 94 0 9 94 4 "oak_leaves" {}] [12 93 0 20 93 4 "oak_leaves" {}]])

(deftest a-canopy-gap-down-one-is-crossed-straight-and-slantwise
  (are [extra goal at] (= :arrived (:status (:done (sp/walk (into canopy extra) [2 95 2] goal at))))
    [] [14 94 2] [2.5 2.5]
    [[0 96 1 20 96 1 "oak_leaves" {}] [0 96 3 20 96 3 "oak_leaves" {}]] [14 94 2] [2.5 2.5]
    [] [14 94 1] [2.5 2.9]
    [] [14 94 3] [2.5 2.1]
    [[10 95 1 11 95 1 "oak_leaves" {}] [10 95 3 11 95 3 "oak_leaves" {}]] [14 94 2] [2.5 2.5]))

(defn ledge
  "The takeoff path of course with n empty cells, then a landing row one cell deep at x 10+n, y ly (z -5..5), and extra."
  [n ly extra]
  (into [[0 63 0 9 63 0 "stone" {}] [(+ 10 n) ly -5 (+ 10 n) ly 5 "stone" {}]] extra))

(deftest a-running-jump-onto-a-one-cell-ledge-is-made-only-where-an-overshoot-is-no-fall-to-death
  (are [n ly beyond status] (= status (:status (:done (sp/walk (ledge n ly beyond) [2 64 0] [(+ 10 n) (inc ly) 4]))))
    3 62 [] :no-plan
    2 63 [] :no-plan
    3 62 [[14 63 -5 14 64 5 "stone" {}]] :arrived
    3 62 [[14 60 -5 14 60 5 "stone" {}]] :arrived))
