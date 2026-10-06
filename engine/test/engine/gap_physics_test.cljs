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
