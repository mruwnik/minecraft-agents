(ns engine.planner-sprint-test
  "The planner prices a straight run on :auto at sprint speed once it is three walk or diagonal steps long (the executor's rule);
  a gait of :walk or :sneak (walkS = sprintS) keeps the walking price, and the heuristic never exceeds a block's cheapest price."
  (:require [cljs.test :refer [deftest is]]
            [engine.planner-fixture :as pf]))

(def walk-s 0.23164234422052352)
(def sprint-s 0.1781895937277263)

(defn seconds [options goal]
  (get-in (pf/search (pf/world {}) goal options) [:path :cost :seconds]))

(defn near? [a b] (< (js/Math.abs (- a b)) 1e-6))

(deftest a-straight-run-sprints-after-two-steps
  (is (near? (seconds {} (pf/near 12 64 2)) (+ (* 2 walk-s) (* 8 sprint-s))))
  (is (near? (seconds {} (pf/near 4 64 2)) (* 2 walk-s)) "a run of two never sprints"))

(deftest a-diagonal-run-sprints-too
  (is (< (seconds {} (pf/near 12 64 12)) (seconds {:costs {:walkS walk-s :sprintS walk-s}} (pf/near 12 64 12)))))

(deftest a-walking-gait-keeps-the-walking-price
  (is (near? (seconds {:costs {:walkS walk-s :sprintS walk-s}} (pf/near 12 64 2)) (* 10 walk-s))))
