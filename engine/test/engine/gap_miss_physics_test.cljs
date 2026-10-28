(ns engine.gap-miss-physics-test
  "The source of the planner's gap-jump miss prior (planner.base/GAP-MISS-JUMPS, the default of jobs.lib.cost's gap-miss-jumps): level gaps of 1-3 cells jumped in
  prismarine-physics by the executor from every start fraction of the run-up, counted by width and run-up cells."
  (:require [cljs.test :refer [deftest is are]]
            [engine.gap-physics-test :as gp]
            [engine.path.planner.base :as base]
            [engine.stairs-physics-test :as sp]))

(def run-ups "run-up cells before the takeoff, the takeoff cell counted: 1 starts in it" [1 2 3 4])
(def fractions "where in its start cell the body stands, along the run" (map #(+ 0.05 (* 0.05 %)) (range 19)))
(def lanes "where across the one-block path the body stands" [0.5 0.65])

(defn outcomes
  "The executor's done status of every jump over width cells from run-up cells r: a seq of [r status]."
  [width]
  (for [r run-ups, f fractions, lane lanes]
    (let [x (- 10 r)]
      [r (:status (:done (sp/walk (gp/course width) [x 64 0] [(+ 15 width) 64 0] [(+ x f) lane])))])))

(defn counts
  "[[fell n] for a run-up of 1 cell, [fell n] for 2 or more] of width's jumps."
  [width]
  (let [os (outcomes width)
        tally (fn [os] [(count (remove (comp #{:arrived} second) os)) (count os)])]
    (is (not-any? (comp #{:no-plan} second) os))
    [(tally (filter (comp #{1} first) os)) (tally (remove (comp #{1} first) os))]))

(deftest gap-miss-prior-is-the-physics-count
  (is (= (mapv #(mapv vec %) (rest (js->clj base/GAP-MISS-JUMPS)))
         (mapv counts [1 2 3]))))

