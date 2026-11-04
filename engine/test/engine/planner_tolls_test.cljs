(ns engine.planner-tolls-test
  "options.tolls: the caller's cells (a Map of cell-key to a factor) cost factor times their own seconds more on a
  planned route (engine.path.planner.nodes), so a route bends round them; no tolls plans as before."
  (:require [cljs.test :refer [deftest is]]
            [engine.path.planner-tuned :as planner]
            [engine.planner-fixture :as pf]))

(def from {:x 0 :y 64 :z 20})
(def goal (pf/near 38 64 20))

(def world (pf/world {}))

(defn tolls [factor cells]
  {:tolls {:cells (js/Map. (clj->js (mapv (fn [[x y z]] [(planner/cell-key x y z) factor]) cells)))}})

;; a wall of tolled cells across the direct line, x 18..22, z 14..26 (feet cells at y 64)
(def wall (for [x (range 18 23) z (range 14 27)] [x 64 z]))

(defn touches-wall? [r] (boolean (some (set wall) (pf/cells r))))

(deftest tolled-cells-bend-the-route
  (let [plain (pf/run world goal {} from)
        bent (pf/run world goal (tolls 20 wall) from)]
    (is (touches-wall? plain) "without tolls the straight way crosses the cells")
    (is (= "found" (:status bent)))
    (is (not (touches-wall? bent)) "with tolls the way goes round")))

(deftest a-toll-the-only-way-still-crosses
  (let [fenced (pf/world {:fill [[20 64 -1 20 66 19 "stone"] [20 64 21 20 66 40 "stone"]]})
        r (pf/run fenced goal (tolls 20 [[20 64 20]]) from)]
    (is (= "found" (:status r)))
    (is (some #{[20 64 20]} (pf/cells r)))))

(deftest no-tolls-plans-as-before
  (let [plain (pf/run world goal {} from)
        none (pf/run world goal (tolls 20 []) from)]
    (is (= (pf/cells plain) (pf/cells none)))
    (is (= (get-in plain [:path :cost :seconds]) (get-in none [:path :cost :seconds])))
    (is (= (:expanded plain) (:expanded none)))))


(deftest an-infinite-toll-is-never-entered-even-as-the-goal
  (let [r (pf/run world (pf/near 38 64 20) (tolls ##Inf [[38 64 20]]) from)]
    (is (not-any? #{[38 64 20]} (pf/cells r)))))

(defn landing-tolls [factor cells]
  {:tolls {:cells (js/Map.)
           :landing (js/Map. (clj->js (mapv (fn [[x y z]] [(planner/cell-key x y z) factor]) cells)))}})

(deftest a-landing-toll-is-paid-by-a-jump-onto-the-cell-not-by-a-level-walk
  (let [level (pf/run world goal (landing-tolls 20 wall) from)
        ridge (pf/world {:fill [[18 64 14 22 64 26 "stone"]]})
        ridge-wall (for [x (range 18 23) z (range 14 27)] [x 65 z])
        plain (pf/run ridge goal {} from)
        bent (pf/run ridge goal (landing-tolls 20 ridge-wall) from)
        on-ridge? (fn [r] (boolean (some (set ridge-wall) (pf/cells r))))]
    (is (touches-wall? level) "level cells: walked over at no toll")
    (is (on-ridge? plain) "no tolls: the way jumps up onto the ridge")
    (is (= "found" (:status bent)))
    (is (not (on-ridge? bent)) "a jump up onto tolled cells goes round the ridge")))
