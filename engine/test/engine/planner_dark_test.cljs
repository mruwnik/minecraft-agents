(ns engine.planner-dark-test
  "options.dark: a cell its test calls dark costs more seconds on a planned route (engine.path.planner.dark), so a route
  detours into the light; a day search where nothing is dark plans as before."
  (:require [cljs.test :refer [deftest is testing]]
            [engine.planner-fixture :as pf]))

(def from {:x 0 :y 64 :z 20})
(def goal (pf/near 38 64 20))

;; the band x 4..36 is dark except its cells at z 6 or less
(defn dark-band [x _y z] (if (and (<= 4 x 36) (> z 6)) 1 0))

(defn dark [at night?] {:dark {:at at :factor 1 :night night?}})

(def world-a
  (pf/world {:fill [[20 64 -2 20 66 5 "stone"] [20 64 7 20 66 19 "stone"] [20 64 21 20 66 40 "stone"]]}))
(def world-far
  (pf/world {:fill [[20 64 -1 20 66 19 "stone"] [20 64 21 20 66 40 "stone"]]}))

(defn crosses? [r z] (boolean (some #{[20 64 z]} (pf/cells r))))

;; night? scales the heuristic by 1 + factor (a route that is dark all the way): exact here without it, so these two
;; tests run by day with a test that still calls cells dark (a seen cave)
(deftest a-dark-direct-way-loses-to-a-lit-detour
  (let [plain (pf/run world-a goal {} from)
        night (pf/run world-a goal (dark dark-band false) from)]
    (is (crosses? plain 20) "without options.dark the shorter dark gap is taken")
    (is (= "found" (:status night)))
    (is (crosses? night 6) "dark costs twice a lit block: the lit gap wins")
    (is (not (crosses? night 20)))))

(deftest a-lit-way-far-longer-still-loses
  (let [short-band (fn [x _y z] (if (and (<= 14 x 24) (> z 6)) 1 0))
        r (pf/run world-far goal (dark short-band false) from)]
    (is (= "found" (:status r)))
    (is (crosses? r 20) "no ban: a lit way over twice as long is not taken")))

(deftest dark-seconds-cost-the-dark-stretch-only
  (let [lit (pf/run world-a goal (dark (fn [_ _ _] 0) true) from)
        dim (pf/run world-a goal (dark (fn [_ _ _] 1) true) from)]
    (is (zero? (get-in lit [:path :cost :darkSeconds])))
    (is (pos? (get-in dim [:path :cost :darkSeconds])))
    (is (< (js/Math.abs (- (get-in dim [:path :cost :darkSeconds]) (get-in dim [:path :cost :seconds]))) 1e-6)
        "factor 1: every dark second costs one more")))

(deftest an-all-dark-world-plans-the-same-route
  (let [plain (pf/run world-a goal {} from)
        dim (pf/run world-a goal (dark (fn [_ _ _] 1) true) from)]
    (is (= (pf/cells plain) (pf/cells dim)))
    (is (= (get-in plain [:path :cost :seconds]) (get-in dim [:path :cost :seconds])))))

(deftest by-day-an-all-lit-world-plans-as-before
  (doseq [w [world-a world-far (pf/world {})]]
    (let [plain (pf/run w goal {} from)
          lit (pf/run w goal (dark (fn [_ _ _] 0) false) from)]
      (is (= (pf/cells plain) (pf/cells lit)))
      (is (= (get-in plain [:path :cost :seconds]) (get-in lit [:path :cost :seconds])))
      (is (= (:expanded plain) (:expanded lit))))))

(deftest a-goal-set-picks-the-cheaper-by-darkness-goal
  (testing "the nearer goal lies in the dark, the farther in the light"
    (let [dark-zone (fn [x _ _] (if (>= x 1) 1 0))
          goals [{:kind "near" :x 12 :y 64 :z 2 :range 0} {:kind "near" :x -1 :y 64 :z 14 :range 0}]
          open (pf/world {})
          r (pf/plan open {:from {:x 0 :y 64 :z 2} :goals goals} (dark dark-zone true))]
      (is (= "found" (:status r)))
      (is (= 1 (:goal r))))))
