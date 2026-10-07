(ns engine.planner-danger-test
  "options.dangers: known hostiles add risk to each move by the time spent near them (engine.path.planner.danger).
  A path detours round a costly danger, skirts a cheap one, and still takes the only way past one (a finite cost)."
  (:require [cljs.test :refer [deftest is testing]]
            [engine.planner-fixture :as pf]))

(def from {:x 0 :y 64 :z 20})
(def goal (pf/near 38 64 20))
(def mob {:x 19.5 :y 64 :z 20.5})

(defn danger [rate] (assoc mob :close 3 :radius 12 :rate rate))

(defn nearest
  "The least distance from a path cell's centre to the mob."
  [r]
  (apply min (map (fn [[x y z]] (js/Math.hypot (- (+ x 0.5) (:x mob)) (- y (:y mob)) (- (+ z 0.5) (:z mob))))
                  (pf/cells r))))

(def open-world (pf/world {}))

(deftest no-dangers-plans-as-before
  (let [none (pf/run open-world goal {} from)
        empty-list (pf/run open-world goal {:dangers []} from)]
    (is (= "found" (:status none)))
    (is (= (pf/cells none) (pf/cells empty-list)))
    (is (= (get-in none [:path :cost]) (get-in empty-list [:path :cost])))
    (is (= (:expanded none) (:expanded empty-list)))))

(deftest an-unarmed-body-detours-wide-round-a-danger
  (let [r (pf/run open-world goal {:dangers [(danger 4)]} from)]
    (is (= "found" (:status r)))
    (is (= [38 64 20] (pf/last-cell r)))
    (is (>= (nearest r) 8) (str "nearest " (nearest r) " path " (pf/cells r)))))

(deftest an-armed-body-passes-nearer-on-a-shorter-path
  (let [unarmed (pf/run open-world goal {:dangers [(danger 4)]} from)
        armed (pf/run open-world goal {:dangers [(danger 0.3)]} from)]
    (is (= "found" (:status armed)))
    (is (< (nearest armed) (nearest unarmed)))
    (is (< (get-in armed [:path :cost :seconds]) (get-in unarmed [:path :cost :seconds])))))

(deftest a-danger-out-of-the-way-changes-nothing
  (let [none (pf/run open-world goal {} from)
        far (pf/run open-world goal {:dangers [{:x 19.5 :y 64 :z 300.5 :close 3 :radius 12 :rate 4}]} from)]
    (is (= (pf/cells none) (pf/cells far)))
    (is (= (:expanded none) (:expanded far)))))

;; a wall across the map at x 20 with a 1-wide gap at z 20: the only way, with the danger standing in it
(def gap-world (pf/world {:fill [[20 64 -2 20 66 19 "stone"] [20 64 21 20 66 40 "stone"]]}))

(deftest the-only-way-past-a-danger-is-still-taken-within-the-node-budget
  (testing "a finite cost: the gap is walked, not refused, and the search ends found within the default max-nodes"
    (let [r (pf/run gap-world goal {:dangers [(danger 4) (danger 4) (danger 4)]} from)]
      (is (= "found" (:status r)))
      (is (= [38 64 20] (pf/last-cell r)))
      (is (some #{[20 64 20]} (pf/cells r)))
      (is (pos? (get-in r [:path :cost :risk])) "the crossing is charged as risk"))))

(defn plan-with [damage-weight rate]
  (pf/run open-world goal {:dangers [(danger rate)] :damageWeight damage-weight} from))

(deftest a-danger-is-priced-at-the-damage-weight-the-hp-price
  (testing "a dearer hp makes the body detour wider round the same danger"
    (is (> (nearest (plan-with 40 0.5)) (nearest (plan-with 2 0.5)))))
  (testing "one formula: hp a second times the weight, so 0.4 hp/s at 10 plans as 2 hp/s at 2"
    (is (= (pf/cells (plan-with 10 0.4)) (pf/cells (plan-with 2 2)))))
  (testing "without a danger the weight changes nothing"
    (is (= (pf/cells (pf/run open-world goal {:damageWeight 40} from)) (pf/cells (pf/run open-world goal {} from))))))
