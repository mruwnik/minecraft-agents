(ns engine.step-off-test
  "jobs.lib.step-off: the cells a body may step to off a cell, against the fake world."
  (:require [cljs.test :refer [deftest is are]]
            [engine.test-util :as tu]
            [jobs.lib.step-off :as step-off]))

(def cell {:x 2 :y 64 :z 0})

(defn world [blocks] (tu/fake-on-floor {:blocks (merge {"2,63,0" "stone"} blocks)}))

(defn goals [blocks & [opts]]
  (set (map (juxt :x :y :z) (step-off/candidates (world blocks) cell (or opts {})))))

(deftest the-nearest-cells-come-first-and-never-the-column-itself
  (let [cs (step-off/candidates (world {}) cell {})]
    (is (= 1 (js/Math.abs (- 2 (:x (first cs)))) (inc (js/Math.abs (:z (first cs))))) "a neighbour first")
    (is (not-any? #(and (= 2 (:x %)) (= 0 (:z %))) cs))))

(deftest a-wall-or-lava-two-blocks-east-is-no-goal
  (are [blocks] (not (contains? (goals blocks) [4 64 0]))
    {"4,64,0" "stone" "4,65,0" "stone"}
    {"4,63,0" "lava"}
    {"4,64,0" "fire" "4,63,0" "stone"}))

(deftest a-drop-is-no-goal
  (is (not (contains? (goals {"4,63,0" "air"}) [4 64 0]))))

(deftest avoided-and-refused-cells-are-left-out
  (is (not (contains? (goals {} {:avoid #{[3 64 0]}}) [3 64 0])))
  (is (not-any? #(= 3 (first %)) (goals {} {:ok? #(not= 3 (:x %))}))))

(deftest nothing-to-stand-on-gives-no-candidate
  (is (empty? (step-off/candidates (tu/fake {:blocks {"2,63,0" "stone"}}) cell {}))))

(deftest a-campfire-is-no-goal-below-in-or-above
  (are [blocks] (not (contains? (goals blocks) [3 64 0]))
    {"3,63,0" "campfire"}
    {"3,63,0" "soul_campfire"}
    {"3,64,0" "campfire" "3,63,0" "stone"}
    {"3,65,0" "campfire" "3,63,0" "stone"}))

(def foreign {:name "farm" :min [3 60 -1] :max [3 70 1] :owner "Miles"})

(defn zone-goals [in]
  (goals {"3,63,0" "stone" "3,63,1" "stone" "3,63,-1" "stone"} {:ok? (step-off/zone-ok in)}))

(deftest a-cell-in-a-foreign-zone-is-skipped-unless-ignore-zones
  (is (not-any? #(and (= 3 (first %)) (<= -1 (nth % 2) 1)) (zone-goals {:zones [foreign] :self "Fake"})))
  (is (seq (zone-goals {:zones [foreign] :self "Fake"})))
  (is (contains? (zone-goals {:zones [foreign] :self "Fake" :ignore-zones? true}) [3 64 0]))
  (is (contains? (zone-goals {:zones [(assoc foreign :owner "Fake")] :self "Fake"}) [3 64 0]))
  (is (contains? (zone-goals {:zones nil :self "Fake"}) [3 64 0]) "no zone list: standing is no act"))
