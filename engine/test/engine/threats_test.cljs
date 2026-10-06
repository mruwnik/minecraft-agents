(ns engine.threats-test
  "jobs.lib.threats' dangers for the planner: sensed real dangers and remembered :threat spots, costed by what the hostile
  reflex would do (flee: dear, fight: cheap)."
  (:require [cljs.test :refer [deftest is are]]
            [engine.test-util :as tu]
            [jobs.lib.threats :as threats]))

(def unarmed {:health 20 :armour 0 :weapon nil :pos {:x 0 :y 64 :z 0}})
(def armed (assoc unarmed :weapon "diamond_sword"))

(deftest stance-is-what-the-reflex-would-do
  (are [body mob stance] (= stance (threats/stance body mob))
    unarmed "zombie" :flee
    armed "zombie" :fight
    armed "creeper" :flee))

(deftest rate-falls-with-a-weapon-and-rises-with-lost-health
  (let [rate #(/ (js/Math.round (* 100 (threats/danger-rate %1 %2))) 100)]
    (is (= 4 (rate unarmed "zombie")) "3 dps x 4 (flee), capped at 4")
    (is (= 0.3 (rate armed "zombie")) "3 dps x 0.1 (fight)")
    (is (= 0.6 (rate (assoc armed :health 10) "zombie")) "health 10: still a fight, x2")
    (is (= 4 (rate (assoc armed :health 5) "zombie")) "health 5: the fight would leave too little, fled")
    (is (= 0.18 (rate (assoc armed :armour 10) "zombie")) "armour 10 takes 40% off")
    (is (= 4 (rate armed "creeper")) "a creeper is always fled")))

(deftest the-list-has-sensed-mobs-then-remembered-spots-and-the-sensed-place-wins
  (let [sensed [{:key "a" :name "zombie" :pos {:x 5 :y 64 :z 0}}]
        remembered [{:key "a" :mob "zombie" :pos {:x 30 :y 64 :z 0}}
                    {:key "b" :mob "skeleton" :pos {:x 40 :y 64 :z 0}}
                    {:key "c" :mob "zombie" :pos {:x 10 :y 64 :z 0}}]
        ds (threats/danger-list unarmed sensed remembered)]
    (is (= [[5 12] [10 16] [40 16]] (mapv (juxt :x :radius) ds)) "sensed a first; a's old spot left out; c nearer than b")
    (is (= {:close 4 :radius 8} (select-keys (first (threats/danger-list unarmed [{:key "k" :name "creeper" :pos {:x 1 :y 64 :z 0}}] []))
                                             [:close :radius])))))

(deftest at-most-max-dangers
  (is (= threats/max-dangers
         (count (threats/danger-list unarmed [] (for [i (range 20)] {:key i :mob "zombie" :pos {:x i :y 64 :z 0}}))))))

(defn zombie [id x z] {:id id :name "zombie" :kind "hostile" :pos {:x x :y 64 :z z}})

(def wall (into {} (for [x [2 3] y [64 65 66] z (range -4 5)] [(str x "," y "," z) "stone"])))

(deftest known-dangers-lists-only-what-the-body-senses-and-could-be-reached-by
  (is (= [[6 4]] (mapv (juxt :x :rate) (threats/known-dangers (tu/fake-on-floor {:entities [(zombie 1 6 0)]}) [])))
      "a zombie in sight on open ground")
  (is (= [[6 0.3]] (mapv (juxt :x #(/ (js/Math.round (* 100 (:rate %))) 100)) (threats/known-dangers (tu/fake-on-floor {:entities [(zombie 1 6 0)]
                                                                                    :inventory [{:name "diamond_sword" :count 1}]})
                                                                 [])))
      "the same with a diamond sword: a fight, cheap")
  (is (= [] (threats/known-dangers (tu/fake-on-floor {:blocks wall :entities [(zombie 1 6 0)]}) []))
      "behind a wall the body never saw it through: not listed (no x-ray)"))
