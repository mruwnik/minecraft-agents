(ns engine.threats-test
  "jobs.lib.threats' dangers for the planner (jobs.lib.cost/danger-list): sensed real dangers and remembered :threat spots, costed by what the hostile
  reflex would do (flee: dear, fight: cheap)."
  (:require [cljs.test :refer [deftest is are]]
            [engine.test-util :as tu]
            [jobs.lib.cost :as cost]
            [jobs.lib.threats :as threats]))

(def unarmed {:health 20 :equipment nil :weapon nil :pos {:x 0 :y 64 :z 0}})
(def armed (assoc unarmed :weapon "diamond_sword"))

(deftest stance-is-what-the-reflex-would-do
  (are [body mob stance] (= stance (cost/stance body mob))
    unarmed "zombie" :flee
    armed "zombie" :fight
    armed "creeper" :flee))

(deftest rate-falls-with-a-weapon-and-armour-and-is-the-hp-a-second-not-the-price
  (let [rate #(/ (js/Math.round (* 100 (cost/danger-rate %1 %2))) 100)]
    (is (= 4 (rate unarmed "zombie")) "3 dps x 4 (flee), capped at 4")
    (is (= 0.3 (rate armed "zombie")) "3 dps x 0.1 (fight)")
    (is (= 0.3 (rate (assoc armed :health 10) "zombie")) "health 10: still a fight; the dearer hp is the planner's damage weight")
    (is (= 4 (rate (assoc armed :health 5) "zombie")) "health 5: the fight would leave too little, fled")
    (is (= 0.2 (rate (assoc armed :equipment {:head {:name "iron_helmet"} :torso {:name "iron_chestplate"}
                                              :feet {:name "iron_boots"}}) "zombie"))
        "10 iron points: vanilla's 8.5 effective points against a 3 hp hit take 34% off")
    (is (= 4 (rate armed "creeper")) "a creeper is always fled")))

(deftest the-list-has-sensed-mobs-then-remembered-spots-and-the-sensed-place-wins
  (let [sensed [{:key "a" :name "zombie" :pos {:x 5 :y 64 :z 0}}]
        remembered [{:key "a" :mob "zombie" :pos {:x 30 :y 64 :z 0}}
                    {:key "b" :mob "skeleton" :pos {:x 40 :y 64 :z 0}}
                    {:key "c" :mob "zombie" :pos {:x 10 :y 64 :z 0}}]
        ds (cost/danger-list unarmed sensed remembered)]
    (is (= [[5 12] [10 16] [40 16]] (mapv (juxt :x :radius) ds)) "sensed a first; a's old spot left out; c nearer than b")
    (is (= {:close 4 :radius 8} (select-keys (first (cost/danger-list unarmed [{:key "k" :name "creeper" :pos {:x 1 :y 64 :z 0}}] []))
                                             [:close :radius])))))

(deftest at-most-max-dangers
  (is (= cost/max-dangers
         (count (cost/danger-list unarmed [] (for [i (range 20)] {:key i :mob "zombie" :pos {:x i :y 64 :z 0}}))))))

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

(deftest danger-terms-are-options-and-the-defaults-keep-the-price
  (let [rate #(cost/danger-rate unarmed "zombie" %)
        sensed [{:key "a" :name "zombie" :pos {:x 5 :y 64 :z 0}}]
        shape #(select-keys (first (cost/danger-list unarmed sensed [] %)) [:close :radius :rate])]
    (is (= (rate nil) (rate {})) "no options: today's rate")
    (is (= 4 (rate {})) "default 3 dps x 4, capped at 4")
    (is (= 1.5 (rate {:stances {:flee 0.5}})) "the flee factor scales the rate")
    (is (< (js/Math.abs (- 0.6 (cost/danger-rate armed "zombie" {:stances {:fight 0.2}}))) 1e-9) "the fight factor")
    (is (= 2 (rate {:max-rate 2})) "max-rate caps one danger")
    (is (= 8 (rate {:max-rate 8 :stances {:flee 4}})) "a higher cap lets more through: 12 capped at 8")
    (is (= {:close 3 :radius 12 :rate 4} (shape nil)) "default shape")
    (is (= {:close 5 :radius 20 :rate 4} (shape {:shape {:sensed {:close 5 :radius 20}}})) "the shape is an option")
    (is (= {:close 3 :radius 7 :rate 4} (shape {:shape {:sensed {:radius 7}}})) "a partial shape keeps the other term")))

(deftest go-to-args-name-the-danger-options
  (is (= {:stances {:flee 2 :fight 0.5} :max-rate 3 :shape {:sensed {:radius 9}}}
         (cost/danger-opts {:flee-factor 2 :fight-factor 0.5 :danger-max-rate 3 :danger-shape {:sensed {:radius 9}}})))
  (is (= {} (cost/danger-opts {})) "absent args: defaults apply downstream"))
