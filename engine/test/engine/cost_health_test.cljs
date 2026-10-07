(ns engine.cost-health-test
  "jobs.lib.cost.health: what an hp costs and how many a walk may spend, and how the walk policy and go-to pass them on."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.test-util :as tu]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.health :as health]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.world :as wworld]))

(deftest an-hp-costs-ten-seconds-more-the-nearer-the-floor
  (is (= 10 health/hp-seconds))
  (are [hp scale] (= scale (health/health-scale hp))
    20 1
    10 2
    5 4
    1 4
    0 4)
  (is (= 10 cost/per-danger) "the fetch price of a point of damage is the same figure"))

(deftest the-budget-is-the-health-over-the-floor-less-a-margin
  (are [body settings budget] (= budget (health/damage-budget body settings))
    {:health 20 :food 20} {} 7
    {:health 16 :food 20} {} 3
    {:health 13 :food 20} {} 0
    {:health 10 :food 20} {} 0
    {:health 20 :food 20} {:max-damage 3} 3
    {:health 20 :food 20} {:min-health 18} 1
    {:health 20 :absorption 4 :food 20} {} 11
    {:health 20 :food 20 :on-fire true} {} 0
    {:health 20 :food 20 :effects ["poison"]} {} 0
    {:health 20 :food 20 :effects ["speed"]} {} 7))

(deftest an-unknown-health-leaves-no-budget
  (are [body] (= 0 (health/damage-budget body {}))
    {:health js/NaN :food 20}
    {:health nil :food 20}
    {:food 20}
    {:health 20 :absorption js/NaN :food 20}))

(deftest the-budget-leaves-the-body-fed-enough-not-to-go-hungry-after-the-drop
  (are [food budget] (= budget (health/damage-budget {:health 20 :food food} {}))
    18 7
    12 5
    8 1
    5 0))

(deftest the-walk-policy-carries-the-budget-weight-and-the-longest-drop-it-survives
  (let [policy (fn [self args] (wworld/body-policy {:primitives (tu/fake {:self self}) :args args}))]
    (is (= [7 10 16] ((juxt :damage-budget :damage-weight :max-drop) (policy {:health 20 :food 20} {}))))
    (is (= [3 12.5 16] ((juxt :damage-budget :damage-weight :max-drop) (policy {:health 16 :food 20} {}))))
    (is (= [1 16] ((juxt :damage-budget :max-drop) (policy {:health 20 :food 20} {:min-health 18}))) "a floor of 18: the drops it refuses are the budget's to refuse")
    (is (= [3 6] ((juxt :damage-budget :max-drop) (policy {:health 20 :food 20} {:max-damage 3}))))))

(deftest with-drops-passes-the-policys-budget-to-the-planner
  (let [o (wplan/with-drops #js {} {:damage-budget 7 :damage-weight 10 :fall-factor 0.5 :max-drop 10})]
    (is (= [7 10 0.5 10] [(.-damageBudget o) (.-damageWeight o) (.-fallFactor o) (.-maxDrop o)]))
    (is (nil? (.-costs o)) "the fall factor is no longer folded into dropFactor"))
  (let [o (wplan/with-drops #js {} {:fall-factor 0.5})]
    (is (nil? (.-damageBudget o)) "no budget in the policy: the planner's default"))
  (is (= 1 (.-maxDrop (wplan/with-drops #js {} {:damage-budget 7 :max-drop 10 :drop-cost false}))) "drop-cost false still takes no drop"))

(deftest the-survivable-budget-leaves-one-hp-and-keeps-a-callers-floor
  (are [body settings budget] (= budget (health/survivable-budget body settings))
    {:health 14 :food 20} {} 13
    {:health 14 :food 8} {} 13
    {:health 20 :absorption 4 :food 20} {} 23
    {:health 14 :food 20} {:max-damage 2} 2
    {:health 14 :food 20} {:min-health 12} 1
    {:health 20 :food 20} {:min-health 18} 1
    {:health 14 :food 20 :on-fire true} {} 0
    {:health 1 :food 20} {} 0
    {:health nil :food 20} {} 0))

(deftest the-walk-policys-hp-price-is-the-callers-seconds-an-hp
  (let [policy (fn [args] (wworld/body-policy {:primitives (tu/fake {:self {:health 20 :food 20}}) :args args}))]
    (is (= 10 (:damage-weight (policy {}))) "default")
    (is (= 25 (:damage-weight (policy {:hp-seconds 25}))))
    (is (= 25 (:damage-weight (wworld/body-policy {:primitives (tu/fake {:self {:health 10 :food 20}}) :args {:hp-seconds 12.5}})))
        "the price still rises with low health")))

(deftest a-danger-max-rate-above-the-default-raises-the-planners-total-cap
  (let [policy (fn [args] (wworld/body-policy {:primitives (tu/fake {:self {:health 20 :food 20}}) :args args}))
        cap (fn [args] (.-dangerCap (wplan/with-drops #js {} (policy args))))]
    (is (= 4 (cap {})) "default")
    (is (= 4 (cap {:danger-max-rate 2})) "a lower rate leaves the total cap")
    (is (= 8 (cap {:danger-max-rate 8})))))

(deftest the-walk-policy-uses-the-survivable-budget-once-the-call-chose-to-go-over
  (let [policy (fn [mem args] (wworld/body-policy {:primitives (tu/fake {:self {:health 14 :food 20}}) :args args :over-budget mem}))]
    (is (= 1 (:damage-budget (policy false {}))))
    (is (= 13 (:damage-budget (policy true {}))))
    (is (= 1 (:damage-budget (policy true {:min-health 12}))) "a caller's floor is never crossed")
    (is (= 2 (:damage-budget (policy true {:max-damage 2}))))))

(deftest body-policy-reads-the-helmet-and-effects-for-the-air
  (let [policy (fn [self & [equipment]] (wworld/body-policy {:primitives (tu/fake {:self self :equipment equipment}) :args {}}))]
    (is (nil? (:air-drain (policy {:health 20 :food 20}))))
    (is (= 0.5 (:air-drain (policy {:health 20 :food 20} {:head {:name "iron_helmet" :enchants [{:name "respiration" :level 1}]}}))))
    (is (= 15 (:air-grace (policy {:health 20 :food 20 :effects [{:name "water_breathing" :duration 300}]}))))))
