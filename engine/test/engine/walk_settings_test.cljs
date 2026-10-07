(ns engine.walk-settings-test
  "The walking damage budget's floor and cap: code default < body setting (memory :walk-settings) < go-to arg."
  (:require [cljs.test :refer [deftest is are]]
            [engine.test-util :as tu]
            [jobs.lib.walk.world :as wworld]))

(defn budget
  "damage-budget of a body at health 20, food 20, with the body setting (nil: none) and go-to args."
  [setting args]
  (let [p (tu/fake {:self {:health 20 :food 20}})
        c {:primitives p :args args
           :view (constantly {:now 0 :data {:entries {:walk-settings (when setting [{:at 0 :data setting}])}}})}]
    (wworld/damage-budget c)))

(deftest body-setting-sits-between-default-and-arg
  (are [setting args expected] (= expected (budget setting args))
    nil {} 7
    {:min-health 16} {} 3
    {:min-health 16} {:min-health 14} 5
    {:min-health 16 :max-damage 2} {} 2
    {:min-health 16 :max-damage 2} {:max-damage 4} 3
    nil {:min-health 18} 1))

(deftest body-setting-is-a-floor-never-crossed
  (is (= {:min-health 16} (wworld/walk-settings {:args {} :view (constantly {:now 0 :data {:entries {:walk-settings [{:at 0 :data {:min-health 16 :other 1}}]}}})}))))
