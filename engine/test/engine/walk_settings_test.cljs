(ns engine.walk-settings-test
  "The walking damage budget's floor and cap: code default < body setting (walk.world :min-health, :max-damage) < go-to arg."
  (:require [cljs.test :refer [deftest is are]]
            [jobs.movement.go-to.health :as gth]
            [engine.args :as a]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [jobs.lib.walk.world :as wworld]))

(defn with-body [setting f]
  (settings/with-settings (into {} (map (fn [[k v]] [(keyword "jobs.lib.walk.world" (name k)) v])) setting) f))

(defn ctx [args] {:primitives (tu/fake {:self {:health 20 :food 20}}) :args args})

(defn budget
  "damage-budget of a body at health 20, food 20, with the body settings (nil: none) and go-to args."
  [setting args]
  (with-body setting #(wworld/damage-budget (ctx args))))

(deftest body-setting-sits-between-default-and-arg
  (are [setting args expected] (= expected (budget setting args))
    nil {} 7
    {:min-health 16} {} 3
    {:min-health 16} {:min-health 14} 5
    {:min-health 16 :max-damage 2} {} 2
    {:min-health 16 :max-damage 2} {:max-damage 4} 3
    nil {:min-health 18} 1))

(deftest a-false-setting-is-no-setting
  (is (= {} (with-body {:min-health false :max-damage false} #(wworld/walk-settings (ctx {})))))
  (is (= {:min-health 16} (with-body {:min-health 16} #(wworld/walk-settings (ctx {:other 1}))))))

(deftest the-body-settings-refuse-out-of-range-values
  (are [k v ok?] (= ok? (nil? (a/problem (keyword "jobs.lib.walk.world" k) v)))
    "min-health" 0 false
    "min-health" 21 false
    "min-health" "12" false
    "min-health" false true
    "min-health" 12 true
    "max-damage" -1 false
    "max-damage" 4 true
    "gait" :run false
    "gait" :sneak true))

(deftest a-body-max-damage-is-a-given-limit-for-go-to-health
  (let [refused #(with-body % (fn [] (gth/refused? (ctx {}) {:damage-refused true})))]
    (is (true? (refused nil)))
    (is (false? (refused {:max-damage 3})))))
