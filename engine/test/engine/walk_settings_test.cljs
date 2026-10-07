(ns engine.walk-settings-test
  "The walking damage budget's floor and cap: code default < body setting (memory :walk-settings) < go-to arg."
  (:require [cljs.test :refer [deftest is are]]
            [jobs.movement.go-to.health :as gth]
            [engine.test-util :as tu]
            [jobs.lib.walk.world :as wworld]))

(defn setting-ctx
  "A ctx with the body setting (nil: none) in memory, written at 0, under policy (nil: the default), seen at now; events go to emitted."
  [setting args policy now emitted]
  {:primitives (tu/fake {:self {:health 20 :food 20}}) :args args
   :emit (fn [kind level fields] (swap! emitted conj [kind level fields]))
   :view (constantly {:now now :data {:entries {:walk-settings (when setting [{:t 0 :data setting}])}
                                      :policies (when policy {:walk-settings policy})}})})

(defn budget
  "damage-budget of a body at health 20, food 20, with the body setting (nil: none) and go-to args."
  [setting args]
  (wworld/damage-budget (setting-ctx setting args nil 0 (atom []))))

(deftest body-setting-sits-between-default-and-arg
  (are [setting args expected] (= expected (budget setting args))
    nil {} 7
    {:min-health 16} {} 3
    {:min-health 16} {:min-health 14} 5
    {:min-health 16 :max-damage 2} {} 2
    {:min-health 16 :max-damage 2} {:max-damage 4} 3
    nil {:min-health 18} 1))

(deftest body-setting-is-a-floor-never-crossed
  (is (= {:min-health 16} (wworld/walk-settings {:args {} :view (constantly {:now 0 :data {:entries {:walk-settings [{:t 0 :data {:min-health 16 :other 1}}]}}})}))))

(deftest a-forever-setting-outlives-the-default-hour
  (let [c #(setting-ctx {:min-health 16} {} % 1e12 (atom []))]
    (is (= {:min-health 16} (wworld/walk-settings (c {:cap 1 :ttl :forever}))))
    (is (= {} (wworld/walk-settings (c {:cap 1 :ttl 1000}))) "an expired entry is no setting")))

(deftest a-bad-body-setting-is-ignored-and-warned-once-per-call
  (are [setting kept bad] (let [emitted (atom [])
                                c (setting-ctx setting {} nil 0 emitted)]
                            (wworld/warn-bad-settings! c)
                            (and (= kept (wworld/walk-settings c))
                                 (= bad (mapv (comp :keys #(nth % 2)) @emitted))
                                 (every? #(= [:walk-settings.bad :info] (subvec % 0 2)) @emitted)))
    {:min-health 0} {} [[:min-health]]
    {:min-health 21 :max-damage 3} {:max-damage 3} [[:min-health]]
    {:min-health "12"} {} [[:min-health]]
    {:max-damage -1} {} [[:max-damage]]
    {:min-health 12 :max-damage 4} {:min-health 12 :max-damage 4} []))

(deftest a-body-max-damage-is-a-given-limit-for-go-to-health
  (let [refused #(gth/refused? (setting-ctx % {} nil 0 (atom [])) {:damage-refused true})]
    (is (true? (refused nil)))
    (is (false? (refused {:max-damage 3})))))
