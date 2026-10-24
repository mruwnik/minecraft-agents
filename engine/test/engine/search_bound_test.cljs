(ns engine.search-bound-test
  "go-to's search bound: a kept dig-route search ends once standing still for it has cost the whole way without it."
  (:require [cljs.test :refer [deftest is]]
            [engine.ready-test :as ready-test]
            [jobs.lib.cost :as cost]
            [jobs.lib.escape :as escape]
            [jobs.lib.walk-risk :as risk]
            [jobs.movement.go-to.escalation :as esc]
            [jobs.movement.go-to.preview :as preview]
            [jobs.movement.go-to.search-bound :as sb]))

(defn ctx-at [now-ms] (assoc (ready-test/ctx-of {:self {:pos {:x 0.5 :y 64 :z 0.5} :health 14}}) :view (fn [] {:now now-ms :data {}})))

(defn close? [a b] (< (js/Math.abs (- a b)) 1e-9))

(deftest the-fallback-is-priced-as-its-whole-way-to-the-goal-and-its-risk
  (let [c (ctx-at 0)]
    (with-redefs [escape/choose (fn ([_ _ _ _] nil) ([_ _ _ _ _] {:step :approach :pos [3 64 0]}))
                  risk/estimate (fn [& _] {:hp 4 :lethal-hp 1})]
      (is (close? (+ (:cost (cost/walk-cost {:distance 3})) (preview/rest-s 17) (cost/risk-s 14 3 1))
                  (sb/fallback-s c [0 64 0] [20 64 0] (constantly true) {}))
          "the walk to the spot, the 17 blocks on from it as a quote prices them, and the walk's hurt and death"))))

(deftest standing-is-priced-by-the-one-risk-formula
  (let [c (ctx-at 2000)]
    (with-redefs [risk/stay-estimate (fn [& _] {:hp 5 :lethal-hp 3 :hostiles 1})]
      (is (close? (+ 2 (cost/risk-s 14 2 3)) (sb/stood-s c 0))
          "two seconds stood and the stay's hurt and death at cost/risk-s"))))

(deftest with-no-way-but-a-dig-route-a-search-is-worth-what-the-goal-is
  (with-redefs [escape/choose (fn ([_ _ _ _] nil) ([_ _ _ _ _] {:step :none}))]
    (is (close? (cost/tour-worth 20) (sb/fallback-s (ctx-at 0) [0 64 0] [20 64 0] (constantly true) {}))
        "no :worth said: a tour's worth of the goal, never unbounded")))

(deftest dig-route-replans-are-spent-once-they-have-taken-as-long-as-the-way-without-one-costs
  (let [m {:replan-budget {:since 1000 :fallback-s 10}}]
    (is (not (esc/replans-spent? (ctx-at 10999) m)))
    (is (esc/replans-spent? (ctx-at 11000) m))
    (is (not (esc/replans-spent? (ctx-at 99999) {})) "none begun")))
