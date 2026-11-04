(ns engine.search-bound-test
  "go-to's search bound: a kept dig-route search ends once standing still for it has cost the whole way without it."
  (:require [cljs.test :refer [deftest is]]
            [engine.ready-test :as ready-test]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.planner :as cost-planner]
            [jobs.lib.escape :as escape]
            [jobs.lib.reach :as reach]
            [jobs.lib.escape.way :as escape-way]
            [jobs.movement.go-to.escalate-choose :as choose]
            [jobs.lib.walk-risk :as risk]
            [jobs.movement.go-to.escalation :as esc]
                        [jobs.movement.go-to.search-bound :as sb]))

(defn ctx-at [now-ms] (assoc (ready-test/ctx-of {:self {:pos {:x 0.5 :y 64 :z 0.5} :health 14}}) :view (fn [] {:now now-ms :data {}})))

(defn close? [a b] (< (js/Math.abs (- a b)) 1e-9))

(deftest the-fallback-is-priced-as-its-whole-way-to-the-goal-and-its-risk
  (let [c (ctx-at 0)]
    (with-redefs [escape-way/choose (fn ([_ _ _ _] nil) ([_ _ _ _ _] {:step :approach :pos [3 64 0]}))
                  risk/estimate (fn [& _] {:hp 4 :lethal-hp 1})]
      (is (close? (+ (:cost (cost/walk-cost {:distance 3})) (cost-planner/rest-s 17) (cost/risk-s 14 3 1))
                  (sb/fallback-s c [0 64 0] [20 64 0] (constantly true) {}))
          "the walk to the spot, the 17 blocks on from it as a quote prices them, and the walk's hurt and death"))))

(deftest standing-is-priced-by-the-one-risk-formula
  (let [c (ctx-at 2000)]
    (with-redefs [risk/stay-estimate (fn [& _] {:hp 5 :lethal-hp 3 :hostiles 1})]
      (is (close? (+ 2 (cost/risk-s 14 2 3)) (sb/stood-s c 0))
          "two seconds stood and the stay's hurt and death at cost/risk-s"))))

(deftest with-no-way-but-a-dig-route-a-search-is-worth-what-the-goal-is
  (with-redefs [escape-way/choose (fn ([_ _ _ _] nil) ([_ _ _ _ _] {:step :none}))]
    (is (= 40 (sb/fallback-s (assoc (ctx-at 0) :worth (constantly 40)) [0 64 0] [20 64 0] (constantly true) {})))
    (is (= ##Inf (sb/fallback-s (ctx-at 0) [0 64 0] [20 64 0] (constantly true) {})) "arrive: the search goes on")))

(deftest at-arrive-worth-the-time-the-chain-has-left-bounds-the-fallback-and-the-dig-budget
  (with-redefs [escape-way/choose (fn ([_ _ _ _] nil) ([_ _ _ _ _] {:step :none}))]
    (let [c (assoc (ctx-at 0) :left (fn [] {:spend {:job-time 25}}))]
      (is (= 25 (sb/fallback-s c [0 64 0] [20 64 0] (constantly true) {})) "no worth: the job time left")
      (is (= 25 (choose/dig-left-s c)))
      (is (= 10 (choose/call-budget-s (assoc c :worth (constantly 10)))) "the lesser of the worth and the time")
      (is (esc/replans-spent? (ctx-at 25000) {:replan-budget {:since 0 :fallback-s (sb/fallback-s c [0 64 0] [20 64 0] (constantly true) {})}})
          "so replans end by the time, not never"))))

(deftest dig-route-replans-are-spent-once-they-have-taken-as-long-as-the-way-without-one-costs
  (let [m {:replan-budget {:since 1000 :fallback-s 10}}]
    (is (not (esc/replans-spent? (ctx-at 10999) m)))
    (is (esc/replans-spent? (ctx-at 11000) m))
    (is (not (esc/replans-spent? (ctx-at 99999) {})) "none begun")))

(deftest a-stair-over-an-open-floor-with-no-filler-is-priced-as-a-fetch-and-the-fallback-is-finite
  (let [c (ctx-at 0)
        p (:primitives c)
        stair {:step :stair :heading "north" :steps 2}]
    (with-redefs [escape/block-at-of (constantly (constantly "air"))
                  escape/unseen-of (constantly (constantly false))
                  escape-way/choose (fn ([_ _ _ _] nil) ([_ _ _ _ _] stair))
                  risk/estimate (fn [& _] {:hp 0 :lethal-hp 0})]
      (is (js/isFinite (escape-way/stair-route-s p [0 64 0] stair)) "no filler carried: the fill is a fetch, not impossible")
      (is (js/isFinite (sb/fallback-s c [0 64 0] [0 70 0] (constantly true) {}))))))

(deftest a-step-priced-unpassable-leaves-the-fallback-at-the-goals-worth
  (with-redefs [escape-way/choose (fn ([_ _ _ _] nil) ([_ _ _ _ _] {:step :stair}))
                escape-way/price (fn ([_ _ _] ##Inf) ([_ _ _ _] ##Inf))
                risk/estimate (fn [& _] {:hp 0 :lethal-hp 0})]
    (let [c (assoc (ctx-at 0) :worth (constantly 40))]
      (is (= 40 (sb/fallback-s c [0 64 0] [20 64 0] (constantly true) {})))
      (is (esc/replans-spent? (ctx-at 1e9) {:replan-budget {:since 0 :fallback-s (sb/fallback-s c [0 64 0] [20 64 0] (constantly true) {})}})))))

(deftest pricing-the-rest-of-a-line-to-a-far-goal-reads-a-bounded-number-of-cells
  (let [reads (atom 0)]
    (with-redefs [reach/seen-lookup (fn ([_ _ _] (fn [& _] (swap! reads inc) :ground)) ([_ _] (fn [& _] :ground)) ([_] (fn [& _] :ground)))]
      (let [near ((escape-way/rest-s-fn (:primitives (ctx-at 0)) [0 64 0]) [-1000 64 0])
            before @reads
            far ((escape-way/rest-s-fn (:primitives (ctx-at 0)) [0 64 0]) [-1000000 64 0])]
        (is (< (- @reads before) (* 4 escape-way/read-line-cells)))
        (is (> far near))))))
