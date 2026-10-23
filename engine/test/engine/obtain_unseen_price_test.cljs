(ns engine.obtain-unseen-price-test
  "items.obtain's choice between a way whose raw needs are all seen and a search for one that is not: each priced, the
  cheaper taken."
  (:require [cljs.test :refer [deftest is]]
            [engine.game :as game]
            [engine.hostile-test :as h]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [jobs.items.obtain.plan :as plan]
            [jobs.items.recipes :as recipes]))

(defn spider [x] {:id 1 :name "spider" :kind "hostile" :pos {:x x :y 64 :z 0} :drops [{:name "string" :count 1}]})

(defn ctx [world]
  (let [view (mem/view (mem/open (tu/tmp-dir) {:now (constantly 0)}))]
    {:primitives (tu/fake-on-floor (merge {:floor h/flight-floor} world)) :view (constantly view)
     :args {:item "white_bed" :count 1 :ways #{:craft :smelt :gather :hunt :search} :explore true}}))

(defn kit [wool]
  (cond-> (into h/sword [{:name "oak_planks" :count 3} {:name "crafting_table" :count 1}])
    (pos? wool) (conj {:name "white_wool" :count wool})))

(defn choice
  "The raw items obtain gathers for a white bed in world with wool carried, and the price of each way: the sheep search
  and the spider hunt."
  [world wool]
  (let [c (ctx (assoc world :inventory (kit wool)))
        v (game/version-of (:primitives c))
        price #(plan/plan-cost c v {:steps [] :gather %})]
    {:gather (:gather (plan/gather-plan c ["white_bed"] 1))
     :search (price {"white_wool" (- 3 wool)})
     :hunt (price {"string" (* 4 (- 3 wool))})}))

(deftest a-hurt-body-searches-for-sheep-by-night-rather-than-fight-a-far-spider-for-each-string-and-a-near-spider-by-day-is-hunted
  (let [far-night (choice {:time 18000 :self {:health 8} :entities [(spider 40)]} 0)
        near-day (choice {:time 6000 :entities [(spider 5)]} 2)]
    (is (< (:search far-night) (:hunt far-night)) "twelve fights of a hurt body cost more than a likely sheep search")
    (is (= {"white_wool" 3} (:gather far-night)))
    (is (< (:hunt near-day) (:search near-day)) "four kills a few blocks off cost less than a search")
    (is (= {"string" 4} (:gather near-day)))))

(deftest no-search-is-priced-when-the-call-may-not-search
  (let [c (assoc-in (ctx {:time 6000 :inventory (kit 0) :entities [(spider 40)]}) [:args :explore] false)]
    (is (= {"string" 12} (:gather (plan/gather-plan c ["white_bed"] 1))) "the seen spider, however dear")))

(deftest another-way-that-leaves-out-nothing-more-ends-the-look-for-one
  (let [c (ctx {:time 6000 :inventory (kit 0)})
        v (game/version-of (:primitives c))
        pl {:steps [] :gather {"white_wool" 3}}]
    (with-redefs [recipes/plan (fn [& _] pl)]
      (is (= pl (plan/preferred c v pl {} "white_bed" 1 {}))
          "the other way gathers the same unseen wool: no way is left, and the plan stays"))))

