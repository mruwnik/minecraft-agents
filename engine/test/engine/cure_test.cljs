(ns engine.cure-test
  "The poisoned trigger, the cure job and the poisoned wait of risky jobs against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.body :as body]
            [jobs.lib.need :as need]))

(def poison [{:name "poison" :amplifier 0 :duration 400}])
(def milk [{:name "milk_bucket" :count 1}])

(defn holds? [world] ((:when (get triggers/all :poisoned)) (tu/fake world) {} {}))

(deftest poisoned-trigger-holds-for-a-hurt-poisoned-body-whatever-it-carries
  (are [world expected] (= expected (boolean (holds? world)))
    {:self {:health 6 :effects poison} :inventory milk} true
    {:self {:health 6 :effects poison} :inventory [{:name "golden_apple" :count 1}]} true
    {:self {:health 6 :effects poison}} true
    {:self {:health 6 :effects poison} :inventory [{:name "bread" :count 1}]} true
    {:self {:health 18 :effects poison} :inventory milk} false
    {:self {:health 6 :effects []} :inventory milk} false
    {:self {:health 6 :effects (conj poison {:name "regeneration" :amplifier 1 :duration 100})} :inventory milk} false))

(deftest cure-drinks-the-milk-and-ends-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/fake {:self {:health 6 :effects poison} :inventory (conj milk {:name "golden_apple" :count 1})})
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)})]
          (core/submit! eng '(jobs.survival.cure) {})
          (await (core/tick! eng))
          (is (not (body/poisoned? p)))
          (is (= [] (:list (core/state eng))))
          (is (= ["milk_bucket"] (mapv #(aget (.-args %) "item") (filter #(= "eat" (.-name %)) (.-calls (.-world p)))))
              "milk before the golden apple"))))))

(deftest cure-without-a-cure-waits
  (let [can-start? (:can-start? (get registry/jobs 'jobs.survival.cure))
        status (fn [world] (first (can-start? {:primitives (tu/fake world) :args {:ways #{:carried}}})))]
    (is (= :later (status {:self {:effects poison}})))
    (is (= :ok (status {:self {:effects poison} :inventory milk})))))

(deftest cure-fetches-a-cure-only-when-that-is-cheaper-than-waiting-the-poison-out
  (let [can-start? (:can-start? (get registry/jobs 'jobs.survival.cure))
        status (fn [price args] (with-redefs [need/item-price (constantly price)]
                                  (first (can-start? {:primitives (tu/fake {:self {:health 6 :effects poison}}) :args args}))))]
    (is (= :ok (status 20 {})) "a fetch worth 20 s against about 190 s of poison")
    (is (= :later (status 1000 {})) "a dear fetch loses to waiting")
    (is (= :later (status ##Inf {})) "no source")
    (is (= :later (status 20 {:ways #{:carried}})) "a call that leaves fetching out waits")))

(deftest a-field-job-waits-while-poisoned
  (are [self expected] (= expected (first ((:fit? (get registry/jobs 'jobs.farm.till)) {:primitives (tu/fake {:self self}) :view (constantly {:data {} :now 0})})))
    {:health 20 :effects poison} :later
    {:health 20 :effects []} :ok))
