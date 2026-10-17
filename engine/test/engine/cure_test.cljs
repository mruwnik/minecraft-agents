(ns engine.cure-test
  "The poisoned trigger, the cure job and the poisoned wait of risky jobs against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.body :as body]))

(def poison [{:name "poison" :amplifier 0 :duration 400}])
(def milk [{:name "milk_bucket" :count 1}])

(defn holds? [world] ((:when (get triggers/all :poisoned)) (tu/fake world) {} {}))

(deftest poisoned-trigger-holds-for-a-hurt-poisoned-body-carrying-a-cure
  (are [world expected] (= expected (boolean (holds? world)))
    {:self {:health 6 :effects poison} :inventory milk} true
    {:self {:health 6 :effects poison} :inventory [{:name "golden_apple" :count 1}]} true
    {:self {:health 6 :effects poison}} false
    {:self {:health 6 :effects poison} :inventory [{:name "bread" :count 1}]} false
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
        status (fn [world] (first (can-start? {:primitives (tu/fake world) :args {}})))]
    (is (= :later (status {:self {:effects poison}})))
    (is (= :ok (status {:self {:effects poison} :inventory milk})))))

(deftest too-hurt-also-while-poisoned
  (are [self expected] (= expected (:reason (body/too-hurt (tu/fake {:self self}))))
    {:health 20 :effects poison} :poisoned
    {:health 3 :effects poison} :low-health
    {:health 20 :effects []} nil))
