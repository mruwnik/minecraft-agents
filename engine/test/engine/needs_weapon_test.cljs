(ns engine.needs-weapon-test
  "The :needs-weapon trigger: a body with no weapon gets a sword by day."
  (:require [cljs.test :refer [deftest is are]]
            [engine.death-test :refer [memory-with]]
            [engine.expr :as expr]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [triggers.survival.needs-weapon :as nw]))

(defn holds? [{:keys [night inv self]}]
  (nw/needs-weapon (tu/fake-on-floor (cond-> {:floor [-10 -10 10 10] :inventory (or inv []) :self self} night (assoc :time 18000)))
                   (mem/view (mem/open (tu/tmp-dir) {:now (constantly 0)})) {}))

(deftest an-unarmed-body-needs-a-sword-by-day-only
  (are [world expected] (= expected (holds? world))
    {} true
    {:inv [{:name "iron_pickaxe" :count 1}]} true
    {:inv [{:name "wooden_sword" :count 1}]} false
    {:inv [{:name "iron_axe" :count 1}]} false
    {:night true} false))

(deftest the-trigger-args-are-valid-obtain-args
  (is (nil? (expr/problem registry/jobs (list 'jobs.items.obtain nw/defaults)))))

(defn holds-with-memory? [memory]
  (nw/needs-weapon (tu/fake-on-floor {:floor [-10 -10 10 10] :inventory []}) memory {}))

(def respawned [:respawned 1100 {}])

(deftest a-body-leaves-a-live-pile-holding-its-weapon-to-the-recovery
  (are [memory expected] (= expected (holds-with-memory? memory))
    (memory-with 2000 [:died 1000 {:inventory [{:name "iron_sword" :count 1}]}] respawned) false
    (memory-with 2000 [:died 1000 {:inventory [{:name "cobblestone" :count 9}]}] respawned) true
    (memory-with 2000 [:died 1000 {:inventory [{:name "iron_sword" :count 1}]}] respawned [:recovered 1500 {:decision :abandoned :death-t 1000}]) true
    (memory-with 400000 [:died 1000 {:inventory [{:name "iron_sword" :count 1}]}] respawned) true))
