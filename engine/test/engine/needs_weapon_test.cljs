(ns engine.needs-weapon-test
  "The :needs-weapon trigger: a body with no weapon gets a sword."
  (:require [cljs.test :refer [deftest is are]]
            [engine.death-test :refer [memory-with]]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.dawn :as dawn]
            [engine.test-util :as tu]
            [triggers.survival.needs-weapon :as nw]))

(defn holds? [{:keys [night inv self time entities]}]
  (nw/needs-weapon (tu/fake-on-floor (cond-> {:floor [-10 -10 10 10] :inventory (or inv []) :self self :entities entities}
                                       night (assoc :time 18000)
                                       time (assoc :time time)))
                   (mem/view (mem/open (tu/tmp-dir) {:now (constantly 0)})) {}))

(deftest an-unarmed-body-needs-a-sword-at-any-hour-the-dawn-grace-allows
  (are [world expected] (= expected (holds? world))
    {} true
    {:inv [{:name "iron_pickaxe" :count 1}]} true
    {:inv [{:name "wooden_sword" :count 1}]} false
    {:inv [{:name "iron_axe" :count 1}]} false
    {:night true} true))

(def zombie {:id 7 :name "zombie" :kind "hostile" :pos {:x 8 :y 64 :z 0}})

(deftest first-light-and-a-known-hostile-keep-an-unarmed-body-in
  (are [world expected] (= expected (holds? world))
    {:time 23500} false
    {:time 400} false
    {:time 1000} true
    {:time 6000 :entities [zombie]} false
    {:time 6000 :entities [(assoc zombie :pos {:x 90 :y 64 :z 0})]} true)
  (is (true? (settings/with-settings {::dawn/with-hostile true} #(holds? {:time 6000 :entities [zombie]}))))
  (is (true? (settings/with-settings {::dawn/grace-ticks 0} #(holds? {:time 23500})))))

(defn holds-with-memory? [memory]
  (nw/needs-weapon (tu/fake-on-floor {:floor [-10 -10 10 10] :inventory []}) memory {}))

(def respawned [:respawned 1100 {}])

(deftest a-body-leaves-a-live-pile-holding-its-weapon-to-the-recovery
  (are [memory expected] (= expected (holds-with-memory? memory))
    (memory-with 2000 [:died 1000 {:inventory [{:name "iron_sword" :count 1}]}] respawned) false
    (memory-with 2000 [:died 1000 {:inventory [{:name "cobblestone" :count 9}]}] respawned) true
    (memory-with 2000 [:died 1000 {:inventory [{:name "iron_sword" :count 1}]}] respawned [:recovered 1500 {:decision :abandoned :death-t 1000}]) true
    (memory-with 400000 [:died 1000 {:inventory [{:name "iron_sword" :count 1}]}] respawned) true))
