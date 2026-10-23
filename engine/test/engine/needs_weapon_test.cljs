(ns engine.needs-weapon-test
  "The :needs-weapon trigger: a body with no weapon gets a sword."
  (:require [cljs.test :refer [deftest is are]]
            [engine.death-test :refer [memory-with]]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.dawn :as dawn]
            [jobs.lib.world-files :as world]
            [engine.test-util :as tu]
            [triggers.survival.needs-weapon :as nw]))

(defn holds? [{:keys [night inv self time entities]}]
  (nw/needs-weapon (tu/fake-on-floor (cond-> {:floor [-10 -10 10 10] :inventory (or inv []) :self self :entities entities}
                                       night (assoc :time 18000)
                                       time (assoc :time time)))
                   (mem/view (mem/open (tu/tmp-dir) {:now (constantly 0)})) {} nil #{}))

(deftest an-unarmed-body-needs-a-sword-at-any-hour
  (are [world expected] (= expected (holds? world))
    {} true
    {:inv [{:name "iron_pickaxe" :count 1}]} true
    {:inv [{:name "wooden_sword" :count 1}]} false
    {:inv [{:name "iron_axe" :count 1}]} false
    {:night true} true))

(def zombie {:id 7 :name "zombie" :kind "hostile" :pos {:x 8 :y 64 :z 0}})

(deftest a-known-hostile-keeps-an-unarmed-body-in-and-the-clock-does-not
  (are [world expected] (= expected (holds? world))
    {:time 23500} true
    {:time 400} true
    {:time 6000 :entities [zombie]} false
    {:time 6000 :entities [(assoc zombie :pos {:x 90 :y 64 :z 0})]} true)
  (is (true? (settings/with-settings {::dawn/with-hostile true} #(holds? {:time 6000 :entities [zombie]})))))

(defn holds-with-memory? [memory]
  (nw/needs-weapon (tu/fake-on-floor {:floor [-10 -10 10 10] :inventory []}) memory {} nil #{}))

(def respawned [:respawned 1100 {}])

(deftest a-body-leaves-a-live-pile-holding-its-weapon-to-the-recovery
  (are [memory expected] (= expected (holds-with-memory? memory))
    (memory-with 2000 [:died 1000 {:inventory [{:name "iron_sword" :count 1}]}] respawned) false
    (memory-with 2000 [:died 1000 {:inventory [{:name "cobblestone" :count 9}]}] respawned) true
    (memory-with 2000 [:died 1000 {:inventory [{:name "iron_sword" :count 1}]}] respawned [:recovered 1500 {:decision :abandoned :death-t 1000}]) true
    (memory-with 400000 [:died 1000 {:inventory [{:name "iron_sword" :count 1}]}] respawned) true))

(deftest a-sword-put-away-by-name-keeps-the-reflex-quiet
  (is (false? (holds-with-memory? (memory-with 2000 [:weapon-stowed 1000 {:item "stone_sword"}]))))
  (is (true? (holds-with-memory? (memory-with 2000)))))

(defn holds-with-listed?
  ([label args] (holds-with-listed? label args nil))
  ([label args ahead] (holds-with-listed? label args ahead nil))
  ([label args ahead own]
   (let [clock (atom 0)
         store (mem/open (tu/tmp-dir) {:now #(deref clock)})]
     (when ahead
       (mem/create-job! store "j0" "jobs.time.noop" {})
       (mem/update-job! store "j0" [] assoc :contract ahead))
     (swap! clock inc)
     (mem/create-job! store "j1" label args)
     (when own
       (mem/update-job! store "j1" [] assoc :contract own))
     (nw/needs-weapon (tu/fake-on-floor {:floor [-10 -10 10 10] :inventory []}) (mem/view store) {} (world/of-data {} {}) (cond-> #{"j1"} ahead (conj "j0"))))))

(deftest a-listed-job-that-brings-a-weapon-is-waited-for
  (are [label args expected] (= expected (holds-with-listed? label args))
    "jobs.items.get-kit" {} false
    "jobs.items.get-kit" {:items ["iron_helmet"]} true
    "jobs.items.obtain" {:item "stone_sword"} false
    "jobs.items.obtain" {:item "torch"} true))

(deftest a-weapon-maker-queued-behind-a-long-job-is-waited-for-only-while-that-is-cheaper
  (is (false? (holds-with-listed? "jobs.items.obtain" {:item "iron_sword"} {:call {:spend {:job-time 60}} :ran-ms 0}))
      "a minute's wait costs a twentieth of the sword")
  (is (true? (holds-with-listed? "jobs.items.obtain" {:item "iron_sword"} {:call {} :ran-ms 0}))
      "behind a job with no end the wait costs the sword's whole worth, more than getting one now"))

(deftest a-started-weapon-maker-is-waited-for-only-for-the-time-it-has-left
  (is (false? (holds-with-listed? "jobs.items.obtain" {:item "iron_sword"} nil {:call {:spend {:job-time 60}} :ran-ms 0}))
      "a minute left costs a twentieth of the sword")
  (is (true? (holds-with-listed? "jobs.items.obtain" {:item "iron_sword"} nil {:call {:spend {:job-time 3600}} :ran-ms 0}))
      "an hour left is past the need: the sword comes too late, a started job or not"))

(deftest a-wait-past-the-need-is-no-gain-when-a-trip-costs-more-than-the-sword
  (is (true? (settings/with-settings {:jobs.lib.cost.value/trip 100000}
               #(holds-with-listed? "jobs.items.obtain" {:item "iron_sword"} {:call {} :ran-ms 0})))
      "the wait costs the sword's whole worth, a trip costs more, but the sword is not there in time"))
