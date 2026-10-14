(ns engine.animal-contract-test
  "food.fish, animals.shear and combat.hunt as contract jobs: they export the contract, quote validly, and end with a
  kind and a cause (0 of N is never done)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.events :as events]
            [engine.hostile-test :as h]
            [engine.registry :as registry]
            [engine.seek-contract-test :as seek]
            [engine.test-util :as tu]
            [jobs.animals.shear :as shear]
            [jobs.combat.hunt :as hunt]
            [jobs.food.fish :as fish]
            [jobs.lib.world-files :as world]))

(defn job [sym] (get registry/jobs sym))

(deftest each-job-exports-the-contract-and-quotes-validly
  (doseq [[sym args] [['jobs.food.fish {:count 2}]
                      ['jobs.animals.shear {:count 2}]
                      ['jobs.combat.hunt {:mob "cow" :count 2}]]]
    (is (some? (:conditions (job sym))) (str sym " has conditions"))
    (is (nil? (:check (job sym))) (str sym " has no old check"))
    (is (vector? (:target (job sym))) (str sym " has a target"))
    (is (some? (:done? (job sym))) (str sym " has done?"))
    (is (nil? (contract/quote-problem ((:quote-of (job sym)) {:primitives (tu/fake {}) :args args :quote-call {:spend {:time 60}}}))) (str sym))))

(deftest old-options-map-to-the-call
  (is (= {:mob "cow" :use {:tools ["_sword"]}} (hunt/legacy-args {:mob "cow" :weapons ["_sword"]})))
  (is (= {:mob "cow" :use {:tools []}} (hunt/legacy-args {:mob "cow" :weapons []})) "no weapons stays no weapons")
  (is (= {:mob "cow" :use {:tools ["iron_sword"]}} (hunt/legacy-args {:mob "cow" :use {:tools ["iron_sword"]} :weapons ["_axe"]})))
  (is (= {:count 1 :spend {:time 9}} (fish/legacy-args {:count 1 :timeout-s 9})))
  (is (= {:count 1 :spend {:time 9}} (shear/legacy-args {:count 1 :timeout-s 9})))
  (is (= {:count 1 :spend {:time 4}} (shear/legacy-args {:count 1 :timeout-s 9 :spend {:time 4}}))))

;; ------------------------------------------------------------------ fish

(def pond (into {} (for [x (range 0 9) z (range 0 9) y [62 63]] [(str x "," y "," z) "water"])))
(def bank (into {} (for [z (range 0 9) y [62 63]] [(str "9," y "," z) "stone"])))
(def rod [{:name "fishing_rod" :count 1}])

(defn ^:async angle
  [args world]
  (await (seek/ending-of #(tu/seeing-all (tu/fake-on-floor (merge {:blocks (merge pond bank) :self {:pos [9.5 64 4.5]}} world)))
                         (list 'jobs.food.fish args))))

(deftest fishing-ends-with-a-kind-and-a-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed} (select-keys (await (angle {:count 2} {:inventory rod :fish {:catch ["cod"] :misses 1}})) [:event])))
        (is (= {:event :stopped :kind :condition-failed :reason :no-rod}
               (await (angle {:count 1 :fetch false} {:inventory []}))))
        (is (= {:event :stopped :kind :condition-failed :reason :no-water}
               (await (angle {:count 1} {:inventory rod :blocks bank}))))
        (is (= {:event :stopped :kind :nothing-possible :reason :no-bite}
               (await (angle {:count 2 :max-misses 3} {:inventory rod :fish {:misses 100}}))))
        (is (= {:event :stopped :kind :limit-reached :reason :timeout}
               (await (angle {:count 2 :spend {:time 0} :max-misses 1000} {:inventory rod :fish {:misses 100}}))))))))

;; ------------------------------------------------------------------ shear

(defn sheep [id x & [more]]
  (merge {:id id :uuid (str "u" id) :name "sheep" :kind "passive" :pos {:x x :y 64 :z 0}} more))

(def shears [{:name "shears" :count 1}])

(defn ^:async shearing
  [args world]
  (await (seek/ending-of #(tu/fake-on-floor (merge {:floor h/flight-floor} world)) (list 'jobs.animals.shear args))))

(deftest shearing-ends-with-a-kind-and-a-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed} (select-keys (await (shearing {} {:inventory shears :entities [(sheep 1 2)]})) [:event])))
        (is (= {:event :stopped :kind :condition-failed :reason :no-shears}
               (await (shearing {:fetch false} {:entities [(sheep 1 2)]}))))
        (is (= {:event :stopped :kind :nothing-possible :reason :none}
               (await (shearing {} {:inventory shears :entities []}))) "no sheep: 0 shorn is not done")
        (is (= {:event :stopped :kind :nothing-possible :reason :all-sheared}
               (await (shearing {} {:inventory shears :entities [(sheep 1 2 {:sheared true})]}))))
        (is (= {:event :stopped :kind :limit-reached :reason :timeout}
               (await (shearing {:spend {:time 0}} {:inventory shears :entities [(sheep 1 2)]}))))))))

;; ------------------------------------------------------------------ hunt

(defn cow [id x] {:id id :name "cow" :kind "passive" :pos {:x x :y 64 :z 0} :drops [{:name "beef" :count 1}]})

(defn ^:async hunting
  [args world]
  (await (seek/ending-of #(tu/fake-on-floor (merge {:floor h/flight-floor} world)) (list 'jobs.combat.hunt args))))

(deftest a-hunt-ends-with-a-kind-and-a-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed} (select-keys (await (hunting {:count 1 :keep 0} {:inventory h/sword :entities [(cow 1 3)]})) [:event])))
        (is (= {:event :stopped :kind :condition-failed :reason :too-few}
               (await (hunting {:count 1 :keep 2} {:inventory h/sword :entities [(cow 1 3)]}))))
        (is (= {:event :stopped :kind :condition-failed :reason :no-color}
               (await (hunting {:mob "sheep" :color "pink" :keep 0} {:inventory h/sword :entities [(sheep 1 3)]}))))
        (is (= {:event :stopped :kind :nothing-possible :reason :none}
               (await (hunting {:count 1 :keep 0} {:inventory h/sword :entities [(assoc (cow 1 3) :invulnerable true)]})))
            "an animal that cannot be hurt: 0 killed is not done")
        (is (= {:event :stopped :kind :condition-failed :reason :unarmed}
               (await (hunting {:mob "zombie" :count 1} {:entities [{:id 1 :name "zombie" :kind "hostile" :pos {:x 3 :y 64 :z 0}}]})))
            "an unarmed body does not fist-fight a hostile")))))

(deftest a-hunt-takes-its-weapon-from-the-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args want] [[{:count 1 :keep 0} "diamond_sword"]
                             [{:count 1 :keep 0 :use {:tools ["iron_sword"]}} "iron_sword"]
                             [{:count 1 :keep 0 :weapons ["iron_"]} "iron_sword"]]]
          (let [p (tu/fake-on-floor {:floor h/flight-floor :inventory seek/two-swords :entities [(cow 1 3)]})
                clock (atom 1000000)
                now (tu/act-clock clock p 20)
                eng (core/create {:primitives p :jobs registry/jobs :dir (tu/tmp-dir) :now now :world (world/of-data {} {} [])
                                  :events (events/make {:sinks [(fn [_])] :now now})})]
            (core/submit! eng (list 'jobs.combat.hunt args) {})
            (dotimes [_ 4] (swap! clock + 700) (await (core/tick! eng)))
            (core/shutdown! eng)
            (is (= want (some-> (first (h/calls p "equip")) .-args .-item)) (pr-str args))))))))
