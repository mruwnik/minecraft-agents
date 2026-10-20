(ns engine.animal-contract-test
  "food.fish, animals.shear and combat.hunt as contract jobs: they export the contract, quote validly, and end with a
  kind and a cause (0 of N is never done)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.breed-test :as bt]
            [engine.events :as events]
            [engine.hostile-test :as h]
            [engine.registry :as registry]
            [engine.seek-contract-test :as seek]
            [engine.test-util :as tu]
            [jobs.animals.breed :as breed]
            [jobs.animals.leash :as leash]
            [jobs.animals.shear :as shear]
            [jobs.animals.unleash :as unleash]
            [jobs.combat.hunt :as hunt]
            [jobs.food.fish :as fish]
            [jobs.lib.world-files :as world]))

(defn job [sym] (get registry/jobs sym))

(deftest each-job-exports-the-contract-and-quotes-validly
  (doseq [[sym args] [['jobs.food.fish {:count 2}]
                      ['jobs.animals.shear {:count 2}]
                      ['jobs.combat.hunt {:item "beef" :count 2}]
                      ['jobs.animals.leash {:mob "cow"}]
                      ['jobs.animals.unleash {}]
                      ['jobs.animals.breed {:mob "cow" :pairs 1}]
                      ['jobs.animals.pen-check {:at [2 64 2]}]
                      ['jobs.animals.cull {:mob "cow" :keep 1}]
                      ['jobs.maintenance.shut {:openings :gates}]
                      ['jobs.animals.maintain {:mob "cow" :box {:min {:x 0 :y 60 :z 0} :max {:x 4 :y 70 :z 4}}}]
                      ['jobs.animals.lead-to {:mob "cow" :pos {:x 5 :y 64 :z 0}}]
                      ['jobs.animals.lure-to {:mob "cow" :pos {:x 5 :y 64 :z 0}}]
                      ['jobs.animals.herd {:mob "cow" :box {:min {:x 0 :y 60 :z 0} :max {:x 4 :y 70 :z 4}} :target 2}]]]
    (is (fn? (:can-start? (job sym))) (str sym " has can-start?"))
    (is (vector? (:target (job sym))) (str sym " has a target"))
    (is (some? (:done? (job sym))) (str sym " has done?"))
    (is (nil? (contract/quote-problem ((:quote-of (job sym)) {:primitives (tu/fake {}) :args args :quote-call {:spend {:job-time 60}}}))) (str sym))))

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
        (is (= {:event :waiting :reason :no-rod}
               (await (angle {:count 1 :ways #{:carried}} {:inventory []}))))
        (is (= {:event :waiting :reason :no-water}
               (await (angle {:count 1} {:inventory rod :blocks bank}))))
        (is (= {:event :stopped :kind :nothing-possible :reason :no-bite}
               (await (angle {:count 2 :settings {:jobs.food.fish/max-misses 3}} {:inventory rod :fish {:misses 100}}))))
        (is (= {:event :stopped :kind :limit-reached :reason :job-time}
               (await (angle {:count 2 :spend {:job-time 0} :settings {:jobs.food.fish/max-misses 1000}} {:inventory rod :fish {:misses 100}}))))))))

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
        (is (= {:event :waiting :reason :no-shears}
               (await (shearing {:ways #{:carried}} {:entities [(sheep 1 2)]}))))
        (is (= {:event :stopped :kind :nothing-possible :reason :none}
               (await (shearing {} {:inventory shears :entities []}))) "no sheep: 0 shorn is not done")
        (is (= {:event :completed} (select-keys (await (shearing {} {:inventory shears :entities [(sheep 1 2 {:sheared true})]})) [:event]))
            "every sheep sheared: the goal holds")
        (is (= {:event :completed} (select-keys (await (shearing {:count 3} {:inventory shears :entities [(sheep 1 2)]})) [:event]))
            "one shorn meets the default :least 1")
        (is (= {:event :stopped :kind :nothing-possible :reason :all-sheared}
               (await (shearing {:count 3 :least 2} {:inventory shears :entities [(sheep 1 2)]})))
            "the sheep ran out below :least")))))

;; ------------------------------------------------------------------ hunt

(defn cow [id x] {:id id :name "cow" :kind "passive" :pos {:x x :y 64 :z 0} :drops [{:name "beef" :count 1}]})

(defn ^:async hunting
  [args world]
  (await (seek/ending-of #(tu/fake-on-floor (merge {:floor h/flight-floor} world)) (list 'jobs.combat.hunt args))))

(defn ^:async hunting-cull
  [args world]
  (await (seek/ending-of #(tu/fake-on-floor (merge {:floor h/flight-floor} world)) (list 'jobs.animals.cull (merge {:mob "cow"} args)))))

(deftest a-hunt-ends-with-a-kind-and-a-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed} (select-keys (await (hunting {:item "beef" :count 1 :keep 0} {:inventory h/sword :entities [(cow 1 3)]})) [:event])))
        (is (= {:event :waiting :reason :too-few}
               (await (hunting {:item "beef" :count 1 :keep 2} {:inventory h/sword :entities [(cow 1 3)]}))))
        (is (= {:event :waiting :reason :no-color}
               (await (hunting {:item "pink_wool" :keep 0} {:inventory h/sword :entities [(sheep 1 3)]}))))
        (is (= {:event :stopped :kind :nothing-possible :reason :none}
               (await (hunting {:item "beef" :count 1 :keep 0} {:inventory h/sword :entities [(assoc (cow 1 3) :invulnerable true)]})))
            "an animal that cannot be hurt: 0 killed is not done")
        (is (= {:event :waiting :reason :unarmed}
               (await (hunting {:item "rotten_flesh" :count 1 :fetch false} {:entities [{:id 1 :name "zombie" :kind "hostile" :pos {:x 3 :y 64 :z 0}}]})))
            "an unarmed body does not fist-fight a hostile: it waits at the start")))))

(deftest a-hunt-takes-its-weapon-from-the-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args want] [[{:item "beef" :count 1 :keep 0} "diamond_sword"]
                             [{:item "beef" :count 1 :keep 0 :use {:tools ["iron_sword"]}} "iron_sword"]
                             [{:item "beef" :count 1 :keep 0 :use {:tools ["iron_"]}} "iron_sword"]]]
          (let [p (tu/fake-on-floor {:floor h/flight-floor :inventory seek/two-swords :entities [(cow 1 3)]})
                clock (atom 1000000)
                now (tu/act-clock clock p 20)
                eng (core/create {:primitives p :jobs registry/jobs :dir (tu/tmp-dir) :now now :world (world/of-data {} {} [])
                                  :events (events/make {:sinks [(fn [_])] :now now})})]
            (core/submit! eng (list 'jobs.combat.hunt args) {})
            (await (tu/run-ticks eng clock 4 700))
            (core/shutdown! eng)
            (is (= want (some-> (first (h/calls p "equip")) .-args .-item)) (pr-str args))))))))

;; ------------------------------------------------------------------ leash, unleash, breed, pen-check

(defn cow-at [id x & [more]]
  (merge {:id id :uuid (str "u" id) :name "cow" :kind "passive" :pos {:x x :y 64 :z 0}} more))

(def lead [{:name "lead" :count 2}])
(def wheat [{:name "wheat" :count 4}])

(defn ^:async ending
  [spec world]
  (await (seek/ending-of #(tu/fake-on-floor (merge {:floor h/flight-floor} world)) spec)))

(defn ^:async ending-with-calves
  "ending where a calf appears for every pair fed."
  [spec world]
  (await (seek/ending-of #(doto (tu/fake-on-floor (merge {:floor h/flight-floor} world)) bt/calf-per-pair!) spec)))

(deftest leashing-ends-with-a-kind-and-a-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed} (select-keys (await (ending '(jobs.animals.leash {:mob "cow"}) {:inventory lead :entities [(cow-at 1 2)]})) [:event])))
        (doseq [[args world kind reason]
                [[{} {:inventory lead :entities []} :nothing-possible :none]
                 [{} {:inventory lead :entities [(cow-at 1 2 {:leashed true :leashHolder 99})]} :nothing-possible :all-leashed]]]
          (is (= {:event :stopped :kind kind :reason reason} (await (ending (list 'jobs.animals.leash (merge {:mob "cow"} args)) world)))))))))

(deftest the-engine-ends-a-job-past-its-time
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[spec world] [['(jobs.animals.leash {:mob "cow" :spend {:job-time 0}}) {:inventory lead :entities [(cow-at 1 2)]}]
                              ['(jobs.animals.unleash {:spend {:job-time 0}}) {:entities [(cow-at 1 2 {:leashed true :leashedToMe true})]}]
                              ['(jobs.animals.breed {:mob "cow" :spend {:job-time 0}}) {:inventory wheat :entities [(cow-at 1 2) (cow-at 2 3)]}]
                              ['(jobs.animals.shear {:spend {:job-time 0}}) {:inventory shears :entities [(sheep 1 2)]}]]]
          (is (= {:event :stopped :kind :limit-reached} (select-keys (await (ending spec world)) [:event :kind])) (str spec)))))))

(deftest unleashing-ends-with-a-kind-and-a-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed} (select-keys (await (ending '(jobs.animals.unleash {}) {:entities [(cow-at 1 2 {:leashed true :leashedToMe true})]})) [:event])))
        (is (= {:event :completed} (select-keys (await (ending '(jobs.animals.unleash {}) {:entities [(cow-at 1 2)]})) [:event]))
            "nobody on a lead: the goal holds, no wait")))))

(deftest breeding-ends-with-a-kind-and-a-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed} (select-keys (await (ending-with-calves '(jobs.animals.breed {:mob "cow"}) {:inventory wheat :entities [(cow-at 1 2) (cow-at 2 3)]})) [:event])))
        (is (= {:event :completed} (select-keys (await (ending-with-calves '(jobs.animals.breed {:mob "cow" :pairs 3}) {:inventory wheat :entities (mapv #(cow-at % %) [1 2 3 4 5 6])})) [:event]))
            "the food ran out after four: done with the count fed")
        (doseq [[args world kind reason]
                [[{} {:inventory wheat :entities [(cow-at 1 2)]} :condition-failed :too-few]
                 [{:mob "rock"} {:inventory wheat :entities []} :impossible :unknown-mob]
                 [{:pairs 2} {:inventory [{:name "wheat" :count 1}] :entities (mapv #(cow-at % %) [1 2 3 4])} :nothing-possible :food-out]]]
          (is (= {:event :stopped :kind kind :reason reason} (await (ending-with-calves (list 'jobs.animals.breed (merge {:mob "cow"} args)) world)))))
        (is (= {:event :stopped :kind :nothing-possible :reason :no-baby}
               (await (ending '(jobs.animals.breed {:mob "cow" :settings {:jobs.animals.breed/calf-wait-s 0}}) {:inventory wheat :entities [(cow-at 1 2) (cow-at 2 3)]})))
            "a pair fed and no calf seen is not done")
        (is (= {:event :completed}
               (select-keys (await (ending-with-calves '(jobs.animals.breed {:mob "cow" :pairs 2 :least 1}) {:inventory [{:name "wheat" :count 1}] :entities (mapv #(cow-at % %) [1 2 3 4])})) [:event]))
            "one fed meets :least 1")))))

(deftest a-pen-check-answers-closed-and-a-leaky-pen-as-done-an-unbounded-fill-as-nothing-possible
  (let [ground (into {} (for [x (range -10 16) z (range -10 16)] [(str x ",63," z) "stone"]))
        ring (into {} (for [x (range -1 6) z (range -1 6) :when (or (#{-1 5} x) (#{-1 5} z))] [(str x ",64," z) "oak_fence"]))
        w (fn [more] {:self {:pos {:x 2 :y 64 :z 2}} :blocks (merge ground ring more)})]
    (async done
      (tu/run-async done
        (fn ^:async t []
          (is (= {:event :completed} (select-keys (await (ending '(jobs.animals.pen-check {:at [2 64 2]}) (w {}))) [:event])))
          (is (= {:event :completed :kind :done}
                 (select-keys (await (ending '(jobs.animals.pen-check {:at [2 64 2] :ways #{:check} :settings {:jobs.animals.pen-check/max-cells 100}}) (w {"2,64,-1" "air"}))) [:event :kind])))
          (is (= {:event :stopped :kind :nothing-possible :reason :unbounded}
                 (await (ending '(jobs.animals.pen-check {:at [2 64 2] :settings {:jobs.animals.pen-check/max-cells 3}}) (w {}))))))))))

;; ------------------------------------------------------------------ cull

(deftest a-pen-check-without-a-pen-is-impossible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :stopped :kind :impossible :reason :no-pen}
               (await (ending '(jobs.animals.pen-check {}) {}))))))))

(deftest a-cull-ends-with-a-kind-and-a-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed} (select-keys (await (hunting-cull {:keep 0} {:inventory h/sword :entities [(cow 1 3)]})) [:event])))
        (is (= {:event :completed :kind :done :reason :keep}
               (await (hunting-cull {:keep 2} {:inventory h/sword :entities [(cow 1 3)]})))
            "no more than :keep already: done, no wait")
        (is (= {:event :stopped :kind :nothing-possible :reason :unreachable}
               (await (hunting-cull {:keep 0} {:inventory h/sword :entities [(assoc (cow 1 3) :invulnerable true)]})))
            "an animal that cannot be hurt: 0 killed is not done")))))

;; ------------------------------------------------------------------ tend, lead-to

(def pen {:min {:x 0 :y 60 :z -5} :max {:x 8 :y 70 :z 5}})
(def far-pos {:x 30 :y 64 :z 0})

(deftest tend-ends-with-a-kind-and-a-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed} (select-keys (await (ending-with-calves (list 'jobs.animals.maintain {:mob "cow" :box pen :target 4}) {:inventory wheat :entities [(cow-at 1 2) (cow-at 2 3)]})) [:event])))
        (is (= {:event :stopped :kind :impossible :reason :no-box}
               (await (ending '(jobs.animals.maintain {}) {:entities [(cow-at 1 2)]}))) "no box and no plan: it can never start")
        (is (= {:event :stopped :kind :impossible :reason :no-mob}
               (await (ending (list 'jobs.animals.maintain {:box pen}) {:entities [(cow-at 1 2)]}))) "no mob and no plan: it can never start")
        (is (= {:event :completed}
               (select-keys (await (ending (list 'jobs.animals.maintain {:mob "cow" :box pen :target 2}) {:entities [(cow-at 1 2) (cow-at 2 3)]})) [:event]))
            "at target with nothing to do: done at once")))))

(deftest lead-to-ends-with-a-kind-and-a-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:floor tu/walk-floor :inventory lead :entities [(cow-at 1 3)]}]
          (is (= {:event :completed} (select-keys (await (ending (list 'jobs.animals.lead-to {:mob "cow" :pos far-pos}) world)) [:event])))
          (is (= {:event :stopped :kind :impossible :reason :no-mob} (await (ending (list 'jobs.animals.lead-to {:pos far-pos}) world))))
          (is (= {:event :stopped :kind :impossible :reason :no-destination} (await (ending '(jobs.animals.lead-to {:mob "cow"}) world))))
          (is (= {:event :waiting :reason :no-fence}
                 (await (ending (list 'jobs.animals.lead-to {:mob "cow" :fence {:x 5 :y 64 :z 0}}) (assoc world :blocks {"5,64,0" "stone"})))))
          (is (= {:event :stopped :kind :nothing-possible :reason :none}
                 (await (ending (list 'jobs.animals.lead-to {:mob "cow" :pos far-pos}) (assoc world :entities [])))))
          (is (= {:event :stopped :kind :nothing-possible :reason :animal-stalled}
                 (await (ending (list 'jobs.animals.lead-to {:mob "cow" :pos far-pos}) (assoc world :entities [(cow-at 1 3 {:trail 9})]))))))))))

(deftest lure-to-ends-with-a-kind-and-a-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:floor tu/walk-floor :inventory wheat :entities [(cow-at 1 3)]}
              lure (fn [args world] (ending (list 'jobs.animals.lure-to (merge {:mob "cow" :pos far-pos} args)) world))]
          (is (= {:event :completed} (select-keys (await (lure {} world)) [:event])))
          (is (= {:event :stopped :kind :impossible :reason :no-mob} (await (ending (list 'jobs.animals.lure-to {:pos far-pos}) world))))
          (is (= {:event :stopped :kind :impossible :reason :no-destination} (await (ending '(jobs.animals.lure-to {:mob "cow"}) world))))
          (is (= {:event :stopped :kind :condition-failed :reason :no-food} (await (lure {} (assoc world :inventory [])))))
          (is (= {:event :stopped :kind :nothing-possible :reason :no-animal} (await (lure {} (assoc world :entities [])))))
          (is (= {:event :stopped :kind :limit-reached :reason :job-time} (await (lure {:spend {:job-time 0}} world)))))))))
