(ns engine.animal-contract-test
  "food.fish, animals.shear and combat.hunt as contract jobs: they export the contract, quote validly, and end with a
  kind and a cause (0 of N is never done)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.contract-limits-test :as limits]
            [engine.core :as core]
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
                      ['jobs.combat.hunt {:mob "cow" :count 2}]
                      ['jobs.animals.leash {:mob "cow"}]
                      ['jobs.animals.unleash {}]
                      ['jobs.animals.breed {:mob "cow" :count 2}]
                      ['jobs.animals.pen-check {:at [2 64 2]}]
                      ['jobs.animals.cull {:mob "cow" :keep 1}]
                      ['jobs.animals.shut-gate {}]
                      ['jobs.animals.tend {:box {:min {:x 0 :y 60 :z 0} :max {:x 4 :y 70 :z 4}}}]
                      ['jobs.animals.lead-to {:mob "cow" :pos {:x 5 :y 64 :z 0}}]
                      ['jobs.animals.lure-to {:mob "cow" :pos {:x 5 :y 64 :z 0}}]
                      ['jobs.animals.herd {:mob "cow" :box {:min {:x 0 :y 60 :z 0} :max {:x 4 :y 70 :z 4}} :target 2}]]]
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
  (is (= {:count 1 :spend {:time 4}} (shear/legacy-args {:count 1 :timeout-s 9 :spend {:time 4}})))
  (is (= {:mob "cow" :spend {:time 9}} (leash/legacy-args {:mob "cow" :timeout-s 9})))
  (is (= {:spend {:time 9}} (unleash/legacy-args {:timeout-s 9})))
  (is (= {:mob "cow" :spend {:time 9}} (breed/legacy-args {:mob "cow" :timeout-s 9}))))

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
               (await (angle {:count 1 :fetch false} {:inventory []}))))
        (is (= {:event :waiting :reason :no-water}
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
        (is (= {:event :completed} (select-keys (await (shearing {:count 3} {:inventory shears :entities [(sheep 1 2)]})) [:event]))
            "one shorn meets the default :least 1")
        (is (= {:event :stopped :kind :nothing-possible :reason :all-sheared}
               (await (shearing {:count 3 :least 2} {:inventory shears :entities [(sheep 1 2)]})))
            "the sheep ran out below :least")
        (is (= {:event :completed} (select-keys (await (shearing {:spend {:time 0}} {:inventory shears :entities [(sheep 1 2)]})) [:event]))
            "shadow mode: the job does not end itself on the wall clock")))))

;; ------------------------------------------------------------------ hunt

(defn cow [id x] {:id id :name "cow" :kind "passive" :pos {:x x :y 64 :z 0} :drops [{:name "beef" :count 1}]})

(defn ^:async hunting
  [args world]
  (await (seek/ending-of #(tu/fake-on-floor (merge {:floor h/flight-floor} world)) (list 'jobs.combat.hunt args))))

(defn ^:async hunting-cull
  [args world]
  (await (seek/ending-of #(tu/fake-on-floor (merge {:floor h/flight-floor} world)) (list 'jobs.animals.cull args))))

(deftest a-hunt-ends-with-a-kind-and-a-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed} (select-keys (await (hunting {:count 1 :keep 0} {:inventory h/sword :entities [(cow 1 3)]})) [:event])))
        (is (= {:event :waiting :reason :too-few}
               (await (hunting {:count 1 :keep 2} {:inventory h/sword :entities [(cow 1 3)]}))))
        (is (= {:event :waiting :reason :no-color}
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

;; ------------------------------------------------------------------ leash, unleash, breed, pen-check

(defn cow-at [id x & [more]]
  (merge {:id id :uuid (str "u" id) :name "cow" :kind "passive" :pos {:x x :y 64 :z 0}} more))

(def lead [{:name "lead" :count 2}])
(def wheat [{:name "wheat" :count 4}])

(defn ^:async ending
  [spec world]
  (await (seek/ending-of #(tu/fake-on-floor (merge {:floor h/flight-floor} world)) spec)))

(deftest leashing-ends-with-a-kind-and-a-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed} (select-keys (await (ending '(jobs.animals.leash {:mob "cow"}) {:inventory lead :entities [(cow-at 1 2)]})) [:event])))
        (doseq [[args world kind reason]
                [[{} {:inventory lead :entities []} :nothing-possible :none]
                 [{} {:inventory lead :entities [(cow-at 1 2 {:leashed true :leashHolder 99})]} :nothing-possible :all-leashed]]]
          (is (= {:event :stopped :kind kind :reason reason} (await (ending (list 'jobs.animals.leash (merge {:mob "cow"} args)) world)))))))))

(deftest leashing-does-not-time-itself
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed} (select-keys (await (ending '(jobs.animals.leash {:mob "cow" :spend {:time 0}}) {:inventory lead :entities [(cow-at 1 2)]})) [:event]))
            "shadow mode: the job does not end itself on the wall clock")))))

(deftest the-engine-ends-a-job-past-its-time-when-limits-are-enforced
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (limits/enforced
                (fn ^:async enforced []
                  (doseq [[spec world] [['(jobs.animals.leash {:mob "cow" :spend {:time 0}}) {:inventory lead :entities [(cow-at 1 2)]}]
                                        ['(jobs.animals.unleash {:spend {:time 0}}) {:entities [(cow-at 1 2 {:leashed true :leashedToMe true})]}]
                                        ['(jobs.animals.breed {:mob "cow" :spend {:time 0}}) {:inventory wheat :entities [(cow-at 1 2) (cow-at 2 3)]}]
                                        ['(jobs.animals.shear {:spend {:time 0}}) {:inventory shears :entities [(sheep 1 2)]}]]]
                    (is (= {:event :stopped :kind :limit-reached} (select-keys (await (ending spec world)) [:event :kind])) (str spec))))))))))

(deftest unleashing-ends-with-a-kind-and-a-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed} (select-keys (await (ending '(jobs.animals.unleash {}) {:entities [(cow-at 1 2 {:leashed true :leashedToMe true})]})) [:event])))
        (is (= {:event :stopped :kind :nothing-possible :reason :none}
               (await (ending '(jobs.animals.unleash {}) {:entities [(cow-at 1 2)]}))) "nobody on a lead: 0 freed is not done")
        (is (= {:event :completed} (select-keys (await (ending '(jobs.animals.unleash {:spend {:time 0}}) {:entities [(cow-at 1 2 {:leashed true :leashedToMe true})]})) [:event]))
            "shadow mode: the job does not end itself on the wall clock")))))

(deftest breeding-ends-with-a-kind-and-a-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed} (select-keys (await (ending '(jobs.animals.breed {:mob "cow"}) {:inventory wheat :entities [(cow-at 1 2) (cow-at 2 3)]})) [:event])))
        (is (= {:event :completed} (select-keys (await (ending '(jobs.animals.breed {:mob "cow" :count 6}) {:inventory wheat :entities (mapv #(cow-at % %) [1 2 3 4 5 6])})) [:event]))
            "the food ran out after four: done with the count fed")
        (doseq [[args world kind reason]
                [[{} {:inventory wheat :entities [(cow-at 1 2)]} :condition-failed :too-few]
                 [{:mob "rock"} {:inventory wheat :entities []} :condition-failed :unknown-mob]
                 [{:count 4} {:inventory [{:name "wheat" :count 1}] :entities (mapv #(cow-at % %) [1 2 3 4])} :nothing-possible :food-out]]]
          (is (= {:event :stopped :kind kind :reason reason} (await (ending (list 'jobs.animals.breed (merge {:mob "cow"} args)) world)))))
        (is (= {:event :completed}
               (select-keys (await (ending '(jobs.animals.breed {:mob "cow" :count 4 :least 1}) {:inventory [{:name "wheat" :count 1}] :entities (mapv #(cow-at % %) [1 2 3 4])})) [:event]))
            "one fed meets :least 1")))))

(deftest a-pen-check-answers-closed-as-done-and-a-leaky-pen-as-a-failed-condition
  (let [ground (into {} (for [x (range -10 16) z (range -10 16)] [(str x ",63," z) "stone"]))
        ring (into {} (for [x (range -1 6) z (range -1 6) :when (or (#{-1 5} x) (#{-1 5} z))] [(str x ",64," z) "oak_fence"]))
        w (fn [more] {:self {:pos {:x 2 :y 64 :z 2}} :blocks (merge ground ring more)})]
    (async done
      (tu/run-async done
        (fn ^:async t []
          (is (= {:event :completed} (select-keys (await (ending '(jobs.animals.pen-check {:at [2 64 2]}) (w {}))) [:event])))
          (is (= {:event :stopped :kind :condition-failed :reason :leak}
                 (await (ending '(jobs.animals.pen-check {:at [2 64 2] :max-cells 100}) (w {"2,64,-1" "air"}))))))))))

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
        (is (= {:event :waiting :reason :too-few}
               (await (hunting-cull {:keep 2} {:inventory h/sword :entities [(cow 1 3)]}))))
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
        (is (= {:event :completed} (select-keys (await (ending (list 'jobs.animals.tend {:box pen :target 4}) {:inventory wheat :entities [(cow-at 1 2) (cow-at 2 3)]})) [:event])))
        (is (= {:event :stopped :kind :impossible :reason :no-box}
               (await (ending '(jobs.animals.tend {}) {:entities [(cow-at 1 2)]}))) "no box and no plan: it can never start")
        (is (= {:event :waiting :reason :nothing-to-do}
               (await (ending (list 'jobs.animals.tend {:box pen :target 2}) {:entities [(cow-at 1 2) (cow-at 2 3)]}))))))))

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
                 (await (ending (list 'jobs.animals.lead-to {:mob "cow" :pos far-pos}) (assoc world :entities [(cow-at 1 3 {:trail 9})])))))
          (is (= {:event :completed}
                 (select-keys (await (ending (list 'jobs.animals.lead-to {:mob "cow" :pos far-pos :spend {:time 0}}) world)) [:event]))
              "shadow mode: the job does not end itself on the wall clock"))))))

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
          (is (= {:event :stopped :kind :limit-reached :reason :timeout} (await (lure {:spend {:time 0}} world)))))))))
