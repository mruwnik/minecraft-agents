(ns engine.village-contract-test
  "village.feed, village.trade and village.roll as contract jobs: they export the contract, quote validly, and end with a
  kind and a cause (a partial run is never done)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.registry :as registry]
            [engine.seek-contract-test :as seek]
            [engine.test-util :as tu]
            [engine.village-trade-test :as vt]))

(defn job [sym] (get registry/jobs sym))

(deftest each-job-exports-the-contract-and-quotes-validly
  (doseq [[sym args] [['jobs.village.breed {:target 3}]
                      ['jobs.village.feed {:villager "v-1" :count 2}]
                      ['jobs.village.trade {:villager "v-1" :buy "bread" :count 2}]
                      ['jobs.village.roll {:villager "v-1" :pos [2 64 0] :item "lectern" :want "enchanted_book"}]]]
    (is (some? (:conditions (job sym))) (str sym " has conditions"))
    (is (nil? (:check (job sym))) (str sym " has no old check"))
    (is (vector? (:target (job sym))) (str sym " has a target"))
    (is (some? (:done? (job sym))) (str sym " has done?"))
    (is (nil? (contract/quote-problem ((:quote-of (job sym)) {:primitives (tu/fake {}) :args args :quote-call {:spend {:time 60}}}))) (str sym))))

(def bread [{:name "bread" :count 5}])

(defn villager [offers & [extra]]
  (merge {:id 1 :name "villager" :kind "passive" :uuid "v-1" :pos {:x 1 :y 64 :z 0} :profession "farmer" :level 2 :offers offers} extra))

(def bread-offer {:cost [{:item "emerald" :count 1}] :gives {:item "bread" :count 1}})

(defn ^:async ends
  [spec world]
  (await (seek/ending-of #(tu/fake-on-floor world) spec)))

(deftest feed-ends-with-a-kind-and-a-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :stopped :kind :condition-failed :reason :gone}
               (await (ends '(jobs.village.feed {:villager "v-1"}) {:inventory bread}))))
        (is (= {:event :waiting :reason :need}
               (await (ends '(jobs.village.feed {:villager "v-1" :fetch false}) {:entities [(villager [])]}))))
        (is (= {:event :waiting :reason :bad-args}
               (await (ends '(jobs.village.feed {:villager "v-1" :item "diamond"}) {:inventory bread}))))))))

(deftest breed-start-facts-are-conditions
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :stopped :kind :impossible :reason :bad-args}
               (await (ends '(jobs.village.breed {}) {}))))
        (is (= {:event :waiting :reason :no-pair}
               (await (ends '(jobs.village.breed {:target 3}) {:entities [(villager [])] :inventory bread}))))
        (is (= {:event :waiting :reason :no-food}
               (await (ends '(jobs.village.breed {:target 3 :fetch false})
                             {:entities [(villager [] {:uuid "v-1" :pos {:x 3 :y 64 :z 0}}) (villager [] {:id 2 :uuid "v-2" :pos {:x 5 :y 64 :z 0}})]}))))))))

(deftest trade-ends-with-a-kind-and-a-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed}
               (select-keys (await (ends '(jobs.village.trade {:villager "v-1" :buy "bread"})
                                         {:entities [(villager [bread-offer])] :inventory [{:name "emerald" :count 3}]}))
                            [:event])))
        (is (= {:event :stopped :kind :condition-failed :reason :payment-short}
               (await (ends '(jobs.village.trade {:villager "v-1" :buy "bread"}) {:entities [(villager [bread-offer])]}))))
        (is (= {:event :stopped :kind :condition-failed :reason :gone}
               (await (ends '(jobs.village.trade {:villager "v-1" :buy "bread"}) {}))))
        (is (= {:event :stopped :kind :condition-failed :reason :no-room}
               (await (ends '(jobs.village.trade {:villager "v-1" :buy "bread"})
                             {:entities [(villager [bread-offer])] :inventory (into [{:name "emerald" :count 5}] (vt/fillers 35))}))))
        (is (= {:event :completed}
               (select-keys (await (ends '(jobs.village.trade {:villager "v-1" :buy "bread" :count 3 :least 1})
                                         {:entities [(villager [bread-offer])] :inventory [{:name "emerald" :count 1}]}))
                            [:event])))
        (is (= {:event :stopped :kind :nothing-possible :reason :no-offer}
               (await (ends '(jobs.village.trade {:villager "v-1" :buy "apple"}) {:entities [(villager [bread-offer])]}))))
        (is (= {:event :waiting :reason :bad-args}
               (await (ends '(jobs.village.trade {:villager "v-1"}) {}))))))))

(def roll-args {:villager "v-1" :pos [2 64 0] :item "lectern" :want "enchanted_book"})
(def librarian {:profession "librarian"})
(def lectern-world {:blocks {"2,64,0" "lectern" "2,63,0" "stone"}})

(deftest roll-start-facts-are-conditions
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[extra world expected]
                [[{:profession "nitwit"} nil {:event :stopped :kind :impossible :reason :nitwit}]
                 [{:baby true} nil {:event :waiting :reason :baby}]
                 [librarian {:blocks {"2,64,0" "stone" "2,63,0" "stone"}} {:event :waiting :reason :occupied}]
                 [{:uses 1} nil {:event :stopped :kind :condition-failed :reason :locked}]]]
          (is (= expected
                 (await (ends (list 'jobs.village.roll roll-args)
                              (merge lectern-world world
                                     {:entities [(villager [(assoc bread-offer :uses (:uses extra 0))] (dissoc (merge librarian extra) :uses))]}))))
              (str extra world)))
        (is (= {:event :stopped :kind :condition-failed :reason :gone}
               (await (ends (list 'jobs.village.roll roll-args) lectern-world))))))))

(deftest roll-waits-on-bad-args
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :waiting :reason :bad-args}
               (await (ends '(jobs.village.roll {:villager "v-1" :pos [2 64 0] :want "enchanted_book"}) {}))))))))
