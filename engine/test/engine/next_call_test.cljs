(ns engine.next-call-test
  "jobs.lib.next-call: the calls an ending names for the agent."
  (:require [cljs.test :refer [deftest is]]
            [engine.contract :as contract]
            [jobs.lib.next-call :as next-call]))

(def defs {:crop {:default "wheat"} :w {:default 3}})

(deftest a-fetch-that-failed-down-the-chain-is-named-then-the-job-again-with-its-own-args
  (let [fetch (contract/ending :condition-failed {:reason :tool :kind "hoe"} nil)
        e (contract/with-because (contract/ending :nothing-possible :no-hoe {:got 0}) fetch)]
    (is (= [{:job "jobs.items.get-tool" :args {:kind "hoe" :explore true}}
            {:job "jobs.farm.grow" :args {:w 5} :again true}]
           (:next (:data (next-call/on-ending {:args {:crop "wheat" :w 5 :submitted-at {:x 0}}} e 'jobs.farm.grow defs)))))))

(deftest a-childs-calls-pass-up-its-resubmit-becoming-a-plain-call
  (let [child (contract/ending :nothing-possible :no-source
                               {:next [{:job "jobs.gather.mine" :args {:item "raw_iron"}}
                                       {:job "jobs.items.obtain" :args {:item "iron_ingot"} :again true}]})]
    (is (= [{:job "jobs.gather.mine" :args {:item "raw_iron"}}
            {:job "jobs.items.obtain" :args {:item "iron_ingot"}}
            {:job "jobs.farm.grow" :args {} :again true}]
           (:next (:data (next-call/on-ending {:args {}} child 'jobs.farm.grow defs)))))))

(deftest done-carries-no-next
  (is (nil? (:next (:data (next-call/on-ending {:args {}} (contract/ending :done nil {:got 1}) 'jobs.farm.grow defs))))))
