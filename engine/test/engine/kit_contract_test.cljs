(ns engine.kit-contract-test
  "jobs.items.get-kit and jobs.items.kitchen as contract jobs: they export the contract."
  (:require [cljs.test :refer [deftest is]]
            [engine.contract :as contract]
            [engine.registry :as registry]
            [engine.test-util :as tu]))

(def args
  {'jobs.items.get-kit {:items ["iron_pickaxe"]}
   'jobs.items.kitchen {:chest {:x 10 :y 64 :z 0} :furnace {:x 1 :y 64 :z 0}}})

(deftest every-kit-job-exports-the-contract-and-quotes-validly
  (let [p (tu/fake {})]
    (doseq [[sym a] args
            :let [job (get registry/jobs sym)]]
      (is (fn? (:can-start? job)) (str sym " has can-start?"))
      (is (map? (:defaults job)) (str sym " has defaults"))
      (is (fn? (:quote-of job)) (str sym " has quote-of"))
      (is (nil? (contract/quote-problem ((:quote-of job) {:primitives p :args a :view (fn [] {:data {} :now 0})}))) (str sym)))
    (doseq [sym '[jobs.items.get-kit]
            :let [job (get registry/jobs sym)]]
      (is (vector? (:target job)) (str sym " has a target"))
      (is (fn? (:done? job)) (str sym " has done?")))))
