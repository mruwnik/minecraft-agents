(ns engine.kit-contract-test
  "jobs.items.get-kit, jobs.storage.kit, jobs.items.kitchen and jobs.items.bake as contract jobs: they export the
  contract and end with a kind and a cause."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.withdraw-test :as wt]))

(def args
  {'jobs.items.get-kit {:items ["iron_pickaxe"]}
   'jobs.storage.kit {:chest {:x 10 :y 64 :z 0} :tools ["hoe"]}
   'jobs.items.kitchen {:chest {:x 10 :y 64 :z 0} :furnace {:x 1 :y 64 :z 0}}
   'jobs.items.bake {:chest {:x 10 :y 64 :z 0}}})

(deftest every-kit-job-exports-the-contract-and-quotes-validly
  (let [p (tu/fake {})]
    (doseq [[sym a] args
            :let [job (get registry/jobs sym)]]
      (is (vector? (:conditions job)) (str sym " has conditions"))
      (is (nil? (:check job)) (str sym " has no old check"))
      (is (map? (:defaults job)) (str sym " has defaults"))
      (is (fn? (:quote-of job)) (str sym " has quote-of"))
      (is (nil? (contract/quote-problem ((:quote-of job) {:primitives p :args a :view (fn [] {:data {} :now 0})}))) (str sym)))
    (doseq [sym '[jobs.items.get-kit jobs.storage.kit jobs.items.bake]
            :let [job (get registry/jobs sym)]]
      (is (vector? (:target job)) (str sym " has a target"))
      (is (fn? (:done? job)) (str sym " has done?")))))

(deftest a-listed-kit-without-a-chest-ends-condition-failed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [sym '[jobs.storage.kit jobs.items.bake]]
          (let [{:keys [eng]} (wt/setup {:containers {"10,64,0" []}})
                e (await (wt/ending-of eng sym {} 6))]
            (is (= {:kind :condition-failed :cause {:reason :no-chest}} (select-keys e [:kind :cause])) (str sym))))))))
