(ns engine.submitted-test
  "The :submitted-at call key (engine.core.ending/moved-or) in the jobs that take a position-bound call (stair has its own cases in stair-test)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.job-api :as job-api]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as world]))

(deftest a-job-submitted-far-from-where-the-body-stands-waits-moved-since-submit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [spec ['(jobs.gather.mine {:block "coal_ore" :submitted-at {:x 80 :y 65 :z 0}})
                      '(jobs.access.bridge {:heading :east :length 3 :submitted-at {:x 80 :y 65 :z 0}})
                      '(jobs.access.pillar {:height 2 :submitted-at {:x 80 :y 65 :z 0}})]]
          (let [clock (atom 1000000)
                p (tu/fake {:self {:pos {:x 0 :y 65 :z 0}} :inventory [{:name "iron_pickaxe" :count 1} {:name "dirt" :count 8}]})
                eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)
                                  :now #(deref clock) :world (world/of-data {} {} [])})
                id (core/submit! eng spec {})]
            (await (tu/run-ticks eng clock 3 500))
            (is (= :moved-since-submit (:reason (:waiting (job-api/summary eng id)))) (pr-str spec))))))))
