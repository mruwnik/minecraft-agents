(ns engine.honest-checks-test
  "A check that declines says why (ctx/wait): the scheduler's job.waiting event carries the reason."
  (:require [cljs.test :refer [deftest is are]]
            [engine.core :as core]
            [engine.hostile-test :as h]))

(defn waiting-reason
  "Submit job with args in an empty world, tick once; the :reason of the job.waiting event, or nil."
  [job args]
  (let [{:keys [eng seen]} (h/setup {})]
    (core/submit! eng (list job args) {})
    (core/tick! eng)
    (some #(when (= :waiting (:kind %)) (:reason %)) @seen)))

(def box {:min {:x 0 :y 60 :z -5} :max {:x 8 :y 70 :z 5}})

(deftest animal-checks-say-why-they-decline
  (are [job args reason] (= reason (waiting-reason job args))
    'jobs.animals.pen-check {} :no-pen
    'jobs.animals.tend {:target 4} :no-box
    'jobs.animals.tend {:box box :target 4} :nothing-to-do
    'jobs.animals.herd {:target 4} :no-mob-or-box
    'jobs.combat.attack {:targets ["zombie"] :absent :wait} :no-target))

(deftest apiary-checks-say-why-they-decline
  (are [job reason] (= reason (waiting-reason job {}))
    'jobs.apiary.maintain :nothing-to-do
    'jobs.apiary.guard :nothing-to-guard))

(deftest build-checks-say-why-they-decline
  (are [job] (= :plan-trouble (waiting-reason job {:plan "no-such-plan"}))
    'jobs.build.from-plan
    'jobs.build.pen
    'jobs.build.rail-line))
