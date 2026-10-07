(ns engine.honest-checks-test
  "A check that declines says why (ctx/wait): the scheduler's job.waiting event carries the reason."
  (:require [cljs.test :refer [deftest is are]]
            [engine.core :as core]
            [engine.build-from-plan-test :as b]
            [engine.harvest-test :as ht]
            [engine.hostile-test :as h]
            [engine.pen-build-test :as pen]))

(defn waiting-reason
  "Submit job with args in an empty world, tick once; the :reason of the job.waiting event, or nil."
  [job args]
  (let [{:keys [eng seen]} (h/setup {})]
    (core/submit! eng (list job args) {})
    (core/tick! eng)
    (some #(when (= :waiting (:kind %)) (:reason %)) @seen)))

(defn waiting-event
  "Like waiting-reason: the job.waiting events' fields, as a vector."
  [job args]
  (let [{:keys [eng seen]} (h/setup {})]
    (core/submit! eng (list job args) {})
    (core/tick! eng)
    (filterv #(= :waiting (:kind %)) @seen)))

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

(deftest plan-trouble-waits-carry-the-text
  (are [job] (= "no such plan" (:why (first (waiting-event job {:plan "no-such-plan"}))))
    'jobs.build.from-plan
    'jobs.build.pen
    'jobs.build.rail-line))

(defn waits-in-world
  "Submit job with args over the fake world spec and plans, tick a few times: the job.waiting events."
  [job args spec plans]
  (let [{:keys [eng seen]} (b/start spec plans [])]
    (core/submit! eng (list job args) {})
    (dotimes [_ 3] (swap! ht/clock + 700) (core/tick! eng))
    (filterv #(= :waiting (:kind %)) @seen)))

(deftest world-checks-say-why-they-decline
  (are [job args spec plans reason] (= [reason] (mapv :reason (waits-in-world job args spec plans)))
    'jobs.build.from-plan {:plan "pen" :fetch false} {:inventory []} {"pen" (b/pen-plan)} :no-items
    'jobs.build.pen {:plan "pen"} (pen/spec pen/kit pen/built-pen) {"pen" (pen/ring-plan)} :already-sound
    'jobs.animals.herd {:mob "cow" :box box :target 0} {} {} :at-target))
