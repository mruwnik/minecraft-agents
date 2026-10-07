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

(deftest bad-argument-checks-say-why-they-decline
  (are [job reason] (= reason (waiting-reason job {}))
    'jobs.items.bake :no-chest
    'jobs.items.craft :bad-args
    'jobs.items.enchant :bad-args
    'jobs.items.give :bad-args
    'jobs.village.trade :bad-args
    'jobs.village.feed :bad-args
    'jobs.village.breed :bad-args
    'jobs.movement.follow :bad-args
    'jobs.movement.linger-near :bad-args))

(deftest log-out-says-why-it-declines
  (are [args reason] (= reason (waiting-reason 'jobs.survival.log-out args))
    {} :not-night
    {:offline-allowed false} :offline-forbidden))

(deftest access-farm-forestry-storage-checks-say-why-they-decline
  (are [job args reason] (= reason (waiting-reason job args))
    'jobs.access.cleanup {} :nothing-to-do
    'jobs.farm.harvest {:plan "nope"} :plan-trouble
    'jobs.farm.plant {:plan "nope"} :plan-trouble
    'jobs.farm.tend {:plan "nope"} :plan-trouble
    'jobs.farm.tend {:box box} :nothing-to-do
    'jobs.farm.tidy {:plan "nope"} :plan-trouble
    'jobs.forestry.maintain {:plan "nope"} :plan-trouble
    'jobs.forestry.prepare {:plan "nope"} :plan-trouble
    'jobs.storage.kit {} :no-chest
    'jobs.storage.make-room {:free 1} :enough-room))

(deftest wear-and-fight-back-say-why-they-decline
  (are [job args reason] (= reason (waiting-reason job args))
    'jobs.items.wear {:item 5} :bad-args
    'jobs.survival.fight-back {} :no-target))

(deftest survival-and-maintenance-checks-say-why-they-decline
  (are [job args reason] (= reason (waiting-reason job args))
    'jobs.build.clear-box {} :bad-args
    'jobs.gather.mine {} :no-block
    'jobs.movement.leave-vehicle {} :not-mounted
    'jobs.survival.breathe {} :not-underwater
    'jobs.survival.extinguish {} :not-burning
    'jobs.survival.get-food {} :not-hungry
    'jobs.survival.night {} :not-night
    'jobs.survival.recover-drops {} :no-drops
    'jobs.survival.respond-to-hostile {} :no-hostile
    'jobs.survival.restore-broken {} :nothing-to-restore
    'jobs.survival.unwedge {} :not-wedged))

(deftest enchant-refuses-a-blank-item-name-with-a-why
  (are [item] (= [[:bad-args "no item name"]] (mapv (juxt :reason :why) (waiting-event 'jobs.items.enchant {:item item})))
    ""
    "  "))
