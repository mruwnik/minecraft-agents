(ns engine.honest-checks-test
  "A check that declines says why (ctx/wait): the scheduler's job.waiting event carries the reason."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.expr :as expr]
            [engine.registry :as registry]
            [engine.build-from-plan-test :as b]
            [engine.harvest-test :as ht]
            [engine.hostile-test :as h]
            [engine.pen-build-test :as pen]
            [engine.test-util :as tu]))

(defn waiting-reason
  "Submit job with args in an empty world (or the given one), tick once; the :reason of the job.waiting event, or nil."
  ([job args] (waiting-reason job args {}))
  ([job args world]
  (let [{:keys [eng seen]} (h/setup world)]
    (core/submit! eng (list job args) {})
    (core/tick! eng)
    (some #(when (= :waiting (:kind %)) (:reason %)) @seen))))

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
    'jobs.animals.leash {:mob "cow" :fetch false} :no-lead
    'jobs.animals.breed {:mob "cow" :fetch false} :no-food
    'jobs.combat.attack {:targets ["zombie"] :absent :wait} :no-target))

(deftest build-checks-say-why-they-decline
  (are [job reason] (= reason (waiting-reason job {:plan "no-such-plan"}))
    'jobs.build.pen :plan-trouble
    'jobs.build.rail-line :plan))

(deftest plan-trouble-waits-carry-the-text
  (are [job] (= "no such plan" (:why (first (waiting-event job {:plan "no-such-plan"}))))
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
    'jobs.build.pen {:plan "pen" :fetch false} {:inventory []} {"pen" (pen/ring-plan)} :no-items))

(defn ^:async ending
  "Submit job with args in an empty world, tick once: whether it is still listed, and its root ending event's kind and
  :reason."
  [job args]
  (let [{:keys [eng seen]} (h/setup {})]
    (core/submit! eng (list job args) {})
    (await (core/tick! eng))
    (merge {:listed (boolean (seq (:list (core/state eng))))}
           (some #(when (#{:completed :stopped} (:kind %)) {:event (:kind %) :reason (:reason %)}) @seen))))

(deftest log-out-says-why-it-declines
  (is (= :not-night (waiting-reason 'jobs.survival.log-out {}))))

(deftest log-out-forbidden-by-its-args-is-impossible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:listed false :event :stopped :reason :offline-forbidden}
               (await (ending 'jobs.survival.log-out {:offline-allowed false}))))))))

(deftest a-goal-that-already-holds-is-done-not-a-wait
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [job ['jobs.survival.breathe 'jobs.survival.eat 'jobs.survival.extinguish 'jobs.survival.get-food
                     'jobs.survival.block-arrow-gap 'jobs.survival.restore-broken 'jobs.survival.unwedge]]
          (is (= {:listed false :event :completed} (select-keys (await (ending job {})) [:listed :event])) (str job)))))))

(deftest access-farm-forestry-storage-checks-say-why-they-decline
  (are [job args reason] (= reason (waiting-reason job args))
    'jobs.farm.harvest {:area {:plan "nope"}} :plan-trouble
    'jobs.farm.plant {:area {:plan "nope"}} :plan-trouble
    'jobs.farm.tend {:area {:plan "nope"}} :plan-trouble
    'jobs.farm.tend {:area box} :nothing-to-do
    'jobs.farm.tidy {:plan "nope"} :plan-trouble
    'jobs.forestry.maintain {:plan "nope"} :plan-trouble
    'jobs.forestry.prepare {:plan "nope"} :plan-trouble))

(deftest survival-and-maintenance-checks-say-why-they-decline
  (are [job args reason] (= reason (waiting-reason job args))
    'jobs.survival.night {} :not-night
    'jobs.survival.recover-drops {} :no-drops))

(deftest a-goal-that-already-holds-passes-the-check
  (are [job] (nil? (waiting-reason job {}))
    'jobs.movement.leave-vehicle
    'jobs.survival.breathe
    'jobs.survival.extinguish
    'jobs.combat.attack
    'jobs.survival.unwedge))

(deftest a-blank-or-non-string-item-is-refused-at-submit
  (are [job item] (re-find #":item must be an item name \(non-empty string\)" (expr/problem registry/jobs (list job {:item item})))
    'jobs.items.enchant ""
    'jobs.items.enchant "  "
    'jobs.items.equip 5))
