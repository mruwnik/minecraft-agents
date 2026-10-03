(ns engine.registry-test
  (:require [cljs.test :refer [deftest is]]
            [engine.registry :as registry]))

(def migrated
  '#{jobs.survival.breathe jobs.survival.eat jobs.survival.extinguish jobs.survival.retreat jobs.survival.sleep jobs.survival.recover jobs.survival.respond-to-hostile jobs.survival.fight-back jobs.survival.get-food
     jobs.forestry.fell-tree jobs.forestry.collect-drops jobs.forestry.plant-sapling
     jobs.forestry.harvest-wood jobs.storage.deposit
     jobs.movement.go-to jobs.movement.pace jobs.movement.look-around
     jobs.time.wait-for-day})

(deftest the-registry-holds-every-job-namespace
  (is (= migrated (set (keys registry/jobs)))))

(deftest every-job-exports-check-and-round-and-a-doc
  (doseq [[sym job] registry/jobs]
    (is (fn? (:check job)) (str sym))
    (is (fn? (:round job)) (str sym))
    (is (string? (:doc job)) (str sym))))

(deftest args-carry-docs-and-defaults
  (let [args (get-in registry/jobs ['jobs.forestry.fell-tree :args])]
    (is (= 16 (get-in args [:radius :default])))
    (is (string? (get-in args [:radius :doc]))))
  (is (nil? (get-in registry/jobs ['jobs.survival.sleep :args])) "args is optional"))
