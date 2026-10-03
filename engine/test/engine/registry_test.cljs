(ns engine.registry-test
  (:require [cljs.test :refer [deftest is]]
            [engine.registry :as registry]))

(def migrated
  '#{jobs.survival.breathe jobs.survival.extinguish jobs.survival.recover
     jobs.survival.respond-to-hostile jobs.survival.fight-back jobs.survival.retreat
     jobs.survival.eat jobs.survival.get-food
     jobs.survival.sleep jobs.survival.shelter jobs.survival.dig-in jobs.survival.log-out
     jobs.combat.attack jobs.animals.breed jobs.animals.shear jobs.animals.cull jobs.animals.tend jobs.survival.recover-drops jobs.maintenance.unstick jobs.storage.make-room
     jobs.forestry.fell-tree jobs.forestry.collect-drops jobs.forestry.plant-sapling
     jobs.forestry.harvest-wood jobs.storage.deposit jobs.storage.withdraw jobs.storage.kit jobs.items.craft jobs.items.give jobs.items.bake jobs.farm.till jobs.farm.fertilize jobs.farm.compost jobs.build.clear-box jobs.farm.find-spot jobs.farm.harvest
     jobs.apiary.harvest
     jobs.apiary.guard
     jobs.combat.hunt jobs.gather.get-seeds jobs.gather.mine
     jobs.movement.go-to jobs.movement.pace jobs.movement.look-around jobs.movement.follow
     jobs.time.wait-for-day jobs.debug.notify})

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
  (is (nil? (get-in registry/jobs ['jobs.time.wait-for-day :args])) "args is optional"))
