(ns engine.registry-test
  (:require [cljs.test :refer [deftest is]]
            [engine.expr :as expr]
            [engine.registry :as registry]
            [engine.triggers :as triggers]))

(def migrated
  '#{jobs.survival.breathe jobs.survival.unwedge jobs.survival.extinguish
     jobs.survival.respond-to-hostile jobs.survival.fight-back jobs.survival.retreat
     jobs.survival.eat jobs.survival.get-food
     jobs.survival.sleep jobs.survival.night jobs.survival.dig-in jobs.survival.dig-niche jobs.survival.log-out
     jobs.combat.attack jobs.animals.breed jobs.animals.shear jobs.animals.cull jobs.animals.tend jobs.animals.pen-check jobs.animals.shut-gate jobs.animals.leash jobs.animals.unleash jobs.animals.lead-to jobs.survival.recover-drops jobs.survival.restore-broken jobs.maintenance.shut-doors jobs.storage.make-room
     jobs.animals.herd
     jobs.forestry.fell-tree jobs.forestry.collect-drops jobs.forestry.plant-sapling
     jobs.forestry.harvest-wood jobs.forestry.maintain jobs.forestry.prepare jobs.storage.deposit jobs.storage.withdraw jobs.storage.kit jobs.items.craft jobs.items.give jobs.items.wear jobs.items.equip jobs.items.bake jobs.farm.till jobs.farm.fertilize jobs.farm.compost jobs.build.clear-box jobs.build.path jobs.farm.find-spot jobs.farm.harvest jobs.farm.plant jobs.farm.tend
     jobs.apiary.harvest
     jobs.apiary.guard
     jobs.apiary.maintain
     jobs.village.trade
     jobs.village.feed jobs.village.breed
     jobs.items.smelt
     jobs.items.enchant jobs.items.obtain jobs.items.get-tool jobs.items.fetch-limits
     jobs.build.from-plan jobs.build.pen jobs.build.rail-line
     jobs.access.pillar
     jobs.explore.search jobs.explore.look
     jobs.combat.hunt jobs.gather.get-seeds jobs.gather.mine
     jobs.movement.go-to jobs.movement.path-preview jobs.movement.look-around jobs.movement.follow jobs.movement.leave-vehicle jobs.movement.mount jobs.movement.linger-near jobs.survival.block-arrow-gap
     jobs.time.wait-for-day jobs.time.wait-for-dusk jobs.debug.notify jobs.debug.walk-plan jobs.debug.access-check
     jobs.access.stair jobs.access.tunnel jobs.access.toggle jobs.farm.tidy
     jobs.access.cleanup jobs.access.leave-tunnel jobs.access.clear-path
     jobs.blocks.dig jobs.blocks.place jobs.blocks.use-on jobs.items.interact
     jobs.memory.set-place jobs.memory.forget-place jobs.memory.remember})

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

(deftest real-jobs-refuse-unknown-arg-keys
  (let [msg (expr/problem registry/jobs '(jobs.movement.go-to {:target [50 40 3]}))]
    (is (re-find #"jobs.movement.go-to has no arg :target; its args are " msg))
    (is (re-find #":pos" msg))
    (is (re-find #":doors" msg)))
  (is (re-find #"jobs.time.wait-for-dusk has no arg :bogus; it takes no args"
               (expr/problem registry/jobs '(jobs.time.wait-for-dusk {:bogus 1}))))
  (is (nil? (expr/problem registry/jobs '(jobs.movement.go-to {:pos [50 40 3]})))))

(deftest the-trigger-registry-holds-the-default-trigger-set
  (is (= #{:suffocating :burning :wedged :hostile-near :hungry :night :died :pen-gate :door-left
           :inventory-nearly-full :scaffold-left :tidy-pending :mounted}
         (set (keys triggers/all))))
  (doseq [[id t] triggers/all]
    (is (= id (:name t)))
    (is (fn? (:when t)) (str id))
    (is (seq? (:job t)) (str id))
    (is (map? (:args t)) (str id))
    (is (#{:cooldown :retry :stop} (:persistence t)) (str id))))

(deftest trigger-lines-keep-their-defaults
  (is (= {:name :scaffold-left :job '(jobs.access.cleanup) :args {} :persistence :stop}
         (dissoc (:scaffold-left triggers/all) :when))
      "a line without :cooldown-s has none"))

(deftest mounted-never-latches-after-a-failed-dismount
  (let [t (:mounted triggers/all)]
    (is (= :cooldown (:persistence t)) "a body still aboard fires again after the cooldown, not only after being off once")
    (is (pos? (:cooldown-s t)))))

(deftest the-facts-table-is-the-one-the-defaults-name
  (is (contains? triggers/facts 'health))
  (is (not (contains? triggers/facts 'stuck))))

(deftest numeric-args-are-bounded-at-submit
  (doseq [form ['(jobs.farm.find-spot {:w 1000})
                '(jobs.farm.find-spot {:h 0})
                '(jobs.farm.find-spot {:range 1000})
                '(jobs.farm.find-spot {:depth 1000000})
                '(jobs.farm.find-spot {:limit 0})
                '(jobs.movement.go-to {:pos [1 64 1] :leg-s 0})
                '(jobs.movement.go-to {:pos [1 64 1] :leg-s 500})
                '(jobs.survival.dig-niche {:reach 100000})
                '(jobs.survival.dig-niche {:reach 0})
                '(jobs.survival.dig-niche {:roof-height 100000})
                '(jobs.farm.till {:center [0 64 0] :radius 1000000})
                '(jobs.farm.find-spot {:w 17})
                '(jobs.farm.find-spot {:range 49})
                '(jobs.farm.find-spot {:depth 17})
                '(jobs.farm.find-spot {:limit 9})
                '(jobs.movement.go-to {:pos [1 64 1] :leg-s 0.05})
                '(jobs.movement.go-to {:pos [1 64 1] :leg-s 120.1})
                '(jobs.survival.dig-niche {:reach 33})
                '(jobs.farm.till {:center [0 64 0] :radius 8})
                '(jobs.farm.till {:center [0 64 0] :radius -1})]]
    (is (string? (expr/problem registry/jobs form)) (pr-str form)))
  (doseq [form ['(jobs.farm.find-spot {:w 16 :h 16 :range 48 :depth 16 :limit 8})
                '(jobs.movement.go-to {:pos [1 64 1] :leg-s 5})
                '(jobs.survival.dig-niche {:reach 16 :roof-height 4})
                '(jobs.farm.till {:center [0 64 0] :radius 7})
                '(jobs.farm.find-spot {:limit 1})
                '(jobs.movement.go-to {:pos [1 64 1] :leg-s 0.1})
                '(jobs.movement.go-to {:pos [1 64 1] :leg-s 120})
                '(jobs.survival.dig-niche {:reach 32})
                '(jobs.farm.till {:center [0 64 0] :radius 0})]]
    (is (nil? (expr/problem registry/jobs form)) (pr-str form))))
