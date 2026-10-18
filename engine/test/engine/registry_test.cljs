(ns engine.registry-test
  (:require-macros [engine.registry-test-macros :refer [describe-message describes-job?]])
  (:require [clojure.string :as str]
            [cljs.test :refer [are deftest is]]
            [engine.args :as args]
            [engine.expr :as expr]
            [engine.primitive :as primitive]
            [engine.registry :as registry]
            [engine.triggers :as triggers]
            [jobs.lib.choose :as choose]))

(def migrated
  '#{jobs.survival.breathe jobs.survival.unwedge jobs.survival.extinguish
     jobs.survival.cure jobs.survival.respond-to-hostile jobs.survival.fight-back jobs.survival.retreat
     jobs.food.fish jobs.survival.eat jobs.survival.get-food jobs.survival.go-home
     jobs.survival.sleep jobs.survival.set-spawn jobs.survival.night jobs.survival.bed-down jobs.survival.dig-in jobs.survival.plug-in jobs.survival.plug-walls jobs.survival.plug-pit jobs.survival.plug-pocket jobs.survival.dig-niche jobs.survival.log-out
     jobs.combat.attack jobs.animals.breed jobs.animals.shear jobs.animals.cull jobs.animals.tend jobs.animals.pen-check jobs.animals.shut-gate jobs.animals.leash jobs.animals.unleash jobs.animals.lead-to jobs.animals.lure-to jobs.survival.recover-drops jobs.survival.restore-broken jobs.maintenance.shut-doors jobs.storage.make-room
     jobs.animals.herd
     jobs.forestry.fell-tree jobs.forestry.fell-from-pillar jobs.items.collect-drops jobs.forestry.plant-sapling jobs.forestry.tidy
     jobs.forestry.harvest-wood jobs.forestry.maintain jobs.forestry.prepare jobs.storage.deposit jobs.storage.withdraw jobs.storage.withdraw-entity jobs.storage.haul jobs.strip.loot-cart jobs.storage.kit jobs.items.craft jobs.items.give jobs.items.pick-up-loose jobs.items.wear jobs.items.equip jobs.items.kitchen jobs.farm.till jobs.farm.irrigate jobs.farm.fertilize jobs.farm.compost jobs.build.clear-box jobs.build.path jobs.farm.find-spot jobs.farm.harvest jobs.farm.plant jobs.farm.tend
     jobs.apiary.harvest
     jobs.apiary.guard
     jobs.apiary.maintain
     jobs.village.trade
     jobs.village.feed jobs.village.breed jobs.village.maintain jobs.village.roll
     jobs.items.smelt
     jobs.items.enchant jobs.items.obtain jobs.items.get-tool jobs.items.get-kit
     jobs.build.from-plan jobs.build.pen jobs.build.rail jobs.build.rail-line
     jobs.access.pillar jobs.access.bridge
     jobs.explore.search jobs.explore.look
     jobs.combat.hunt jobs.gather.get-seeds jobs.gather.make-obsidian jobs.gather.mine
     jobs.movement.go-to jobs.movement.look-around jobs.movement.follow jobs.movement.leave-vehicle jobs.movement.mount jobs.movement.boat-launch jobs.movement.boat-drive jobs.movement.boat-land jobs.movement.cross-water jobs.movement.linger-near jobs.survival.block-arrow-gap
     jobs.time.wait-for-day jobs.time.wait-for-dusk jobs.time.noop jobs.debug.notify jobs.debug.ready-probe
     jobs.access.stair jobs.access.tunnel jobs.access.toggle jobs.farm.tidy
     jobs.access.cleanup jobs.access.leave-tunnel jobs.access.clear-path jobs.strip.seal jobs.strip.corridor jobs.strip.site
     jobs.blocks.dig jobs.blocks.fill-cells jobs.blocks.place jobs.blocks.use-on jobs.items.interact
     jobs.memory.set-place jobs.memory.forget-place jobs.memory.remember jobs.memory.reserve jobs.memory.unreserve})

(def primitive-names
  "The table's keys as job names: jobs.primitive.<kebab-case>."
  (set (map #(symbol (str "jobs.primitive." (str/replace (name %) #"[A-Z]" (fn [c] (str "-" (str/lower-case c)))))) (keys primitive/table))))

(deftest the-registry-holds-every-job-namespace
  (is (= migrated (set (remove primitive-names (keys registry/jobs))))))

(deftest every-primitive-is-a-job-by-name
  (is (= 29 (count primitive-names)))
  (is (every? #(contains? registry/jobs %) primitive-names))
  (is (contains? registry/jobs 'jobs.primitive.move-to))
  (is (= [:pos] (get-in registry/jobs ['jobs.primitive.dig :target]))))

(deftest a-job-declares-its-category-or-gets-the-default-of-its-group
  (are [sym category part-of] (= [category part-of] ((juxt :category :part-of) (get registry/jobs sym)))
    'jobs.movement.go-to :goal []
    'jobs.primitive.dig :phase []
    'jobs.debug.notify :debug []
    'jobs.survival.night :reflex []
    'jobs.survival.bed-down :phase '[jobs.survival.night]))

(deftest a-job-declares-its-tags
  (is (= [:mine :dig :ore :light] (:tags (get registry/jobs 'jobs.gather.mine))))
  (is (= [:food] (:tags (get registry/jobs 'jobs.survival.get-food))))
  (is (= [] (:tags (get registry/jobs 'jobs.movement.go-to)))))

(deftest a-job-declares-it-is-position-bound
  (is (= #{'jobs.access.stair 'jobs.access.bridge 'jobs.access.pillar 'jobs.gather.mine}
         (set (keep (fn [[id e]] (when (:position-bound? e) id)) registry/jobs)))))

(deftest a-primitive-job-refuses-unknown-arg-keys
  (is (re-find #"jobs.primitive.dig has no arg :bogus; its args are .*:pos"
               (expr/problem registry/jobs '(jobs.primitive.dig {:bogus 1}))))
  (is (nil? (expr/problem registry/jobs '(jobs.primitive.dig {:pos [1 2 3] :timeoutS 5})))))

(deftest every-job-exports-checks-and-round-and-a-doc
  (doseq [[sym job] registry/jobs]
    (is (nil? (:check job)) (str sym))
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
  (is (= #{:suffocating :burning :wedged :hostile-near :poisoned :hungry :needs-bed :needs-weapon :night :dusk-home :died :pen-gate :door-left
           :inventory-nearly-full :scaffold-left :tidy-pending :mounted :loose-items}
         (set (keys triggers/all))))
  (doseq [[id t] triggers/all]
    (is (= id (:name t)))
    (is (fn? (:when t)) (str id))
    (is (seq? (:job t)) (str id))
    (is (map? (:args t)) (str id))
    (is (#{:cooldown :retry :stop} (:persistence t)) (str id))))

(deftest default-trigger-jobs-are-valid
  (doseq [[id t] triggers/all]
    (is (nil? (expr/problem registry/jobs (:job t))) (str id))))

(deftest mounted-never-latches-after-a-failed-dismount
  (let [t (:mounted triggers/all)]
    (is (= :cooldown (:persistence t)) "a body still aboard fires again after the cooldown, not only after being off once")
    (is (pos? (:cooldown-s t)))))

(deftest numeric-args-are-bounded-at-submit
  (doseq [form ['(jobs.farm.find-spot {:w 1000})
                '(jobs.farm.find-spot {:h 0})
                '(jobs.farm.find-spot {:range 1000})
                '(jobs.farm.find-spot {:depth 1000000})
                '(jobs.farm.find-spot {:limit 0})
                '(jobs.movement.go-to {:pos [1 64 1] :leg-alert-s 0})
                '(jobs.movement.go-to {:pos [1 64 1] :leg-alert-s 500})
                '(jobs.survival.dig-niche {:reach 100000})
                '(jobs.survival.dig-niche {:reach 0})
                '(jobs.survival.dig-niche {:roof-height 100000})
                '(jobs.farm.till {:center [0 64 0] :radius 1000000})
                '(jobs.farm.find-spot {:w 17})
                '(jobs.farm.find-spot {:range 49})
                '(jobs.farm.find-spot {:depth 17})
                '(jobs.farm.find-spot {:limit 9})
                '(jobs.movement.go-to {:pos [1 64 1] :leg-alert-s 0.05})
                '(jobs.movement.go-to {:pos [1 64 1] :leg-alert-s 120.1})
                '(jobs.survival.dig-niche {:reach 33})
                '(jobs.farm.till {:center [0 64 0] :radius 8})
                '(jobs.farm.till {:center [0 64 0] :radius -1})]]
    (is (string? (expr/problem registry/jobs form)) (pr-str form)))
  (doseq [form ['(jobs.farm.find-spot {:w 16 :h 16 :range 48 :depth 16 :limit 8})
                '(jobs.movement.go-to {:pos [1 64 1] :leg-alert-s 5})
                '(jobs.survival.dig-niche {:reach 16 :roof-height 4})
                '(jobs.farm.till {:center [0 64 0] :radius 7})
                '(jobs.farm.find-spot {:limit 1})
                '(jobs.movement.go-to {:pos [1 64 1] :leg-alert-s 0.1})
                '(jobs.movement.go-to {:pos [1 64 1] :leg-alert-s 120})
                '(jobs.survival.dig-niche {:reach 32})
                '(jobs.farm.till {:center [0 64 0] :radius 0})]]
    (is (nil? (expr/problem registry/jobs form)) (pr-str form))))

(deftest every-default-use-names-a-chooser-that-exists
  (doseq [[sym job] registry/jobs
          [purpose spec] (:use (:defaults job))]
    (is (choose/use-spec? purpose spec) (str sym " defaults :use " (pr-str {purpose spec})))))

(deftest an-unknown-use-spec-is-not-known
  (is (not (choose/use-spec? :tools :swim)))
  (is (not (choose/use-spec? :blocks :nonsense)))
  (is (not (choose/use-spec? :food :build))))

(deftest a-job-file-with-a-check-fails-to-load
  (is (re-find #"defines check; a job declares can-start\? or can-run\?"
               (describe-message "(ns jobs.probe)\n(defn check [c] nil)\n(defn round [c] :continue)")))
  (is (re-find #"defines check"
               (describe-message "(ns jobs.probe)\n(defn check [c] nil)\n(defn can-start? [c] nil)\n(defn round [c] :continue)")))
  (is (nil? (describe-message "(ns jobs.probe)\n(def safe? 1)\n(def fit? 1)\n(def conditions [])\n(defn round [c] :continue)"))))

(deftest a-job-without-safe-or-fit-fails-to-load-naming-the-missing-export
  (is (re-find #"job jobs.probe must export safe\? and fit\?"
               (describe-message "(ns jobs.probe)\n(defn round [c] :continue)")))
  (is (re-find #"must export fit\? "
               (describe-message "(ns jobs.probe)\n(def safe? 1)\n(defn round [c] :continue)")))
  (is (re-find #"must export safe\? "
               (describe-message "(ns jobs.probe)\n(def fit? 1)\n(defn round [c] :continue)"))))

(deftest a-file-with-only-a-round-is-a-job-and-one-without-is-a-helper
  (is (describes-job? "(ns jobs.probe)\n(def safe? 1)\n(def fit? 1)\n(defn round [c] :continue)"))
  (is (not (describes-job? "(ns jobs.probe)\n(defn helper [c] :continue)"))))

(deftest every-job-default-passes-its-own-args-spec
  (doseq [[sym job] registry/jobs
          :let [data (:args job)
                bad (when (seq data)
                      (args/first-problem sym data (args/defaults data)))]]
    (is (nil? bad) (str sym " default " (pr-str bad)))))
