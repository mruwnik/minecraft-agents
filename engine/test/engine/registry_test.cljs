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
     jobs.survival.cure jobs.survival.respond-to-hostile
     jobs.food.fish jobs.survival.eat jobs.survival.get-food jobs.survival.go-home
     jobs.survival.sleep jobs.survival.set-spawn jobs.survival.night jobs.survival.leave-shelter jobs.survival.bed-down jobs.survival.shut-in jobs.survival.wall-in jobs.survival.dig-pit jobs.survival.dig-pocket jobs.survival.enter-room jobs.survival.stand-in jobs.survival.hide-out jobs.survival.log-out
     jobs.combat.attack jobs.animals.breed jobs.animals.shear jobs.animals.cull jobs.animals.maintain jobs.animals.pen-check jobs.animals.leash jobs.animals.unleash jobs.animals.lead-to jobs.animals.lure-to jobs.animals.bring jobs.survival.recover-drops jobs.survival.restore-broken jobs.maintenance.shut jobs.storage.make-room
     jobs.animals.herd
     jobs.forestry.fell-tree jobs.items.collect-drops jobs.forestry.plant-sapling
     jobs.forestry.harvest-wood jobs.forestry.maintain jobs.forestry.prepare jobs.storage.deposit jobs.storage.withdraw jobs.storage.haul jobs.strip.loot-cart jobs.items.craft jobs.items.give jobs.items.pick-up-loose jobs.items.equip jobs.items.kitchen jobs.farm.till jobs.farm.irrigate jobs.farm.fertilize jobs.farm.compost jobs.blocks.clear-cells jobs.build.path jobs.farm.find-spot jobs.farm.grow jobs.farm.harvest jobs.farm.plant jobs.farm.tend
     jobs.apiary.harvest
     jobs.apiary.guard
     jobs.apiary.maintain
     jobs.village.trade
     jobs.village.breed jobs.village.roll
     jobs.items.smelt
     jobs.items.enchant jobs.items.obtain jobs.items.get-tool jobs.items.get-kit
     jobs.build.from-plan jobs.build.light-area jobs.build.pen jobs.build.rail jobs.build.rail-line
     jobs.access.pillar jobs.access.bridge
     jobs.explore.search jobs.explore.look
     jobs.combat.hunt jobs.gather.get-seeds jobs.gather.cast-obsidian jobs.gather.mine jobs.gather.dig-for jobs.gather.pick-ores jobs.gather.shaft-descent jobs.gather.strip-tunnel
     jobs.movement.go-to jobs.movement.chase jobs.movement.reach jobs.movement.look-around jobs.movement.stay-near jobs.movement.run-away jobs.movement.leave-vehicle jobs.movement.mount jobs.movement.boat-launch jobs.movement.boat-recover jobs.movement.boat-drive jobs.movement.boat-land jobs.movement.cross-water jobs.survival.block-arrow-gap
     jobs.time.wait-for-light jobs.time.noop jobs.debug.notify jobs.debug.ready-probe
     jobs.access.stair jobs.access.record-stair jobs.access.tunnel jobs.access.toggle jobs.farm.tidy jobs.farm.maintain
     jobs.access.cleanup jobs.access.clear-path jobs.strip.seal jobs.strip.corridor jobs.strip.site jobs.strip.all jobs.strip.base jobs.strip.smelt jobs.strip.haul
     jobs.blocks.dig jobs.blocks.fill-cells jobs.blocks.place jobs.items.use
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
    'jobs.survival.leave-shelter :reflex []
    'jobs.survival.bed-down :phase '[jobs.survival.night]))

(deftest a-job-declares-its-tags
  (is (= [:mine :dig :ore :light] (:tags (get registry/jobs 'jobs.gather.mine))))
  (is (= [:food] (:tags (get registry/jobs 'jobs.survival.get-food))))
  (is (= [] (:tags (get registry/jobs 'jobs.movement.go-to)))))

(deftest a-job-declares-it-is-position-bound
  (is (= #{'jobs.access.stair 'jobs.access.bridge 'jobs.access.pillar 'jobs.gather.mine 'jobs.gather.dig-for}
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
  (is (nil? (get-in registry/jobs ['jobs.time.wait-for-light :args])) "args is optional"))

(deftest real-jobs-refuse-unknown-arg-keys
  (let [msg (expr/problem registry/jobs '(jobs.movement.go-to {:target [50 40 3]}))]
    (is (re-find #"jobs.movement.go-to has no arg :target; its args are " msg))
    (is (re-find #":pos" msg))
    (is (re-find #":doors" msg)))
  (is (re-find #"jobs.time.wait-for-light has no arg :bogus; it takes no args"
               (expr/problem registry/jobs '(jobs.time.wait-for-light {:bogus 1}))))
  (is (nil? (expr/problem registry/jobs '(jobs.movement.go-to {:pos [50 40 3]}))))
  (is (re-find #"jobs.access.stair has no arg :stock-torches"
               (expr/problem registry/jobs '(jobs.access.stair {:dir :down :stock-torches false})))))

(deftest the-trigger-registry-holds-the-default-trigger-set
  (is (= #{:suffocating :burning :wedged :hostile-near :poisoned :hungry :stuck-underground :needs-bed :needs-weapon :night :dawn :dusk-home :died :pen-gate :door-left
           :inventory-nearly-full :scaffold-left :tidy-pending :mounted :loose-items :part-cut-tree :replant-owed :ore-seen}
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
                '(jobs.movement.chase {:pos [1 64 1] :leg-s 0})
                '(jobs.movement.chase {:pos [1 64 1] :leg-s 500})
                '(jobs.farm.find-spot {:w 17})
                '(jobs.farm.find-spot {:range 49})
                '(jobs.movement.chase {:pos [1 64 1] :leg-s 0.05})
                '(jobs.movement.chase {:pos [1 64 1] :leg-s 120.1})
                '(jobs.farm.till {:area {:pos [0 64 0] :radius -1}})]]
    (is (string? (expr/problem registry/jobs form)) (pr-str form)))
  (doseq [form ['(jobs.farm.find-spot {:w 16 :h 16 :range 48})
                '(jobs.movement.chase {:pos [1 64 1] :leg-s 5})
                '(jobs.farm.till {:area {:pos [0 64 0] :radius 7}})
                '(jobs.movement.chase {:pos [1 64 1] :leg-s 0.1})
                '(jobs.movement.chase {:pos [1 64 1] :leg-s 120})
                '(jobs.farm.till {:area {:pos [0 64 0] :radius 0}})]]
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

(deftest an-empty-or-misnamed-file-is-refused-not-taken-for-a-helper
  (is (re-find #"must declare \(ns jobs.probe"  (describe-message "")))
  (is (re-find #"must declare \(ns jobs.probe" (describe-message "(ns jobs.other)\n(defn helper [c] :continue)"))))

(deftest a-file-with-only-a-round-is-a-job-and-one-without-is-a-helper
  (is (describes-job? "(ns jobs.probe)\n(def safe? 1)\n(def fit? 1)\n(defn round [c] :continue)"))
  (is (not (describes-job? "(ns jobs.probe)\n(defn helper [c] :continue)"))))

(deftest every-job-default-passes-its-own-args-spec
  (doseq [[sym job] registry/jobs
          :let [data (:args job)
                bad (when (seq data)
                      (args/first-problem sym data (args/defaults data)))]]
    (is (nil? bad) (str sym " default " (pr-str bad)))))

(deftest strip-tunnel-called-without-a-block-checks-its-tools-for-stone
  (is (= "stone" (:block (args/defaults (:args (get registry/jobs 'jobs.gather.strip-tunnel)))))))
