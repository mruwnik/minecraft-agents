(ns engine.walk-dangers-test
  "The walks' plans (jobs.lib.near/plan!) keep away from the dangers the body knows of (jobs.lib.threats): wide round a
  zombie for an unarmed body, nearer with a sword, straight with dangers off (a walk up to the mob it fights), and
  round a remembered :threat spot."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [jobs.lib.near :as near]
            [jobs.lib.threats :as threats]
            [jobs.lib.walk.world :as wworld]))

(def zombie {:id 9 :uuid "u9" :name "zombie" :kind "hostile" :pos {:x 20.5 :y 64 :z 0.5}})

(defn nearest
  "The least distance from a step's stand point to the zombie's place."
  [steps]
  (apply min (map (fn [{:keys [px pz]}] (js/Math.hypot (- px 20.5) (- pz 0.5))) steps)))

(declare plan-steps)

(defn ^:async plan-past
  "near/plan! from [0 64 0] to [40 64 0] for a body with world spec (merged) and :threat entries remembered first, with
  dangers on or off, with the settings overrides: the plan's steps."
  [world remembered dangers & [overrides]]
  (await (settings/with-settings (or overrides {}) (fn ^:async plan [] (await (plan-steps world remembered dangers))))))

(defn ^:async plan-steps [world remembered dangers]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor (merge {:floor [-5 -20 45 20]} world))
        out (atom nil)
        parent {:round (fn ^:async planning-round [c]
                         (doseq [t remembered] (if (:death-t t) (ctx/remember! c :deaths t core/deaths-policy) (threats/remember! c t)))
                         (reset! out (await (near/plan! (assoc c :args {}) [40 64 0] 0 :never (wworld/body-policy c) [] false nil nil true dangers)))
                         (ctx/end c :done nil))}
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'planning-parent parent)
                          :triggers {} :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng '(planning-parent) {})
    (await (tu/tick-until-idle! eng 20))
    (:steps @out)))

(deftest an-unarmed-body-plans-wide-round-a-zombie-it-sees
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [steps (await (plan-past {:entities [zombie]} [] true))]
          (is (seq steps))
          (is (>= (nearest steps) 8) (str "nearest " (nearest steps))))))))

(deftest the-danger-options-change-the-plan-and-the-defaults-keep-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plain (await (plan-past {:entities [zombie]} [] true))
              default (await (plan-past {:entities [zombie]} [] true {:jobs.lib.cost.danger/flee-factor 4 :jobs.lib.cost.danger/fight-factor 0.1 :jobs.lib.cost.danger/max-rate 4}))
              cheap-flee (await (plan-past {:entities [zombie]} [] true {:jobs.lib.cost.danger/flee-factor 0.1}))
              capped (await (plan-past {:entities [zombie]} [] true {:jobs.lib.cost.danger/max-rate 0.2}))
              narrow (await (plan-past {:entities [zombie]} [] true {:jobs.lib.cost.danger/shape {:sensed {:close 1 :radius 3}}}))]
          (is (= (mapv (juxt :x :z) plain) (mapv (juxt :x :z) default)) "today's values as options: the same plan")
          (is (< (nearest cheap-flee) (nearest plain)) "a cheaper flee passes nearer")
          (is (< (nearest capped) (nearest plain)) "a lower cap passes nearer")
          (is (< (nearest narrow) (nearest plain)) "a narrower danger passes nearer"))))))

(deftest a-sword-passes-nearer-and-dangers-off-goes-straight
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [unarmed (await (plan-past {:entities [zombie]} [] true))
              armed (await (plan-past {:entities [zombie] :inventory [{:name "diamond_sword" :count 1}]} [] true))
              off (await (plan-past {:entities [zombie]} [] false))]
          (is (< (nearest armed) (nearest unarmed)))
          (is (< (nearest off) 1) "dangers off: straight through the zombie's place"))))))

(deftest a-remembered-threat-spot-is-kept-away-from
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [steps (await (plan-past {:entities [(assoc zombie :visible false)]} [{:mob "zombie" :id 9 :uuid "u9" :pos {:x 20.5 :y 64 :z 0.5} :ended 1}] true))]
          (is (>= (nearest steps) 8) (str "nearest " (nearest steps))))))))

(def death-at (fn [x] {:pos {:x x :y 64 :z 0.5} :cause "zombie" :death-t 990000}))

(deftest a-recent-death-is-kept-away-from-but-a-walk-to-its-spot-goes-straight-there
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [round (await (plan-past {} [(death-at 20.5)] true))
              to-pile (await (plan-past {} [(death-at 40.5)] true))]
          (is (>= (nearest round) 8) (str "nearest " (nearest round)))
          (is (= 40 (:x (last to-pile))) "the pile at the death spot is reached, not walled off"))))))

(deftest a-remembered-threat-spot-of-a-mob-the-world-no-longer-has-is-forgotten
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [steps (await (plan-past {} [{:mob "creeper" :id 9 :uuid "u9" :pos {:x 20.5 :y 64 :z 0.5} :ended 1}] true))]
          (is (< (nearest steps) 1) (str "nearest " (nearest steps))))))))

(deftest a-kept-search-is-new-once-a-danger-moved-died-or-came
  (let [pw #js {:snapshot nil :table nil :space nil}
        with (fn [& ds] (wworld/danger-key (wworld/with-dangers pw (clj->js (vec ds)))))
        z {:mob "zombie" :x 20.5 :y 64 :z 0.5 :rate 4}]
    (is (nil? (wworld/danger-key pw)))
    (is (= (with z) (with (assoc z :x 21.2))) "a step within its 4 blocks: the same search goes on")
    (is (not= (with z) (with (assoc z :x 30.5))) "moved on")
    (is (not= (with z) (with)) "died: none left")
    (is (not= (with z) (with z (assoc z :mob "skeleton"))) "another came")))

(defn key-of [world]
  (let [p (tu/fake-on-floor (merge {:floor [-5 -20 45 20]} world))
        pw (wworld/with-dangers (wworld/path-world p) (clj->js (threats/known-dangers p [])))]
    (wworld/danger-key pw)))

(deftest the-search-key-follows-the-body-s-weapon-and-health
  (let [unarmed (key-of {:entities [zombie]})
        armed (key-of {:entities [zombie] :inventory [{:name "diamond_sword" :count 1}]})
        hurt (key-of {:entities [zombie] :inventory [{:name "diamond_sword" :count 1}] :self {:health 6}})]
    (is (= unarmed (key-of {:entities [zombie]})) "same body, same key")
    (is (not= unarmed armed) "a weapon picked up: a new search")
    (is (not= armed hurt) "health lost: a new search")))
