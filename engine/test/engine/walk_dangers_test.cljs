(ns engine.walk-dangers-test
  "The walks' plans (jobs.lib.near/plan!) keep away from the dangers the body knows of (jobs.lib.threats): wide round a
  zombie for an unarmed body, nearer with a sword, straight with dangers off (a walk up to the mob it fights), and
  round a remembered :threat spot."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.lib.near :as near]
            [jobs.lib.threats :as threats]
            [jobs.lib.walk.world :as wworld]))

(def zombie {:id 9 :uuid "u9" :name "zombie" :kind "hostile" :pos {:x 20.5 :y 64 :z 0.5}})

(defn nearest
  "The least distance from a step's stand point to the zombie's place."
  [steps]
  (apply min (map (fn [{:keys [px pz]}] (js/Math.hypot (- px 20.5) (- pz 0.5))) steps)))

(defn ^:async plan-past
  "near/plan! from [0 64 0] to [40 64 0] for a body with world spec (merged) and :threat entries remembered first, with
  dangers on or off: the plan's steps."
  [world remembered dangers]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor (merge {:floor [-5 -20 45 20]} world))
        out (atom nil)
        parent {:check (constantly true)
                :round (fn ^:async planning-round [c]
                         (doseq [t remembered] (threats/remember! c t))
                         (reset! out (await (near/plan! c [40 64 0] 0 :never (wworld/body-policy c) [] false nil nil true dangers)))
                         :done)}
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
        (let [steps (await (plan-past {} [{:mob "zombie" :id 9 :uuid "u9" :pos {:x 20.5 :y 64 :z 0.5} :ended 1}] true))]
          (is (>= (nearest steps) 8) (str "nearest " (nearest steps))))))))

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
