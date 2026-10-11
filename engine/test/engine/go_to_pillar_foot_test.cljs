(ns engine.go-to-pillar-foot-test
  "A plan begun on a pillar with a known melee hostile at its foot does not step down into the mob's reach
  (jobs.lib.near/plan!, the planner every walk uses)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [box]]
            [jobs.lib.near :as near]
            [jobs.lib.walk.world :as wworld]))

(def pillar (box 0 64 0 0 66 0 "stone"))

(def zombie {:id 9 :uuid "u9" :name "zombie" :kind "hostile" :pos {:x 2.5 :y 64 :z 0.5}})

(defn ^:async plan-from-pillar
  "near/plan! from the top of the pillar to [20 64 0]: the plan's steps."
  [dangers]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor {:floor [-5 -20 45 20] :blocks pillar :self {:pos [0 67 0]} :entities [zombie]})
        out (atom nil)
        parent {:check (constantly true)
                :round (fn ^:async planning-round [c]
                         (reset! out (await (near/plan! (assoc c :args {}) [20 64 0] 0 :never (wworld/body-policy c) [] false nil nil true dangers)))
                         :done)}
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'planning-parent parent)
                          :triggers {} :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng '(planning-parent) {})
    (await (tu/tick-until-idle! eng 20))
    @out))

(defn nearest-after-pillar [steps]
  (apply min 99 (for [{:keys [px pz]} (rest steps)] (js/Math.hypot (- px 2.5) (- pz 0.5)))))

(deftest a-plan-from-a-pillar-does-not-step-down-within-reach-of-the-zombie-at-its-foot
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (await (plan-from-pillar true))
              near (nearest-after-pillar (:steps plan))]
          (is (>= near 3) (str "nearest step after the pillar top " near " " (pr-str (select-keys plan [:status :reason])) " " (pr-str (mapv (juxt :px :pz) (:steps plan))))))))))
