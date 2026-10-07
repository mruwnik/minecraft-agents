(ns engine.walk-tolls-test
  "go-to's :tolls (jobs.lib.near/plan!, wworld/with-tolls): the cells a caller prices cost more on a walk's plan, so it
  bends round them; jobs.lib.cost farm-tolls and zone-tolls build them."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.lib.cost :as cost]
            [jobs.lib.near :as near]
            [jobs.lib.walk.world :as wworld]
            [jobs.lib.walk.plan :as wplan]))

(def wall (for [x (range 19 22) z (range -6 7)] [x 64 z]))

(defn ^:async plan-with
  "near/plan! from [0 64 0] to [40 64 0] over an open floor with tolls (a vector of {:x :y :z :factor}): the plan's steps."
  [tolls]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor {:floor [-5 -20 45 20]})
        out (atom nil)
        parent {:check (constantly true)
                :round (fn ^:async planning-round [c]
                         (reset! out (await (near/plan! c [40 64 0] 0 :never (wworld/body-policy c) [] false nil nil true false nil nil tolls)))
                         :done)}
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'planning-parent parent)
                          :triggers {} :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng '(planning-parent) {})
    (await (tu/tick-until-idle! eng 20))
    (:steps @out)))

(defn on-wall? [steps]
  (boolean (some (fn [{:keys [x z]}] (contains? (set wall) [x 64 z])) steps)))

(deftest tolled-cells-bend-a-walks-plan-round-them
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plain (await (plan-with nil))
              bent (await (plan-with (cost/farm-tolls wall)))]
          (is (seq plain))
          (is (on-wall? plain) "no tolls: straight over the cells")
          (is (seq bent))
          (is (not (on-wall? bent)) "tolls: round them"))))))

(deftest tolls-reach-the-planner-and-the-search-key-and-survive-walls
  (let [pw #js {:snapshot nil :table nil :space nil}
        tolled (wworld/with-tolls pw (cost/zone-tolls [[5 64 0] [6 64 1]]))
        other (wworld/with-tolls pw (cost/zone-tolls [[5 64 0]]))]
    (is (nil? (wworld/tolls-key (wworld/with-tolls pw nil))) "no tolls: the key of a plain search")
    (is (nil? (.-tolls (wplan/plan-options pw 1 nil nil))))
    (is (not= (wworld/tolls-key tolled) (wworld/tolls-key other)))
    (is (= (wworld/tolls-key tolled) (wworld/tolls-key (wworld/with-tolls pw (cost/zone-tolls [[6 64 1] [5 64 0]])))))
    (is (= 2 (.-size (.-cells (.-tolls (wplan/plan-options tolled 1 nil nil))))))
    (is (= (wworld/tolls-key tolled) (wworld/tolls-key (wworld/with-dangers tolled nil))) "other costs keep the tolls")))

(deftest tolls-problem-names-a-malformed-entry
  (is (nil? (wworld/tolls-problem nil)))
  (is (nil? (wworld/tolls-problem [])))
  (is (nil? (wworld/tolls-problem [{:x 1 :y 64 :z 2 :factor 3}])))
  (doseq [bad [{:x 1 :y 64 :z 2} {:x 1 :y 64 :z 2 :factor nil} {:x 1 :y 64 :z 2 :factor "3"}
               {:x 1 :y 64 :z 2 :factor js/NaN} {:x 1 :y 64 :z 2 :factor -1} {:x 1 :y 64 :factor 3}
               {:x 1 :y 64 :z js/Infinity :factor 3} [1 64 2] nil]]
    (is (string? (wworld/tolls-problem [bad])) (pr-str bad)))
  (is (string? (wworld/tolls-problem {:x 1 :y 64 :z 2 :factor 3})) "not a sequence"))
