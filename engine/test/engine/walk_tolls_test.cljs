(ns engine.walk-tolls-test
  "go-to's :tolls (jobs.lib.near/plan!, walk/with-tolls): the cells a caller prices cost more on a walk's plan, so it
  bends round them; jobs.lib.cost farm-tolls and zone-tolls build them."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.lib.cost :as cost]
            [jobs.lib.near :as near]
            [jobs.lib.walk :as walk]))

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
                         (reset! out (await (near/plan! c [40 64 0] 0 :never (walk/body-policy c) [] false nil nil true false nil nil tolls)))
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
        tolled (walk/with-tolls pw (cost/zone-tolls [[5 64 0] [6 64 1]]))
        other (walk/with-tolls pw (cost/zone-tolls [[5 64 0]]))]
    (is (nil? (walk/tolls-key (walk/with-tolls pw nil))) "no tolls: the key of a plain search")
    (is (nil? (.-tolls (walk/plan-options pw 1 nil nil))))
    (is (not= (walk/tolls-key tolled) (walk/tolls-key other)))
    (is (= (walk/tolls-key tolled) (walk/tolls-key (walk/with-tolls pw (cost/zone-tolls [[6 64 1] [5 64 0]])))))
    (is (= 2 (.-size (.-cells (.-tolls (walk/plan-options tolled 1 nil nil))))))
    (is (= (walk/tolls-key tolled) (walk/tolls-key (walk/with-dangers tolled nil))) "other costs keep the tolls")))
