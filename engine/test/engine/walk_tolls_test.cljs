(ns engine.walk-tolls-test
  "go-to's :tolls (jobs.lib.near/plan!, wworld/with-tolls): the cells a caller prices cost more on a walk's plan, so it
  bends round them; jobs.lib.cost farm-tolls and zone-tolls build them, and jobs.lib.threats/water-tolls the water a drowned was met in."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.ready-test :as ready-test]
            [engine.test-util :as tu]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.tolls :as tolls]
            [jobs.lib.near :as near]
            [jobs.lib.threats :as threats]
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
        parent {:round (fn ^:async planning-round [c]
                         (reset! out (await (near/plan! c [40 64 0] 0 :never (wworld/body-policy c) [] false nil nil true false nil nil tolls)))
                         (ctx/end c :done nil))}
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

(deftest a-toll-of-infinity-is-a-cell-no-plan-enters
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [shut (await (plan-with (tolls/tolls (for [y [64 65 66] [x _ z] wall] [x y z]) ##Inf)))]
          (is (seq shut))
          (is (not (on-wall? shut)) "round the column, as if solid stone (a one-high wall could be climbed)"))))))

(deftest tolls-reach-the-planner-and-the-search-key-and-survive-walls
  (let [pw #js {:snapshot nil :table nil :space nil}
        tolled (wworld/with-tolls pw (cost/zone-tolls [[5 64 0] [6 64 1]]))
        other (wworld/with-tolls pw (cost/zone-tolls [[5 64 0]]))]
    (is (nil? (wworld/tolls-key (wworld/with-tolls pw nil))) "no tolls: the key of a plain search")
    (is (nil? (.-tolls (wplan/plan-options pw 1 nil nil))))
    (is (not= (wworld/tolls-key tolled) (wworld/tolls-key other)))
    (is (= (wworld/tolls-key tolled) (wworld/tolls-key (wworld/with-tolls pw (cost/zone-tolls [[6 64 1] [5 64 0]])))))
    (is (= 2 (.-size (.-cells (.-tolls (wplan/plan-options tolled 1 nil nil))))))
    (is (= (wworld/tolls-key tolled) (wworld/tolls-key (wworld/with-dangers tolled nil))) "other costs keep the tolls")
    (is (= [[5 64 0 12] [6 64 1 10]] (mapv (juxt :x :y :z :factor) (wworld/tolls-key (wworld/with-tolls tolled [{:x 5 :y 64 :z 0 :factor 2}]))))
        "tolls added later add to the cells already tolled")))

(def lake
  "A fake world: the floor x -10..50, z -10..10 with a lake x 0..40, z 5..8, water y 61..63 (its top level with the floor)."
  {:floor [-10 -10 50 10] :self {:pos {:x 1.5 :y 64 :z 3.5}}
   :blocks (merge (tu/box 0 61 5 40 63 8 "water") (tu/box -1 60 4 41 60 9 "stone"))})

(defn met-in-the-lake
  "A ctx over lake remembering a hit by attacker taken at the lake's shore [1 64 4], 10 min ago (1 h :hurt ttl)."
  [attacker]
  (let [now 36000000]
    (assoc (ready-test/ctx-of lake)
           :view (fn [] {:now now :data {:policies {:hurt {:cap 50 :ttl 3600000}}
                                         :entries {:hurt [{:t (- now 600000) :data {:health 15 :pos {:x 1.5 :y 64 :z 4.5} :attacker {:name attacker}}}]}}}))))

(deftest a-drowned-met-at-a-lake-tolls-all-its-water-not-a-sphere-round-the-spot
  (let [c (met-in-the-lake "drowned")
        tolls (threats/water-tolls c)
        cells (set (map (juxt :x :y :z) tolls))
        costed (fn [dangers?] (wworld/tolls-key (wworld/costed-world c (wworld/path-world (:primitives c)) {:dangers? dangers?})))]
    (is (= (* 41 4 3) (count cells)) "every water cell of the lake, the far end 39 blocks off too")
    (is (contains? cells [40 61 8]))
    (is (not (contains? cells [1 64 4])) "the dry shore is no water")
    (is (every? #(pos? (:factor %)) tolls))
    (is (= cells (set (map (juxt :x :y :z) (costed true)))) "go-to's costed world carries them")
    (is (nil? (costed false)) "a walk that ignores dangers does not")
    (is (= [] (threats/water-tolls (met-in-the-lake "zombie"))) "a land mob is priced round its point (planner-dangers)")
    (is (= [] (threats/water-tolls (ready-test/ctx-of lake))) "nothing met: no tolls")))

(deftest tolls-problem-names-a-malformed-entry
  (is (nil? (wworld/tolls-problem nil)))
  (is (nil? (wworld/tolls-problem [])))
  (is (nil? (wworld/tolls-problem [{:x 1 :y 64 :z 2 :factor 3}])))
  (is (nil? (wworld/tolls-problem [{:x 1 :y 64 :z 2 :factor ##Inf}])))
  (doseq [bad [{:x 1 :y 64 :z 2} {:x 1 :y 64 :z 2 :factor nil} {:x 1 :y 64 :z 2 :factor "3"}
               {:x 1 :y 64 :z 2 :factor js/NaN} {:x 1 :y 64 :z 2 :factor -1} {:x 1 :y 64 :factor 3}
               {:x 1 :y 64 :z js/Infinity :factor 3} [1 64 2] nil]]
    (is (string? (wworld/tolls-problem [bad])) (pr-str bad)))
  (is (string? (wworld/tolls-problem {:x 1 :y 64 :z 2 :factor 3})) "not a sequence"))
