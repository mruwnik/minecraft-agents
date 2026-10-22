(ns engine.stair-test
  "jobs.access.stair: step geometry, the stop rules, and whole stairs against the fake world."
  (:require [clojure.string :as str]
            [engine.child-ends :as ce]
            [engine.constants :as constants]
            [engine.sky-perception :as sky]
            [jobs.lib.shelter.sense :as sh-sense]
            [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.job-api :as job-api]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as world]
            [jobs.access.stair :as stair]
            [jobs.lib.ore-pick :as ore-pick]
            [jobs.lib.access :as access]
            [jobs.lib.bridge :as bridge]
            [jobs.lib.dig-look :as dig-look]
            [jobs.access.stair.step :as sstep]
            [jobs.lib.ledger :as ledger]
            [engine.settings :as settings]
            [engine.contract :as contract]
            [jobs.lib.stair-cells :as sc]
            [jobs.access.stair.limits :as sset]
            [jobs.lib.walk.drive :as drive]
            [jobs.lib.walk.plan :as wplan]
            [plan.shape :as shape]))

(def job 'jobs.access.stair)

;; ---------------------------------------------------------------- pure

(deftest step-cells-per-direction-and-heading
  (are [dir heading next cut]
       (= {:next next :cut cut :floor (update next 1 dec) :under (update next 1 - 2)}
          (shape/step-cells [0 65 0] dir heading))
    :down :east [1 64 0] [[1 66 0] [1 65 0] [1 64 0]]
    :down :west [-1 64 0] [[-1 66 0] [-1 65 0] [-1 64 0]]
    :down :south [0 64 1] [[0 66 1] [0 65 1] [0 64 1]]
    :down :north [0 64 -1] [[0 66 -1] [0 65 -1] [0 64 -1]]
    :up :east [1 66 0] [[0 67 0] [1 67 0] [1 66 0]]
    :up :west [-1 66 0] [[0 67 0] [-1 67 0] [-1 66 0]]
    :up :south [0 66 1] [[0 67 0] [0 67 1] [0 66 1]]
    :up :north [0 66 -1] [[0 67 0] [0 67 -1] [0 66 -1]]))

(deftest a-plan-stair-part-has-the-cells-the-stair-job-cuts
  (doseq [heading [:north :east :south :west]]
    (let [from [3 70 -2]
          feet (take 12 (iterate #(:next (shape/step-cells % :down heading)) from))
          dug (into #{} (mapcat #(:cut (shape/step-cells % :down heading))) feet)]
      (is (= dug (set (shape/part-cells {:stair {:from from :heading heading :to-y 58}}))) (name heading))
      (is (not-any? (set (shape/part-cells {:stair {:from from :heading heading :to-y 58}})) [from (update from 1 inc)])))))

(defn solid-at
  "A block-at over the cells named solid, air elsewhere."
  [& cells]
  (let [named (set cells)]
    (fn [cell] (if (named cell) "stone" "air"))))

(deftest a-floor-is-placed-against-a-neighbour-or-from-the-cell-under-it-else-it-has-no-support
  (let [floor [4 60 0]
        plans #(bridge/bridge-plans % floor 70.0)]
    (is (= [[floor]] (plans (solid-at [3 60 0]))) "a face beside it")
    (is (= [[[4 59 0] floor]] (plans (solid-at [3 59 0]))) "only the cell under has a face: support first")
    (is (empty? (plans (solid-at [4 61 0]))) "the underside of the block over it cannot be aimed at from above")
    (is (= [[floor]] (bridge/bridge-plans (solid-at [4 61 0]) floor 60.5)) "but it can from below")
    (is (empty? (plans (solid-at [9 9 9]))) "a void pocket")
    (is (= [[[3 60 0] floor]] (bridge/bridge-plans (solid-at [2 60 0]) floor 70.0 {:back [-1 0] :depth 2}))
        "a solid cell back along the line: the cells between are placed from it")
    (is (empty? (bridge/bridge-plans (solid-at [1 60 0]) floor 70.0 {:back [-1 0] :depth 2})) "but not beyond the depth")
    (is (empty? (bridge/bridge-plans (fn [cell] (when-not (= cell [2 60 0]) "air")) floor 70.0 {:back [-1 0] :depth 2}))
        "an unseen support is no support: the stair looks at it first")))

(deftest a-flat-run-ends-where-the-next-floor-has-a-face-or-is-solid
  (let [feet [0 65 0]
        void-to (fn [x] (fn [[cx cy _ :as cell]] (if (and (<= cy 64) (>= cx x)) "stone" (when (<= cy 70) "air"))))
        run (fn [block-at max] (bridge/flat-run block-at feet [1 0] 70.0 nil max))]
    (is (= 3 (run (void-to 5) 8)) "the third bridge cell leaves a next floor [4 63 0] with stone [5 63 0] ahead of it")
    (is (nil? (run (void-to 12) 8)) "no support within the length")
    (is (nil? (run (fn [cell] (when-not (= cell [2 65 0]) ((void-to 5) cell))) 8)) "an unsensed cell on the way")
    (is (nil? (run (fn [cell] (if (= cell [1 64 0]) "stone" ((void-to 5) cell))) 8)) "the first floor is solid: the ledge does not end there")
    (is (nil? (run (fn [cell] (if (= cell [2 65 0]) "stone" ((void-to 5) cell))) 8)) "no room for the body over a cell of the run")))

(deftest a-cave-floor-bridge-looks-at-the-unseen-cells-back-along-the-line-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/seeing-within (tu/fake {:self {:pos {:x 10 :y 64 :z 0}} :blocks {}}) 0.5)
              mem* (atom {})
              c {:primitives p :args {:dir :down :heading :east} :id "stair-stub" :root "stair-stub" :slots []
                 :view (fn [] {:now 1000000 :data {:entries {(mem/job-kind "stair-stub") [{:t 1000000 :data @mem*}]}
                                                    :policies {(mem/job-kind "stair-stub") mem/job-policy}}})
                 :update-mem (fn [f more] (apply swap! mem* f more))}
              looks (atom [])]
          (with-redefs [dig-look/look-at! (fn [_ cell] (swap! looks conj cell) (js/Promise.resolve true))]
            (await (sstep/look-back! c [14 60 0]))
            (await (sstep/look-back! c [14 60 0])))
          (is (= [[13 60 0] [12 60 0]] (take 2 @looks)) "the nearest cell back first")
          (is (apply distinct? @looks) "each cell once"))))))

(deftest an-unseen-neighbour-is-tried-only-after-the-cells-with-a-known-face
  (let [floor [4 60 0]
        unseen-east (fn [cell] (cond (= cell [5 60 0]) nil (= cell [4 59 1]) "stone" :else "air"))]
    (is (= [[[4 59 0] floor] [floor]] (bridge/bridge-plans unseen-east floor 70.0)))))

(deftest stair-index-finds-the-step-on-the-line-only
  (are [feet dir heading i] (= i (sc/stair-index [0 65 0] feet dir heading 8))
    [0 65 0] :down :east 0
    [3 62 0] :down :east 3
    [0 62 -3] :down :north 3
    [2 67 0] :up :east 2
    [3 63 0] :down :east nil
    [3 62 1] :down :east nil
    [-1 66 0] :down :east nil
    [9 56 0] :down :east nil))

(defn world-fn
  "A block-at over a map {[x y z] name}: stone everywhere below y 65 unless named, air above, nil for :unloaded."
  [named]
  (fn [[_ y _ :as cell]]
    (let [n (get named cell (if (< y 65) "stone" "air"))]
      (when-not (= :unloaded n) n))))

(defn stop [named accept & {:keys [zones dir filler?] :or {zones [] dir :down}}]
  (let [in {:block-at (world-fn named) :feet [0 65 0] :zones zones :footprints #{} :ledger #{}}]
    (sc/stop-of in (assoc (sc/step-cells [0 65 0] dir :east) :up? (not= :down dir) :filler? filler?) accept)))

(deftest the-stop-rules-of-one-step
  (are [named accept zones reason] (= reason (:reason (stop named accept :zones zones)))
    {} #{:fluid-adjacent} [] nil
    {[2 64 0] "lava"} #{:fluid-adjacent} [] :hazard
    {[2 64 0] "lava"} #{:fluid-adjacent :lava-adjacent} [] nil
    {[1 64 1] "water"} #{:fluid-adjacent} [] nil
    {[1 64 1] "water"} #{} [] :hazard
    {[1 64 1] "water" [1 64 -1] "lava"} #{:fluid-adjacent} [] :hazard
    {[1 63 0] "air"} #{:fluid-adjacent} [] :no-floor
    {[1 63 0] "lava"} #{:fluid-adjacent} [] :no-floor
    {[1 62 0] "cave_air"} #{:fluid-adjacent} [] :cave-below
    {[1 62 0] "water"} #{:fluid-adjacent} [] :cave-below
    {[1 62 0] :unloaded} #{:fluid-adjacent} [] :not-loaded
    {[1 65 0] "water"} #{:fluid-adjacent} [] :fluid-in-cut
    {} #{:fluid-adjacent} nil :no-zones
    {} #{:fluid-adjacent} [{:name "keep" :min [1 60 -1] :max [3 70 1]}] :zone
    {} #{:fluid-adjacent} [{:name "dig ok" :min [1 60 -1] :max [3 70 1] :allow #{:dig}}] nil
    {[1 67 0] "gravel" [1 66 0] "stone"} #{:fluid-adjacent} [] :hazard
    {[1 67 0] "gravel" [1 66 0] "stone"} #{:falling-block} [] nil))

(deftest a-cut-cell-that-is-the-floor-of-an-earlier-stair-is-refused
  (let [in {:block-at (world-fn {}) :feet [0 65 0] :zones [] :footprints #{} :ledger #{}}
        cells (shape/step-cells [0 65 0] :down :east)]
    (is (= {:reason :undercuts-way :cell [1 64 0] :block "stone"}
           (select-keys (sc/stop-of in (assoc cells :ways #{[1 64 0]}) #{}) [:reason :cell :block])))
    (is (nil? (sc/stop-of in (assoc cells :ways #{[5 60 0]}) #{})) "an unrelated floor")
    (is (nil? (sc/stop-of in cells #{})) "no known way")
    (is (= :undercuts-way (:reason (sc/stop-of (assoc in :ways #{[1 64 0]}) cells #{}))) "ways in the rules input")
    (is (nil? (sc/stop-of (assoc in :block-at (world-fn {[1 64 0] "air"})) (assoc cells :ways #{[1 64 0]}) #{}))
        "a floor that is air now is no way")))

(deftest a-level-step-whose-floor-is-a-step-of-an-earlier-stair-is-refused-not-floored
  (let [in {:block-at (world-fn {[1 64 0] "air"}) :feet [0 65 0] :zones [] :footprints #{} :ledger #{}}
        cells (shape/step-cells [0 65 0] :level :east)]
    (is (= {:reason :undercuts-way :cell [1 64 0]}
           (select-keys (sc/stop-of in (assoc cells :ways #{[1 63 0]} :filler? true) #{}) [:reason :cell])))
    (is (= :no-floor (:reason (sc/stop-of in (assoc cells :ways #{[5 60 0]}) #{}))) "an unrelated stair: the floor is bridged as before")))

(deftest way-floors-are-the-floors-of-the-steps-walked
  (is (= [[1 63 0] [2 62 0]] (sc/way-floors [0 65 0] :down :east 2)))
  (is (= [[0 65 -1]] (sc/way-floors [0 65 0] :up :north 1)))
  (is (= [] (sc/way-floors [0 65 0] :down :east 0))))

(deftest a-stair-that-goes-on-from-an-earlier-one-is-joined-to-it
  (is (= {:floors [[1 64 0] [2 65 0] [3 66 0] [4 67 0]] :gone #{[[1 64 0] [2 65 0]]}}
         (sc/joined-way [[[1 64 0] [2 65 0]] [[9 60 9]]] [2 65 0] [[3 66 0] [4 67 0]]))
      "it began on the last floor of the first")
  (is (= {:floors [[3 66 0] [4 67 0]] :gone #{[[4 67 0]]}}
         (sc/joined-way [[[4 67 0]] [[9 60 9]]] [2 65 0] [[3 66 0] [4 67 0]]))
      "a way wholly inside the new one is dropped")
  (is (= {:floors [[3 66 0]] :gone #{}} (sc/joined-way [[[9 60 9]]] [2 65 0] [[3 66 0]]))))

(declare block-at stair! east ground setup tick-out! stone)

(deftest a-finished-stair-remembers-its-floors-and-a-second-one-stops-at-them
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng out]} (await (stair! {:blocks ground} east (fn [_])))
              ways (mapcat (comp :floors :data) (mem/entries (mem/view (:store eng)) :stair-way))]
          (is (= :done (:status @out)))
          (is (= [[1 63 0] [2 62 0] [3 61 0]] (vec ways))))))))

(deftest a-stair-without-a-heading-goes-the-way-the-body-faces
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng out]} (await (stair! {:blocks ground} {:dir :down :steps 2} (fn [_])))
              ways (mapcat (comp :floors :data) (mem/entries (mem/view (:store eng)) :stair-way))]
          (is (= :done (:status @out)) (pr-str @out))
          (is (= [[0 63 1] [0 62 2]] (vec ways))))))))

(deftest a-stair-with-a-y-above-the-feet-and-no-dir-goes-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (stair! {:blocks (merge ground (stone -3 12 67 72))} {:heading :east :y 67} (fn [_])))]
          (is (= :done (:status @out)) (pr-str @out))
          (is (= 2 (:steps @out))))))))

(def second-east {:dir :down :heading :east :steps 1})

(defn ^:async second-stair!
  "A first stair (east, 3 down) over ground; then the body stands at (0 64 0), made open, and a second stair runs east, its first cut being (1 63 0), the floor of the first one's
  first step. prep2 runs on the world before the second stair; mem2 on the store."
  [prep2 mem2]
  (let [args (atom east)
        s (await (tick-out! (setup {:blocks ground} args (fn [_]))))
        {:keys [eng p clock]} s
        world-state (.. p -world -state)]
    (fake/set-block! p [0 64 0] "air")
    (swap! world-state assoc-in [:self :pos] [0 64 0])
    (prep2 p)
    (mem2 (:store eng))
    (reset! args second-east)
    (reset! (:out s) :not-done)
    (core/submit! eng '(recording-parent) {})
    (await (tick-out! s))))

(deftest a-second-stair-under-the-first-stops-at-its-floor
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (second-stair! (fn [_]) (fn [_])))]
          (is (= :stopped (:status @out)))
          (is (= :undercuts-way (:reason @out)))
          (is (= [1 63 0] (:cell @out)))
          (is (= 0 (:steps @out)))
          (is (= "stone" (block-at p [1 63 0])) "the first stair's floor is never cut"))))))

(deftest a-floor-dug-away-since-no-longer-stops-a-stair
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (second-stair! (fn [p] (fake/set-block! p [1 63 0] "air")) (fn [_])))]
          (is (not= :undercuts-way (:reason @out)))
          (is (= :done (:status @out))))))))

(deftest a-floor-remembered-in-another-dimension-does-not-stop-a-stair
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [forget-all (fn [store] (mem/forget! store :stair-way)
                           (mem/write! store :stair-way {:dim "the_nether" :floors [[1 63 0]]}))
              {:keys [out]} (await (second-stair! (fn [_]) forget-all))]
          (is (not= :undercuts-way (:reason @out)))
          (is (= :done (:status @out))))))))

(deftest a-crop-or-farmland-in-the-cut-is-refused-and-never-dug
  (are [named cell block] (= {:reason :crop :cell cell :block block} (stop named #{}))
    {[1 66 0] "wheat"} [1 66 0] "wheat"
    {[1 65 0] "carrots"} [1 65 0] "carrots"
    {[1 64 0] "farmland"} [1 64 0] "farmland"
    {[1 65 0] "melon_stem"} [1 65 0] "melon_stem"
    {[1 65 0] "attached_pumpkin_stem"} [1 65 0] "attached_pumpkin_stem"))

(deftest a-stop-names-the-cell-and-the-hazards
  (let [s (stop {[2 64 0] "lava"} #{:fluid-adjacent})]
    (is (= [1 64 0] (:cell s)))
    (is (= [{:reason :fluid-adjacent :fluid "lava" :at [2 64 0]}] (:hazards s))))
  (is (= {:reason :zone :zone "keep" :cell [1 64 0]}
         (stop {} #{:fluid-adjacent} :zones [{:name "keep" :min [1 60 0] :max [1 64 0]}]))))

(deftest gravel-over-the-head-going-up-is-a-hazard-only-:falling-block-takes
  (let [named {[0 67 0] "stone" [0 68 0] "gravel" [1 65 0] "stone"}]
    (is (= :hazard (:reason (stop named #{:fluid-adjacent} :dir :up))))
    (is (nil? (stop named #{:falling-block} :dir :up)) "accepted: it lands on the body and is dug out")
    (is (nil? (stop (assoc named [0 68 0] "stone" [1 68 0] "gravel" [1 67 0] "stone") #{:falling-block} :dir :up))
        "gravel over the next column falls into the cut")))

(deftest gravel-hangs-over-a-cut-column-through-air-only
  (let [cut [[0 67 0] [1 67 0] [1 66 0]]
        at (fn [m] #(get m % "air"))]
    (is (= ["gravel" [1 70 0]] (sc/hanging-above (at {[1 70 0] "gravel"}) cut)) "air between")
    (is (nil? (sc/hanging-above (at {[1 70 0] "gravel" [1 69 0] "stone"}) cut)) "rock holds it")
    (is (nil? (sc/hanging-above (at {[1 70 0] "gravel"}) [[1 60 0]])) "out of the scan range")
    (is (= ["sand" [0 68 0]] (sc/hanging-above (at {[0 68 0] "sand"}) cut)) "the body's own column too")))

(deftest target-steps-from-args
  (are [args steps] (= steps (sc/target-steps args [0 65 0]))
    {:dir :down :heading :east :steps 4} 4
    {:dir :down :heading :east :y 57} 8
    {:dir :up :heading :east :y 71} 6
    {:dir :level :heading :east :steps 2} 2)
  (are [args] (:error (sc/target-steps args [0 65 0]))
    {:dir :level :heading :east :y 65}
    {:dir :down :heading :east}
    {:dir :down :heading :east :y 70}
    {:dir :sideways :heading :east :steps 2}
    {:dir :down :heading :up :steps 2}
    {:dir :down :heading :east :steps 0}))

;; ---------------------------------------------------------------- the fake world

(defn stone
  "Stone over x0..x1, y0..y1, z -2..2, as fake blocks."
  [x0 x1 y0 y1]
  (into {} (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range -2 3)] [(str x "," y "," z) "stone"])))

(deftest target-steps-y-behind-the-feet-depends-on-the-feet
  (are [args here?] (= here? (boolean (:here (sc/target-steps args [0 65 0]))))
    {:dir :down :heading :east :y 70} true
    {:dir :up :heading :east :y 60} true
    {:dir :down :heading :east :y 65} true
    {:dir :down :heading :east} false
    {:dir :level :heading :east :y 65} false
    {:dir :sideways :heading :east :steps 2} false))

(def ground (stone -3 12 50 64))
(def pick [{:name "iron_pickaxe" :count 1}])
(def east {:dir :down :heading :east :steps 3})

(defn setup
  "An engine over the fake world and world data (zones, default none; nil: never read; plans); a recording parent runs
  the job as its child and keeps its result in :out."
  [spec args prep]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake (merge {:self {:pos {:x 0 :y 65 :z 0}} :inventory pick} (dissoc spec :zones :plans :wrap-rounds :raw)))
        out (atom :not-done)
        w (world/of-data (:plans spec {}) {} (get spec :zones []))
        parent {:round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call c :kid job (if (satisfies? IDeref args) @args args)))]
                           (if (= :continue r)
                             r
                             (do (reset! out (if (:raw spec) r (tu/outcome r))) (ctx/end c :done nil)))))}
        jobs (reduce-kv (fn [m k wrap] (update-in m [k :round] wrap)) (assoc registry/jobs 'recording-parent parent) (:wrap-rounds spec))
        eng (core/create {:primitives p :jobs jobs
                          :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock) :world w
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (prep p)
    (let [id (core/submit! eng '(recording-parent) {})]
      {:eng eng :p p :clock clock :seen seen :out out :id id})))

(defn waiting
  "The reason the parent of the stair child waits with (job.waiting), or nil."
  [{:keys [eng id]}]
  (:waiting (job-api/summary eng id)))

(defn ^:async tick-out! [{:keys [eng clock] :as s}]
  (loop [i 0]
    (when (and (< i 300) (seq (:list (core/state eng))))
      (swap! clock + 500)
      (await (core/tick! eng))
      (recur (inc i))))
  s)

(defn ^:async stair! [spec args prep]
  (await (tick-out! (setup spec args prep))))

(defn digs [p] (mapv #(js->clj (.-pos (.-args %)) :keywordize-keys true)
                     (filter #(= "dig" (.-name %)) (.-calls (.-world p)))))
(defn block-at [p [x y z]] (.-name (.blockAt p #js {:x x :y y :z z})))
(defn events-of [{:keys [seen]} kind] (filter #(= kind (:kind %)) @seen))

(deftest a-stair-through-snow-is-dug-by-hand-with-an-empty-bag-and-no-fetch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [snow (update-vals ground (constantly "snow_block"))]
          (doseq [args [(assoc east :ways #{:carried}) east]]
            (let [s (await (stair! {:blocks snow :inventory []} args (fn [_])))]
              (is (= :done (:status @(:out s))))
              (is (= 3 (:steps @(:out s))))
              (is (seq (digs (:p s))) "dug by hand")
              (is (empty? (filter #(= :need (:slot %)) (events-of s :child_started))) "no tool is fetched for a hand-breakable block")
              (is (nil? (waiting s))))))))))

(deftest a-stair-queued-in-a-farm-stops-before-digging-the-crop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (stair! {:blocks (assoc ground "1,64,0" "wheat")} east (fn [_])))]
          (is (= :crop (:reason @out)))
          (is (= [1 64 0] (:cell @out)))
          (is (= "wheat" (:block @out)))
          (is (= 0 (:steps @out)))
          (is (empty? (digs p))))))))

(deftest a-stair-digs-a-crafting-table-in-its-cut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (stair! {:blocks (assoc ground "1,65,0" "crafting_table")} east (fn [_])))]
          (is (not= :base-block (:reason @out)) (pr-str @out))
          (is (some #{{:x 1 :y 65 :z 0}} (digs p))))))))

(deftest a-stair-started-in-a-hut-never-digs-its-chest-bed-or-furnace
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [block ["chest" "furnace" "white_bed"]]
          (let [{:keys [out p]} (await (stair! {:blocks (assoc ground "1,65,0" block)} east (fn [_])))]
            (is (= :base-block (:reason @out)) block)
            (is (= [1 65 0] (:cell @out)) block)
            (is (str/includes? (pr-str @out) (str "(" block ": a base block")) (pr-str @out))
            (is (= 0 (:steps @out)) block)
            (is (empty? (digs p)) block)
            (is (= block (block-at p [1 65 0])) block)))))))

(defn move-after-digs
  "A prep that puts the body on [7 65 2] (on the ground beside the stair), or on pos, after each of the first n digs (a reflex walking it off the stair)."
  ([n] (move-after-digs n [7 65 2]))
  ([n pos]
  (fn [p]
    (let [world (.-world p)
          left (atom n)]
      (.override world "dig"
                 (fn ^:async f [token a impl]
                   (let [r (await (impl token a))]
                     (when (pos? @left)
                       (swap! left dec)
                       (swap! (.. world -state) assoc-in [:self :pos] pos))
                     r)))))))

;; Sorrel #14: a night reflex took the body home between two digs; the resumed stair ended :off-stair.
(deftest a-stair-that-finds-the-body-off-its-line-walks-back-and-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (stair! {:blocks ground} east (move-after-digs 1)))]
          (is (= :done (:status @out)))
          (is (= 3 (:steps @out)))
          (is (seq (events-of s :stair.return))))))))

(deftest a-stair-whose-line-is-far-off-stops-off-stair-without-walking-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (stair! {:blocks ground} east (move-after-digs 1 [90 65 2])))]
          (is (= :off-stair (:reason @out)))
          (is (empty? (events-of s :stair.return))))))))

(deftest off-stair-stands-once-the-walks-back-are-used-up-and-says-where-the-body-is
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (stair! {:blocks ground} east (move-after-digs 100)))]
          (is (= :off-stair (:reason @out)))
          (is (= [0 65 0] (:origin @out)))
          (is (= [7 0 2] (:offset @out))))))))

(deftest three-steps-down-east-in-stone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (stair! {:blocks ground} east (fn [_])))]
          (is (= :done (:status @out)))
          (is (= 3 (:steps @out)))
          (is (= [3 62 0] (:at @out)))
          (is (= [{:cell [1 64 0] :block "stone"}
                  {:cell [2 64 0] :block "stone"} {:cell [2 63 0] :block "stone"}
                  {:cell [3 64 0] :block "stone"} {:cell [3 63 0] :block "stone"} {:cell [3 62 0] :block "stone"}]
                 (:dug @out)))
          (is (= "stone" (block-at p [3 61 0])) "the floor is never dug")
          (is (= 3 (count (events-of s :stair.step))))
          (is (empty? (events-of s :stair.done)) "the ending is job.ended's, no event restates it"))))))

(deftest every-cell-is-dug-through-the-dig-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (await (stair! {:blocks ground} east (fn [_])))]
          (is (= 6 (count (digs p))))
          (is (= 6 (count (ce/ends s :dug)))))))))

(deftest a-dig-child-that-waits-three-rounds-is-one-try-and-resumes-the-same-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom [])
              waiting (fn [round]
                        (fn ^:async waiting-round [c]
                          (swap! calls conj (:pos (:args c)))
                          (if (<= (count @calls) 4) :continue (await (round c)))))
              {:keys [out p]} (await (stair! {:blocks ground :wrap-rounds {'jobs.blocks.dig waiting}} east (fn [_])))]
          (is (= :done (:status @out)) (pr-str @out))
          (is (= 3 (:steps @out)))
          (is (= 1 (count (set (take 5 @calls)))) "the waiting rounds all dig the one cell"))))))

(deftest one-call-cuts-the-whole-stair
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock out]} (setup {:blocks ground :inventory (conj pick {:name "torch" :count 10})} east (fn [_]))]
          (swap! clock + 500)
          (await (core/tick! eng))
          (is (= :done (:status @out)) "one round is the whole stair")
          (is (= [3 62 0] (:at @out))))))))

(deftest down-by-target-y-north
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (stair! {:blocks ground} {:dir :down :heading :north :y 63} (fn [_])))]
          (is (= :done (:status @out)))
          (is (= [0 63 -2] (:at @out))))))))

(deftest up-out-of-a-one-wide-shaft
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [shaft (apply dissoc ground (for [y (range 59 65)] (str "0," y ",0")))
              {:keys [out]} (await (stair! {:blocks shaft :self {:pos {:x 0 :y 59 :z 0}}}
                                           {:dir :up :heading :east :steps 6} (fn [_])))]
          (is (= :done (:status @out)))
          (is (= [6 65 0] (:at @out)) "on the surface")
          (is (every? #(<= 59 (get-in % [:cell 1])) (:dug @out)) "never digs below the start"))))))

(deftest a-level-run-cuts-feet-and-head-cells-and-records-no-way
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [rock (dissoc (stone -3 12 50 67) "0,65,0" "0,66,0")
              {:keys [out p eng]} (await (stair! {:blocks rock} {:dir :level :heading :east :steps 2} (fn [_])))]
          (is (= :done (:status @out)) (pr-str @out))
          (is (= [2 65 0] (:at @out)))
          (is (= [[1 66 0] [1 65 0] [2 66 0] [2 65 0]] (mapv :cell (:dug @out))))
          (is (= "stone" (block-at p [2 64 0])) "the floor is never dug")
          (is (empty? (mem/entries (mem/view (:store eng)) :stair-way)) "a flat run is no stair way"))))))

(deftest a-pickaxe-that-breaks-in-the-last-stair-dig-says-tool-broke-and-tool-none
  ;; no equip follows the last dig, so only the after-dig check can tell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [spec {:blocks ground :inventory [{:name "stone_pickaxe" :count 1 :durability 4}]}
              total (count (digs (:p (await (stair! spec east (fn [_]))))))
              n (atom 0)
              prep (fn [p]
                     (let [world (.-world p)]
                       (.override world "dig"
                                  (fn ^:async f [token a impl]
                                    (let [r (await (impl token a))]
                                      (when (= total (swap! n inc))
                                        (swap! (.. world -state) assoc :inventory []))
                                      r)))))
              s (await (stair! spec east prep))]
          (is (pos? total))
          (is (= 1 (count (events-of s :tool.broke))))
          (is (= 1 (count (events-of s :tool.none)))))))))

(deftest a-pickaxe-lost-mid-stair-ends-no-tool-with-no-fetch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [n (atom 0)
              prep (fn [p]
                     (let [world (.-world p)]
                       (.override world "dig"
                                  (fn ^:async f [token a impl]
                                    (let [r (await (impl token a))]
                                      (when (= 1 (swap! n inc))
                                        (swap! (.. world -state) assoc :inventory []))
                                      r)))))
              s (await (stair! {:blocks ground} (assoc east :ways #{:carried} :need-drop true) prep))]
          (is (= [:stopped :no-tool "pickaxe"] ((juxt :status :reason :tool) @(:out s))))
          (is (re-find #":need-drop false" (str (:why @(:out s)))) "the stop says how to dig it by hand")
          (is (nil? (waiting s))))))))

(deftest lava-two-ahead-stops-before-opening-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (stair! {:blocks (assoc ground "3,63,0" "lava")}
                                                   (assoc east :steps 5) (fn [_])))]
          (is (= :stopped (:status @out)))
          (is (= :hazard (:reason @out)))
          (is (= 1 (:steps @out)))
          (is (= "lava" (block-at p [3 63 0])))
          (is (not-any? #(= {:x 2 :y 63 :z 0} %) (digs p)) "the cell beside the lava is not dug")
          (is (= "lava" (:fluid (first (:hazards @out)))))
          (is (empty? (events-of s :stair.stopped)) "the ending is job.ended's, no event restates it"))))))

(deftest water-beside-stops-by-default-and-is-taken-only-when-named
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [wet (assoc ground "2,63,1" "water")
              refused (await (stair! {:blocks wet} east (fn [_])))
              taken (:out (await (stair! {:blocks wet} (assoc east :accept #{:fluid-adjacent}) (fn [_]))))]
          (is (= :hazard (:reason @(:out refused))))
          (is (= [2 63 0] (:cell @(:out refused))))
          (is (not-any? #(= {:x 2 :y 63 :z 0} %) (digs (:p refused))) "the cell beside the water is not opened")
          (is (= :done (:status @taken))))))))

(deftest water-found-beside-a-cell-the-stair-just-dug-is-plugged-with-a-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [prep (fn [p]
                     (let [world (.-world p)]
                       (.override world "dig"
                                  (fn ^:async f [token a impl]
                                    (let [r (await (impl token a))]
                                      (swap! (.. world -state) assoc-in [:blocks [1 64 1]] "water")
                                      r)))))
              {:keys [out p]} (await (stair! {:blocks ground :inventory (conj pick {:name "cobblestone" :count 8})} east prep))]
          (is (= :hazard (:reason @out)))
          (is (= [1 64 0] (:plugged @out)))
          (is (= "cobblestone" (block-at p [1 64 0])) "the opened cell is shut again"))))))

(deftest a-later-stair-never-digs-the-plug-an-earlier-one-set-against-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [prep (fn [p]
                     (let [world (.-world p)]
                       (.override world "dig"
                                  (fn ^:async f [token a impl]
                                    (let [r (await (impl token a))]
                                      (swap! (.. world -state) assoc-in [:blocks [1 64 1]] "water")
                                      r)))))
              {:keys [out p eng clock] :as first-run} (await (stair! {:blocks ground :inventory (conj pick {:name "cobblestone" :count 8})} east prep))
              _ (is (= [1 64 0] (:plugged @out)))
              _ (swap! (.. (.-world p) -state) assoc-in [:blocks [1 64 1]] "stone")
              digs-before (count (digs p))
              out2 (atom :not-done)
              id (core/submit! eng '(jobs.access.stair {:dir :down :heading :east :steps 3 :here true}) {})
              _ (await (tick-out! (assoc first-run :out out2 :id id)))
              stopped (last (events-of first-run :stopped))]
          (is (= "cobblestone" (block-at p [1 64 0])) "the plug stays shut")
          (is (= digs-before (count (digs p))) "nothing was dug")
          (is (= [1 64 0] (:cell stopped)))
          (is (= :plugged (:reason stopped))))))))

(deftest water-beside-a-dug-cell-with-no-block-to-plug-it-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [prep (fn [p]
                     (let [world (.-world p)]
                       (.override world "dig"
                                  (fn ^:async f [token a impl]
                                    (let [r (await (impl token a))]
                                      (swap! (.. world -state) assoc-in [:blocks [1 64 1]] "water")
                                      (swap! (.. world -state) assoc :inventory pick)
                                      r)))))
              {:keys [out]} (await (stair! {:blocks ground} (assoc east :ways #{:carried}) prep))]
          (is (= :water-unplugged (:reason @out)))
          (is (= [1 64 0] (:cell @out))))))))

(deftest water-let-into-the-cut-stops-the-stair
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [prep (fn [p]
                     (let [world (.-world p)]
                       (.override world "dig"
                                  (fn ^:async f [token a impl]
                                    (let [r (await (impl token a))
                                          {:keys [x y z]} (js->clj (.-pos a) :keywordize-keys true)]
                                      (swap! (.. world -state) assoc-in [:blocks [x y z]] "water")
                                      r)))))
              {:keys [out]} (await (stair! {:blocks ground} (assoc east :accept #{:fluid-adjacent}) prep))]
          (is (= :fluid-in-cut (:reason @out)))
          (is (= [1 64 0] (:cell @out))))))))

(deftest air-under-the-next-floor-stops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (stair! {:blocks (dissoc ground "2,61,0")} east (fn [_])))]
          (is (= :cave-below (:reason @out)))
          (is (= [2 61 0] (:cell @out)))
          (is (= 1 (:steps @out)))
          (is (= "stone" (block-at p [2 64 0])) "nothing of the step is cut"))))))

(deftest air-under-the-next-floor-is-bridged-when-filler-is-carried
  (is (nil? (:reason (stop {[1 62 0] "cave_air"} #{:fluid-adjacent} :filler? true))))
  (is (= :cave-below (:reason (stop {[1 62 0] "water"} #{:fluid-adjacent} :filler? true))) "a fluid under it still stops"))

(deftest a-stair-goes-on-down-through-a-cave-with-filler
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [inv (conj pick {:name "cobblestone" :count 10})
              {:keys [out p]} (await (stair! {:blocks (dissoc ground "2,61,0") :inventory inv} east (fn [_])))]
          (is (= :done (:status @out)))
          (is (= 3 (:steps @out))))))))

(def cave-gap
  "Open air at x 1, y 62..65 over the stone ground: the floor of the first step up is missing, and so is what is under it."
  (apply dissoc ground (for [y (range 62 65) z (range -2 3)] (str "1," y "," z))))

(def up-east {:dir :up :heading :east :steps 1})

(deftest a-gap-in-the-next-floor-is-bridged-with-carried-filler
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [inv (conj pick {:name "cobblestone" :count 10})
              {:keys [out p]} (await (stair! {:blocks cave-gap :inventory inv} up-east (fn [_])))]
          (is (= :done (:status @out)))
          (is (= [1 66 0] (:at @out)))
          (is (= "cobblestone" (block-at p [1 65 0])) "the floor was placed")
          (is (= "cobblestone" (block-at p [1 64 0])) "from the cell under it, the only one with a face (the ground beside it)")
          (is (= {"cobblestone" 2} (:spent @out)))
          (is (= [[1 64 0] [1 65 0]] (:spent-at @out)))
          (is (= 2 (count (filter #(= "place" (.-name %)) (.-calls (.-world p)))))))))))

(deftest a-body-on-the-edge-of-its-cell-overlaps-the-floor-it-would-place-beside-it
  (is (sstep/box-overlaps? {:x 20299.77 :y 161 :z 20336.5} [20300 161 20336]) "x .77: the box reaches .07 into the next cell")
  (is (not (sstep/box-overlaps? {:x 20299.5 :y 161 :z 20336.5} [20300 161 20336])) "centred")
  (is (not (sstep/box-overlaps? {:x 20299.77 :y 161 :z 20336.5} [20300 160 20336])) "a cell under the feet is free")
  (let [pos {:x 20299.695 :y 161 :z 20336.5} cell [20300 161 20336]]
    (is (not (sstep/box-overlaps? pos cell)) "x .695: the server's box just stops at the face")
    (is (sstep/box-overlaps? pos cell constants/body-half-width) "the client's wider box reaches a hair into it")))

(defn scaffold [{:keys [eng]}] (mapv #(select-keys % [:cell :item :purpose]) (ledger/open-entries (mem/view (:store eng)))))

(deftest a-noted-stair-leaves-its-bridged-floor-to-cleanup
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [inv (conj pick {:name "cobblestone" :count 10})
              noted (assoc up-east :note {:escalation true})
              floor [{:cell [1 64 0] :item "cobblestone" :purpose :stair-floor}
                     {:cell [1 65 0] :item "cobblestone" :purpose :stair-floor}]]
          (is (= floor (scaffold (await (stair! {:blocks cave-gap :inventory inv} noted (fn [_])))))
              "go-to's stair: the floor goes into the scaffold ledger")
          (is (= [] (scaffold (await (stair! {:blocks cave-gap :inventory inv} up-east (fn [_])))))
              "another caller's stair keeps its floor")
          (is (= [] (await (settings/with-settings {:jobs.access.stair.limits/take-back-floors? false}
                             (fn ^:async kept [] (scaffold (await (stair! {:blocks cave-gap :inventory inv} noted (fn [_]))))))))
              "the setting keeps go-to's floor too"))))))

(deftest a-gap-in-the-next-floor-without-filler-gets-some-and-bridges
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (stair! {:blocks (assoc cave-gap "-1,64,0" "dirt" "-1,64,1" "dirt" "-1,64,-1" "dirt") :inventory pick} up-east (fn [_])))]
          (is (= :done (:status @out)) (pr-str (:ending @out)))
          (is (not= "air" (block-at p [1 65 0])) "the floor was placed with blocks it got"))))))

(deftest a-gap-in-the-next-floor-without-filler-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [inv (conj pick {:name "diamond" :count 5} {:name "gold_block" :count 5})
              {:keys [out p]} (await (stair! {:blocks cave-gap :inventory inv} (assoc up-east :ways #{:carried}) (fn [_])))]
          (is (= :no-floor (:reason @out)))
          (is (= :none (:filler @out)))
          (is (re-find #"filler" (:why @out)))
          (is (empty? (filter #(= "place" (.-name %)) (.-calls (.-world p))))))))))

(deftest a-top-level-stair-with-no-progress-ends-stopped-not-completed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              [seen sink] (tu/capture-sink)
              p (tu/fake {:self {:pos {:x 0 :y 65 :z 0}} :inventory pick :blocks cave-gap})
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)
                                :now #(deref clock) :world (world/of-data {} {} [])
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (core/submit! eng (list job (assoc up-east :ways #{:carried})) {})
          (await (tick-out! {:eng eng :clock clock}))
          (let [ended (filter #(and (= :job (:source %)) (#{:completed :stopped} (:kind %))) @seen)
                e (first ended)]
            (is (= 1 (count ended)))
            (is (= :stopped (:kind e)))
            (is (= :stopped (get-in e [:data :status])))
            (is (= :no-floor (get-in e [:data :reason])))
            (is (= [1 65 0] (get-in e [:data :cell])))
            (is (= 0 (get-in e [:data :steps])))))))))

(deftest bedrock-in-the-cut-is-unbreakable-not-no-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [inventory [pick []]]
          (let [{:keys [out p]} (await (stair! {:blocks (assoc ground "1,64,0" "bedrock") :inventory inventory} east (fn [_])))]
            (is (= :unbreakable (:reason @out)))
            (is (= [1 64 0] (:cell @out)))
            (is (= "bedrock" (:block @out)))
            (is (empty? (digs p)))))))))

(def cave-below
  "A cave under the first step down: the next feet cell (1 64 0) is open (already cut), and the floor under it at (1 63 0)
  has only air round it, but the cell over it (the feet cell) whose underside cannot be aimed at."
  (-> (apply dissoc ground (for [x (range 0 3) y (range 60 64) z (range -1 2)] (str x "," y "," z)))
      (dissoc "1,64,0" "1,65,0")))

(deftest a-cave-floor-with-a-solid-cell-further-back-is-bridged-from-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [inv (conj pick {:name "cobblestone" :count 10})
              {:keys [out p]} (await (stair! {:blocks cave-below :inventory inv} (assoc east :steps 1) (fn [_])))]
          (is (= :done (:status @out)) (pr-str (dissoc @out :dug)))
          (is (= "cobblestone" (block-at p [0 63 0])) "under the floor the body stands on, against the solid cell behind it")
          (is (= "cobblestone" (block-at p [1 63 0])) "the missing floor"))))))

(def cave-beside-wall
  "cave-below with the cave one cell narrower: the side wall (z -1) stays, so the missing floor (1 63 0) has a solid neighbour in sight."
  (-> (apply dissoc ground (for [x (range 0 3) y (range 60 64) z (range 0 2)] (str x "," y "," z)))
      (dissoc "1,64,0" "1,65,0")))

(deftest a-cave-floor-beside-a-seen-wall-is-bridged-against-it-and-the-stair-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [inv (conj pick {:name "cobblestone" :count 10})
              {:keys [out p]} (await (stair! {:blocks cave-beside-wall :inventory inv} (assoc east :steps 1) (fn [_])))]
          (is (= :done (:status @out)) (pr-str (dissoc @out :dug)))
          (is (= "cobblestone" (block-at p [1 63 0])) "the missing floor, placed against the side wall")
          (is (= 1 (count (filter #(= "place" (.-name %)) (.-calls (.-world p))))) "one block, no chain behind it"))))))

(def cave-void
  "The cave of cave-below reaches back past the bridge depth and on under the whole ground: no solid cell within reach of
  the floor, and no flat run ahead reaches one."
  (apply dissoc cave-below (for [x (range -3 13) y (range 60 64) z (range -2 3)] (str x "," y "," z))))

(deftest a-stair-that-cannot-bridge-a-cave-shuts-its-opening-or-names-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [inv (conj pick {:name "cobblestone" :count 10})
              sealed (await (stair! {:blocks cave-void :inventory inv} (assoc east :steps 1) (fn [_])))
              bare (await (stair! {:blocks cave-void :inventory pick} (assoc east :steps 1 :ways #{:carried}) (fn [_])))]
          (is (= :no-support-floor (:reason @(:out sealed))))
          (is (= [[1 64 0]] (:sealed @(:out sealed))))
          (is (= "cobblestone" (block-at (:p sealed) [1 64 0])) "the cut cell the cave opens into is shut")
          (is (= "air" (block-at (:p sealed) [1 63 0])) "no floor was placed")
          (is (= :no-floor (:reason @(:out bare))))
          (is (= [[1 64 0]] (:open-to @(:out bare)))))))))

(deftest a-bridge-plan-the-world-answers-no-support-to-gives-way-to-the-next
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [inv (conj pick {:name "cobblestone" :count 10})
              n (atom 0)
              prep (fn [p]
                     (let [world (.-world p)]
                       (.override world "place"
                                  (fn ^:async f [token a impl]
                                    (if (= 1 (swap! n inc))
                                      #js {:status "no-support"}
                                      (await (impl token a)))))))
              {:keys [out p]} (await (stair! {:blocks (dissoc ground "1,63,0" "1,62,0") :inventory inv} (assoc east :steps 1) prep))]
          (is (= :done (:status @out)) (pr-str (dissoc @out :dug)))
          (is (< 1 @n) "a second plan was tried")
          (is (= "cobblestone" (block-at p [1 63 0]))))))))

(deftest one-refused-place-of-a-bridged-floor-is-tried-again-the-next-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [inv (conj pick {:name "cobblestone" :count 10})
              n (atom 0)
              prep (fn [p]
                     (.override (.-world p) "place"
                                (fn ^:async f [token a impl]
                                  (if (= 1 (swap! n inc))
                                    #js {:status "failed" :reason "place-failed: block is still air"}
                                    (await (impl token a))))))
              {:keys [out p]} (await (stair! {:blocks (dissoc ground "1,63,0" "1,62,0") :inventory inv} (assoc east :steps 1) prep))]
          (is (= :done (:status @out)) (pr-str (dissoc @out :dug)))
          (is (= "cobblestone" (block-at p [1 63 0]))))))))

(def ledge-over-void
  "The body stands on the last stone of a ledge (x 0, y 64) with a void under it and east of it (x 1..5), nothing to place against,
  then solid ground again."
  (apply dissoc ground (concat (for [x (range -3 6) y (range 56 64) z (range -2 3)] (str x "," y "," z))
                               (for [x (range 1 6) z (range -2 3)] (str x ",64," z)))))

(deftest a-stair-down-on-a-ledge-over-a-void-bridges-flat-to-the-far-side-and-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [inv (conj pick {:name "cobblestone" :count 16})
              {:keys [out p]} (await (stair! {:blocks ledge-over-void :inventory inv} east (fn [_])))]
          (is (= :done (:status @out)) (pr-str (dissoc @out :dug)))
          (is (every? #(not= "air" (block-at p [% 64 0])) [1 2 3 4]) "the flat bridge over the void")
          (is (= 3 (:steps @out)) "the three steps asked, counted from the bridge's end")
          (is (= [7 62 0] (:at @out))))))))

(deftest a-floor-the-world-keeps-refusing-ends-place-failed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [inv (conj pick {:name "cobblestone" :count 10})
              prep (fn [p]
                     (.override (.-world p) "place"
                                (fn ^:async f [_ _ _] #js {:status "failed" :reason "place-failed: block is still air"})))
              {:keys [out]} (await (stair! {:blocks (dissoc ground "1,63,0" "1,62,0") :inventory inv} (assoc east :steps 1) prep))]
          (is (= :place-failed (:reason @out)) (pr-str (dissoc @out :dug))))))))

(deftest lava-for-a-floor-is-never-bridged-it-is-exposed-lava
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [inv (conj pick {:name "cobblestone" :count 10})
              world (assoc cave-gap "1,65,0" "lava")
              {:keys [out p]} (await (stair! {:blocks world :inventory inv} up-east (fn [_])))
              stopped (:out (await (stair! {:blocks world :inventory inv} (assoc up-east :on-lava :stop) (fn [_]))))]
          (is (= :lava-unsealed (:reason @out)) "beside the feet: sealed, here with nothing to place it against")
          (is (= :no-support (:place @out)))
          (is (empty? (filter #(= "place" (.-name %)) (.-calls (.-world p)))))
          (is (= :lava-exposed (:reason @stopped))))))))

(deftest a-zone-ahead-stops-at-its-edge
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zone {:name "garden" :min [3 50 -2] :max [6 70 2] :allow #{:place}}
              {:keys [out p]} (await (stair! {:blocks ground :zones [zone]} (assoc east :steps 5) (fn [_])))]
          (is (= :zone (:reason @out)))
          (is (= "garden" (:zone @out)))
          (is (= 2 (:steps @out)))
          (is (not-any? #(<= 3 (:x %)) (digs p))))))))

(deftest a-first-step-wholly-in-a-zone-fails-its-condition-and-digs-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zone {:name "garden" :min [1 50 -2] :max [6 70 2]}
              {:keys [out p]} (await (stair! {:blocks ground :zones [zone]} (assoc east :steps 5) (fn [_])))]
          (is (= [:stopped :refused] ((juxt :status :reason) @out)) "a failed condition ends the child")
          (is (= ["garden"] (:zones (:cause (:ending @out)))))
          (is (empty? (digs p))))))))

(deftest no-zone-list-stops-before-any-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (stair! {:blocks ground :zones nil} east (fn [_])))]
          (is (= :no-zones (:reason @out)))
          (is (= 0 (:steps @out)))
          (is (empty? (digs p))))))))

(deftest a-full-pack-with-nothing-to-toss-waits-before-a-lost-drop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [full (into pick (repeat 35 {:name "diamond" :count 64}))
              {:keys [out p] :as s} (await (stair! {:blocks ground :inventory full} east (fn [_])))]
          (is (= [:stopped :no-free-slot] ((juxt :status :reason) @out)) "the stair ends for its caller to decide")
          (is (empty? (digs p))))))))

(deftest a-stack-with-room-is-room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [full (into pick (cons {:name "cobblestone" :count 10} (repeat 34 {:name "dirt" :count 64})))
              {:keys [out]} (await (stair! {:blocks ground :inventory full} east (fn [_])))]
          (is (= :done (:status @out))))))))

(deftest no-pickaxe-for-stone-stops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (stair! {:blocks ground :inventory []} (assoc east :ways #{:carried} :need-drop true) (fn [_])))]
          (is (= [:condition-failed :no-tool] ((juxt :kind contract/cause-key) (:ending @out))))
          (is (= "pickaxe" (:tool (:cause (:ending @out)))))
          (is (empty? (digs p))))))))

(deftest with-need-drop-unset-a-stair-no-pickaxe-can-be-had-for-digs-by-hand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [args [(assoc east :ways #{:carried}) east]]
          (let [{:keys [out p]} (await (stair! {:blocks ground :inventory []} args (fn [_])))]
            (is (= [:done 3] [(:status @out) (:steps @out)]) "no fetch allowed, or the fetch failed: the cut is dug bare-handed")
            (is (seq (digs p)))))))))

(deftest bad-args-end-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (stair! {:blocks ground :raw true} {:dir :down} (fn [_])))]
          (is (= [:impossible :bad-args] [(:kind @out) (contract/cause-key @out)]))
          (is (string? (:why (:cause @out)))))))))

(deftest y-behind-the-feet-is-a-failed-condition-not-impossible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (stair! {:blocks ground} {:dir :down :heading :east :y 70} (fn [_])))]
          (is (= [:condition-failed :y-not-beyond-feet] ((juxt :kind contract/cause-key) (:ending @out))))
          (is (empty? (digs p))))))))

(deftest a-stair-to-the-height-the-feet-stand-at-is-already-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (stair! {:blocks ground} {:dir :down :heading :east :y 65} (fn [_])))]
          (is (= :done (:status @out)))
          (is (:already? @out))
          (is (empty? (digs p))))))))

(defn ^:async dig-reached!
  "Resolves once the job's dig call is waiting on the world."
  [world]
  (loop []
    (when-not (some #(= "dig" (.-name %)) (.-calls world))
      (await (js/Promise. (fn [resolve] (js/setTimeout resolve 5))))
      (recur))))

(deftest a-cut-mid-dig-resumes-to-the-same-stair
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p out] :as s} (setup {:blocks ground} east (fn [_]))
              world (.-world p)]
          (await (core/tick! eng))
          (.hold world "dig")
          (let [running (core/tick! eng)]
            (await (dig-reached! world))
            (core/cut! eng (core/holder eng) :test nil)
            (await running))
          (await (tick-out! s))
          (is (= :done (:status @out)))
          (is (= [3 62 0] (:at @out)))
          (is (= 6 (count (:dug @out)))))))))

(deftest a-cut-after-a-dig-landed-still-records-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p out] :as s} (setup {:blocks ground} east (fn [_]))
              world (.-world p)]
          (.override world "dig" (fn ^:async f [token a impl]
                                   (let [r (await (impl token a))]
                                     (.override world "dig" nil)
                                     (core/cut! eng (core/holder eng) :test nil)
                                     r)))
          (dotimes [_ 3] (await (core/tick! eng)))
          (await (tick-out! s))
          (is (= :done (:status @out)))
          (is (= 6 (count (:dug @out))))
          (is (= 6 (count (digs p)))))))))

(deftest no-way-back-stops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [prep (fn [p]
                     (let [world (.-world p)]
                       (.override world "steer"
                                  (fn ^:async f [token a impl]
                                    (let [r (await (impl token a))]
                                      (swap! (.. world -state) assoc-in [:blocks [0 66 0]] "stone")
                                      r)))))
              {:keys [out p]} (await (stair! {:blocks ground} east prep))]
          (is (= :no-way-back (:reason @out)))
          (is (= 1 (:steps @out)))
          (is (= [1 64 0] (:at @out)))
          (is (= 1 (count (digs p))) "nothing more is cut"))))))

(deftest a-step-that-does-not-land-in-the-cut-stops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [prep (fn [p]
                     (let [world (.-world p)]
                       (.override world "steer"
                                  (fn ^:async f [token a impl]
                                    (let [r (await (impl token a))]
                                      (swap! (.. world -state) assoc-in [:self :pos] [1 64 1])
                                      r)))))
              {:keys [out]} (await (stair! {:blocks (dissoc ground "1,64,1")} east prep))]
          (is (= :step-failed (:reason @out)))
          (is (= [1 64 0] (:cell @out)))
          (is (nil? (:walk @out)) "the walk child's ending is the :because, not copied"))))))

(deftest a-falling-block-hazard-names-the-block
  (let [stop (stop {[1 67 0] "gravel" [1 66 0] "stone"} #{:fluid-adjacent} :zones [])]
    (is (re-find #"gravel" (str (:why stop))))))

(deftest a-stair-that-reached-its-last-step-is-done-even-when-the-way-back-is-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [total (count (digs (:p (await (stair! {:blocks ground} east (fn [_]))))))
              n (atom 0)
              prep (fn [p]
                     (let [world (.-world p)]
                       (.override world "dig"
                                  (fn ^:async f [token a impl]
                                    (let [r (await (impl token a))]
                                      (when (= total (swap! n inc))
                                        (swap! (.. world -state) assoc-in [:blocks [0 65 0]] "stone"))
                                      r)))))
              {:keys [out]} (await (stair! {:blocks ground} east prep))]
          (is (pos? total))
          (is (= :done (:status @out)))
          (is (= 3 (:steps @out))))))))

(deftest a-cell-is-judged-again-right-before-its-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [prep (fn [p]
                     (let [world (.-world p)]
                       (.override world "equip"
                                  (fn ^:async f [token a impl]
                                    (swap! (.. world -state) assoc-in [:blocks [1 64 1]] "lava")
                                    (await (impl token a))))))
              {:keys [out p]} (await (stair! {:blocks ground} east prep))]
          (is (= :hazard (:reason @out)))
          (is (= [1 64 0] (:cell @out)))
          (is (empty? (digs p))))))))

(deftest a-cell-that-keeps-refilling-stops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [prep (fn [p]
                     (let [world (.-world p)]
                       (.override world "dig"
                                  (fn ^:async f [token a impl]
                                    (let [r (await (impl token a))]
                                      (swap! (.. world -state) assoc-in [:blocks [1 64 0]] "gravel")
                                      r)))))
              {:keys [out p]} (await (stair! {:blocks ground} east prep))]
          (is (= :refills (:reason @out)))
          (is (= (sset/max-fall-digs) (count (digs p))) "a falling block gets more digs than other refills")
          (is (= [{:cell [1 64 0] :block "stone"}] (:dug @out)) "a dig that left the same block is not recorded"))))))

(deftest gravel-that-falls-into-the-cut-a-few-times-is-dug-until-clear
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [refills (atom 5)
              prep (fn [p]
                     (let [world (.-world p)]
                       (.override world "dig"
                                  (fn ^:async f [token a impl]
                                    (let [r (await (impl token a))]
                                      (when (pos? (swap! refills dec))
                                        (swap! (.. world -state) assoc-in [:blocks [1 64 0]] "gravel"))
                                      r)))))
              {:keys [out]} (await (stair! {:blocks ground} east prep))]
          (is (= :done (:status @out))))))))

(deftest gravel-that-lands-in-the-step-while-the-body-walks-in-is-dug-and-the-step-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (tu/each-async [[1 64 0] [1 65 0]]
          (fn ^:async land-in [cell]
          (let [landed (atom false)
                prep (fn [p]
                       (let [world (.-world p)]
                         (.override world "steer"
                                    (fn ^:async f [token a impl]
                                      (when-not @landed
                                        (reset! landed true)
                                        (swap! (.. world -state) assoc-in [:blocks cell] "gravel"))
                                      (await (impl token a))))))
                {:keys [out p]} (await (stair! {:blocks ground} east prep))]
            (is (= :done (:status @out)) (pr-str cell))
            (is (= 3 (:steps @out)) (pr-str cell))
            (is (= "air" (block-at p cell)) (pr-str cell))))))))))

(def hanging-gravel-world
  "Stone round a body at [0 65 0] with two cells of gravel stacked over the top cell of a stair up east."
  (-> (stone -3 12 50 75)
      (dissoc "0,65,0" "0,66,0")
      (assoc "1,68,0" "gravel" "1,69,0" "gravel")))

(deftest gravel-standing-over-the-open-cut-has-dropped-before-the-body-walks-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock out]} (setup {:blocks hanging-gravel-world} (assoc up-east :accept #{:falling-block}) (fn [_]))
              opened (atom nil)
              walked-under (atom false)]
          (loop [i 0]
            (when (and (< i 300) (seq (:list (core/state eng))))
              (swap! clock + 500)
              (await (core/tick! eng))
              (when (and (nil? @opened) (= "air" (block-at p [1 67 0]))) (reset! opened @clock))
              (when (and (= "gravel" (block-at p [1 68 0])) (< 0.5 (.-x (.-pos (.self p))))) (reset! walked-under true))
              (when (and @opened (>= (- @clock @opened) 2000))
                (doseq [c [[1 68 0] [1 69 0]]] (fake/set-block! p c "air")))
              (recur (inc i))))
          (is (= :done (:status @out)) (pr-str @out))
          (is (false? @walked-under) "the body stayed out of the cut while gravel stood over it"))))))

(deftest a-stair-up-cuts-its-top-cell-from-beside-under-seen-gravel-or-an-unseen-cell-at-low-health
  (let [cut [[0 67 0] [1 66 0] [1 65 0]]]
    (are [over unseen health dir cuts-beside?]
         (= cuts-beside? (some? (stair/undermine-cell (fn [c] (if (= c [0 68 0]) over "stone")) (fn [c] (and unseen (= c [0 68 0]))) health [0 65 0] dir cut)))
      "gravel" false 20 :up true
      "stone" true 6 :up true
      "stone" true 20 :up false
      "stone" false 6 :up false
      "gravel" false 20 :down false)))

(deftest gravel-over-the-bodys-own-column-is-cut-from-beside-so-it-falls-into-the-empty-column
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [landed-under (atom [])
              fall (fn [p] (let [world (.-world p)]
                             (.override world "dig" (fn ^:async f [token a impl]
                                                      (let [r (await (impl token a))
                                                            {:strs [x y z]} (js->clj (.-pos a))]
                                                        (when (= [0 67 0] [x y z])
                                                          (let [to (first (filter #(= "air" (block-at p [0 % 0])) (range 65 68)))]
                                                            (fake/set-block! p [0 68 0] "air")
                                                            (fake/set-block! p [0 to 0] "gravel")
                                                            (swap! landed-under conj (= 0 (js/Math.floor (.. p (self) -pos -x))))))
                                                        r)))))
              {:keys [out p]} (await (stair! {:blocks (-> (stone -3 12 50 75) (dissoc "0,65,0" "0,66,0" "-1,65,0" "-1,66,0") (assoc "0,68,0" "gravel"))}
                                             (assoc up-east :accept #{:falling-block}) fall))]
          (is (= [false] @landed-under) "the gravel fell while the body stood out of its column")
          (is (= :done (:status @out)) (pr-str @out))
          (is (every? #(= "air" (block-at p [0 % 0])) [65 66 67]) "the gravel that fell was dug"))))))

(deftest gravel-that-keeps-landing-on-the-body-ends-the-stair-instead-of-suffocating-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [land (fn [p] (let [world (.-world p)]
                             (.override world "dig" (fn ^:async f [token a impl]
                                                      (let [r (await (impl token a))
                                                            pos (.-pos (.self p))]
                                                        (fake/set-block! p [(js/Math.floor (.-x pos)) (inc (js/Math.floor (.-y pos))) (js/Math.floor (.-z pos))] "gravel")
                                                        r)))))
              {:keys [out p]} (await (stair! {:blocks (dissoc (stone -3 12 50 75) "0,65,0" "0,66,0")}
                                             {:dir :up :heading :east :steps 6 :accept #{:falling-block}} land))]
          (is (= :stopped (:status @out)))
          (is (= :hazard (:reason @out)) (pr-str (:reason @out)))
          (is (<= (count (digs p)) (+ 3 sstep/max-landings))))))))

(deftest a-cell-put-back-that-does-not-fall-is-named-as-placed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [prep (fn [p]
                     (let [world (.-world p)]
                       (.override world "dig"
                                  (fn ^:async f [token a impl]
                                    (let [r (await (impl token a))]
                                      (swap! (.. world -state) assoc-in [:blocks [1 64 0]] "stone")
                                      r)))))
              {:keys [out]} (await (stair! {:blocks ground} east prep))]
          (is (= :refills (:reason @out)))
          (is (re-find #"put back" (str (:why @out)))))))))

(deftest the-bodys-own-plan-footprint-ahead-stops-the-stair-unless-the-spare-own-builds-setting-is-false
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (shape/with-author {:id "wall" :parts [{:id "w" :cells [[2 63 0]] :want "stone"}]} "Fake")
              dug? (fn [p] (some #(= {:x 2 :y 63 :z 0} %) (digs p)))
              {:keys [out p]} (await (stair! {:blocks ground :plans {"wall" plan}} east (fn [_])))
              {out2 :out p2 :p} (await (stair! {:blocks ground :plans {"wall" plan}} (assoc east :settings {:jobs.lib.access/spare-own-builds false}) (fn [_])))]
          (is (= :footprint (:reason @out)))
          (is (= "wall" (:plan @out)))
          (is (not (dug? p)))
          (is (not= :footprint (:reason @out2)))
          (is (dug? p2)))))))

(deftest another-plans-footprint-ahead-stops-and-names-the-plan
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "wall" :parts [{:id "w" :cells [[2 63 0]] :want "stone"}]}
              {:keys [out p]} (await (stair! {:blocks ground :plans {"wall" plan}} east (fn [_])))]
          (is (= :footprint (:reason @out)))
          (is (= "wall" (:plan @out)))
          (is (= [2 63 0] (:cell @out)))
          (is (not-any? #(= {:x 2 :y 63 :z 0} %) (digs p))))))))

;; ---------------------------------------------------------------- way-back

(defn ^:async way-back-of
  "drive/way-back from (1 64 1) to origin over the block map (its answer, awaited)."
  [blocks origin]
  (await (drive/way-back {:primitives (tu/fake {:blocks blocks :self {:pos {:x 1 :y 64 :z 1}}})} origin)))

(defn slab
  "Stone at y for x in xs, z 0..2."
  [y xs]
  (into {} (for [x xs z (range 3)] [(str x "," y "," z) "stone"])))

(deftest way-back-judges-the-plan-like-walk-plan
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [level (merge (slab 63 (range 5)) (slab 63 (range 7 12)))]
          (doseq [[blocks origin expected]
                  [[level [10 64 1] nil]
                   [(merge (slab 63 (range 5)) (slab 64 (range 7 12))) [10 65 1]
                    {:reason :no-way-back :why :refused :kind :gap-up}]
                   [(merge level (slab 66 (range 4 7))) [10 64 1]
                    {:reason :no-way-back :why :refused :kind :gap-low-ceiling}]]]
            (is (= expected (some-> (await (way-back-of blocks origin)) (dissoc :step))))))))))

(deftest way-back-walks-round-a-gap-under-a-low-ceiling
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [at (fn [y xs zs] (into {} (for [x xs z zs] [(str x "," y "," z) "stone"])))
              blocks (merge (at 63 (range 5) (range -8 11)) (at 63 (range 7 13) (range -8 11)) (at 63 [5 6] [9 10])
                            (at 66 (range 4 7) (range -8 9)))]
          (is (nil? (await (way-back-of blocks [10 64 1])))))))))

;; the way-back search runs in slices: a timer set before it fires before it ends
(deftest way-back-yields-to-other-work-during-its-search
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [order (atom [])]
          (js/setTimeout #(swap! order conj :timer) 0)
          (await (tu/with-expansion-budget 16 #(way-back-of (slab 63 (range 60)) [50 64 1])))
          (swap! order conj :planned)
          (is (= [:timer :planned] @order)))))))

(def around-the-cut {:name "plot" :min [1 60 -1] :max [3 70 1]})

(deftest a-stair-in-a-zone-follows-its-owner-and-the-opt-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[owner extra status] [["Fake" {} :done]
                                      ["fake" {} :done]
                                      ["Miles" {} :stopped]
                                      ["Miles" {:ignore-zones? true} :done]]]
          (let [{:keys [out]} (await (stair! {:blocks ground :zones [(assoc around-the-cut :owner owner)]}
                                             (merge east extra) (fn [_])))]
            (is (= status (:status @out)) (pr-str [owner extra]))))))))

(deftest an-up-stair-ignores-air-under-its-solid-floor
  (is (nil? (:reason (stop {[1 65 0] "stone" [1 64 0] "cave_air"} #{:fluid-adjacent} :dir :up))))
  (is (= :cave-below (:reason (stop {[1 62 0] "cave_air"} #{:fluid-adjacent}))) "going down still refuses"))

(deftest a-level-step-ignores-what-is-under-its-floor
  (are [named] (nil? (:reason (stop named #{:fluid-adjacent} :dir :level)))
    {[1 63 0] "cave_air"}
    {[1 63 0] "water"}))

(deftest by-hand-digs-stone-with-no-pickaxe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (stair! {:blocks ground :inventory []} (assoc east :ways #{:carried} :need-drop false) (fn [_])))]
          (is (= :done (:status @out)) (pr-str @out))
          (is (nil? (waiting s)))
          (is (seq (digs p)) "dug bare-handed"))))))

(deftest only-a-root-stair-marks-itself-for-the-ore-seen-trigger
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [marks (atom 0)
              run (fn ^:async run1 [root?]
                    (reset! marks 0)
                    (with-redefs [ore-pick/mark! (fn [_ _] (swap! marks inc))]
                      (let [s (setup {:blocks ground} (atom east) (fn [_]))]
                        (when root? (core/submit! (:eng s) (list 'jobs.access.stair east) {}))
                        (await (tick-out! s))))
                    @marks)]
          (is (pos? (await (run true))) "a stair called by an agent")
          (is (zero? (await (run false))) "a stair a parent runs"))))))

(deftest a-stair-leaves-an-exposed-ore-beside-the-way-to-the-ore-seen-trigger
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [on (await (stair! {:blocks (assoc ground "1,64,1" "coal_ore")} east tu/seeing-all))]
          (is (= :done (:status @(:out on))))
          (is (= "coal_ore" (block-at (:p on) [1 64 1]))))))))

(defn ^:async with-sight-of-no-sky
  "(f) with the body underground whatever the fake's missing light data say (the column fallback of sh-sense/buried? sees only
  the cell over the head, which a stair's own clearance opens)."
  [f]
  (with-redefs [sh-sense/buried? (constantly true) sh-sense/buried-at? (constantly true)] (await (f))))

(deftest a-stair-goes-on-cutting-and-walking-its-own-steps-at-night-from-a-dug-in-start-underground
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (with-sight-of-no-sky #(stair! {:blocks (dissoc ground "0,60,0" "0,61,0") :self {:pos {:x 0 :y 60 :z 0}} :time 18000} east (fn [p] (tu/seeing-all p) (swap! (fake/state p) assoc :light-default [0 0])))))]
          (is (= :done (:status @out)) "not held night-sheltered between its steps")
          (is (= 3 (:steps @out)))
          (is (nil? (waiting s))))))))

(defn ^:async night-stair!
  [args]
  (await (stair! {:blocks ground :time 13000} args (fn [p] (tu/seeing-all p) (sky/with-sky p)))))

(deftest a-stair-down-from-open-ground-at-night-digs-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (await (night-stair! east))]
          (is (not= :exposed (:reason (waiting s))) "the first step shelters: no wait for light")
          (is (seq (digs p))))))))

(deftest a-stair-up-from-open-ground-at-night-waits-exposed-and-digs-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (night-stair! (assoc east :dir :up)))]
          (is (= [:stopped :exposed] ((juxt :status :reason) @out)) "a called stair ends at once with the reason")
          (is (empty? (digs p))))))))

(deftest a-pickaxe-less-stair-looks-at-the-unseen-cut-first-and-then-waits-no-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [shown (atom false)
              unseen (fn [p]
                       (aset p "sensedAt" (fn [pos] (if @shown (.blockAt p pos) #js {:unknown true})))
                       (.override (.-world p) "look" (fn [token a impl] (reset! shown true) (impl token a))))
              {:keys [eng clock p]} (setup {:blocks ground :inventory []} (assoc east :ways #{:carried} :need-drop true) unseen)
              id (core/submit! eng (list job (assoc east :ways #{:carried} :need-drop true)) {})]
          (dotimes [_ 6]
            (swap! clock + 700)
            (await (core/tick! eng)))
          (is (re-find #":need-drop false" (:hint (core/waiting eng id))) "the wait says how to dig it by hand")
          (is (= :no-tool (:reason (core/waiting eng id))) "listed: it waits, it does not start and end")
          (is (empty? (digs p))))))))

(deftest a-stair-with-only-a-spent-pickaxe-stops-tool-spent-unless-told-otherwise
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[uses args dug? status reason] [[3 east false :stopped :tool-spent]
                                                [4 east true :done nil]
                                                [3 (assoc east :settings {:jobs.lib.trip/spent-uses 0}) true :done nil]]]
          (let [{:keys [out p]} (await (stair! {:blocks ground :inventory [{:name "stone_pickaxe" :count 1 :durability uses}]} args (fn [_])))]
            (is (= dug? (boolean (seq (digs p)))) (pr-str uses args))
            (is (= [status reason] [(:status @out) (:reason @out)]) (pr-str uses args))))))))

(deftest a-way-out-stair-with-a-worn-last-pickaxe-gets-a-spare-first-though-it-may-use-the-last-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (stair! {:blocks (assoc ground "-2,64,0" "chest") :containers {"-2,64,0" [{:name "stone_pickaxe" :count 1}]}
                                :inventory [{:name "stone_pickaxe" :count 1 :durability 4 :max-durability 131}]}
                               (assoc east :settings {:jobs.lib.trip/spent-uses 0}) tu/seeing-all))]
          (is (= :done (:status @(:out s))))
          (is (= 1 (count (events-of s :tool.spare)))))))))

(deftest a-stair-through-dirt-goes-on-with-only-a-worn-shovel
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dirt (update-vals ground (constantly "dirt"))
              {:keys [out p]} (await (stair! {:blocks dirt :inventory [{:name "stone_shovel" :count 1 :durability 3}]} east (fn [_])))]
          (is (seq (digs p)))
          (is (= [:done nil] [(:status @out) (:reason @out)])))))))

(deftest a-stair-submitted-far-from-where-the-body-stands-fails-moved-since-submit-unless-told-here

  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[extra dug? reason] [[{} true nil]
                                     [{:submitted-at {:x 1 :y 65 :z 0}} true nil]
                                     [{:submitted-at {:x 80 :y 65 :z 0}} false :moved-since-submit]
                                     [{:submitted-at {:x 80 :y 65 :z 0} :here true} true nil]]]
          (let [{:keys [p] :as s} (await (stair! {:blocks ground :inventory pick} (merge east extra) (fn [_])))
                w (:cause (:ending @(:out s)))]
            (is (= dug? (boolean (seq (digs p)))) (pr-str extra))
            (is (= reason (:reason w)) (pr-str extra))
            (is (= (when reason 80) (:distance w)) (pr-str extra))))))))

(deftest a-stair-never-digs-the-floor-under-a-door
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (assoc ground "2,65,0" "dirt" "2,66,0" "oak_door" "2,67,0" "oak_door")
              {:keys [out p]} (await (stair! {:blocks blocks} east (fn [_])))]
          (is (= [:stopped :base-block] [(:status @out) (:reason @out)]) (pr-str @out))
          (is (not-any? #(= {:x 2 :y 65 :z 0} %) (digs p)) "the block the door stands on is kept"))))))

;; ------------------------------------------------------------------ the contract

(deftest the-job-exports-the-contract-and-quotes-validly
  (let [def (get registry/jobs job)
        c {:primitives (tu/fake {:self {:pos {:x 0 :y 65 :z 0}} :blocks ground :inventory pick}) :args east}
        q ((:quote-of def) c)]
    (is (= [:dir :heading :y] (:target def)))
    (is (some? (:can-start? def)))
    (is (nil? (:check def)))
    (is (nil? (contract/quote-problem q)) (pr-str q))
    (is (= [3 62 0] (:moves-to q)))
    (is (< (js/Math.abs (- (* 3 (nth (get-in ((:quote-of def) (assoc-in c [:args :dir] :level)) [:uses :job-time]) 1))
                           (* 2 (nth (get-in q [:uses :job-time]) 1))))
           1e-9)
        "a level step cuts 2 cells, a stair step 3: the mean time is 2/3 of it")))

(defn ^:async ending-of
  "The ending of a stair run as a contract parent's ctx/call child reads it, as [kind cause-key]."
  [spec args prep]
  (let [e (:out (await (stair! (assoc spec :raw true) args prep)))]
    [(:kind @e) (contract/cause-key @e)]))

(deftest a-stair-ends-with-a-kind-and-a-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [refill (fn [p] (let [world (.-world p)]
                               (.override world "dig" (fn ^:async f [token a impl]
                                                        (let [r (await (impl token a))]
                                                          (swap! (.. world -state) assoc-in [:blocks [1 64 0]] "gravel")
                                                          r)))))
              lose-floor (fn [p] (let [world (.-world p)]
                                   (.override world "steer" (fn ^:async f [token a impl]
                                                              (let [r (await (impl token a))]
                                                                (swap! (.. world -state) assoc-in [:blocks [0 66 0]] "stone")
                                                                r)))))
              strand (fn [p] (let [world (.-world p)]
                               (.override world "steer" (fn ^:async f [token a impl]
                                                          (let [r (await (impl token a))]
                                                            (swap! (.. world -state) assoc-in [:self :pos] [1 64 1])
                                                            r)))))
              run (fn ^:async run1 [spec args prep] (await (ending-of spec args prep)))]
          (is (= [:done :done] (await (run {:blocks ground} east (fn [_])))))
          (is (= [:condition-failed :crop] (await (run {:blocks (assoc ground "1,64,0" "wheat")} east (fn [_])))))
          (is (= [:condition-failed :base-block] (await (run {:blocks (assoc ground "1,65,0" "chest")} east (fn [_])))))
          (is (= [:condition-failed :no-zones] (await (run {:blocks ground :zones nil} east (fn [_])))))
          (is (= [:impossible :bad-args] (await (run {:blocks ground} {:dir :down} (fn [_])))))
          (is (= [:condition-failed :no-tool] (await (run {:blocks ground :inventory []} (assoc east :ways #{:carried} :need-drop true) (fn [_])))))
          (is (= [:condition-failed :no-free-slot]
                 (await (run {:blocks ground :inventory (into pick (repeat 35 {:name "dirt" :count 64}))} east (fn [_])))))
          (is (= [:condition-failed :refused]
                 (await (run {:blocks ground :zones [{:name "garden" :min [1 50 -2] :max [6 70 2]}]} east (fn [_])))))
          (is (= [:condition-failed :moved-since-submit]
                 (await (run {:blocks ground} (assoc east :submitted-at {:x 80 :y 65 :z 0}) (fn [_])))))
          (is (= [:condition-failed :tool-spent]
                 (await (run {:blocks ground :inventory [{:name "stone_pickaxe" :count 1 :durability 3}]} east (fn [_])))))
          (is (= [:nothing-possible :refills] (await (run {:blocks ground} east refill))))
          (is (= [:condition-failed :no-way-back] (await (run {:blocks ground} east lose-floor))))
          (is (= [:nothing-possible :step-failed] (await (run {:blocks (dissoc ground "1,64,1")} east strand)))))))))

(deftest a-stopped-stair-hands-over-what-it-did-as-data
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [lose-floor (fn [p] (let [world (.-world p)]
                                   (.override world "steer" (fn ^:async f [token a impl]
                                                              (let [r (await (impl token a))]
                                                                (swap! (.. world -state) assoc-in [:blocks [0 66 0]] "stone")
                                                                r)))))
              e @(:out (await (stair! {:blocks ground :raw true} east lose-floor)))]
          (is (= 1 (:steps (:data e))))
          (is (= [1 64 0] (:at (:data e))))
          (is (= 1 (count (:dug (:data e))))))))))

(deftest a-pickaxe-fetch-failing-ends-no-tool-with-the-fetch-as-because
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (stair! {:blocks ground :inventory []} (assoc east :need-drop true) (fn [_])))]
          (is (= [:stopped :no-tool] ((juxt :status :reason) @(:out s))))
          (is (empty? (digs (:p s))) "nothing dug bare-handed"))))))

(def hill
  "Stone over x 1..12, y 65..75 beside the ground of ground: a hill for a stair up east from [0 65 0]."
  (merge ground (stone 1 12 65 75)))

(deftest lava-in-the-line-beyond-the-cut-is-sealed-after-the-rest-of-the-cut-is-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [lava [4 69 0]
              inv (conj pick {:name "cobblestone" :count 10})
              ;; unseen (taken for stone) until the cell beside it is dug, as lava buried in the rock is
              hidden (fn [p]
                       (let [open? #(= "air" (block-at p [3 69 0]))
                             hide (fn [f] (fn [pos]
                                            (let [r (f pos)]
                                              (if (and (= lava [(.-x pos) (.-y pos) (.-z pos)]) (not (open?)))
                                                (doto (js/Object.assign #js {} r) (aset "name" "stone"))
                                                r))))]
                         (doseq [f ["sensedAt" "seenBlockAt"] :when (aget p f)]
                           (aset p f (hide (aget p f))))))
              {:keys [out p] :as s} (await (stair! {:blocks (assoc hill "4,69,0" "lava") :inventory inv}
                                                   (assoc up-east :steps 3) hidden))]
          (is (= :done (:status @out)))
          (is (= "cobblestone" (block-at p lava)))
          (is (= 1 (count (events-of s :stair.sealed)))))))))
