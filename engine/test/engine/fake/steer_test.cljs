(ns engine.fake.steer-test
  "The fake's steer and pathWorld as pure functions over cljs world data; the cases of js/fake-steer.test.mjs. The
  drag of leashed and tempted animals after a walk is the animals ns's: here the after-walk hook is a recording stub."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.fake.steer :as steer]
            [engine.test-util :as tu]))

(def east (- (/ js/Math.PI 2))) ; mineflayer yaw: dx = -sin(yaw)

(defn floor
  ([] (floor 63 -2 12))
  ([y x0 x1] (into {} (for [x (range x0 (inc x1))] [[x y 0] "stone"]))))

(defn rig
  "{:state world-atom :owner owner-atom :env steer opts}; the owner is \"t\"."
  ([] (rig {} [0 64 0]))
  ([blocks pos] (rig blocks pos {}))
  ([blocks pos extra]
   (let [state (atom (merge {:blocks (merge (floor) blocks) :states {} :self {:pos pos} :controls {}} extra))
         owner (atom "t")
         walks (atom [])]
     {:state state
      :owner owner
      :walks walks
      :env {:state state :owner-of #(deref owner) :after-walk (fn [w from] (swap! walks conj from) w)}})))

(defn run [{:keys [env]} decide & [args]] (steer/steer! env "t" (merge {:decide decide} args)))
(defn pos-of [{:keys [state]}] (get-in @state [:self :pos]))
(defn later [] (js/Promise. (fn [resolve] (js/setImmediate resolve))))
(defn rejection [p] (.then p (fn [_] nil) (fn [e] e)))

;; aims east at the target x until within 0.3 of it
(defn walk-to
  ([target-x] (walk-to target-x {}))
  ([target-x {:keys [jump on-pose] :or {on-pose identity}}]
   (fn [pose]
     (on-pose pose)
     (if (< (js/Math.abs (- (:x pose) target-x)) 0.3)
       {:done {:status "arrived"}}
       {:controls {:forward true :jump (boolean jump)} :yaw east}))))

(defn ladder [x ys] (into {} (for [y ys] [[x y 0] "ladder"])))

(deftest walking-five-blocks-along-x-on-a-floor-arrives-in-a-plausible-number-of-ticks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (rig)
              r (await (run p (walk-to 5.5)))]
          (is (= "done" (:status r)))
          (is (= {:status "arrived"} (:result r)))
          (is (<= 20 (:ticks r) 40) (str "ticks " (:ticks r)))
          (is (= [5 64 0] (pos-of p))))))))

(deftest a-sprinting-walk-takes-fewer-ticks-than-a-plain-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [sprint (fn [pose] (if (< (js/Math.abs (- (:x pose) 5.5)) 0.3) {:done {}} {:controls {:forward true :sprint true} :yaw east}))
              fast (await (run (rig) sprint))
              slow (await (run (rig) (walk-to 5.5)))]
          (is (< (:ticks fast) (:ticks slow))))))))

(deftest the-first-pose-is-where-the-body-stands-on-the-ground-not-climbing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [poses (atom [])]
          (await (run (rig) (fn [pose] (swap! poses conj pose) {:done {}})))
          (is (= {:x 0.5 :y 64 :z 0.5 :vy 0 :on-ground true :on-climbable false :in-water false :collided false :yaw 0}
                 (dissoc (first @poses) :t))))))))

(deftest a-body-standing-in-water-reads-in-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run (rig {[0 64 0] "water"} [0 64 0]) (fn [pose] {:done {:in-water (:in-water pose)}})))]
          (is (= {:in-water true} (:result r))))))))

(deftest a-one-block-step-up-needs-jump-without-it-the-body-is-collided-and-does-not-rise
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (rig {[3 64 0] "stone"} [0 64 0])
              poses (atom [])
              r (await (run p (walk-to 5.5 {:on-pose #(swap! poses conj %)}) {:timeout-s 1}))
              {:keys [collided y x]} (last @poses)]
          (is (= "timeout" (:status r)))
          (is (true? collided))
          (is (= 64 y))
          (is (< x 3)))))))

(deftest a-one-block-step-up-with-jump-rises-onto-the-block-and-stays-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (rig (floor 64 3 12) [0 64 0])
              r (await (run p (walk-to 5.5 {:jump true})))]
          (is (= "done" (:status r)))
          (is (= 65 (second (pos-of p)))))))))

(deftest a-two-block-wall-is-never-climbed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (rig {[3 64 0] "stone" [3 65 0] "stone"} [0 64 0])
              r (await (run p (walk-to 5.5 {:jump true}) {:timeout-s 1}))]
          (is (= "timeout" (:status r)))
          (is (= 64 (second (pos-of p)))))))))

(deftest a-drop-lands-at-the-lower-floor-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (into {} (mapcat (fn [x] [[[x 63 0] "air"] [[x 62 0] "stone"]]) [3 4 5 6]))
              p (rig blocks [0 64 0])
              r (await (run p (walk-to 5.5)))]
          (is (= "done" (:status r)))
          (is (= 63 (second (pos-of p)))))))))

(deftest a-ladder-climbs-with-jump-and-does-not-climb-without-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [wall (into {} (for [y [64 65 66 67]] [[2 y 0] "stone"]))
              p (rig (merge (ladder 1 [64 65 66]) wall) [1 64 0])
              idle (await (run p (fn [pose] {:done {:y (:y pose) :climbing (:on-climbable pose)}})))
              r (await (run p (fn [pose] (if (>= (:y pose) 66.9) {:done {:y (:y pose)}} {:controls {:jump true}}))))]
          (is (= {:y 64 :climbing true} (:result idle)))
          (is (= "done" (:status r)))
          (is (>= (:y (:result r)) 66.9)))))))

(deftest on-a-ladder-without-jump-the-body-climbs-down-to-the-floor
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (rig (ladder 1 [64 65 66]) [1 66 0])
              r (await (run p (fn [pose] (if (<= (:y pose) 64) {:done {:y (:y pose)}} {:controls {}}))))]
          (is (= {:y 64} (:result r))))))))

(deftest a-solid-cell-above-the-head-stops-a-climb
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (rig (merge (ladder 1 (range 64 71)) {[1 68 0] "stone"}) [1 64 0])
              r (await (run p (fn [_] {:controls {:jump true}}) {:timeout-s 1}))]
          (is (= "timeout" (:status r)))
          (is (< (:y (:pose r)) 67)))))))

(deftest a-timeout-resolves-with-the-pose-and-clears-the-controls
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (rig)
              r (await (run p (fn [_] {:controls {:forward true} :yaw 0}) {:timeout-s 0.05}))]
          (is (= "timeout" (:status r)))
          (is (number? (:x (:pose r))))
          (is (= {} (:controls @(:state p)))))))))

(deftest a-decide-that-throws-resolves-failed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run (rig) (fn [_] (throw (js/Error. "boom")))))]
          (is (= {:status "failed" :reason "Error: boom"} r)))))))

(deftest a-cut-mid-walk-rejects-with-the-cut-error-and-the-controls-are-cleared
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (rig)
              result (run p (fn [_] {:controls {:forward true} :yaw east}))]
          (await (later))
          (await (later))
          (is (true? (:forward (:controls @(:state p)))))
          (reset! (:owner p) "other")
          (let [err (await (rejection result))]
            (is (= "cut" (:code (ex-data err))))
            (is (= {} (:controls @(:state p))))))))))

(deftest a-stale-token-is-cut-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [err (await (rejection (steer/steer! (:env (rig)) "old" {:decide (fn [_] {:done {}})})))]
          (is (= "cut" (:code (ex-data err)))))))))

(deftest bad-args-are-bad-args
  (are [args] (= "bad-args" (:code (ex-data (try (steer/steer! (:env (rig)) "t" args) (catch :default e e)))))
    {}
    {:decide :not-a-fn}
    {:decide identity :timeout-s 0}
    {:decide identity :timeout-s 121}
    {:decide identity :timeout-s "5"}))

(deftest a-timeout-of-120-s-is-allowed
  (is (= 2400 (steer/budget-of 120)))
  (is (= 1 (steer/budget-of 0.05))))

;; ---- pathWorld

(defn at-state [snapshot [x y z]] (.stateAt ^js snapshot x y z))
(def fixture (delay (tu/require-here "./js/path/fixture.mjs")))
(def unloaded (delay (.-UNLOADED ^js (tu/require-here "./js/path/snapshot.mjs"))))
(defn state-id [name props] (.stateId ^js @fixture name (clj->js props)))

(deftest path-world-reads-a-placed-block-as-its-state-id-and-air-elsewhere-in-the-column
  (let [{:keys [snapshot table space]} (steer/path-world @(:state (rig {[3 64 0] "oak_planks"} [0 64 0])))]
    (is (= (state-id "oak_planks" {}) (at-state snapshot [3 64 0])))
    (is (= 0 (at-state snapshot [4 64 0])))
    (is (some? table))
    (is (= "object" (goog/typeOf space)))))

(deftest path-world-leaves-untouched-columns-unloaded
  (let [{:keys [snapshot]} (steer/path-world @(:state (rig)))]
    (is (= @unloaded (at-state snapshot [1000 64 1000])))
    (is (false? (.hasColumn ^js snapshot 62 62)))))

(deftest cells-near-an-unreachable-or-no-path-target-are-stone-to-the-planner-the-bodys-own-cells-are-not
  (let [w @(:state (rig {} [0 64 0] {:blocks (floor 63 -8 12) :unreachable #{[8 64 0]} :no-path #{[0 64 6]}}))
        {:keys [snapshot]} (steer/path-world w)
        stone (state-id "stone" {})]
    (is (= [stone stone stone stone] (mapv #(at-state snapshot %) [[8 64 0] [5 66 3] [11 62 0] [0 64 6]])))
    (is (= [0 0 0] (mapv #(at-state snapshot %) [[0 64 0] [0 65 0] [12 64 0]])))))

;; the target's range reaches the body: its own two cells stay air, the cells beside it are stone
(deftest the-bodys-own-cells-stay-air-when-an-unreachable-target-is-within-the-burying-range
  (let [{:keys [snapshot]} (steer/path-world @(:state (rig {} [0 64 0] {:unreachable #{[2 64 0]}})))
        stone (state-id "stone" {})]
    (is (= [0 0 stone stone] (mapv #(at-state snapshot %) [[0 64 0] [0 65 0] [1 64 0] [0 66 0]])))))

;; ---- what the fake moveTo did around a walk: the after-walk hook

(deftest the-after-walk-hook-runs-once-with-the-start-when-a-walk-is-over
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (rig {} [0 64 0])]
          (await (run p (walk-to 5.5)))
          (is (= [[0 64 0]] @(:walks p))))))))

(deftest the-after-walk-hook-sees-the-world-after-the-walk-and-its-result-is-kept
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (rig)
              env (assoc (:env p) :after-walk (fn [w _] (assoc w :dragged (get-in w [:self :pos]))))]
          (await (steer/steer! env "t" {:decide (walk-to 5.5)}))
          (is (= [5 64 0] (:dragged @(:state p)))))))))

(deftest the-after-walk-hook-also-runs-after-a-timeout-and-a-failure
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (rig)]
          (await (run p (fn [_] {:controls {}}) {:timeout-s 0.05}))
          (await (run p (fn [_] (throw (js/Error. "x")))))
          (is (= 2 (count @(:walks p)))))))))

(deftest a-cut-walk-runs-no-after-walk-hook
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (rig)
              result (run p (fn [_] {:controls {:forward true} :yaw east}))]
          (await (later))
          (reset! (:owner p) "other")
          (await (rejection result))
          (is (= [] @(:walks p))))))))

;; ---- rails, no-collision blocks

(deftest a-body-walks-along-rails
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (rig (into {} (for [x [1 2 3 4 5]] [[x 64 0] "powered_rail"])) [0 64 0])
              r (await (run p (walk-to 5.5)))]
          (is (= "done" (:status r)))
          (is (= [5 64 0] (pos-of p))))))))

(deftest a-body-walks-through-a-lone-small-block-on-flat-ground
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [names ["lever" "stone_button" "torch" "wall_torch" "oak_pressure_plate" "oak_sign"]
              rigs (mapv #(rig {[3 64 0] %} [0 64 0]) names)
              results (await (js/Promise.all (to-array (map #(run % (walk-to 5.5)) rigs))))]
          (is (= (repeat 6 "done") (map :status results)))
          (is (= (repeat 6 [5 64 0]) (map pos-of rigs))))))))

;; ---- doors, gates and trapdoors: open ones let the body through, shut ones are walls

(defn door-rig [blocks states] (rig blocks [0 64 0] {:states states}))
(defn gate [open] {:blocks {[3 64 0] "oak_fence_gate"} :states {[3 64 0] {:open open :facing "east"}}})

(deftest a-body-walks-through-an-open-gate-and-stops-at-a-shut-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [through (await (run (door-rig (:blocks (gate true)) (:states (gate true))) (walk-to 5.5)))
              shut (await (run (door-rig (:blocks (gate false)) (:states (gate false))) (walk-to 5.5) {:timeout-s 1}))]
          (is (= ["done" "timeout"] [(:status through) (:status shut)]))
          (is (< (:x (:pose shut)) 3)))))))

(deftest a-body-walks-through-the-open-halves-of-a-door
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks {[3 64 0] "oak_door" [3 65 0] "oak_door"}
              states (fn [open] {[3 64 0] {:half "lower" :open open} [3 65 0] {:half "upper" :open open}})
              through (await (run (door-rig blocks (states true)) (walk-to 5.5)))
              shut (await (run (door-rig blocks (states false)) (walk-to 5.5) {:timeout-s 1}))]
          (is (= ["done" "timeout"] [(:status through) (:status shut)])))))))

(deftest a-body-climbs-a-ladder-up-through-an-open-trapdoor-and-not-through-a-shut-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [hatch {[3 64 0] "ladder" [3 65 0] "ladder" [3 66 0] "oak_trapdoor" [2 66 0] "stone" [4 66 0] "stone" [3 63 1] "stone"}
              climb (fn [pose] (if (>= (:y pose) 67) {:done {:status "arrived"}} {:controls {:forward false :jump true} :yaw 0}))
              run-with (fn [open] (run (rig hatch [3 64 0] {:states {[3 66 0] {:open open}}}) climb {:timeout-s 2}))
              [open shut] (await (js/Promise.all #js [(run-with true) (run-with false)]))]
          (is (= ["done" "timeout"] [(:status open) (:status shut)])))))))

(deftest path-world-reads-the-open-state-of-a-gate
  (let [state-at (fn [open] (at-state (:snapshot (steer/path-world @(:state (door-rig (:blocks (gate open)) (:states (gate open)))))) [3 64 0]))]
    (is (= (state-id "oak_fence_gate" {:open true :facing "east"}) (state-at true)))
    (is (= (state-id "oak_fence_gate" {:open false :facing "east"}) (state-at false)))))
