(ns engine.walk-sight-test
  "jobs.lib.walk.sight (the floor gate's pure parts) and the gate's seams in jobs.lib.walk: decide-tick's executor prefix,
  resolution, breach; the marks a plan over a masked snapshot carries; the live gate env; the missed floors. The gate in
  physics is engine.walk-gate-physics-test."
  (:require [cljs.test :refer [deftest is are testing]]
            [engine.path.blocks :as blocks]
            [engine.path.executor :as ex]
            [engine.path.fixture :as fx]
            [engine.path.planner.base :as base]
            [engine.path.space :as space]
            [jobs.lib.walk :as walk]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.sight :as ws]))

(defn near? [a b eps] (< (js/Math.abs (- a b)) eps))

;; ---------------------------------------------------------------- the model

(deftest the-model-is-prismarines-walk
  (testing "a forward input at yaw -pi/2 (facing +x) adds 0.98 a along +x; sneaking 0.3 of that"
    (let [a (:a (ws/floor-model "stone"))
          [dx dz] (ws/heading {:forward true} (- (/ js/Math.PI 2)) a)
          [sx] (ws/heading {:forward true :sneak true} (- (/ js/Math.PI 2)) a)]
      (is (near? dx (* 0.98 a) 1e-9))
      (is (near? dz 0 1e-9))
      (is (near? sx (* 0.3 0.98 a) 1e-9))))
  (testing "a diagonal input is normalised to a"
    (let [a 0.1 [dx dz] (ws/heading {:forward true :right true} 0 a)]
      (is (near? (js/Math.hypot dx dz) a 1e-9)))))

(deftest braking-distances-match-the-physics-table
  ;; design v3.3 section 0 (sim.mjs): the stored speed of a walk or sprint and the distance a back input stops it in. The
  ;; sim's brake stops dead at zero speed and leaves out the 0.98 input factor; a real back input would reverse the body, so
  ;; the controller's brake coasts its last ticks: within a few hundredths
  (are [floor v sim] (near? (ws/brake-dist v (ws/floor-model floor)) sim 0.025)
    "stone" 0.118 0.02
    "stone" 0.153 0.06
    "ice" 0.241 0.72
    "blue_ice" 0.256 0.83
    "slime_block" 0.144 0.14)
  (is (<= 0.108 (ws/brake-dist 0.08 (ws/air)) 0.13) "a walk-off at 0.08, back in the air"))

;; ---------------------------------------------------------------- where to stop

(def tk {:x 7 :y 64 :z 0 :h 0 :px 7.5 :pz 0.5})
(def east {:x 8 :y 62 :z 0 :h 0 :px 8.5 :pz 0.5})
(def from-west {:x 6 :y 64 :z 0 :h 0 :px 6.5 :pz 0.5})
(def opts {:peek-gap 0.05 :end-gap 0.01 :margin 0.02 :still-ticks 5 :max-ticks 100 :look-blocks 6})

(deftest the-peek-point-and-the-safe-box
  (is (= [7.95 0.5] (ws/target tk east opts)))
  (is (= [7.95 0.95] (ws/target tk {:x 8 :y 64 :z 1 :px 8.5 :pz 1.5} opts)) "diagonal: each axis at its edge")
  (is (= [js/-Infinity 7.99 0 1] (ws/safe-box from-west tk east opts)) "open behind, end-gap before the gate's edge"))

(deftest hold-stays-still-at-the-peek-point-and-moves-from-short-of-it
  (let [box (ws/safe-box from-west tk east opts)
        T (ws/target tk east opts)
        m (ws/floor-model "stone")
        yaw (- (/ js/Math.PI 2))]
    (is (= :none (:choice (ws/hold box T m yaw {:x 7.95 :z 0.5 :vx 0 :vz 0}))))
    (is (true? (:forward (:controls (ws/hold box T m yaw {:x 7.5 :z 0.5 :vx 0 :vz 0})))))
    (is (true? (:back (:controls (ws/hold box T m yaw {:x 7.9 :z 0.5 :vx 0.15 :vz 0})))) "fast at the edge: brake")))

;; ---------------------------------------------------------------- marks, sensing, breach

(defn st [x y z move] {:x x :y y :z z :h 0 :move move :corner false :px (+ x 0.5) :pz (+ z 0.5)})

(def road [(st 1 64 0 :start) (st 2 64 0 :walk) (st 3 64 0 :walk) (st 4 62 0 :drop) (st 5 62 0 :walk)])

(deftest marks-go-on-steps-whose-floor-is-unknown
  (let [marked (ws/mark-steps road (fn [x y z] (= [4 61 0] [x y z])))]
    (is (= [nil nil nil true nil] (mapv :unseen-floor marked)))
    (is (= 3 (ws/pending marked #{} 1)))
    (is (nil? (ws/pending marked #{3} 1)))))

(deftest sense-resolves-a-plain-floor-and-stops-at-another
  (let [marked (ws/mark-steps road (fn [x y z] (= [4 61 0] [x y z])))
        env (fn [answer] {:see (constantly answer) :plain? #(= "stone" %)})
        pose {:x 2.5 :y 64 :z 0.5}]
    (is (= {:k 3 :gstate {}} (ws/sense (env nil) marked {} 1 pose)) "unknown: pending")
    (is (= {:k nil :gstate {:resolved #{3}}} (ws/sense (env "stone") marked {} 1 pose)))
    (is (= {:status :replan :why :changed :cells [[4 61 0]] :step 3}
           (dissoc (:done (ws/sense (env "lava") marked {} 1 pose)) :at)))))

(deftest a-body-on-a-pending-floor-is-a-breach
  (let [marked (ws/mark-steps road (fn [x y z] (= [4 61 0] [x y z])))]
    (is (= [4 61 0] (ws/breach marked 3 {:x 4.5 :y 62 :z 0.5 :on-ground true})))
    (is (nil? (ws/breach marked 3 {:x 3.9 :y 64 :z 0.5 :on-ground true})))
    (is (nil? (ws/breach marked 3 {:x 4.5 :y 62.4 :z 0.5 :on-ground false})) "in the air: not stood on")))

;; ---------------------------------------------------------------- decide-tick's seams

(defn env [see]
  {:policy ex/policy :steps (ws/mark-steps road (fn [x y z] (= [4 61 0] [x y z])))
   :gate {:see see :plain? #(= "stone" %) :floors (constantly "stone") :opts opts}})

(def still {:y 64 :z 0.5 :vx 0 :vy 0 :vz 0 :on-ground true})

(deftest the-executor-gets-the-steps-up-to-the-takeoff-only
  (let [e (env (constantly nil))
        [s] (walk/decide-tick e (walk/start-state (:steps e) {:x 1.5 :z 0.5}) (assoc still :x 1.5))]
    (is (= 3 (count (:steps (:exec s)))) "steps 0..2: the takeoff is the last")))

(deftest a-floor-seen-mid-walk-hands-on-the-rest-with-the-index-kept
  (let [seen (atom nil)
        e (env (fn [_] @seen))
        [s1] (walk/decide-tick e (walk/start-state (:steps e) {:x 1.5 :z 0.5}) (assoc still :x 1.5))
        _ (reset! seen "stone")
        [s2] (walk/decide-tick e s1 (assoc still :x 1.6))]
    (is (= 5 (count (:steps (:exec s2)))))
    (is (= (:i (:exec s1)) (:i (:exec s2))))))

(deftest arriving-at-the-takeoff-does-not-end-the-walk
  (let [e (env (constantly nil))
        s0 (assoc-in (walk/start-state (:steps e) {:x 3.5 :z 0.5}) [:exec :i] 2)
        [_ out] (walk/decide-tick e s0 (assoc still :x 3.5))]
    (is (nil? (:done out)) (pr-str out))
    (is (some? (:controls out)))))

(deftest standing-on-the-pending-floor-ends-the-walk-pushed
  (let [e (env (constantly nil))
        s0 (assoc-in (walk/start-state (:steps e) {:x 3.5 :z 0.5}) [:exec :i] 2)
        [_ out] (walk/decide-tick e s0 (assoc still :x 4.5 :y 62))]
    (is (= {:status :replan :why :pushed :cells [[4 61 0]] :step 3} (dissoc (:done out) :at)))
    (is (= [4 61 0] (:cell (:warn out))))))

(deftest without-a-gate-decide-tick-is-the-executor
  (let [steps road
        s0 (walk/start-state steps {:x 1.5 :z 0.5})
        pose (assoc still :x 1.5)
        [s out] (walk/decide-tick {:policy ex/policy :steps steps} s0 pose)
        r (ex/tick ex/policy (:exec s0) pose)]
    (is (= (:state r) (:exec s)))
    (is (= (:yaw r) (:yaw out)))))

;; ---------------------------------------------------------------- marks from a masked plan, missed floors

(defn masked-pw
  "A pathWorld over fills whose cells in unknown read void_air, with the mask's options on the snapshot (.unknown)."
  [fills unknown]
  (let [table (blocks/default-state-table)
        void (first (blocks/state-ids table "void_air"))
        snapshot (fx/fixture-snapshot {:fill fills})]
    (doseq [[x y z] unknown] (.setState snapshot x y z void))
    (set! (.-unknown snapshot) #js {:id void :missed (js/Set.)})
    #js {:snapshot snapshot :table table :space space/space}))

(deftest a-plan-over-a-masked-snapshot-marks-its-unseen-floors
  (let [pw (masked-pw [[0 63 0 9 63 0 "stone" {}]] #{[5 63 0]})
        steps (wplan/path-steps pw #js {:steps (clj->js (map (fn [x] {:x x :y 64 :z 0 :h 0 :move 1 :px (+ x 0.5) :pz 0.5})
                                                            (range 2 8)))})]
    (is (= [5] (keep #(when (:unseen-floor %) (:x %)) steps)))))

(deftest a-plan-over-an-unmasked-snapshot-has-no-marks
  (let [pw #js {:snapshot (fx/fixture-snapshot {:fill [[0 63 0 9 63 0 "stone" {}]]})
                :table (blocks/default-state-table) :space space/space}
        steps (wplan/path-steps pw #js {:steps (clj->js (map (fn [x] {:x x :y 64 :z 0 :h 0 :move 1 :px (+ x 0.5) :pz 0.5})
                                                            (range 2 8)))})]
    (is (not-any? :unseen-floor steps))))

(deftest a-floor-given-up-as-missed-joins-the-masks-missed-set
  (let [pw (masked-pw [[0 63 0 9 63 0 "stone" {}]] #{[5 63 0]})]
    (ws/note-missed! pw {:status :replan :why :unseen :cells [[5 63 0]]})
    (ws/note-missed! pw {:status :replan :why :changed :cells [[6 63 0]]})
    (is (= #{(base/cell-key 5 63 0)} (set (.-missed (.-unknown (.-snapshot pw))))))))

(deftest the-live-gate-env-is-there-only-for-marked-steps
  (is (nil? (walk/gate-env #js {} road)))
  (let [p #js {:sensedAt (fn [^js a] (if (= 5 (.-x a)) #js {:name "stone" :fullCube true} #js {:unknown true}))
               :glance (fn [_] nil) :feel (fn [_] nil)}
        {:keys [see plain?]} (walk/gate-env p (ws/mark-steps road (constantly true)))]
    (is (nil? (see [4 61 0])) "unknown and not glanced")
    (is (true? (plain? (see [5 63 0]))))
    (is (false? (plain? #js {:name "magma_block" :fullCube true})))
    (is (false? (plain? #js {:name "oak_slab" :fullCube false})))))

(deftest see-depth-is-the-deepest-dark-drop-seen-from-the-peek
  (is (= 1 (ws/see-depth 4)) "dark, near 4")
  (is (= 4 (ws/see-depth 7)) "a torch in a hand, near 7")
  (is (= 0 (ws/see-depth 1))))

(defn run-ticks
  "decide-tick from state s0 with the body held at pose, at most n ticks: [ticks out] at the first done (or the last)."
  [e s0 pose n]
  (loop [s s0 t 1]
    (let [[s' out] (walk/decide-tick e s pose)]
      (if (or (:done out) (>= t n)) [t out] (recur s' (inc t))))))

(deftest a-floor-seen-changed-while-the-body-still-moves-brakes-first-then-ends-the-walk
  ;; releasing the keys at once slides a walking body 0.3 blocks on, off the end of its takeoff
  (let [e (env (constantly "lava"))
        s0 (assoc-in (walk/start-state (:steps e) {:x 3.5 :z 0.5}) [:exec :i] 2)
        moving (assoc still :x 3.8 :vx 0.2)
        [s1 out1] (walk/decide-tick e s0 moving)
        [_ out2] (walk/decide-tick e s1 (assoc still :x 3.9))]
    (is (nil? (:done out1)) "moving: the walk is not ended yet")
    (is (not (:forward (:controls out1))) (pr-str out1))
    (is (= :changed (:why (:done out2))) "still: the walk ends")))

(deftest a-walk-that-starts-on-a-takeoff-is-held-there
  (let [steps (ws/mark-steps (subvec road 2) (fn [x y z] (= [4 61 0] [x y z])))
        e (assoc (env (constantly nil)) :steps steps)
        s0 (walk/start-state steps {:x 3.5 :z 0.5})
        [s out] (walk/decide-tick e s0 (assoc still :x 3.5))]
    (is (nil? (:done out)) (pr-str out))
    (is (some? (:controls out)))
    (is (= 0 (:i (:exec s))) "the executor walks to the takeoff, the one step it has")
    (is (= :unseen (:why (:done (second (run-ticks e s0 (assoc still :x 3.5) 200))))) "the floor is given up, no crash")))

(deftest a-body-blocked-on-the-last-leg-before-a-gate-ends-stuck
  (let [e (env (constantly nil))
        [t out] (run-ticks e (walk/start-state (:steps e) {:x 1.5 :z 0.5}) (assoc still :x 2.5) 600)]
    (is (= :stuck (:status (:done out))) (pr-str out))
    (is (<= t (+ (:no-progress-ticks ex/policy) 5)) t)))

(deftest a-body-pressed-against-a-wall-on-the-takeoff-is-still-and-released
  (let [steps (ws/mark-steps (subvec road 2) (fn [x y z] (= [4 61 0] [x y z])))
        e (assoc (env (constantly nil)) :steps steps)
        s0 (walk/start-state steps {:x 3.5 :z 0.5})
        [_ out] (run-ticks e s0 (assoc still :x 3.5) 200)]
    (is (<= (:ticks (:done out)) (+ (:still-ticks opts) 2)) "the pose never moves: still, not max-ticks")))
