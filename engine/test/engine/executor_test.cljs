(ns engine.executor-test
  "engine.path.executor: the pure decision half of walking a planned path."
  (:require [cljs.test :refer [deftest is are]]
            [engine.path.executor :as ex]))

(def p ex/policy)

(defn step
  ([x y z move] (step x y z move {}))
  ([x y z move extra]
   (merge {:x x :y y :z z :h 0 :move move :corner false :px (+ x 0.5) :pz (+ z 0.5)} extra)))

(defn pose
  ([x y z] (pose x y z {}))
  ([x y z extra]
   (merge {:x x :y y :z z :vy 0 :on-ground true :on-climbable false :in-water false :collided false} extra)))

(defn approx= [a b] (< (Math/abs (- a b)) 1e-6))

(defn yaw-eq?
  "Equal modulo a full turn."
  [a b]
  (let [d (mod (- a b) (* 2 Math/PI))]
    (or (< d 1e-6) (> d (- (* 2 Math/PI) 1e-6)))))

(def straight
  (vec (cons (step 0 64 3 :start)
             (map #(step % 64 3 :walk) (range 1 10)))))

(defn state-at [steps i & {:as extra}]
  (merge (ex/start steps 0) {:i i} extra))

(defn controls-of [r] (:controls r))

;; steps-of

(deftest steps-of-move-codes
  (are [code kw] (= kw (:move (first (ex/steps-of (clj->js [{:x 1 :y 2 :z 3 :h 0 :move code :corner false :px 1.5 :pz 3.5}])))))
    0 :start 1 :walk 2 :diagonal 3 :jump 4 :drop 5 :gap 6 :corner 7 :climb-up
    8 :climb-down 9 :jump-climb 10 :open 11 :swim 12 :swim-up 13 :swim-down 14 :exit))

(deftest steps-of-plain-step
  (is (= [{:x 1 :y 2 :z 3 :h 8 :move :walk :corner false :px 1.5 :pz 3.5}]
         (ex/steps-of #js [#js {:x 1 :y 2 :z 3 :h 8 :move 1 :corner false :px 1.5 :pz 3.5}]))))

(deftest steps-of-optional-fields
  (let [[s] (ex/steps-of #js [#js {:x 1 :y 2 :z 3 :h 0 :move 11 :corner true :px 1.5 :pz 3.5
                                   :cx 1.0 :cz 3.2 :swim true :opens #js [#js {:x 1 :y 2 :z 3}]}])]
    (is (= 1.0 (:cx s)))
    (is (= 3.2 (:cz s)))
    (is (true? (:swim s)))
    (is (true? (:corner s)))
    (is (= [{:x 1 :y 2 :z 3}] (:opens s)))))

(deftest stand-y-adds-sixteenths
  (is (= 64.5 (ex/stand-y {:y 64 :h 8})))
  (is (= 64 (ex/stand-y {:y 64 :h 0}))))

;; refusal

(deftest refusal-supported-plan
  (is (nil? (ex/refusal straight)))
  (is (nil? (ex/refusal [(step 0 64 0 :start) (step 1 64 0 :jump) (step 2 63 0 :drop)
                         (step 2 63 1 :climb-up) (step 2 64 1 :jump-climb) (step 2 63 1 :climb-down)
                         (step 3 63 2 :corner) (step 3 63 3 :diagonal)]))))

(deftest refusal-unsupported-kinds
  (are [steps kind reason at]
       (= {:status :refused :kind kind :at at :reason reason} (ex/refusal steps))
    [(step 0 64 0 :start) (step 10 64 3 :gap)] :gap "unsupported step kind :gap at [10 64 3]" [10 64 3]
    [(step 0 64 0 :start) (step 1 64 0 :walk {:opens [{:x 1}]})] :open "unsupported step kind :open at [1 64 0]" [1 64 0]
    [(step 0 64 0 :start) (step 1 64 0 :walk {:swim true})] :swim "unsupported step kind :swim at [1 64 0]" [1 64 0]
    [(step 0 64 0 :start) (step 1 64 0 :swim-up)] :swim-up "unsupported step kind :swim-up at [1 64 0]" [1 64 0]))

(deftest refusal-names-the-first
  (is (= :gap (:kind (ex/refusal [(step 0 64 0 :start) (step 1 64 0 :gap) (step 2 64 0 :swim)])))))

;; corner slides

(defn solid-set [cells] (fn [x y z] (contains? cells [x y z])))

(deftest free-side-picks-the-open-side
  (let [prev (step 0 64 0 :start)
        corner (step 1 64 1 :corner)]
    (are [solid expected] (= expected (ex/free-side prev corner (solid-set solid)))
      #{[1 64 0]} [0 1]
      #{[0 64 1]} [1 0]
      #{[1 65 0]} [0 1]
      #{[0 65 1]} [1 0]
      #{} nil
      #{[1 64 0] [0 64 1]} nil)))

(deftest free-side-checks-at-the-higher-step
  (let [prev (step 0 64 0 :start)
        corner (step 1 65 1 :corner)]
    (is (= [0 1] (ex/free-side prev corner (solid-set #{[1 65 0]}))))
    (is (= nil (ex/free-side prev corner (solid-set #{[1 64 0]}))))))

(deftest with-free-sides-marks-only-corners
  (let [steps [(step 0 64 0 :start) (step 1 64 1 :corner {:corner true}) (step 2 64 1 :walk)]
        out (ex/with-free-sides steps (solid-set #{[1 64 0]}))]
    (is (= [0 1] (:free (nth out 1))))
    (is (not (contains? (nth out 0) :free)))
    (is (not (contains? (nth out 2) :free)))))

;; reached?

(deftest reached-by-move-class
  (are [s pz expected] (= expected (ex/reached? p s pz))
    (step 3 64 3 :walk) (pose 3.5 64.0 3.5) true
    (step 3 64 3 :walk) (pose 3.5 64.4 3.5) true
    (step 3 64 3 :walk) (pose 3.5 64.6 3.5) false
    (step 3 64 3 :walk) (pose 4.1 64.0 3.5) false
    (step 3 64 3 :walk) (pose 3.5 64.0 2.9) false
    (step 3 64 3 :climb-up) (pose 3.5 63.95 3.5) true
    (step 3 64 3 :climb-up) (pose 3.5 63.8 3.5) false
    (step 3 64 3 :climb-up) (pose 3.5 66.0 3.5) true
    (step 3 64 3 :jump-climb) (pose 3.5 63.95 3.5) true
    (step 3 64 3 :jump-climb) (pose 3.5 63.8 3.5) false
    (step 3 64 3 :climb-down) (pose 3.5 64.4 3.5) true
    (step 3 64 3 :climb-down) (pose 3.5 64.6 3.5) false
    (step 3 64 3 :climb-down) (pose 3.5 60.0 3.5) true))

;; tick: reaching and overshoot

(deftest start-state
  (is (= {:steps straight :i 1 :since 0 :tick 0 :yaw nil} (ex/start straight 0))))

(deftest tick-advances-on-reach
  (let [r (ex/tick p (state-at straight 1) (pose 1.5 64 3.5))]
    (is (= 2 (:i (:state r))))
    (is (= 1 (:since (:state r))))
    (is (= 1 (:tick (:state r))))))

(deftest tick-skips-overshot-steps
  (let [r (ex/tick p (state-at straight 1) (pose 3.5 64 3.5))]
    (is (= 4 (:i (:state r))))))

(deftest tick-overshoot-lookahead-is-bounded
  (let [r (ex/tick p (state-at straight 1) (pose 6.5 64 3.5))]
    (is (= 1 (:i (:state r))))))

(deftest tick-stays-without-reach
  (let [r (ex/tick p (state-at straight 1 :since 0) (pose 0.6 64 3.5))]
    (is (= 1 (:i (:state r))))
    (is (= 0 (:since (:state r))))))

;; arrival

(deftest arrived-needs-distance-and-ground
  (let [st (state-at straight 9)]
    (are [ps status] (= status (:status (:done (ex/tick p st ps))))
      (pose 9.5 64 3.5) :arrived
      (pose 9.2 64 3.5) :arrived
      (pose 9.5 64 3.5 {:on-ground false}) nil
      (pose 9.5 64 3.5 {:on-ground false :on-climbable true}) :arrived)))

(deftest final-step-reached-but-far-stays-current
  (let [steps [(step 0 64 3 :start) (step 1 64 3 :walk {:px 1.9 :pz 3.5})]
        r (ex/tick p (state-at steps 1) (pose 1.2 64 3.5))]
    (is (nil? (:done r)))
    (is (= 1 (:i (:state r))))
    (is (true? (:forward (controls-of r))))))

(deftest arrived-reports-the-position
  (is (= {:status :arrived :at [9.5 64 3.5]} (:done (ex/tick p (state-at straight 9) (pose 9.5 64 3.5))))))

(deftest arrived-by-overshoot-of-the-final-step
  (is (= :arrived (:status (:done (ex/tick p (state-at straight 7) (pose 9.5 64 3.5)))))))

;; off plan

(deftest off-plan-cases
  (let [st (state-at straight 5)]
    (are [ps] (= :off-plan (:status (:done (ex/tick p st ps))))
      (pose 4.5 64 5.5)
      (pose 4.5 64 1.5)
      (pose 4.5 62.4 3.5)
      (pose 4.5 67.2 3.5))))

(deftest on-plan-cases
  (let [st (state-at straight 5)]
    (are [ps] (nil? (:done (ex/tick p st ps)))
      (pose 4.5 64 4.5)
      (pose 4.5 64 3.0)
      (pose 4.5 62.6 3.5)
      (pose 4.5 66.4 3.5))))

(deftest off-plan-reports-position-and-step
  (is (= {:status :off-plan :at [4.5 64 8.0] :step 5}
         (:done (ex/tick p (state-at straight 5) (pose 4.5 64 8.0))))))

;; stuck

(deftest stuck-after-sixty-ticks
  (let [steps [(step 0 64 3 :start) (step 1 65 3 :jump)]
        st (state-at steps 1 :tick 59)
        ps (pose 0.5 64 3.5)
        r60 (ex/tick p st ps)
        r61 (ex/tick p (:state r60) ps)]
    (is (nil? (:done r60)))
    (is (= :stuck (:status (:done r61))))
    (is (= 1 (:step (:done r61))))
    (is (= :jump (:move (:done r61))))
    (is (= [1 65 3] (:target (:done r61))))
    (is (re-find #"^no progress on step 1 \(:jump to \[1 65 3\]\) for 3\.\d s$" (:why (:done r61))))))

;; aim and yaw

(deftest aims-at-crossing-then-point
  (let [steps [(step 0 64 3 :start) (step 1 64 3 :walk {:cx 1.0 :cz 3.2 :px 1.5 :pz 3.9})]
        st (state-at steps 1)
        yaw-of (fn [ps] (:yaw (ex/tick p st ps)))]
    (is (yaw-eq? (yaw-of (pose 0.3 64 3.9)) (Math/atan2 (- (- 1.0 0.3)) (- (- 3.2 3.9)))))
    (let [inside (ex/tick p st (pose 1.2 64 3.0))]
      (is (yaw-eq? (:yaw inside) (Math/atan2 (- (- 1.5 1.2)) (- (- 3.9 3.0))))))))

(deftest aims-at-point-once-near-crossing
  (let [steps [(step 0 64 3 :start) (step 1 64 3 :walk {:cx 1.0 :cz 3.2 :px 1.5 :pz 3.9})]
        r (ex/tick p (state-at steps 1) (pose 0.8 64 3.2))]
    (is (yaw-eq? (:yaw r) (Math/atan2 (- (- 1.5 0.8)) (- (- 3.9 3.2)))))))

(deftest corner-aims-at-midpoint-then-point
  (let [steps [(step 0 64 0 :start) (step 1 64 1 :corner {:corner true :free [0 1] :px 1.5 :pz 1.5})]
        st (state-at steps 1)
        mid-x 1.0 mid-z 1.5]
    (let [r (ex/tick p st (pose 0.5 64 0.5))]
      (is (yaw-eq? (:yaw r) (Math/atan2 (- (- mid-x 0.5)) (- (- mid-z 0.5))))))
    (let [r (ex/tick p st (pose 0.5 64 1.2))]
      (is (yaw-eq? (:yaw r) (Math/atan2 (- (- 1.5 0.5)) (- (- 1.5 1.2))))))))

(deftest yaw-by-direction
  (let [steps [(step 0 64 3 :start) (step 5 64 3 :walk {:px 5.5 :pz 3.5})
               (step 5 64 3 :walk) (step 5 64 3 :walk)]
        east (ex/tick p (state-at steps 1) (pose 1.5 64 3.5))
        west (ex/tick p (state-at [(step 9 64 3 :start) (step 5 64 3 :walk)] 1) (pose 8.5 64 3.5))
        south (ex/tick p (state-at [(step 3 64 0 :start) (step 3 64 5 :walk)] 1) (pose 3.5 64 1.5))
        north (ex/tick p (state-at [(step 3 64 9 :start) (step 3 64 5 :walk)] 1) (pose 3.5 64 8.5))]
    (is (yaw-eq? (:yaw east) (- (/ Math/PI 2))))
    (is (yaw-eq? (:yaw west) (/ Math/PI 2)))
    (is (yaw-eq? (:yaw south) Math/PI))
    (is (yaw-eq? (:yaw north) 0))))

(deftest still-keeps-the-yaw
  (let [steps [(step 0 64 3 :start) (step 1 65 3 :jump {:px 1.5 :pz 3.5})]
        r (ex/tick p (state-at steps 1 :yaw 1.25) (pose 1.45 64 3.5))
        fresh (ex/tick p (state-at steps 1) (pose 1.45 64 3.5))]
    (is (false? (:forward (controls-of r))))
    (is (= 1.25 (:yaw r)))
    (is (= 1.25 (:yaw (:state r))))
    (is (= 0 (:yaw fresh)))))

(deftest moving-records-the-yaw
  (let [r (ex/tick p (state-at straight 1) (pose 0.6 64 3.5))]
    (is (true? (:forward (controls-of r))))
    (is (= (:yaw r) (:yaw (:state r))))
    (is (= 0 (:pitch r)))))

;; jumping

(def rise-steps [(step 0 64 3 :start) (step 3 65 3 :jump {:px 3.5 :pz 3.5})])

(deftest jump-for-a-rise-within-reach
  (are [ps jump?] (= jump? (:jump (controls-of (ex/tick p (state-at rise-steps 1) ps))))
    (pose 2.4 64 3.5) true
    (pose 1.9 64 3.5) false
    (pose 2.4 64 3.5 {:on-ground false}) false
    (pose 2.4 64 3.5 {:on-ground false :on-climbable true}) true))

(deftest no-jump-for-a-small-rise
  (let [steps [(step 0 64 3 :start) (step 3 64 3 :walk {:h 8 :px 3.5 :pz 3.5})]]
    (is (false? (:jump (controls-of (ex/tick p (state-at steps 1) (pose 2.4 64 3.5))))))))

(deftest climb-up-jumps-until-over
  (let [steps [(step 0 64 3 :start) (step 0 66 3 :climb-up {:px 0.5 :pz 3.5})]
        jumps? (fn [y] (:jump (controls-of (ex/tick p (state-at steps 1) (pose 0.5 y 3.5 {:on-ground false})))))]
    (is (true? (jumps? 64.5)))
    (is (true? (jumps? 66.1)))
    (is (false? (jumps? 66.3)))))

(deftest jump-climb-jumps-until-over
  (let [steps [(step 0 64 3 :start) (step 0 66 3 :jump-climb {:px 0.5 :pz 3.5})]
        r (ex/tick p (state-at steps 1) (pose 0.5 65.0 3.5))]
    (is (true? (:jump (controls-of r))))))

(deftest climb-down-never-jumps
  (let [steps [(step 0 66 3 :start) (step 0 64 3 :climb-down {:px 0.5 :pz 3.5})]
        r (ex/tick p (state-at steps 1) (pose 0.5 66.0 3.5 {:on-climbable true}))]
    (is (false? (:jump (controls-of r))))))

;; sprint

(deftest sprint-on-a-straight-walk
  (is (true? (:sprint (controls-of (ex/tick p (state-at straight 1) (pose 0.6 64 3.5)))))))

(deftest sprint-conditions
  (let [walk-then-jump (assoc straight 3 (step 3 65 3 :jump))
        short-end (subvec straight 0 3)
        crossing (assoc straight 2 (step 2 64 3 :walk {:cx 2.0 :cz 3.5}))]
    (are [st ps sprint?] (= sprint? (:sprint (controls-of (ex/tick p st ps))))
      (state-at straight 1) (pose 0.6 64 3.5) true
      (state-at straight 1) (pose 0.6 64 3.5 {:on-ground false}) false
      (state-at walk-then-jump 1) (pose 0.6 64 3.5) false
      (state-at walk-then-jump 2) (pose 1.6 64 3.5) false
      (state-at short-end 1) (pose 0.6 64 3.5) false
      (state-at crossing 1) (pose 0.6 64 3.5) false
      (state-at straight 1) (pose 0.6 64 3.5) true)
    (is (false? (:sprint (controls-of (ex/tick (assoc p :sprint false) (state-at straight 1) (pose 0.6 64 3.5))))))))

(deftest controls-shape
  (let [r (ex/tick p (state-at straight 1) (pose 0.6 64 3.5))]
    (is (= #{:forward :back :left :right :jump :sneak :sprint} (set (keys (controls-of r)))))
    (is (every? false? (map (controls-of r) [:back :left :right :sneak])))))

;; re-plan bookkeeping

(deftest after-walk-branches
  (are [replans done expected] (= expected (ex/after-walk p replans done))
    0 {:status :arrived :at [1 2 3]} {:finish {:status :arrived :at [1 2 3] :replans 0}}
    2 {:status :arrived :at [1 2 3]} {:finish {:status :arrived :at [1 2 3] :replans 2}}
    0 {:status :off-plan :at [1 2 3] :step 4} {:replan 1}
    4 {:status :off-plan :at [1 2 3] :step 4} {:replan 5}
    5 {:status :off-plan :at [1 2 3] :step 4} {:finish {:status :gave-up :reason :replan-limit :replans 5 :at [1 2 3]}}
    3 {:status :stuck :at [1 2 3] :step 4 :why "w"} {:finish {:status :stuck :at [1 2 3] :step 4 :why "w" :replans 3}}))
