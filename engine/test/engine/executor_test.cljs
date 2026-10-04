(ns engine.executor-test
  "engine.path.executor: the pure decision half of walking a planned path."
  (:require [cljs.test :refer [deftest is are]]
            [engine.path.executor :as ex]
            [engine.path.planner-tuned :as planner]))

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
  (is (nil? (ex/refusal p straight)))
  (is (nil? (ex/refusal p [(step 0 64 0 :start) (step 1 64 0 :jump) (step 2 63 0 :drop)
                         (step 2 63 1 :climb-up) (step 2 64 1 :jump-climb) (step 2 63 1 :climb-down)
                         (step 3 63 2 :corner) (step 3 63 3 :diagonal)]))))

(deftest refusal-unsupported-kinds
  (are [steps kind reason at]
       (= {:status :refused :kind kind :at at :reason reason} (ex/refusal p steps))
    [(step 0 64 0 :start) (step 1 64 0 :open)] :open "unsupported step kind :open at [1 64 0]" [1 64 0]
    [(step 0 64 0 :start) (step 1 64 0 :walk {:opens [{:x 1}]})] :open "unsupported step kind :open at [1 64 0]" [1 64 0]
    [(step 0 64 0 :start) (step 1 64 0 :walk {:swim true})] :swim "unsupported step kind :swim at [1 64 0]" [1 64 0]
    [(step 0 64 0 :start) (step 1 64 0 :swim-up)] :swim-up "unsupported step kind :swim-up at [1 64 0]" [1 64 0]))

(deftest refusal-names-the-first
  (is (= :open (:kind (ex/refusal p [(step 0 64 0 :start) (step 1 64 0 :open) (step 2 64 0 :swim)])))))

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

;; gap jumps

(defn gap-steps
  "A course along +x: start, takeoff (10 64 0), landing n cells past it (y 63 for a down gap), a walk after."
  [n & {:keys [landing-y prev-move] :or {landing-y 64 prev-move :walk}}]
  [(step 9 64 0 :start) (step 10 64 0 prev-move) (step (+ 11 n) landing-y 0 :gap)
   (step (+ 12 n) landing-y 0 :walk)])

(deftest refusal-gap-walked
  (are [steps] (nil? (ex/refusal p steps))
    (gap-steps 1)
    (gap-steps 2)
    (gap-steps 3)
    (gap-steps 2 :landing-y 63)))

(deftest refusal-gap-kinds-and-reasons
  (are [steps kind reason at]
       (= {:status :refused :kind kind :at at :reason reason} (ex/refusal p steps))
    (gap-steps 4) :gap-width "gap jump at [15 64 0] over 4 cells" [15 64 0]
    [(step 9 64 0 :start) (step 10 64 0 :walk) (step 12 64 1 :gap)]
    :gap-width "gap jump at [12 64 1] not in a straight line" [12 64 1]
    (gap-steps 2 :landing-y 65) :gap-up "gap jump up at [13 65 0] (not measured yet)" [13 65 0]
    (update (gap-steps 2) 2 assoc :low-ceiling true)
    :gap-low-ceiling "gap jump at [13 64 0] under a ceiling lower than 3 blocks" [13 64 0]
    (gap-steps 2 :prev-move :climb-up) :gap-takeoff "gap jump at [13 64 0] from a ladder" [13 64 0]
    (gap-steps 2 :prev-move :jump-climb) :gap-takeoff "gap jump at [13 64 0] from a ladder" [13 64 0]))

(deftest refusal-gap-takeoff-comes-first
  (is (= :gap-takeoff (:kind (ex/refusal p (update (gap-steps 4 :prev-move :climb-down) 2 assoc :low-ceiling true))))))

(deftest with-gap-ceilings-marks-by-cell
  (let [at (fn [prev-h cells steps] (map #(true? (:low-ceiling %))
                                         (ex/with-gap-ceilings p (assoc-in steps [1 :h] prev-h) (solid-set cells))))]
    (are [prev-h cells marked?] (= marked? (nth (at prev-h cells (gap-steps 2)) 2))
      0 #{[10 66 0]} true
      0 #{[11 66 0]} true
      0 #{[12 66 0]} true
      0 #{[13 66 0]} false
      0 #{[12 65 0]} false
      0 #{[10 67 0]} false
      8 #{[10 67 0]} true
      0 #{} false)))

(deftest with-gap-ceilings-leaves-other-steps-alone
  (let [steps (gap-steps 2)
        out (ex/with-gap-ceilings p steps (constantly true))]
    (is (= [(nth steps 0) (nth steps 1) (nth steps 3)] [(nth out 0) (nth out 1) (nth out 3)]))
    (is (true? (:low-ceiling (nth out 2))))))

(deftest with-gap-ceilings-checks-the-right-cells-in-other-directions
  (let [steps [(step 0 64 9 :start) (step 0 64 10 :walk) (step 0 64 7 :gap)]
        marked? (fn [cell] (:low-ceiling (nth (ex/with-gap-ceilings p steps (solid-set #{cell})) 2)))]
    (is (true? (marked? [0 66 10])))
    (is (true? (marked? [0 66 9])))
    (is (true? (marked? [0 66 8])))
    (is (nil? (marked? [0 66 7])))
    (is (nil? (marked? [1 66 9])))))

(deftest with-gap-ceilings-then-refusal
  (let [steps (ex/with-gap-ceilings p (gap-steps 2) (solid-set #{[11 66 0]}))]
    (is (= :gap-low-ceiling (:kind (ex/refusal p steps))))
    (is (nil? (ex/refusal p (ex/with-gap-ceilings p (gap-steps 2) (solid-set #{[14 66 0]})))))))

;; planner limits

(def swim-moves [:swim :swim-up :swim-down :exit])

(deftest planner-limits-kinds-are-the-moves-the-policy-lacks
  (are [moves kinds] (= kinds (.-kinds (ex/planner-limits (assoc p :moves moves) (constantly false))))
    (:moves p) (+ planner/AVOID-WATER planner/AVOID-OPEN)
    (conj (:moves p) :open) planner/AVOID-WATER
    (into (:moves p) swim-moves) planner/AVOID-OPEN
    (into (:moves p) (conj swim-moves :open)) 0
    (disj (:moves p) :climb-down) (+ planner/AVOID-CLIMB planner/AVOID-WATER planner/AVOID-OPEN)))

(defn gap-allowed?
  "The planner-limits gap test for a jump from (10 64 0), stand h, reached by move code, to n cells along +x."
  [n & {:keys [landing-y move h solid] :or {landing-y 64 move 1 h 0 solid #{}}}]
  ((.-gap (ex/planner-limits p (solid-set solid))) 10 64 0 h move (+ 11 n) landing-y 0 0))

(deftest planner-limits-gap-test-is-the-refusal
  (are [args allowed?] (= allowed? (apply gap-allowed? args))
    [1] true
    [2] true
    [3] true
    [1 :landing-y 63] true
    [2 :landing-y 63] true
    [3 :landing-y 63] true
    [4] false
    [1 :landing-y 65] false
    [2 :move 7] false
    [2 :move 8] false
    [2 :move 9] false
    [2 :solid #{[10 66 0]}] false
    [2 :solid #{[12 66 0]}] false
    [2 :solid #{[13 66 0]}] true
    [2 :h 8 :solid #{[10 67 0]}] false
    [2 :solid #{[10 67 0]}] true))

(deftest past-edge-in-all-directions
  (are [landing x z expected] (approx= expected (ex/past-edge (step 10 64 0 :walk) landing (pose x 64 z)))
    (step 12 64 0 :gap) 10.5 0.5 -0.5
    (step 12 64 0 :gap) 11.0 0.5 0.0
    (step 12 64 0 :gap) 11.3 0.9 0.3
    (step 8 64 0 :gap) 10.5 0.5 -0.5
    (step 8 64 0 :gap) 10.0 0.5 0.0
    (step 10 64 -2 :gap) 10.9 0.5 -0.5
    (step 10 64 -2 :gap) 10.2 0.0 0.0
    (step 10 64 2 :gap) 10.2 0.5 -0.5
    (step 10 64 2 :gap) 10.2 1.0 0.0))

(defn gap-jump-of
  "The :jump control on the gap step of a course, at a pose."
  [steps ps]
  (:jump (controls-of (ex/tick p (state-at steps 2) ps))))

(deftest gap-jump-at-the-takeoff-edge
  (are [n x expected] (= expected (gap-jump-of (gap-steps n) (pose x 64 0.5)))
    1 10.7 false
    1 10.85 true
    2 10.55 false
    2 10.65 true
    3 10.85 false
    3 10.95 true
    1 11.4 false
    3 11.4 false))

(deftest gap-jump-down-two-walks-from-the-edge
  (are [x expected] (= expected (gap-jump-of (gap-steps 2 :landing-y 63) (pose x 64 0.5)))
    10.65 false
    10.95 false
    11.05 true
    11.25 true
    11.35 false))

(deftest gap-jump-down-other-widths-as-level
  (are [n x expected] (= expected (gap-jump-of (gap-steps n :landing-y 63) (pose x 64 0.5)))
    1 10.85 true
    3 10.85 false
    3 10.95 true))

(deftest gap-jump-needs-the-ground
  (is (false? (gap-jump-of (gap-steps 1) (pose 10.9 64 0.5 {:on-ground false})))))

(deftest gap-jump-needs-the-takeoff-height
  (is (false? (gap-jump-of (gap-steps 1) (pose 10.9 65.2 0.5)))))

(deftest gap-jump-from-a-dip
  (are [x expected] (= expected (gap-jump-of (gap-steps 1) (pose x 63 0.5)))
    11.5 true
    11.3 true
    11.0 false))

(deftest gap-sprint-by-width
  (are [n ps expected] (= expected (:sprint (controls-of (ex/tick p (state-at (gap-steps n) 2) ps))))
    1 (pose 10.5 64 0.5) false
    2 (pose 10.5 64 0.5) true
    3 (pose 10.5 64 0.5) true
    3 (pose 11.5 64.4 0.5 {:on-ground false}) true
    1 (pose 11.5 64.4 0.5 {:on-ground false}) false)
  (are [n expected] (= expected (:sprint (controls-of (ex/tick p (state-at (gap-steps n :landing-y 63) 2) (pose 10.5 64 0.5)))))
    1 false
    2 false
    3 true)
  (is (false? (:sprint (controls-of (ex/tick (assoc p :sprint false) (state-at (gap-steps 3) 2) (pose 10.5 64 0.5)))))))

(deftest gap-reached-only-on-the-ground
  (are [ps expected] (= expected (:i (:state (ex/tick p (state-at (gap-steps 1) 2) ps))))
    (pose 12.5 64 0.5 {:on-ground false}) 2
    (pose 12.5 64 0.5) 3))

(deftest gap-in-the-air-does-not-skip-ahead
  (let [steps (conj (gap-steps 1) (step 14 64 0 :walk))]
    (are [ps expected] (= expected (:i (:state (ex/tick p (state-at steps 2) ps))))
      (pose 13.5 64 0.5 {:on-ground false}) 2
      (pose 13.5 64 0.5) 4)))

(deftest gap-landing-in-the-lookahead-needs-the-ground
  (are [ps expected] (= expected (:i (:state (ex/tick p (state-at (gap-steps 1) 1) ps))))
    (pose 12.5 64 0.5 {:on-ground false}) 1
    (pose 12.5 64 0.5) 3))

(deftest gap-overshoot-aims-back-at-the-landing
  (let [r (ex/tick p (state-at (gap-steps 1) 2) (pose 13.0 64 0.5 {:on-ground false}))]
    (is (yaw-eq? (:yaw r) (/ Math/PI 2)))))

;; re-plan bookkeeping

(deftest after-walk-branches
  (are [replans done expected] (= expected (ex/after-walk p replans done))
    0 {:status :arrived :at [1 2 3]} {:finish {:status :arrived :at [1 2 3] :replans 0}}
    2 {:status :arrived :at [1 2 3]} {:finish {:status :arrived :at [1 2 3] :replans 2}}
    0 {:status :off-plan :at [1 2 3] :step 4} {:replan 1}
    4 {:status :off-plan :at [1 2 3] :step 4} {:replan 5}
    5 {:status :off-plan :at [1 2 3] :step 4} {:finish {:status :gave-up :reason :replan-limit :replans 5 :at [1 2 3]}}
    3 {:status :stuck :at [1 2 3] :step 4 :why "w"} {:finish {:status :stuck :at [1 2 3] :step 4 :why "w" :replans 3}}))
