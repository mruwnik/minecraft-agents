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
                                   :cx 1.0 :cz 3.2 :swim true :hatch true :opens #js [#js {:x 1 :y 2 :z 3}]}])]
    (is (true? (:hatch s)))
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

(def swim-plan
  "Drop into a pond, swim along, dive, come up, exit onto the far bank."
  [(step 0 63 0 :start) (step 1 62 0 :drop {:swim true}) (step 2 62 0 :swim {:swim true})
   (step 2 61 0 :swim-down {:swim true}) (step 2 62 0 :swim-up {:swim true})
   (step 3 62 1 :corner {:swim true :corner true}) (step 4 63 1 :exit)])

(deftest refusal-swimming-is-walked
  (is (nil? (ex/refusal p swim-plan)))
  (is (nil? (ex/refusal p [(step 5 62 0 :start {:swim true}) (step 6 63 0 :jump)]))))

(deftest refusal-unsupported-kinds
  (are [steps kind reason at]
       (= {:status :refused :kind kind :at at :reason reason} (ex/refusal p steps))
    [(step 0 64 0 :start) (step 1 64 0 :open)] :open "unsupported step kind :open at [1 64 0]" [1 64 0]
    [(step 0 64 0 :start) (step 1 64 0 :walk {:opens [{:x 1}]})] :open "unsupported step kind :open at [1 64 0]" [1 64 0]))

(deftest refusal-the-door-policy-walks-steps-that-open-something
  (are [steps] (nil? (ex/refusal ex/door-policy steps))
    [(step 0 64 0 :start) (step 1 64 0 :walk {:opens [{:x 1 :y 64 :z 0}]})]
    [(step 0 64 0 :start) (step 1 65 0 :open {:opens [{:x 1 :y 65 :z 0}]})])
  (is (= (conj (:moves p) :open) (:moves ex/door-policy)))
  (is (= (dissoc ex/door-policy :moves) (dissoc p :moves))))

(deftest planner-limits-do-not-avoid-opening-under-the-door-policy
  (are [policy kinds] (= kinds (.-kinds (ex/planner-limits policy (constantly false))))
    p 4
    ex/door-policy 0))

(def dry-moves (apply disj (:moves p) [:swim :swim-up :swim-down :exit]))

(deftest refusal-a-policy-that-cannot-swim
  (are [steps kind at]
       (= {:status :refused :kind kind :at at :reason (str "unsupported step kind " kind " at " (pr-str at))}
          (ex/refusal (assoc p :moves dry-moves) steps))
    [(step 0 64 0 :start) (step 1 64 0 :walk {:swim true})] :swim [1 64 0]
    [(step 0 64 0 :start {:swim true}) (step 1 64 0 :walk)] :swim [0 64 0]
    [(step 0 64 0 :start) (step 1 64 0 :swim-up)] :swim-up [1 64 0]))

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

(deftest hop-corner-only-when-the-open-side-is-a-hole-in-the-takeoffs-column-under-a-low-ceiling
  ;; takeoff [0 64 0], landing [1 64 1]; wall [1 64 0] so the open side is [0 1] (takeoff's x, landing's z)
  (let [prev (step 0 64 0 :start)
        corner (step 1 64 1 :corner {:corner true :free [0 1]})
        low #{[1 64 0] [1 66 1]}]
    (are [solid expected] (= expected (ex/hop-corner? prev corner (solid-set solid)))
      low true
      (conj low [0 63 1]) false                     ; floor under the open side: no hole
      #{[1 64 0]} false                             ; no ceiling over the landing: the step-up lifts the body
      (conj low [0 66 1]) false)                    ; the open side as low: the body cannot rise above the landing either
    (is (false? (ex/hop-corner? prev (assoc corner :free [1 0]) (solid-set low))))
    (is (= [nil true nil] (mapv :hop (ex/with-corner-hops [prev corner (step 2 64 1 :walk)] (solid-set low)))))))

(deftest hop-now-on-the-tick-the-box-clears-the-row
  (let [prev (step 0 64 0 :start)
        corner (step 1 64 1 :corner {:corner true :free [0 1] :hop true})]
    ;; box edge at z + 0.31 must pass 1.31 - z left; yaw 0 faces -z, so a push along +z is yaw pi
    (is (true? (ex/hop-now? p prev corner (pose 0.5 64 1.25 {:vz 0.05}) Math/PI true)))
    (is (false? (ex/hop-now? p prev corner (pose 0.5 64 0.9 {:vz 0.05}) Math/PI true)))
    (is (false? (ex/hop-now? p prev corner (pose 0.5 64 1.25 {:vz 0.05 :on-ground false}) Math/PI true)))
    (is (false? (ex/hop-now? p prev (dissoc corner :hop) (pose 0.5 64 1.25 {:vz 0.05}) Math/PI true)))))

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

;; the climb into a trapdoor's cell over a ladder (:hatch, aimed at the ladder's wall): reached only once held there, on
;; the ladder's top edge or climbing a trapdoor the client climbs; a body bobbing over the ladder's top is not there yet
(deftest a-hatch-step-is-reached-once-held-in-its-cell
  (are [ps expected] (= expected (ex/reached? p (step 3 64 3 :climb-up {:hatch true :px 3.8}) ps))
    (pose 3.7 64.0 3.5 {:on-ground true}) true
    (pose 3.5 64.3 3.5 {:on-ground false :on-climbable true}) true
    (pose 3.5 64.1 3.5 {:on-ground false :on-climbable false}) false
    (pose 3.5 63.95 3.5 {:on-ground false :on-climbable true}) false
    (pose 3.5 63.95 3.5 {:on-ground true}) false))

(deftest a-hatch-step-steers-at-the-ladders-wall
  (let [steps [(step 3 63 3 :start) (step 3 64 3 :climb-up {:hatch true :px 3.8}) (step 4 65 3 :jump)]
        r (ex/tick p (state-at steps 1) (pose 3.5 64.1 3.5 {:on-ground false}))]
    (is (= 1 (get-in r [:state :i])))
    (is (yaw-eq? (ex/yaw-to 3.5 3.5 3.8 3.5) (:yaw r)))))

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

(deftest refusal-gap-without-sprint
  (let [hungry (assoc p :sprint false)]
    (are [steps] (nil? (ex/refusal hungry steps))
      (gap-steps 1)
      (gap-steps 2)
      (gap-steps 2 :landing-y 63))
    (is (= {:status :refused :kind :gap-sprint :at [14 64 0] :reason "gap jump at [14 64 0] over 3 cells needs a sprint"}
           (ex/refusal hungry (gap-steps 3))))))

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
    (:moves p) planner/AVOID-OPEN
    (conj (:moves p) :open) 0
    dry-moves (+ planner/AVOID-WATER planner/AVOID-OPEN)
    (disj (:moves p) :swim-down) (+ planner/AVOID-WATER planner/AVOID-OPEN)
    (into dry-moves (conj swim-moves :open)) 0
    (disj (:moves p) :climb-down) (+ planner/AVOID-CLIMB planner/AVOID-OPEN)))

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

(deftest planner-limits-gap-test-needs-a-sprint-for-a-wide-gap
  (let [gap (fn [n ly] ((.-gap (ex/planner-limits (assoc p :sprint false) (solid-set #{}))) 10 64 0 0 1 (+ 11 n) ly 0 0))]
    (are [n ly allowed?] (= allowed? (gap n ly))
      1 64 true
      2 64 true
      3 64 false
      2 63 true
      3 63 false)))

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
    2 10.7 false
    2 10.85 true
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
    2 (pose 10.5 64 0.5) false
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

;; swimming

(defn wet
  "A pose floating in water at feet height y."
  [x y z & {:as extra}]
  (pose x y z (merge {:on-ground false :in-water true} extra)))

(def pond
  "Start on the bank, drop into water at (1 62 0), swim to (3 62 0), exit onto the bank at (4 63 0)."
  [(step 0 63 0 :start) (step 1 62 0 :drop {:swim true}) (step 2 62 0 :swim {:swim true})
   (step 3 62 0 :swim {:swim true}) (step 4 63 0 :exit) (step 5 63 0 :walk)])

(defn swim-controls
  "The controls on step i of steps at a pose."
  [steps i ps]
  (controls-of (ex/tick p (state-at steps i) ps)))

(deftest swimming-holds-the-feet-near-the-step-height
  (are [y jump?] (= jump? (:jump (swim-controls pond 2 (wet 1.6 y 0.5))))
    61.5 true
    62.0 true
    62.15 true
    62.25 false
    62.6 false))

(deftest swimming-moves-forward-towards-the-step
  (let [r (ex/tick p (state-at pond 2) (wet 1.6 62 0.5))]
    (is (true? (:forward (controls-of r))))
    (is (yaw-eq? (:yaw r) (- (/ Math/PI 2))))))

(deftest swim-down-lets-the-body-sink
  (let [steps [(step 2 62 0 :start {:swim true}) (step 2 61 0 :swim-down {:swim true})]]
    (are [y] (false? (:jump (swim-controls steps 1 (wet 2.5 y 0.5))))
      62.0
      61.8
      61.6)))

(deftest swim-up-rises
  (let [steps [(step 2 60 0 :start {:swim true}) (step 2 61 0 :swim-up {:swim true})]]
    (is (true? (:jump (swim-controls steps 1 (wet 2.5 60.2 0.5)))))))

(deftest exit-holds-jump-while-in-the-water
  (are [ps jump?] (= jump? (:jump (swim-controls pond 4 ps)))
    (wet 3.6 62.2 0.5) true
    (wet 3.9 62.7 0.5) true
    (pose 4.1 63.0 0.5) false))

(deftest wading-out-jumps-in-the-water-without-ground
  (let [steps [(step 5 62 0 :start {:swim true}) (step 6 63 0 :jump)]]
    (are [ps jump?] (= jump? (:jump (swim-controls steps 1 ps)))
      (wet 5.3 62.4 0.5) true
      (wet 5.3 62.0 0.5 :on-ground true) true
      (pose 5.3 62.4 0.5 {:on-ground false}) false)))

(deftest no-sprint-in-water
  (let [steps (vec (cons (step 0 62 0 :start {:swim true}) (map #(step % 62 0 :walk {:swim true}) (range 1 6))))]
    (is (false? (:sprint (swim-controls steps 1 (wet 0.6 62 0.5 :on-ground true)))))
    (is (false? (:sprint (swim-controls straight 1 (pose 0.6 64 3.5 {:in-water true})))))))

(deftest a-climb-in-water-keeps-the-climb-rules
  (let [steps [(step 0 64 3 :start) (step 0 62 3 :climb-down {:px 0.5 :pz 3.5})]]
    (is (false? (:jump (swim-controls steps 1 (wet -0.2 61.9 3.5 :on-climbable true)))))))

(deftest swim-steps-are-reached-while-floating
  (are [s ps reached?] (= reached? (ex/reached? p s ps))
    (step 2 62 0 :swim {:swim true}) (wet 2.5 62.3 0.5) true
    (step 2 62 0 :swim {:swim true}) (wet 2.5 61.6 0.5) true
    (step 2 62 0 :swim {:swim true}) (wet 2.5 62.6 0.5) false
    (step 2 61 0 :swim-up {:swim true}) (wet 2.5 60.6 0.5) true))

(deftest a-floating-body-arrives-at-a-final-swim-step
  (let [steps [(step 0 62 0 :start {:swim true}) (step 1 62 0 :swim {:swim true})]]
    (are [ps status] (= status (:status (:done (ex/tick p (state-at steps 1) ps))))
      (wet 1.5 62.2 0.5) :arrived
      (pose 1.5 62.2 0.5 {:on-ground false}) nil
      (wet 1.5 62.7 0.5) nil)))

(deftest a-final-land-step-needs-the-ground-even-from-the-water
  (is (nil? (:done (ex/tick p (state-at pond 5) (wet 5.5 63.0 0.5))))))

(deftest sinking-in-water-is-not-falling-off-the-plan
  (are [ps status] (= status (:status (:done (ex/tick p (state-at pond 2) ps))))
    (wet 1.7 59.0 0.5) nil
    (pose 1.7 59.0 0.5 {:on-ground false}) :off-plan
    (wet 1.7 62.0 2.5) :off-plan
    (wet 1.7 66.0 0.5) :off-plan))

(deftest a-swim-step-gets-longer-before-stuck
  (let [ticks (:swim-no-progress-ticks p)
        ps (wet 1.6 59.0 0.5)
        stuck? (fn [tick] (= :stuck (:status (:done (ex/tick p (state-at pond 2 :tick tick) ps)))))]
    (is (> ticks (:no-progress-ticks p)))
    (is (false? (stuck? (dec ticks))))
    (is (true? (stuck? ticks)))))

(deftest a-land-step-out-of-the-water-keeps-the-land-limit
  (let [steps [(step 5 62 0 :start {:swim true}) (step 6 63 0 :jump)]
        stuck? (fn [tick] (= :stuck (:status (:done (ex/tick p (state-at steps 1 :tick tick) (wet 5.3 62.4 0.5))))))]
    (is (true? (stuck? (:no-progress-ticks p))))))

;; re-plan bookkeeping

(deftest after-walk-branches
  (are [replans done expected] (= expected (ex/after-walk p replans done))
    0 {:status :arrived :at [1 2 3]} {:finish {:status :arrived :at [1 2 3] :replans 0}}
    2 {:status :arrived :at [1 2 3]} {:finish {:status :arrived :at [1 2 3] :replans 2}}
    0 {:status :off-plan :at [1 2 3] :step 4} {:replan 1}
    4 {:status :off-plan :at [1 2 3] :step 4} {:replan 5}
    5 {:status :off-plan :at [1 2 3] :step 4} {:finish {:status :gave-up :reason :replan-limit :replans 5 :at [1 2 3]}}
    3 {:status :stuck :at [1 2 3] :step 4 :why "w"} {:finish {:status :stuck :at [1 2 3] :step 4 :why "w" :replans 3}}))

;; a small step under a low ceiling: pressed on the block ahead, any rise is jumped

(def small-rise-steps [(step 0 64 3 :start) (step 3 64 3 :walk {:h 1 :px 3.5 :pz 3.5})])

(deftest jump-for-a-small-rise-only-when-pressed-on-the-block
  (are [ps jump?] (= jump? (:jump (controls-of (ex/tick p (state-at small-rise-steps 1) ps))))
    (pose 2.4 64 3.5) false
    (pose 2.4 64 3.5 {:collided true}) true
    (pose 2.4 64 3.5 {:collided true :on-ground false}) false
    (pose 2.4 64.0625 3.5 {:collided true}) false))

;; corner jumps: a diagonal jump that slides along a corner needs the side cells clear at the landing's level

(defn corner-allowed?
  "The planner-limits corner test (a body that cannot sprint) for a jump from (10 64 0) to (11 65 1) with solid cells."
  [solid]
  ((.-corner (ex/planner-limits (assoc p :sprint false) (solid-set solid))) 10 64 0 0 11 65 1 0))

(deftest planner-limits-corner-test-is-the-high-corner-rule
  (are [solid allowed?] (= allowed? (corner-allowed? solid))
    #{} true
    #{[11 64 0]} true      ; a corner block no higher than the landing's floor
    #{[11 65 0]} false     ; collision at the landing's feet level
    #{[10 66 1]} false     ; ... or at its head level
    #{[11 67 0]} true      ; above the body
    #{[10 65 1]} false))

(def corner-jump-steps [(step 10 64 0 :start) (step 11 65 1 :jump {:corner true})])

(deftest with-high-corners-marks-corner-jumps-with-a-high-side
  (are [solid marked?] (= marked? (boolean (:high-corner (nth (ex/with-high-corners p corner-jump-steps (solid-set solid)) 1))))
    #{[11 64 0]} false
    #{[11 65 0]} true
    #{[11 66 0]} true))

(deftest with-high-corners-leaves-other-steps-alone
  (let [steps [(step 10 64 0 :start) (step 11 65 1 :jump) (step 12 65 1 :walk {:corner true})]]
    (is (= steps (ex/with-high-corners p steps (solid-set #{[11 65 0] [12 65 0]}))))))

;; a corner jump past a high side block is sprinted; a body that cannot sprint (policy :sprint false) refuses it

(def high-corner-steps (ex/with-high-corners p corner-jump-steps (solid-set #{[11 65 0]})))

(deftest a-high-corner-jump-is-sprinted
  (is (true? (ex/sprint? p high-corner-steps 1 {:on-ground true :in-water false})))
  (is (false? (ex/sprint? p high-corner-steps 1 {:on-ground false :in-water false})))
  (is (false? (ex/sprint? p high-corner-steps 1 {:on-ground true :in-water true})))
  (is (false? (ex/sprint? p corner-jump-steps 1 {:on-ground true :in-water false}))))

(deftest a-high-corner-jump-is-refused-only-without-sprint
  (let [no-sprint (assoc p :sprint false)]
    (is (nil? (ex/refusal p high-corner-steps)))
    (is (= :corner-jump (:kind (ex/refusal no-sprint high-corner-steps))))
    (is (true? ((.-corner (ex/planner-limits p (solid-set #{[11 65 0]}))) 10 64 0 0 11 65 1 0)))
    (is (false? ((.-corner (ex/planner-limits no-sprint (solid-set #{[11 65 0]}))) 10 64 0 0 11 65 1 0)))))

(deftest refusal-names-a-high-corner-jump
  (let [no-sprint (assoc p :sprint false)
        steps (ex/with-high-corners no-sprint corner-jump-steps (solid-set #{[11 65 0]}))]
    (is (= {:status :refused :kind :corner-jump :at [11 65 1]} (select-keys (ex/refusal no-sprint steps) [:status :kind :at])))
    (is (nil? (ex/refusal no-sprint corner-jump-steps)))))

;; a rise jumped straight up when pressed on its wall

(def rise-north [(step 5 64 5 :start) (step 5 65 4 :jump)])

(deftest a-jump-up-at-its-wall-goes-up-before-forward
  (are [ps forward jump] (= {:forward forward :jump jump}
                            (select-keys (controls-of (ex/tick p (state-at rise-north 1) ps)) [:forward :jump]))
    (pose 5.5 64 5.31) false true          ; pressed on the wall: straight up
    (pose 5.5 64 5.35) false true
    (pose 5.5 64 5.5) true true            ; a body further off runs at it as before
    (pose 5.5 65.0013 5.31) true false     ; up on the step's height: forward
    (pose 5.5 64.5 5.31 {:on-ground false}) false false))

(deftest a-jump-in-water-or-round-a-corner-keeps-forward
  (are [st ps] (:forward (controls-of (ex/tick p st ps)))
    (state-at rise-north 1) (pose 5.5 64 5.31 {:in-water true})
    (state-at [(step 5 64 5 :start) (step 5 65 4 :jump {:cx 1})] 1) (pose 5.5 64 5.31)
    (state-at [(step 4 64 5 :start) (step 5 65 4 :jump {:corner true})] 1) (pose 5.5 64 5.31)))

(deftest a-diagonal-jump-with-free-sides-pressed-on-its-block-goes-up-first
  (are [ps forward] (= forward (:forward (controls-of (ex/tick p (state-at [(step 4 64 5 :start) (step 5 65 4 :jump)] 1) ps))))
    (pose 5.5 64 5.31) false
    (pose 5.5 65.0013 5.31) true))

;; The planner asks planner-limits about every gap jump and corner jump it looks at; its answers must be the executor's own
;; refusals (gap-refused with low-ceiling?, high-corner?), whatever form they take.
(defn ceiling-world
  "solid? over a few columns with blocks at various heights round 0 64 0."
  [x y z]
  (contains? #{[1 66 0] [2 67 0] [0 68 0] [3 66 0] [0 66 1] [1 65 1] [-1 67 0] [0 65 -1] [1 64 1] [1 66 -2]} [x y z]))

(deftest planner-limits-answer-as-the-executor-refusals
  (let [limits (ex/planner-limits ex/door-policy ceiling-world)
        gap (.-gap limits)
        corner (.-corner (ex/planner-limits (assoc ex/door-policy :sprint false) ceiling-world))
        gap-cases (for [move (range (count ex/move-names)) h [0 8] [dx dz] [[1 0] [-1 0] [0 1] [0 -1] [1 1]] n (range 1 6)
                        dy [-1 0 1] lh [0 4]]
                    [move h (* dx n) dy (* dz n) lh])
        corner-cases (for [[lx lz] [[1 1] [-1 1] [1 -1] [-1 -1]] ly [63 64 65] lh [0 8]] [lx ly lz lh])]
    (is (= [] (remove (fn [[move h lx dy lz lh]]
                        (let [prev {:x 0 :y 64 :z 0 :h h :move (nth ex/move-names move)}
                              st {:x lx :y (+ 64 dy) :z lz :h lh :move :gap}
                              expected (nil? (ex/gap-refused ex/door-policy prev
                                                             (cond-> st (ex/low-ceiling? ex/door-policy prev st ceiling-world)
                                                               (assoc :low-ceiling true))))]
                          (= expected (true? (gap 0 64 0 h move lx (+ 64 dy) lz lh)))))
                      gap-cases)))
    (is (= [] (remove (fn [[lx ly lz lh]]
                        (= (not (ex/high-corner? ex/door-policy 0 0 lx ly lz lh ceiling-world))
                           (true? (corner 0 64 0 0 lx ly lz lh))))
                      corner-cases)))))

;; a non-climb step from a vine or ladder cell: while the body is above the step's stand height pushing forward only presses
;; it into a block over the exit (the client then climbs), so it waits, falling to the floor

(def vine-exit-steps [(step 0 64 3 :start) (step 1 64 3 :walk)])

(deftest no-forward-on-a-climbable-above-a-walk-step
  (are [steps i ps forward?] (= forward? (:forward (controls-of (ex/tick p (state-at steps i) ps))))
    vine-exit-steps 1 (pose 0.5 64.9 3.5 {:on-ground false :on-climbable true :collided true}) false
    vine-exit-steps 1 (pose 0.5 64.05 3.5 {:on-ground false :on-climbable true}) true
    vine-exit-steps 1 (pose 0.5 64.9 3.5 {:on-ground false :on-climbable false}) true
    [(step 0 64 3 :start) (step 0 66 3 :climb-up {:px 0.5 :pz 3.5})] 1 (pose 0.3 64.9 3.5 {:on-ground false :on-climbable true}) true
    [(step 0 64 3 :start) (step 1 66 3 :walk)] 1 (pose 0.5 64.9 3.5 {:on-ground false :on-climbable true}) true))
