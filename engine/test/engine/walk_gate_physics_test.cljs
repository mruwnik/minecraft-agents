(ns engine.walk-gate-physics-test
  "The floor gate (jobs.lib.walk.sight) walked in prismarine-physics, the body's own client physics: the real
  jobs.lib.walk/decide-tick drives the body over a plan through a step whose floor the body has not seen. The body sees as
  perception does: a glance is a ray from the eye to the cell's centre inside the view cone (the body's real yaw and
  pitch), with a clear line, lit or within near; a cell the grown hitbox reaches is felt. The gate's invariant: the body
  never stands on a floor of a gate step that it has not seen, or that is not plain (a breach). Each course has a twin
  without the gate, which breaches; the approach and the hold each have a twin with that part switched off, which leaves
  the takeoff."
  (:require [cljs.test :refer [deftest is testing]]
            [engine.floor-sight-pin-test :as pin]
            [engine.game :as game]
            [engine.path.executor :as ex]
            [engine.path.planner.unknown :as unknown]
            [engine.perception.rays :as rays]
            [engine.sight :as sight]
            [engine.stairs-physics-test :as sp]
            [jobs.lib.walk :as walk]
            [jobs.lib.walk.sight :as ws]))

(def max-ticks 600)
(def near 4)
(def cone (rays/grid {:fov 70 :aspect (/ 16 9) :ray-deg 1}))

(def air-names #{"air" "void_air" "cave_air"})
(def plain-names #{"stone" "blue_ice" "ice" "packed_ice" "soul_sand" "slime_block" "grass_block"})

(defn name-at [fills [x y z]] (or (nth ((sp/fill-index fills) x y z) 6 nil) "air"))

(defn full? [fills x y z] (not (contains? air-names (name-at fills [x y z]))))

(defn visible?
  "A glance from the bot's eye at the cell: in the cone, a clear line to its centre, lit or within near."
  [fills lit? bot [x y z]]
  (let [e (.-entity bot) p (.-position e)
        ox (.-x p) oy (+ (.-y p) game/eye-height) oz (.-z p)
        vx (- (+ x 0.5) ox) vy (- (+ y 0.5) oy) vz (- (+ z 0.5) oz)
        dist (js/Math.hypot vx vy vz)]
    (and (rays/in-cone? (rays/basis (.-yaw e) (.-pitch e)) (:hx cone) (:hy cone) vx vy vz)
         (sight/line-clear ox oy oz (+ x 0.5) (+ y 0.5) (+ z 0.5) #(full? fills %1 %2 %3))
         (or lit? (<= dist near)))))

(defn felt? [bot [x y z :as cell]]
  (let [p (.-position (.-entity bot))]
    (rays/touches? (.-x p) (.-y p) (.-z p) cell 1)))

(defn steps-of
  "The plan over the course's true blocks (the planner's own steps), the steps whose floor is in unknown marked."
  [fills from goal unknown]
  (some-> (sp/plan fills from goal) (ws/mark-steps (fn [x y z] (contains? unknown [x y z])))))

(defn run
  "Walk course {:fills :from :goal :unknown #{floor cells} :lit :at [px pz] :push (fn [tick state pose] [dvx dvz] or nil)
  :policy} with the gate (gate? true) or without. {:done :steps :breaches :poses :known :ticks}."
  [{:keys [fills from goal unknown lit at push policy] :or {policy ex/policy}} gate?]
  (let [{:keys [mc physics]} @sp/lib
        world (sp/world-of fills)
        phys ((.-Physics physics) mc world)
        _ (set! (.-playerHalfWidth phys) (sp/body-half))
        steps (steps-of fills from goal unknown)
        bot (sp/bot-at from at)
        known (atom (set (remove unknown (for [s steps] (ws/floor-cell s)))))
        see (fn [cell]
              (when (or (contains? @known cell) (felt? bot cell) (visible? fills lit bot cell))
                (swap! known conj cell)
                (name-at fills cell)))
        env (cond-> {:policy policy :steps steps}
              gate? (assoc :gate {:see see :plain? #(contains? plain-names %)
                                  :floors #(name-at fills %) :opts (ws/gate-opts)}))]
    (if (nil? steps)
      {:done {:status :no-plan}}
      (loop [t 0 state (walk/start-state steps (sp/pose-of bot)) poses [] breaches []]
        (let [pose (sp/pose-of bot)
              cell (ws/support-cell pose)
              breaches (cond-> breaches
                         (and cell (contains? unknown cell)
                              (or (not (contains? @known cell)) (not (contains? plain-names (name-at fills cell)))))
                         (conj [t cell pose]))
              _ (doseq [c unknown] (when (felt? bot c) (swap! known conj c)))
              [state' {:keys [done controls yaw pitch]}] (walk/decide-tick env state pose)]
          (if (or done (>= t max-ticks))
            {:done done :steps steps :breaches breaches :poses poses :known @known :ticks t :state state'}
            (do (set! (.-yaw (.-entity bot)) yaw)
                (set! (.-pitch (.-entity bot)) (or pitch 0))
                (.apply (.simulatePlayer phys (new (.-PlayerState physics) bot (clj->js controls)) world) bot)
                (when-let [[dvx dvz] (and push (push t state' pose))]
                  (let [v (.-velocity (.-entity bot))]
                    (set! (.-x v) (+ (.-x v) dvx))
                    (set! (.-z v) (+ (.-z v) dvz))))
                (recur (inc t) state' (conj poses pose) breaches))))))))

(defn max-x [poses] (apply max (map :x poses)))

;; ---------------------------------------------------------------- courses
;; A 1-wide road along +x at z 0 (walls on both sides at z -1 and 1), floor top y 64, start at x 1. The gate step is the
;; first cell past the takeoff at x 7: a drop of 2 onto x 8 (floor y 61) or a level floor at x 8 (y 63).

(defn road
  "The road's fills: floor of floor-name from x 0 to 7, walls, and past x 7 the gate: :drop (a pit 2 deep, floor stone at
  y 61, then a stone floor on to x 14 at y 61) or :level (stone at y 63 on to x 14)."
  [floor-name kind]
  (into [[0 63 0 7 63 0 floor-name {}]
         [-1 63 -1 15 67 -1 "stone" {}] [-1 63 1 15 67 1 "stone" {}] [-1 63 0 -1 67 0 "stone" {}]]
        (case kind
          :drop [[8 61 0 14 61 0 "stone" {}] [8 60 -1 14 67 -1 "stone" {}] [8 60 1 14 67 1 "stone" {}]
                 [15 60 0 15 67 0 "stone" {}]]
          :level [[8 63 0 14 63 0 "stone" {}] [15 63 0 15 67 0 "stone" {}]])))

(def drop-floor [8 61 0])
(def level-floor [8 63 0])

(defn course [floor-name kind lit & {:as more}]
  (merge {:fills (road floor-name kind) :from [1 64 0] :goal (if (= kind :drop) [12 62 0] [12 64 0])
          :unknown #{(if (= kind :drop) drop-floor level-floor)} :lit lit}
         more))

(def edge 8) ; the takeoff's edge facing the gate (x)

(defn report [r] (pr-str (select-keys r [:done :breaches :ticks])))

(defn no-overshoot? [r] (<= (max-x (:poses r)) (- edge 0.009)))

;; ---------------------------------------------------------------- the scenarios

(deftest g1-g2-a-dark-drop-stops-the-body-at-the-peek-point-still-and-gives-the-floor-up
  (doseq [[floor more] [["stone" {}] ["ice" {}] ["blue_ice" {}] ["slime_block" {}]
                        ["soul_sand" {:fills (conj (road "stone" :drop) [4 63 0 7 63 0 "soul_sand" {}])}]]]
    (testing floor
      (let [r (run (merge (course floor :drop false) more) true)
            x (:x (peek (:poses r)))]
        (is (= [:replan :unseen [drop-floor]] ((juxt :status :why :cells) (:done r))) (report r))
        (is (empty? (:breaches r)) (report r))
        (is (<= 7.93 x 7.991) (str "stopped at " x " " (report r)))
        (is (< (:ticks (:done r)) (:max-ticks (ws/gate-opts))) (str "settled, not timed out " (report r)))))))

(deftest g1-twin-without-the-gate-the-body-walks-off-onto-the-unseen-floor
  (let [r (run (course "stone" :drop false) false)]
    (is (seq (:breaches r)) (report r))))

(deftest g3-no-overshoot-past-the-takeoff-on-any-floor-at-a-sprint
  (doseq [floor ["stone" "ice" "blue_ice" "slime_block"]]
    (testing floor
      (let [r (run (course floor :drop false) true)]
        (is (no-overshoot? r) (str "max x " (max-x (:poses r)) " " (report r)))
        (is (not= :pushed (:why (:done r))) (report r))))))

(deftest g3-twin-without-the-gate-blue-ice-slides-past-the-edge
  (let [r (run (course "blue_ice" :drop false) false)]
    (is (seq (:breaches r)) (report r))))

(defn leg-push
  "A push of dv along +x once the body passes x0 (on the way to the takeoff)."
  [x0 dv]
  (let [done (atom false)]
    (fn [_t _state pose]
      (when (and (not @done) (>= (:x pose) x0))
        (reset! done true)
        [dv 0]))))

(def takeoff-end
  "The body's centre past this x has left the takeoff (x 7): its hitbox (0.3 each way) no longer stands on it."
  (+ edge 0.3))

;; Unpushed, the executor's own stop at the takeoff is enough on every floor: the approach only matters after a push.
(deftest g3-a-push-on-the-way-is-braked-on-the-takeoff-and-its-twin-without-the-approach-falls-off
  ;; 0.3 along the road on blue ice, 1.5 blocks before the takeoff (a mob's hit gives about 0.4)
  (let [c #(course "blue_ice" :drop false :push (leg-push 5.5 0.3))
        on (run (c) true)
        off (with-redefs [ws/approach (fn [controls & _] controls)] (run (c) true))]
    (is (< (max-x (:poses on)) takeoff-end) (str "max x " (max-x (:poses on)) " " (report on)))
    (is (= :unseen (:why (:done on))) (report on))
    (is (> (max-x (:poses off)) takeoff-end) (str "approach off: max x " (max-x (:poses off)) " " (report off)))))

(deftest a-lit-drop-is-seen-from-the-peek-point-and-walked
  (doseq [floor ["stone" "blue_ice"]]
    (testing floor
      (let [r (run (course floor :drop true) true)]
        (is (= :arrived (:status (:done r))) (report r))
        (is (empty? (:breaches r)) (report r))))))

(deftest g5-a-dark-level-floor-is-felt-at-the-edge-and-walked-with-no-breach
  (doseq [floor ["stone" "blue_ice"]]
    (testing floor
      (let [r (run (course floor :level false) true)]
        (is (= :arrived (:status (:done r))) (report r))
        (is (empty? (:breaches r)) (report r))))))

(defn magma-road [] (conj (road "stone" :level) [8 63 0 8 63 0 "magma_block" {}]))

(deftest g5-a-level-floor-felt-as-magma-ends-the-walk-changed-before-it-is-stood-on
  (let [r (run (course "stone" :level false :fills (magma-road)) true)]
    (is (= [:replan :changed [level-floor]] ((juxt :status :why :cells) (:done r))) (report r))
    (is (empty? (:breaches r)) (report r))))

(deftest g5-twin-without-the-gate-the-magma-floor-is-stood-on
  (let [r (run (course "stone" :level false :fills (magma-road)) false)]
    (is (seq (:breaches r)) (report r))))

(deftest g4-a-sneaking-or-walking-body-stops-short-of-the-edge
  (doseq [floor ["stone" "blue_ice"] gait [:sneak :walk]]
    (testing [floor gait]
      (let [r (run (course floor :drop false :policy (assoc ex/policy :sprint false :gait gait)) true)]
        (is (empty? (:breaches r)) (report r))
        (is (no-overshoot? r) (str "max x " (max-x (:poses r)) " " (report r)))))))

(defn held-push
  "A push of dv along +x once the body has held still at the gate for 2 ticks."
  [dv]
  (let [done (atom false)]
    (fn [_t state _pose]
      (when (and (not @done) (>= (get-in state [:gate :still] 0) 2))
        (reset! done true)
        [dv 0]))))

(deftest g6-a-push-while-held-is-braked-on-the-takeoff-and-its-twin-without-the-hold-falls-off
  ;; (a body thrown off sees the floor on the way down, so it is no breach: the hold keeps it on the takeoff)
  (let [r (run (course "stone" :drop false :push (held-push 0.2)) true)
        pushed (atom false)
        push (let [p (held-push 0.2)] (fn [t state pose] (when-let [dv (p t state pose)] (reset! pushed true) dv)))
        hold ws/hold
        ;; the twin holds the body at the peek point as before, and gives no input once pushed
        off (with-redefs [ws/hold (fn [& args] (if @pushed {:controls (zipmap [:forward :back :left :right :jump :sprint :sneak] (repeat false)) :choice :none} (apply hold args)))]
              (run (course "stone" :drop false :push push) true))]
    (is (empty? (:breaches r)) (report r))
    (is (= :unseen (:why (:done r))) (report r))
    (is (< (max-x (:poses r)) takeoff-end) (str "max x " (max-x (:poses r))))
    (is (> (max-x (:poses off)) takeoff-end) (str "hold off: max x " (max-x (:poses off)) " " (report off)))))

(deftest g6-a-push-too-strong-to-brake-throws-the-body-into-the-pit-and-it-sees-the-floor-on-the-way-down
  (let [r (run (course "blue_ice" :drop false :push (held-push 0.4)) true)]
    (is (empty? (:breaches r)) (report r))
    (is (= :arrived (:status (:done r))) (report r))))

(deftest g6-twin-without-the-gate-there-is-no-hold
  (let [r (run (course "stone" :drop false :push (held-push 0.1)) false)]
    (is (seq (:breaches r)) (report r))))

;; ---- airborne entries into the takeoff

(defn step-down-road
  "The road with the takeoff x 7 one block lower (floor y 62) than the road before it (a drop into the takeoff), or half a
  block lower (a bottom slab at y 63); the pit past it is deeper (floor y 59; 60 past the slab), out of near sight in the dark."
  [half?]
  (into (road "stone" :drop)
        (concat
         (if half?
           [[7 63 0 7 63 0 "stone_slab" {:type "bottom"}]]
           [[7 63 0 7 63 0 "air" {}] [7 62 0 7 62 0 "stone" {}]])
         (if half?
           [[8 61 0 14 61 0 "air" {}] [8 60 0 14 60 0 "stone" {}]]
           [[8 60 0 14 61 0 "air" {}] [8 59 0 14 59 0 "stone" {}] [8 59 -1 14 59 -1 "stone" {}] [8 59 1 14 59 1 "stone" {}]]))))

(defn step-down-course [half?]
  (let [fy (if half? 60 59)]
    {:fills (step-down-road half?) :from [1 64 0] :goal [12 (inc fy) 0] :unknown #{[8 fy 0]} :lit false}))

(deftest a-drop-into-the-takeoff-lands-and-stops-short-of-the-gate
  (doseq [[label half?] [["1-block drop" false] ["half-block drop" true]]]
    (testing label
      (let [r (run (step-down-course half?) true)]
        (is (= :unseen (:why (:done r))) (report r))
        (is (empty? (:breaches r)) (report r))
        (is (no-overshoot? r) (str "max x " (max-x (:poses r)) " " (report r)))))))

(deftest a-drop-into-the-takeoff-twin-without-the-gate-breaches
  (let [r (run (step-down-course false) false)]
    (is (seq (:breaches r)) (report r))))

(defn step-up-road
  "The road with the takeoff x 7 one block higher (floor y 64): a jump up into the takeoff; the pit floor is then 3 down."
  []
  (into (road "stone" :drop)
        [[7 64 0 7 64 0 "stone" {}] [-1 64 -1 15 68 -1 "stone" {}] [-1 64 1 15 68 1 "stone" {}]]))

(deftest g7-a-jump-into-the-takeoff-stops-short-of-the-gate
  (let [r (run {:fills (step-up-road) :from [1 64 0] :goal [12 62 0] :unknown #{drop-floor} :lit false} true)]
    (is (= :unseen (:why (:done r))) (report r))
    (is (empty? (:breaches r)) (report r))
    (is (no-overshoot? r) (str "max x " (max-x (:poses r)) " " (report r)))))

;; A diagonal jump up past a corner block as high as the landing (a :high-corner jump, sprinted on the ground) into the
;; takeoff [3 65 3] (floor y 64), then the gate: a drop of 2 east onto [4 63 3] (unseen floor y 62), a pit on to x 6.
(def corner-course
  [[-2 60 -2 8 63 8 "stone" {}]
   [3 64 3 3 64 3 "stone" {}] [3 64 2 3 65 2 "stone" {}] [2 62 3 2 63 3 "air" {}]
   [3 64 4 3 67 4 "stone" {}]
   [4 63 3 6 63 3 "air" {}]
   [4 64 2 6 67 2 "stone" {}] [4 64 4 6 67 4 "stone" {}] [7 64 3 7 67 3 "stone" {}]])

(deftest g7-a-high-corner-sprint-jump-into-the-takeoff-stops-short-of-the-gate
  (let [r (run {:fills corner-course :from [2 64 2] :goal [6 63 3] :unknown #{[4 62 3]} :lit false} true)]
    (is (some? (:steps r)) "a plan")
    (is (some :high-corner (:steps r)) (pr-str (:steps r)))
    (is (empty? (:breaches r)) (report r))
    (is (= :unseen (:why (:done r))) (report r))))

(deftest g7-twin-without-the-gate-the-corner-jump-breaches
  (let [r (run {:fills corner-course :from [2 64 2] :goal [6 63 3] :unknown #{[4 62 3]} :lit false} false)]
    (is (seq (:breaches r)) (report r))))

;; ---- the planner's sight depths rest on the gate's stop

(def pinned-stop "The peek stop the planner's SEE-LIT-DEPTH and LIGHT-DEPTH were pinned at (engine.floor-sight-pin-test)." 0.87)

(deftest the-gates-worst-stop-sees-the-planners-lit-depths
  (let [courses [(course "stone" :drop false) (course "ice" :drop false) (course "blue_ice" :drop false)
                 (course "slime_block" :drop false) (step-down-course false) (step-down-course true)
                 {:fills (step-up-road) :from [1 64 0] :goal [12 62 0] :unknown #{drop-floor} :lit false}]
        stops (for [c courses :let [r (run c true)]] (- (:x (peek (:poses r))) 7))
        worst (apply min stops)]
    (is (>= worst pinned-stop) (str "stops " (pr-str stops)))
    (doseq [d [unknown/SEE-LIT-DEPTH unknown/LIGHT-DEPTH]]
      (is (pin/seen? (pin/cliff d) :lit [worst 0.5] :aimed [1 (- 63 d) 0]) (str "d " d " from x " worst)))
    (is (pin/seen? (pin/cliff unknown/SEE-LIT-DEPTH) :lit [pinned-stop 0.5] :aimed [1 (- 63 unknown/SEE-LIT-DEPTH) 0]))))
