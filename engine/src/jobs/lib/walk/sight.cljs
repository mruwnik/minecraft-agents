(ns jobs.lib.walk.sight
  "The floor gate of a walk over unseen cells (go-to :unknown): the body never stands on a floor it has not seen. A step
  whose floor was unseen when the plan was made carries :unseen-floor (mark-steps); while that floor is still unknown it
  is a pending gate, and the walk (jobs.lib.walk/decide-tick) gives the executor the steps only up to the takeoff before
  it. Every tick the walk glances at the pending floor: a plain floor (as the plan assumed) lets the walk on, anything else
  ends it {:status :replan :why :changed}. Near the gate the stop controller (control) keeps the body able to stop short of
  the floor's column: on the way to the takeoff it holds forward back while the body could not stop in time; on the takeoff
  it moves the body to the peek point at the takeoff's edge and holds it there, looking at the floor. A floor still unknown
  once the body is still and has looked ends the walk {:status :replan :why :unseen :cells [floor]} (missed: the caller's
  next plan avoids it). A body that ends up on the floor anyway (a push) ends it {:status :replan :why :pushed}.
  The controller predicts with prismarine-physics' own update (the body's client physics): pure, no world reads."
  (:require [engine.args :as a]
            [engine.game :as game]
            [engine.path.planner.base :as base]
            [engine.settings :as settings]))

(a/defargs settings
  {::peek-gap {:default 0.05 :doc "How far short of the takeoff's edge facing the unseen floor (blocks, each axis) the body stops to look at it." :spec (a/num-in 0 0.5)}
   ::end-gap {:default 0.01 :doc "How far short of that edge the body's centre always stays (blocks, each axis)." :spec (a/num-in 0 0.5)}
   ::margin {:default 0.02 :doc "Blocks the stop controller keeps spare on the way to the takeoff." :spec (a/num-in 0 1)}
   ::still-ticks {:default 5 :doc "Ticks the body stands still at the gate, looking at the floor, before an unseen floor is given up as missed." :spec (a/int-in 1 nil)}
   ::max-ticks {:default 100 :doc "Ticks the body may spend on the takeoff before the gate gives the floor up as missed." :spec (a/int-in 1 nil)}
   ::look-blocks {:default 6 :doc "Within this many blocks of a pending floor the walk looks at it." :spec (a/num-in 0 nil)}})

(defn gate-opts
  "The settings as a map {:peek-gap :end-gap :margin :still-ticks :max-ticks :look-blocks}."
  []
  (into {} (map (fn [k] [(keyword (name k)) (settings/get settings k)])) (keys settings)))

;; ---------------------------------------------------------------- the body's physics (prismarine-physics 1.11)

(def slipperiness {"slime_block" 0.8 "ice" 0.98 "packed_ice" 0.98 "frosted_ice" 0.98 "blue_ice" 0.989})
(def velocity-factors {"soul_sand" 0.4 "honey_block" 0.4})
(def default-slip 0.6)

(defn ground
  "The horizontal model of a body on a floor: inertia f (slipperiness x 0.91), the walk's acceleration a (0.1 x
  0.1627714 / f^3; x1.3 sprinting) and the velocity factor of a soul sand or honey floor."
  ([slip factor] (ground slip factor false))
  ([slip factor sprint]
   (let [f (* slip 0.91)]
     {:f f :a (* (if sprint 0.13 0.1) (/ 0.1627714 (* f f f))) :factor factor})))

(defn air
  "The horizontal model of a body in the air (sprinting adds 0.006 to the acceleration)."
  ([] (air false))
  ([sprint] {:f 0.91 :a (if sprint 0.026 0.02) :factor 1}))

(defn floor-model
  "The ground model of a floor block by name (nil: stone)."
  ([name] (floor-model name false))
  ([name sprint] (ground (get slipperiness name default-slip) (get velocity-factors name 1) sprint)))

(defn heading
  "The velocity change [dvx dvz] of controls (forward back left right sneak) at yaw under acceleration a: prismarine's
  applyHeading (inputs 0.98, sneak x0.3, a diagonal input normalised)."
  [{:keys [forward back left right sneak]} yaw a]
  (let [m (if sneak 0.3 1)
        s (* (- (if right 1 0) (if left 1 0)) 0.98 m)
        fw (* (- (if forward 1 0) (if back 1 0)) 0.98 m)
        speed (js/Math.sqrt (+ (* s s) (* fw fw)))]
    (if (< speed 0.01)
      [0 0]
      (let [k (/ a (max speed 1))
            s (* s k) fw (* fw k)
            y (- js/Math.PI yaw)
            sn (js/Math.sin y) cs (js/Math.cos y)]
        [(- (+ (* s cs) (* fw sn))) (- (* fw cs) (* s sn))]))))

(defn zeroed [v] (if (< (js/Math.abs v) 0.003) 0 v))

(defn step
  "One tick of the horizontal state [x z vx vz] under model with the velocity change [dvx dvz]."
  [[x z vx vz] {:keys [f factor]} [dvx dvz]]
  (let [vx (+ (zeroed vx) dvx) vz (+ (zeroed vz) dvz)
        x (+ x vx) z (+ z vz)]
    [x z (* vx factor f) (* vz factor f)]))

(def inputs
  "Every control set the controller chooses from: no input, and each of 8 headings walking or sneaking."
  (into [{}]
        (for [fw [:forward nil :back] st [:left nil :right] sn [false true]
              :when (or fw st)]
          (cond-> {:sneak sn} fw (assoc fw true) st (assoc st true)))))

(defn deltas
  "The velocity change [dvx dvz] of each of inputs at yaw under model, in inputs' order."
  [model yaw]
  (mapv #(heading % yaw (:a model)) inputs))

(defn brake
  "Of the velocity changes ds (deltas), the one that leaves the least speed after this tick (no reversing past what that
  input gives)."
  [[_ _ vx vz] ds]
  (let [vx (zeroed vx) vz (zeroed vz)]
    (apply min-key (fn [[dx dz]] (js/Math.hypot (+ vx dx) (+ vz dz))) ds)))

(defn rollout
  "The positions [[x z] ...] of a body braking as hard as it can (brake each tick, ds the deltas at its yaw) from state
  until still, at most 60 ticks; the last is where it stops."
  [state model ds]
  (loop [s state n 0 out []]
    (let [[x z vx vz] s]
      (if (or (>= n 60) (and (zero? (zeroed vx)) (zero? (zeroed vz))))
        (conj out [x z])
        (let [s' (step s model (brake s ds))]
          (recur s' (inc n) (conj out [(s' 0) (s' 1)])))))))

(defn brake-dist
  "The distance a body at speed v (along one line) covers braking as hard as it can under model."
  [v model]
  (let [[x] (peek (rollout [0 0 v 0] model (deltas model (- (/ js/Math.PI 2)))))] x))

;; ---------------------------------------------------------------- marks and gates

(defn see-depth
  "The deepest drop whose landing floor the body sees from the peek point when only cells within near count as seen (the
  dark): the eye 1.62 over the takeoff, 0.55 short of the landing column's centre, so the floor of a drop d is
  sqrt(0.55^2 + (d + 2.12)^2) away (engine.floor-sight-pin-test). 4 -> 1, 7 (a torch in a hand) -> 4."
  [near]
  (js/Math.max 0 (js/Math.floor (- (js/Math.sqrt (js/Math.max 0 (- (* near near) (* 0.55 0.55)))) 2.12))))

(defn floor-cell [{:keys [x y z]}] [x (dec y) z])

(defn mark-steps
  "steps with :unseen-floor on each step after the first that stands on its floor (h 0) where (unknown-floor? x y z) of
  that floor cell is true."
  [steps unknown-floor?]
  (vec (map-indexed (fn [i s]
                      (cond-> s
                        (and (pos? i) (zero? (:h s)) (apply unknown-floor? (floor-cell s))) (assoc :unseen-floor true)))
                    steps)))

(defn pending
  "The index of the first step from i on (at least 1) with :unseen-floor not yet resolved, or nil."
  [steps resolved i]
  (first (filter #(and (:unseen-floor (nth steps %)) (not (contains? resolved %))) (range (max 1 i) (count steps)))))

(defn floor-cells
  "The floor cells of steps' marked steps, as #{[x y z]}."
  [steps]
  (into #{} (comp (filter :unseen-floor) (map floor-cell)) steps))

(defn support-cell
  "The cell a body on the ground at pose stands on: under its centre, the block whose top it stands on."
  [{:keys [x y z on-ground]}]
  (when on-ground [(js/Math.floor x) (dec (js/Math.ceil (- y 1e-6))) (js/Math.floor z)]))

;; ---------------------------------------------------------------- where to stop

(defn side
  "-1, 0 or 1: where cell coordinate b lies from a."
  [a b] (js/Math.sign (- b a)))

(defn target
  "The peek point on the takeoff tk facing the gate step k: each axis toward k peek-gap short of the takeoff's edge, the
  other axis the takeoff's stand point."
  [tk k {:keys [peek-gap]}]
  (let [sx (side (:x tk) (:x k)) sz (side (:z tk) (:z k))]
    [(case sx 1 (- (inc (:x tk)) peek-gap) -1 (+ (:x tk) peek-gap) (:px tk))
     (case sz 1 (- (inc (:z tk)) peek-gap) -1 (+ (:z tk) peek-gap) (:pz tk))]))

(defn safe-box
  "[x0 x1 z0 z1] the takeoff tk's column the body's centre must stay in: end-gap short of each edge facing the gate step k,
  open on the side of the step before it (prev, known floor), the cell's edge elsewhere."
  [prev tk k {:keys [end-gap]}]
  (let [lim (fn [c to from]
              (let [st (side c to) sb (when from (side c from))]
                [(cond (= -1 st) (+ c end-gap) (= -1 sb) js/-Infinity :else c)
                 (cond (= 1 st) (- (inc c) end-gap) (= 1 sb) js/Infinity :else (inc c))]))
        [x0 x1] (lim (:x tk) (:x k) (:x prev))
        [z0 z1] (lim (:z tk) (:z k) (:z prev))]
    [x0 x1 z0 z1]))

(defn inside? [[x0 x1 z0 z1] [x z]] (and (<= x0 x x1) (<= z0 z z1)))

(defn excess
  "How far [x z] lies outside the box (0 inside)."
  [[x0 x1 z0 z1] [x z]]
  (+ (max 0 (- x0 x) (- x x1)) (max 0 (- z0 z) (- z z1))))

;; ---------------------------------------------------------------- the stop controller

(defn look-at
  "[yaw pitch] from the eye of a body at pose to the centre of cell [x y z] (mineflayer's angles)."
  [{px :x py :y pz :z} [x y z]]
  (let [dx (- (+ x 0.5) px) dz (- (+ z 0.5) pz) dy (- (+ y 0.5) (+ py game/eye-height))]
    [(js/Math.atan2 (- dx) (- dz)) (js/Math.atan2 dy (js/Math.hypot dx dz))]))

(defn hold
  "On the takeoff: the inputs (at yaw, which looks at the floor) this tick that bring the body nearest the peek point T
  while every place it could brake to stays inside box. model is the floor's (or the air's). {:controls :choice}: choice
  :none when no input is best."
  [box T model yaw {:keys [x z vx vz]}]
  (let [s [x z vx vz]
        ds (deltas model yaw)
        rated (for [[in d] (map vector inputs ds)
                    :let [s1 (step s model d)
                          path (rollout s1 model ds)
                          [rx rz] (peek path)
                          out (reduce + (map #(excess box %) (cons [(s1 0) (s1 1)] path)))]]
                {:in in :out out :d (+ (js/Math.hypot (- rx (T 0)) (- rz (T 1))) (if (seq in) 0.005 0))})
        safe (filter #(zero? (:out %)) rated)
        best (if (seq safe) (apply min-key :d safe) (apply min-key :out rated))]
    {:controls (merge {:forward false :back false :left false :right false :jump false :sprint false :sneak false} (:in best))
     :choice (if (seq (:in best)) :move :none)}))

(defn path-left
  "Blocks along the plan from the body at pose through the stand points of steps i .. k-1, then on to the takeoff's limit
  (the safe box's edge facing k, along the last leg)."
  [steps i k box {:keys [x z]}]
  (let [pts (cons [x z] (map (fn [j] [(:px (nth steps j)) (:pz (nth steps j))]) (range i k)))
        legs (reduce + (map (fn [[ax az] [bx bz]] (js/Math.hypot (- bx ax) (- bz az))) pts (rest pts)))
        tk (nth steps (dec k)) g (nth steps k)
        [x0 x1 z0 z1] box
        ex (case (side (:x tk) (:x g)) 1 (- x1 (:px tk)) -1 (- (:px tk) x0) js/Infinity)
        ez (case (side (:z tk) (:z g)) 1 (- z1 (:pz tk)) -1 (- (:pz tk) z0) js/Infinity)]
    (+ legs (min ex ez))))

(def far-blocks "Blocks along the plan beyond which no stop reaches the takeoff (the longest, a sprint on blue ice, is under 3)." 4)

(defn approach
  "On the way to the takeoff: the executor's controls, less sprint, less forward, or with back instead, the first whose
  stop (braking as hard as it can under the slowest of models) still ends margin short of left blocks along the plan.
  Farther than far-blocks the controls are left as they are (no stop is that long)."
  [controls left models now {:keys [vx vz]} {:keys [margin]}]
  (if (> left far-blocks)
    controls
    (let [v (js/Math.hypot vx vz)
          worst (fn [v'] (apply max (map #(brake-dist v' %) models)))
          ends (fn [{:keys [forward back sprint sneak]}]
                 (let [m (if sprint (:sprint now) (:walk now))
                       a (* (:a m) 0.98 (if sneak 0.3 1) (cond forward 1 back -1 :else 0))
                       v1 (+ (zeroed v) a)]
                   (+ v1 (worst (max 0 (* v1 (:factor m) (:f m)))))))
          tries [controls (assoc controls :sprint false) (assoc controls :forward false :sprint false)]]
      (or (first (filter #(<= (ends %) (- left margin)) (filter :forward (take 2 tries))))
          (when (<= (ends (nth tries 2)) left) (nth tries 2))
          ;; no sneak: the stop above was reckoned with a full back input
          (assoc controls :forward false :back true :sprint false :sneak false)))))

(defn floor-name [floors cell] (when floors (floors cell)))

(defn models-ahead
  "The models a stop on the way to the takeoff may brake under: the floor under the body, the takeoff's floor, and the air
  when the body is in the air or a step up to the takeoff leaves the ground (a drop or jump)."
  [steps i k floors pose]
  (let [tk (nth steps (dec k))
        airborne? (or (not (:on-ground pose))
                      (some #(contains? #{:drop :jump :gap} (:move (nth steps %))) (range (max 1 i) k)))]
    (cond-> [(floor-model (floor-name floors (support-cell (assoc pose :on-ground true))))
             (floor-model (floor-name floors (floor-cell tk)))]
      airborne? (conj (air)))))

(defn reached-takeoff?
  "The body is in the takeoff's column and at its height (within half a block)."
  [tk {:keys [x y z]}]
  (and (= (:x tk) (js/Math.floor x)) (= (:z tk) (js/Math.floor z))
       (<= (js/Math.abs (- y (+ (:y tk) (/ (:h tk) 16)))) 0.5)))

(defn on-takeoff?
  "The executor is at the takeoff's step (at-step?) and the gate holds the body there, or the body has reached the takeoff."
  [tk at-step? gstate pose]
  (boolean (and at-step? (or (:held gstate) (reached-takeoff? tk pose)))))

(defn control
  "The gate's controls this tick, with k the pending gate and i the executor's step: on the way, approach (the executor's
  controls held back); on the takeoff (i is k-1 and reached), hold, looking at the floor. Within look-blocks the body looks
  at the floor (pitch, and yaw on the takeoff). {:out {:controls :yaw :pitch} :gstate}, or {:done ...} once the floor is
  given up as missed: still for still-ticks on the takeoff, or max-ticks there; a step the plan lights (:light) is
  {:status :light} then, for the walk to place a torch."
  [{:keys [floors opts]} steps k i gstate pose {:keys [controls yaw pitch]}]
  (let [tk (nth steps (dec k)) g (nth steps k)
        prev (when (>= k 2) (nth steps (- k 2)))
        box (safe-box prev tk g opts)
        cell (floor-cell g)
        [lyaw lpitch] (look-at pose cell)
        near? (<= (js/Math.hypot (- (+ 0.5 (cell 0)) (:x pose)) (- (+ 0.5 (cell 2)) (:z pose))) (:look-blocks opts))
        on? (on-takeoff? tk (= i (dec k)) gstate pose)
        at [(:x pose) (:y pose) (:z pose)]]
    (if-not on?
      (let [now-floor (floor-name floors (support-cell (assoc pose :on-ground true)))
            now (if (:on-ground pose)
                  {:walk (floor-model now-floor) :sprint (floor-model now-floor true)}
                  {:walk (air) :sprint (air true)})
            c (approach controls (path-left steps i k box pose) (models-ahead steps i k floors pose) now pose opts)]
        {:out {:controls c :yaw yaw :pitch (if near? lpitch pitch)} :gstate gstate})
      (let [n (inc (:held gstate 0))
            T (target tk g opts)
            model (if (:on-ground pose) (floor-model (floor-name floors (support-cell pose))) (air))
            {:keys [controls choice]} (hold box T model lyaw pose)
            ;; on slime the client's onGround flickers every tick while the body stands: not falling is enough
            at-rest? (and (< (js/Math.abs (:vy pose 0)) 0.1)
                          (zero? (zeroed (:vx pose))) (zero? (zeroed (:vz pose))))
            ;; a move that gets the body nowhere (a wall in the way) is a stop: let go of the keys
            pos [(:x pose) (:z pose)]
            stuck (if (and at-rest? (= :move choice) (= pos (:pos gstate))) (inc (:stuck gstate 0)) 0)
            blocked? (>= stuck 2)
            still? (or (and at-rest? (= :none choice)) blocked?)
            s (if still? (inc (:still gstate 0)) 0)
            gstate' (assoc gstate :held n :still s :pos pos :stuck stuck)
            controls (if blocked? (zipmap (keys controls) (repeat false)) controls)]
        (if (or (>= s (:still-ticks opts)) (>= n (:max-ticks opts)))
          {:done (if (:light g)
                   {:status :light :cells [cell] :at at :step k :ticks n}
                   {:status :replan :why :unseen :cells [cell] :at at :step k :ticks n})
           :gstate gstate'}
          {:out {:controls controls :yaw lyaw :pitch lpitch} :gstate gstate'})))))

;; ---------------------------------------------------------------- one tick of the gate

(defn sense
  "Resolve pending gates from i on whose floor the body knows now ((see cell): its block, nil while unknown): a plain one
  ((plain? block)) joins resolved, any other ends the walk {:status :replan :why :changed}. {:k :gstate :done}: k the first
  gate still pending (nil: none)."
  [{:keys [see plain?]} steps gstate i pose]
  (loop [gstate gstate]
    (let [k (pending steps (:resolved gstate #{}) i)]
      (if (nil? k)
        {:k nil :gstate gstate}
        (let [cell (floor-cell (nth steps k))
              id (see cell)]
          (cond
            (nil? id) {:k k :gstate gstate}
            (plain? id) (recur (-> gstate (update :resolved (fnil conj #{}) k) (dissoc :held :still)))
            :else {:k k :gstate gstate
                   :done {:status :replan :why :changed :cells [cell] :at [(:x pose) (:y pose) (:z pose)] :step k}}))))))

(defn breach
  "The pending floor (k's) the body at pose stands on, or nil: the gate's invariant broken."
  [steps k pose]
  (when k
    (let [cell (floor-cell (nth steps k))]
      (when (= cell (support-cell pose)) cell))))

(defn note-missed!
  "A walk that gave a floor up (done {:why :unseen :cells}) adds it to the missed Set of the mask on pw's snapshot (cell
  keys; the planner's options.unknown.missed), so the next plan over that mask does not count on seeing it; one whose
  torch could not go on a takeoff ({:why :no-light :cells}), or lit no floor it could see (:no-light), adds that to the
  noLight Set (options.unknown.noLight)."
  [^js pw {:keys [why cells no-light]}]
  (when-let [^js u (some-> pw .-snapshot .-unknown)]
    (let [mark! (fn [field cells]
                  (let [^js marked (or (unchecked-get u field) (doto (js/Set.) (->> (unchecked-set u field))))]
                    (doseq [[x y z] cells] (.add marked (base/cell-key x y z)))))]
      (when-let [field (case why :unseen "missed" :no-light "noLight" nil)]
        (mark! field cells))
      (when (seq no-light) (mark! "noLight" no-light)))))
