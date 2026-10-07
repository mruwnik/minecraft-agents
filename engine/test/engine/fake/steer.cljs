(ns engine.fake.steer
  "The fake's steer and pathWorld over cljs world data {:blocks {[x y z] name} :states {[x y z] props}
  :self {:pos [x y z]} :controls {} :unreachable #{[x y z]} :no-path #{[x y z]}}.

  steer is a toy kinematic walker, not physics: every tick (a setImmediate, so tests are fast) it asks decide for
  controls, then moves 0.2 blocks (0.26 sprinting) along the yaw, steps up at most 0.6 (1.25 with jump), drops to the
  next floor at once, and climbs or descends a ladder. A move into a cell holding a fence, gate, wall, pane or bamboo is
  judged by the body's box against the joined blocks' boxes (path/space.mjs), so a body slips beside a post as in the game. The kinematics are pure functions (body, controls, yaw, world)
  -> body. A tick budget of timeout-s * 20 (at most 2400) stands in for the real time bound.

  steer! runs the loop against a world atom. It resolves to {:status :done :result r :ticks n}, {:status :timeout
  :pose p} or {:status :failed :reason s} (cljs data; the primitives' JS shapes are made when the fake is wired in),
  and rejects with cut-error when the owner changes. When a walk resolves, (after-walk world from-pos) -> world runs
  once, in place of the fake moveTo's drag of leashed animals and tempted followers (those live in the animals ns); a
  cut walk runs nothing. path-world builds the planner's snapshot through engine.path.fixture, path/blocks.mjs and
  path/space.mjs by interop.
  Test-only; ported from the deleted js/fake-steer.mjs."
  (:require [engine.fake.doors :as doors]
            [engine.path.fixture :as fx]
            [engine.fake.node :as node]))

(def max-ticks 2400)
(def ticks-per-s 20)
(def walk 0.2)
(def sprint 0.26)
(def step 0.6)
(def jump-step 1.25)
(def climb 0.2)
(def slip 0.15)
(def height 1.8)
(def scan-down 4)
(def fall-limit 256)
(def bury 3)
(def passable
  #{"air" "water" "ladder" "vine" "short_grass" "tall_grass" "rail" "powered_rail" "detector_rail" "activator_rail"})
;; small blocks (and crops) with no collision box: the planner and the game walk through them
(def no-collision #"^(lever|torch|wall_torch|redstone_torch|redstone_wall_torch|wheat|carrots|potatoes|beetroots)$|_(button|pressure_plate|sign|wall_sign|hanging_sign)$")
(def climbable-names #{"ladder" "vine"})
;; blocks whose boxes leave part of their cell free: the steer judges a move into such a cell by boxes, not by cell
(def narrow-re #"_fence$|_fence_gate$|_wall$|_pane$|^glass_pane$|^iron_bars$|^bamboo$")

(defn round [n] (/ (js/Math.round (* n 1e6)) 1e6))
(defn floor [n] (js/Math.floor n))
(defn bad-args [message] (ex-info message {:code "bad-args" :bad-args true}))
(defn cut-error [] (ex-info "cut: the ownership token changed" {:code "cut"}))

(defn name-at [w x y z] (get-in w [:blocks [x y z]] "air"))

(defn solid? [w x y z]
  (let [name (name-at w x y z)]
    (and (not (passable name))
         (not (re-find no-collision name))
         (not (doors/open? w [x y z])))))

(defn climbable? [w x y z]
  (or (contains? climbable-names (name-at w x y z)) (doors/climbs-through? w [x y z])))

(defn ground-at
  "Ground level of the cell (x, z) for a body whose feet are at y: the first y' from one above down to scan-down below
  with a solid cell under it and two free cells above, else nil."
  [w x y z]
  (let [top (inc (floor y))]
    (->> (range top (- top scan-down 2) -1)
         (filter (fn [g] (and (solid? w x (dec g) z) (not (solid? w x g z)) (not (solid? w x (inc g) z)))))
         first)))

(declare body-clear?)

(defn narrow-cell?
  "Does the body's cell at (x, z), feet or head row, hold a block whose collision leaves part of the cell free?"
  [w x y z]
  (let [cy (floor y)]
    (boolean (some #(re-find narrow-re (name-at w x % z)) [cy (inc cy)]))))

(defn narrow-or-same?
  "Is (x, z) in the body's own cell or in a cell holding a narrow block (the cells the box judgement covers)?"
  [w body x z]
  (or (and (= (floor x) (floor (:x body))) (= (floor z) (floor (:z body))))
      (narrow-cell? w (floor x) (:y body) (floor z))))

(defn horizontal
  "The body after the horizontal part of a tick. A move whose cell holds a narrow block (a post, gate, wall, pane or
  bamboo) is judged by the body's box against the blocks' boxes, as the planner's space judges it; a blocked move
  slides along one axis, as the game's collision does."
  [w body {:keys [forward jump] :as controls} yaw]
  (if-not forward
    (assoc body :collided false)
    (let [dist (if (:sprint controls) sprint walk)
          x (- (:x body) (* (js/Math.sin yaw) dist))
          z (- (:z body) (* (js/Math.cos yaw) dist))
          [cx cz] [(floor x) (floor z)]]
      (cond
        (narrow-cell? w cx (:y body) cz)
        (if-let [[x' z'] (->> [[x z] [x (:z body)] [(:x body) z]]
                              (filter (fn [[x' z']] (and (narrow-or-same? w body x' z') (body-clear? w x' (:y body) z'))))
                              first)]
          (assoc body :x x' :z z' :collided (not= [x' z'] [x z]))
          (assoc body :collided true))
        :else
      (if (and (= cx (floor (:x body))) (= cz (floor (:z body))))
        (assoc body :x x :z z :collided false)
        (let [g (ground-at w cx (:y body) cz)
              rise (if (nil? g) js/Infinity (- g (:y body)))
              cell (floor (:y body))
              wet? (fn [y] (= "water" (name-at w cx y cz)))]
          (cond
            ;; a swimmer moves through open water, or over its surface, with no floor under it
            (and (or (wet? cell) (wet? (dec cell))) (not (solid? w cx cell cz)) (not (solid? w cx (inc cell) cz)))
            (assoc body :x x :z z :collided false)
            (or (<= rise step) (and (<= rise jump-step) jump)) (assoc body :x x :y g :z z :collided false)
            :else (assoc body :collided true))))))))

(defn vertical
  "The body after the vertical part of a tick. A jump without forward, on the ground out of water, lifts the body to the
  top of a jump (the next tick's walk, from there, steps onto what is ahead); the lift falls at the tick after."
  [w body {:keys [jump forward]}]
  (let [[cx cz] [(floor (:x body)) (floor (:z body))]
        cell (floor (:y body))]
    (cond
      (and jump (not forward) (not (:lifted body)) (= (:y body) cell) (solid? w cx (dec cell) cz)
           (not= "water" (name-at w cx cell cz)) (not (climbable? w cx cell cz)))
      (assoc body :y (round (+ (:y body) jump-step)) :vy 0.42 :lifted true)
      (:lifted body) (dissoc body :lifted)
      ;; swimming: a jump rises a cell, with none the body sinks a cell per tick, and a body in the air over water
      ;; falls into it one cell at a time (it neither flies nor drops through to the floor at once)
      (= "water" (name-at w cx cell cz))
      (cond
        (and jump (not (solid? w cx (inc cell) cz))) (assoc body :y (inc cell) :vy 0.3)
        (solid? w cx (dec cell) cz) (assoc body :vy 0)
        :else (assoc body :y (dec cell) :vy -0.3))
      (and (not (solid? w cx (dec cell) cz)) (= "water" (name-at w cx (dec cell) cz)))
      (assoc body :y (dec cell) :vy -0.3)
      :else
    (if (climbable? w cx cell cz)
      (if jump
        (if (solid? w cx (floor (+ (:y body) climb height)) cz)
          (assoc body :vy 0)
          (assoc body :y (round (+ (:y body) climb)) :vy climb))
        (let [lower (round (- (:y body) slip))
              below (floor lower)]
          (if (solid? w cx below cz)
            (assoc body :y (inc below) :vy (- (inc below) (:y body)))
            (assoc body :y lower :vy (- slip)))))
      (let [ground (->> (range (dec cell) (- cell 1 fall-limit) -1) (filter #(solid? w cx % cz)) first)]
        (if (or (solid? w cx (dec cell) cz) (nil? ground))
          (assoc body :vy 0)
          (assoc body :y (inc ground) :vy (- (inc ground) (:y body)))))))))

(defn step-body
  "One tick of the walker: horizontal then vertical."
  [w body controls yaw]
  (vertical w (horizontal w body controls yaw) controls))

(declare start-body)

(def swim-ticks 60)

(defn swim-lands?
  "Does a body in the water swim to the cell `toward` and stand in it? It holds forward and jump every tick, through
  the same step-body as a walk: the water branch of vertical bobs it a cell, and the horizontal step climbs at most
  jump-step, so a rim up to about one block over the surface is reached and walls or a higher rim are not."
  [w [tx ty tz]]
  (let [start (start-body w)
        yaw (js/Math.atan2 (- (- (+ tx 0.5) (:x start))) (- (- (+ tz 0.5) (:z start))))
        arrived? (fn [b] (and (= [tx tz] [(floor (:x b)) (floor (:z b))]) (= ty (:y b)) (solid? w tx (dec ty) tz)))]
    (boolean (some arrived? (take swim-ticks (iterate #(step-body w % {:forward true :jump true} yaw) start))))))

(defn pose-of [w body yaw]
  (let [[cx cz] [(floor (:x body)) (floor (:z body))]
        cell (floor (:y body))]
    {:x (:x body)
     :y (:y body)
     :z (:z body)
     :vy (:vy body)
     :on-ground (and (solid? w cx (dec cell) cz) (= (:y body) cell))
     :on-climbable (climbable? w cx cell cz)
     :in-water (= "water" (name-at w cx cell cz))
     :collided (:collided body)
     :yaw yaw
     :t (js/Date.now)}))

(defn start-body [w]
  (let [[x y z] (get-in w [:self :pos])
        mid (if (:body-hitbox w) identity #(+ % 0.5))]    ; hitbox mode: the position is the true one, not a cell corner
    {:x (mid x) :y y :z (mid z) :vy 0 :collided false}))

;; --- the planner's view of the world

(defn buried
  "The old walker gave up on a target in :unreachable or :no-path whatever the range: the planner has no such switch,
  so the cells within bury of one (not the body's own) are stone in the planner's view, which leaves no spot to stand
  on within a walk's range of it. Cells that hold a block already stay what they are. [[pos \"stone\"] ...]"
  [w]
  (let [[bx by bz] (get-in w [:self :pos])
        body-cell? (fn [[x y z]] (and (= x bx) (= z bz) (or (= y by) (= y (inc by)))))
        span (range (- bury) (inc bury))]
    (->> (concat (:unreachable w) (:no-path w))
         (mapcat (fn [[cx cy cz]] (for [dx span dy span dz span] [(+ cx dx) (+ cy dy) (+ cz dz)])))
         (remove #(or (body-cell? %) (contains? (:blocks w) %)))
         (map (fn [pos] [pos "stone"])))))

(def blocks-mod (delay (node/require-here "./js/path/blocks.mjs")))
(def space (delay (node/require-here "./js/path/space.mjs")))

(defn in-view-fn
  "Whether a cell's chunk column lies within the world's :view-chunks of the body's chunk (always, without it)."
  [w]
  (if-let [n (:view-chunks w)]
    (let [[bx _ bz] (get-in w [:self :pos])
          bcx (bit-shift-right (floor bx) 4)
          bcz (bit-shift-right (floor bz) 4)]
      (fn [[[x _ z] _]]
        (and (<= (js/Math.abs (- (bit-shift-right x 4) bcx)) n)
             (<= (js/Math.abs (- (bit-shift-right z 4) bcz)) n))))
    (constantly true)))

(defn path-world-build
  "{:snapshot :table :space}: the fake's blocks as the planner reads them. The fixture joins fences, walls and panes
  to their neighbours, as the server does: a lone post is a gap a body slips through. With :view-chunks only the
  columns near the body are loaded (in-view-fn)."
  [w]
  (let [entries (filter (in-view-fn w) (concat (:blocks w) (buried w)))
        blocks (map (fn [[[x y z :as pos] name]] [x y z name (doors/path-props w pos)]) entries)]
    {:snapshot (fx/fixture-snapshot {:blocks blocks})
     :table (.defaultStateTable ^js @blocks-mod)
     :space @space}))

(def path-world-cache
  "The last path-world: [inputs result]. A world is an immutable map, so the snapshot is rebuilt only when a map that
  path-world reads is no longer the identical one (any block, state or marker write replaces it), or the body moved
  while its position matters (a view window, or buried cells that skip the body's own)."
  (atom nil))

(defn path-world-inputs [w]
  (let [pos-matters (or (:view-chunks w) (seq (:unreachable w)) (seq (:no-path w)))]
    [(:blocks w) (:states w) (:unreachable w) (:no-path w) (:view-chunks w) (when pos-matters (get-in w [:self :pos]))]))

(defn path-world
  "path-world-build, reused while its inputs are the identical values (callers do not mutate the snapshot)."
  [w]
  (let [inputs (path-world-inputs w)
        [last-inputs result] @path-world-cache]
    (if (and last-inputs (every? true? (map identical? inputs last-inputs)))
      result
      (let [result (path-world-build w)]
        (reset! path-world-cache [inputs result])
        result))))

(defn local-blocks
  "[x y z name props] for the cells within 2 of (cx, cz), rows cy-1 .. cy+2, air where the world holds nothing (so every
  column is loaded for the planner's snapshot)."
  [w cx cy cz]
  (for [x (range (- cx 2) (+ cx 3)) y (range (dec cy) (+ cy 3)) z (range (- cz 2) (+ cz 3))]
    [x y z (name-at w x y z) (doors/path-props w [x y z])]))

(defn body-clear?
  "Does the body's box (the planner's half-width, feet at y, 1.8 tall) centred at (px, pz) touch no collision box of the
  blocks around it? Fences are joined to their neighbours as the server joins them."
  [w px y pz]
  (let [[cx cy cz] [(floor px) (floor y) (floor pz)]
        snapshot (fx/fixture-snapshot {:blocks (local-blocks w cx cy cz)})
        boxes (.boxesNear ^js @space snapshot (.defaultStateTable ^js @blocks-mod) cx cy cz y (+ y height))]
    (not (.bodyHits ^js @space boxes px pz))))

;; --- the tick loop

(defn decide-safely [decide pose]
  (try
    (or (decide pose) {})
    (catch :default err {:failed (subs (str err) 0 (min 200 (count (str err))))})))

(defn budget-of
  "The tick budget for a timeout-s given to steer, or throws bad-args."
  [timeout-s]
  (when-not (and (number? timeout-s) (js/Number.isFinite timeout-s) (< 0 timeout-s) (<= timeout-s 120))
    (throw (bad-args "steer timeoutS must be a number in (0, 120]")))
  (min max-ticks (js/Math.ceil (* timeout-s ticks-per-s))))

(defn steer!
  "Walk the world atom's body under decide's control. opts: {:state atom :owner-of fn :after-walk fn}; args:
  {:decide (pose -> {:controls :yaw :done :failed}) :timeout-s n}. Returns a promise."
  [{:keys [state owner-of after-walk]} token {:keys [decide timeout-s] :or {timeout-s 60}}]
  (when-not (fn? decide) (throw (bad-args "steer needs decide, a function")))
  (let [budget (budget-of timeout-s)
        from (get-in @state [:self :pos])]
    (js/Promise.
     (fn [resolve reject]
       (let [body (atom (start-body @state))
             yaw (atom 0)
             ticks (atom 0)
             finish (fn [settle value]
                      (swap! state assoc :controls {})
                      (when (= settle resolve) (swap! state after-walk from))
                      (settle value))
             tick (fn tick []
                    (cond
                      (not= (owner-of) token) (finish reject (cut-error))
                      (>= @ticks budget) (finish resolve {:status "timeout" :pose (pose-of @state @body @yaw)})
                      :else
                      (let [_ (swap! ticks inc)
                            out (decide-safely decide (pose-of @state @body @yaw))
                            controls (or (:controls out) {})]
                        (cond
                          (:failed out) (finish resolve {:status "failed" :reason (:failed out)})
                          (:done out) (finish resolve {:status "done" :result (:done out) :ticks @ticks})
                          :else
                          (do (swap! state assoc :controls controls)
                              (when (number? (:yaw out)) (reset! yaw (:yaw out)))
                              (reset! body (step-body @state @body controls @yaw))
                              (swap! state assoc-in [:self :pos]
                                     (if (:body-hitbox @state)
                                       [(:x @body) (floor (:y @body)) (:z @body)]
                                       [(floor (:x @body)) (floor (:y @body)) (floor (:z @body))]))
                              (when (seq (:wires @state))
                                (swap! state doors/tick-wires (get-in @state [:self :pos])))
                              (js/setImmediate tick))))))]
         (js/setImmediate tick))))))
