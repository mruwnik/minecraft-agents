(ns engine.fake.steer
  "The fake's steer and pathWorld over cljs world data {:blocks {[x y z] name} :states {[x y z] props}
  :self {:pos [x y z]} :controls {} :unreachable #{[x y z]} :no-path #{[x y z]}}.

  steer is a toy kinematic walker, not physics: every tick (a setImmediate, so tests are fast) it asks decide for
  controls and turns the head (world :yaw/:pitch) to its yaw and pitch, then moves 0.2 blocks (0.26 sprinting, 0.06 sneaking) along the yaw, steps up at most 0.6 (1.25 with jump), drops to the
  next floor once its box (half-width) has left the floor it is on (supported?), and climbs or descends a ladder. A
  move into a cell holding a fence, gate, wall, pane or bamboo is
  judged by the body's box against the joined blocks' boxes (engine.path.space), so a body slips beside a post as in the game; without :body-hitbox the position kept is the cell, or :sub-pos beside a post (sub-pos). The kinematics are pure functions (body, controls, yaw, world)
  -> body. A tick budget of timeout-s * 20 (at most 2400) stands in for the real time bound.

  steer! runs the loop against a world atom. It resolves to {:status :done :result r :ticks n}, {:status :timeout
  :pose p} or {:status :failed :reason s} (cljs data; the primitives' JS shapes are made when the fake is wired in),
  and rejects with cut-error when the owner changes. When a walk resolves, (after-walk world from-pos) -> world runs
  once, in place of the fake moveTo's drag of leashed animals and tempted followers (those live in the animals ns); a
  cut walk runs nothing. path-world builds the planner's snapshot through engine.path.fixture, engine.path.blocks and
  engine.path.space.
  Test-only; ported from the deleted js/fake-steer.mjs."
  (:require [engine.constants :as k]
            [engine.fake.doors :as doors]
            [engine.fake.raw-world :as raw]
            [engine.path.blocks :as blocks]
            [engine.path.fixture :as fx]
            [engine.path.space :as space]
            [engine.fake.node :as node]))

(def max-ticks 2400)
(def ticks-per-s 20)
(def walk 0.2)
(def sprint 0.26)
(def sneak-walk 0.06)
(def step 0.6)
(def jump-step 1.25)
(def climb 0.2)
(def slip 0.15)
(def height 1.8)
(def half-width k/hitbox-half-width)
(def scan-down 6)
(def fall-limit 256)
(def bury 3)
(def deg (/ 180 js/Math.PI))
(def passable
  #{"air" "water" "bubble_column" "ladder" "vine" "short_grass" "tall_grass" "rail" "powered_rail" "detector_rail" "activator_rail"})
;; small blocks (and crops) with no collision box: the planner and the game walk through them
(def no-collision #"^(lever|torch|wall_torch|redstone_torch|redstone_wall_torch|wheat|carrots|potatoes|beetroots|sugar_cane)$|_(button|pressure_plate|sign|wall_sign|hanging_sign)$")
(def climbable-names #{"ladder" "vine"})
;; blocks whose boxes leave part of their cell free: the steer judges a move into such a cell by boxes, not by cell
(def narrow-re #"_fence$|_fence_gate$|_wall$|_pane$|^glass_pane$|^iron_bars$|^bamboo$")

(defn round [n] (/ (js/Math.round (* n 1e6)) 1e6))
(defn floor [n] (js/Math.floor n))
(defn bad-args [message] (ex-info message {:code "bad-args" :bad-args true}))
(defn cut-error [] (ex-info "cut: the ownership token changed" {:code "cut"}))

(defn name-at [w x y z] (get-in w [:blocks [x y z]] "air"))

(defn wet?
  "Is the cell water or a bubble column (swum like water)?"
  [w x y z]
  (contains? #{"water" "bubble_column"} (name-at w x y z)))

(defn dragging?
  "Is the cell a bubble column that pulls down (drag true)? An upward one lifts."
  [w x y z]
  (and (= "bubble_column" (name-at w x y z)) (boolean (get-in w [:states [x y z] :drag]))))

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

(defn under-box
  "The columns [x z] under the body's box (half-width each side of x, z)."
  [x z]
  (distinct (for [bx [(- x half-width) (+ x half-width)] bz [(- z half-width) (+ z half-width)]] [(floor bx) (floor bz)])))

(defn supported?
  "Does a floor under the body's box hold it at feet y (a whole number)? The game holds a body up while any part of its
  box is over a floor, so a body cutting a corner past a hole stays up."
  [w x y z]
  (and (= y (floor y)) (boolean (some (fn [[cx cz]] (solid? w cx (dec y) cz)) (under-box x z)))))

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

(defn box-clear? [w body x z] (and (narrow-or-same? w body x z) (body-clear? w x (:y body) z)))

(defn clamp-axis
  "How far along one axis, from (x0, z0) toward (x1, z1), the body's box goes before it touches a narrow block's box
  (bisected): the point [x z] where the game's collision stops it."
  [w body [x0 z0] [x1 z1]]
  (let [at (fn [f] [(round (+ x0 (* f (- x1 x0)))) (round (+ z0 (* f (- z1 z0))))])]
    (loop [lo 0 hi 1 n 0]
      (if (= n 12)
        (at lo)
        (let [m (/ (+ lo hi) 2)]
          (if (apply box-clear? w body (at m)) (recur m hi (inc n)) (recur lo m (inc n))))))))

(defn heading
  "The yaw the held movement keys move the body along when its head faces head (a key to the side or back moves it that way,
  as a head turn in a walk's glance needs), nil with none held."
  [{:keys [forward back left right]} head]
  (let [dirs (cond-> [] forward (conj head) back (conj (+ head js/Math.PI)) left (conj (+ head (/ js/Math.PI 2))) right (conj (- head (/ js/Math.PI 2))))
        sx (reduce + (map js/Math.sin dirs))
        sz (reduce + (map js/Math.cos dirs))]
    (when (> (js/Math.hypot sx sz) 1e-6) (js/Math.atan2 sx sz))))

(defn horizontal
  "The body after the horizontal part of a tick. A move whose cell holds a narrow block (a post, gate, wall, pane or
  bamboo) is judged by the body's box against the blocks' boxes, as the planner's space judges it; a blocked move
  slides along one axis, else moves along x then z as far as the boxes let it (pressed against the block), as the
  game's collision does."
  [w body {:keys [forward jump sneak] :as controls} head]
  (if-let [yaw (heading controls head)]
    (let [dist (cond sneak sneak-walk (and forward (:sprint controls)) sprint :else walk)
          x (- (:x body) (* (js/Math.sin yaw) dist))
          z (- (:z body) (* (js/Math.cos yaw) dist))
          [cx cz] [(floor x) (floor z)]]
      (cond
        (narrow-cell? w cx (:y body) cz)
        (if-let [[x' z'] (->> [[x z] [x (:z body)] [(:x body) z]]
                              (filter (fn [[x' z']] (and (not= [x' z'] [(:x body) (:z body)]) (box-clear? w body x' z'))))
                              first)]
          (assoc body :x x' :z z' :collided (not= [x' z'] [x z]))
          (let [[x' _] (clamp-axis w body [(:x body) (:z body)] [x (:z body)])
                [_ z'] (clamp-axis w body [x' (:z body)] [x' z])]
            (assoc body :x x' :z z' :collided true)))
        :else
      (if (and (= cx (floor (:x body))) (= cz (floor (:z body))))
        (assoc body :x x :z z :collided false)
        (let [g (ground-at w cx (:y body) cz)
              rise (if (nil? g) js/Infinity (- g (:y body)))
              cell (floor (:y body))
              wet-y? (fn [y] (wet? w cx y cz))]
          (cond
            ;; a swimmer moves through open water, or over its surface, with no floor under it
            (and (or (wet-y? cell) (wet-y? (dec cell))) (not (solid? w cx cell cz)) (not (solid? w cx (inc cell) cz)))
            (assoc body :x x :z z :collided false)
            ;; a drop waits until the box has left the floor it is on
            (and (< rise 0) (supported? w x (:y body) z) (not (solid? w cx cell cz)) (not (solid? w cx (inc cell) cz)))
            (assoc body :x x :z z :collided false)
            (or (<= rise step) (and (<= rise jump-step) jump)) (assoc body :x x :y g :z z :collided false)
            :else (assoc body :collided true))))))
    (assoc body :collided false)))

(defn vertical
  "The body after the vertical part of a tick. A jump without forward, on the ground out of water, lifts the body to the
  top of a jump (the next tick's walk, from there, steps onto what is ahead); the lift falls at the tick after."
  [w body {:keys [jump forward]}]
  (let [[cx cz] [(floor (:x body)) (floor (:z body))]
        cell (floor (:y body))]
    (cond
      (and jump (not forward) (not (:lifted body)) (= (:y body) cell) (solid? w cx (dec cell) cz)
           (not (wet? w cx cell cz)) (not (climbable? w cx cell cz)))
      (assoc body :y (round (+ (:y body) jump-step)) :vy 0.42 :lifted true)
      (:lifted body) (dissoc body :lifted)
      ;; swimming: a jump rises a cell, with none the body sinks a cell per tick, and a body in the air over water
      ;; falls into it one cell at a time (it neither flies nor drops through to the floor at once)
      (wet? w cx cell cz)
      (cond
        (and (not (dragging? w cx cell cz)) (not (solid? w cx (inc cell) cz))
             (or jump (= "bubble_column" (name-at w cx cell cz))))
        (assoc body :y (inc cell) :vy 0.3)
        (solid? w cx (dec cell) cz) (assoc body :vy 0)
        :else (assoc body :y (dec cell) :vy -0.3))
      (and (not (solid? w cx (dec cell) cz)) (wet? w cx (dec cell) cz))
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
        (if (or (solid? w cx (dec cell) cz) (supported? w (:x body) (:y body) (:z body)) (nil? ground))
          (assoc body :vy 0)
          (assoc body :y (inc ground) :vy (- (inc ground) (:y body)))))))))

(defn step-body
  "One tick of the walker: horizontal then vertical. :vx and :vz are the tick's horizontal move (blocks a tick, as
  mineflayer's velocity)."
  [w body controls yaw]
  (let [moved (horizontal w body controls yaw)]
    (vertical w (assoc moved :vx (- (:x moved) (:x body)) :vz (- (:z moved) (:z body))) controls)))

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
     :vx (:vx body 0)
     :vy (:vy body)
     :vz (:vz body 0)
     :on-ground (and (or (solid? w cx (dec cell) cz) (supported? w (:x body) (:y body) (:z body))) (= (:y body) cell))
     :on-climbable (climbable? w cx cell cz)
     :in-water (wet? w cx cell cz)
     :collided (:collided body)
     :yaw yaw
     :t (js/Date.now)}))

(defn sub-pos
  "Non-hitbox mode: the body's true [x y z] where its cell's centre is inside a block (beside a fence post, against a shut
  gate), kept while the body is still in that cell; nil else."
  [w]
  (let [[x y z :as sub] (:sub-pos w)]
    (when (and sub (= [(floor x) y (floor z)] (get-in w [:self :pos]))) sub)))

(defn start-body [w]
  (let [[x y z] (get-in w [:self :pos])
        [sx _ sz] (sub-pos w)
        mid (if (:body-hitbox w) identity #(+ (floor %) 0.5))]    ; hitbox mode: the position is the true one; else the cell's centre
    {:x (or sx (mid x)) :y y :z (or sz (mid z)) :vx 0 :vy 0 :vz 0 :collided false}))

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
  (let [in-view? (in-view-fn w)
        add (fn [acc [[x y z :as pos] name :as entry]]
              (if (in-view? entry) (conj! acc [x y z name (doors/path-props w pos)]) acc))
        blocks (persistent! (reduce add (reduce-kv (fn [acc pos name] (add acc [pos name])) (transient []) (:blocks w)) (buried w)))]
    {:snapshot (fx/fixture-snapshot {:blocks blocks})
     :table (blocks/default-state-table)
     :space space/space}))

(defn masked
  "snapshot read through unknown, live-snapshot.mjs's mask option: a cell the body's memory ((.known unknown cx sy cz), sy the
  world section y: ids + 1, 0 never seen; nil: none of it seen) has not got reads (.-id unknown), a known cell its
  remembered id; unloaded stays unloaded. A known cell w (the fake's world) still holds as remembered reads the
  snapshot's id: the fixture joins fences to their neighbours, which the body's memory of the fake does not. It
  carries unknown, as the live snapshot does. Like the live one a section is masked once, on its first read, into a copy
  (a per-cell masked read was 11% of the planner tests' CPU); the copies are dropped when (.memoryEpoch unknown), the memory's
  write count, moves, as a view kept across rounds read memory live. sectionHas stays unmasked (live masks it): it is the planner's
  pre-check that lets it skip reading cells, so a true where live says false only makes it read the masked cells, same plan.
  pinned (a set of [x y z], the buried cells of :unreachable and :no-path) stay as they are: they stand for the old
  walker's refusal, which no unseen cell can hide."
  [^js snapshot ^js unknown pinned w]
  (let [view (js/Object.create snapshot)
        id (.-id unknown)
        pinned-by-section (delay (group-by (fn [[x y z]] [(bit-shift-right x 4) (bit-shift-right y 4) (bit-shift-right z 4)]) pinned))
        writes (or (.-memoryEpoch unknown) (constantly 0))
        sections (js/Map.)
        last #js {:cx nil :sy nil :cz nil :ids nil :writes -1}
        section-of (fn [cx sy cz]
                     (let [key (+ (* (+ (* (bit-and cx 0xFFFF) 65536) (bit-and cz 0xFFFF)) 64) (+ sy 16))]
                       (or (.get sections key)
                           (let [ids (js/Uint16Array. 4096)
                                 mem (.known unknown cx sy cz)
                                 x0 (bit-shift-left cx 4) y0 (bit-shift-left sy 4) z0 (bit-shift-left cz 4)]
                             (dotimes [i 4096]
                               (let [x (+ x0 (bit-and i 15)) y (+ y0 (bit-shift-right i 8)) z (+ z0 (bit-and (bit-shift-right i 4) 15))
                                     s (.stateAt snapshot x y z)
                                     m (if mem (dec (aget mem i)) -1)]
                                 (aset ids i (cond (== s 0xFFFF) s
                                                   (neg? m) id
                                                   (or (== m s) (== m (raw/state-at-pos w [x y z]))) s
                                                   :else m))))
                             (doseq [[x y z] (get @pinned-by-section [cx sy cz])]
                               (aset ids (bit-or (bit-shift-left (bit-and y 15) 8) (bit-shift-left (bit-and z 15) 4) (bit-and x 15))
                                     (.stateAt snapshot x y z)))
                             (.set sections key ids)
                             ids))))]
    (set! (.-stateAt view) (fn [x y z]
                             (let [cx (bit-shift-right x 4) sy (bit-shift-right y 4) cz (bit-shift-right z 4)
                                   w (writes)]
                               (when-not (= w (.-writes last))
                                 (.clear sections)
                                 (set! (.-writes last) w)
                                 (set! (.-cx last) nil))
                               (when-not (and (== cx (.-cx last)) (== sy (.-sy last)) (== cz (.-cz last)))
                                 (set! (.-ids last) (section-of cx sy cz))
                                 (set! (.-cx last) cx) (set! (.-sy last) sy) (set! (.-cz last) cz))
                               (aget (.-ids last) (bit-or (bit-shift-left (bit-and y 15) 8) (bit-shift-left (bit-and z 15) 4) (bit-and x 15))))))
    (set! (.-unknown view) unknown)
    view))

(def path-world-cache
  "The last path-world: [inputs result]. A world is an immutable map, so the snapshot is rebuilt only when a map that
  path-world reads is no longer the identical one (any block, state or marker write replaces it), or the body's position
  changed what it reads: its chunk under a view window, its position with buried cells (which skip the body's own)."
  (atom nil))

(defn path-world-inputs
  "[maps where]: the maps path-world reads, and what of the body's position it reads."
  [w]
  (let [[bx _ bz :as pos] (get-in w [:self :pos])]
    [[(:blocks w) (:states w) (:unreachable w) (:no-path w) (:view-chunks w)]
     [(when (:view-chunks w) [(bit-shift-right (floor bx) 4) (bit-shift-right (floor bz) 4)])
      (when (or (seq (:unreachable w)) (seq (:no-path w))) pos)]]))

(def patch-limit "Most changed cells path-world patches into the last snapshot instead of rebuilding it." 200)

(defn block-changes
  "[[pos name-or-nil] ...] where new-blocks differs from old-blocks (nil: the cell is gone), nil when more than patch-limit."
  [old-blocks new-blocks]
  (let [changes (volatile! (transient []))
        over? #(> (count @changes) patch-limit)
        note! (fn [pos name] (vswap! changes conj! [pos name]) (when (over?) (reduced :over)))
        r1 (reduce-kv (fn [_ pos name] (if (= name (get old-blocks pos)) nil (note! pos name))) nil new-blocks)
        r2 (when-not (= r1 :over)
             (reduce-kv (fn [_ pos _] (if (contains? new-blocks pos) nil (note! pos nil))) nil old-blocks))]
    (when-not (or (= r1 :over) (= r2 :over))
      (persistent! @changes))))

(defn column-of [[x _ z]] [(bit-shift-right x 4) (bit-shift-right z 4)])

(defn patched
  "The path-world of w made from last (the result for old-blocks) by changing the cells that differ, on a fork of its
  snapshot, or nil when a rebuild is needed: too many changes, a block in a column the snapshot has not got, or the
  last block of a column removed (a rebuild leaves that column unloaded)."
  [last old-blocks w]
  (let [blocks (:blocks w)
        changes (block-changes old-blocks blocks)
        ^js old-snapshot (:snapshot last)
        loaded? (fn [[pos]] (let [[cx cz] (column-of pos)] (.hasColumn old-snapshot cx cz)))
        emptied? (fn [removed]
                   (let [cols (into #{} (map (comp column-of first)) removed)]
                     (and (seq cols)
                          (not-every? (fn [col] (reduce-kv (fn [_ pos _] (when (= col (column-of pos)) (reduced true))) nil blocks))
                                      cols))))]
    (when (and changes (every? loaded? changes) (not (emptied? (remove second changes))))
      (let [^js snapshot (.fork old-snapshot)]
        (doseq [[[x y z :as pos] name] changes]
          (.setState snapshot x y z (if name (fx/state-id name (doors/path-props w pos)) 0)))
        (doseq [[[x y z]] changes] (fx/connect! snapshot [x y z x y z]))
        (assoc last :snapshot snapshot)))))

(defn path-world
  "path-world-build, reused while its inputs are the same (callers do not mutate the snapshot). When only :blocks differ
  and few cells, with no buried cells and no view window, the last snapshot is forked and patched (patched)."
  [w]
  (let [[maps where :as inputs] (path-world-inputs w)
        [[last-maps last-where] result] @path-world-cache
        [blocks & others] maps
        [last-blocks & last-others] last-maps]
    (if (and last-maps (every? true? (map identical? maps last-maps)) (= where last-where))
      result
      (let [result (or (when (and last-maps (every? true? (map identical? others last-others)) (= where last-where)
                                  (nil? (:view-chunks w)) (empty? (:unreachable w)) (empty? (:no-path w)))
                         (patched result last-blocks w))
                       (path-world-build w))]
        (reset! path-world-cache [inputs result])
        result))))

(defn local-blocks
  "[x y z name props] for the cells within 2 of (cx, cz), rows cy-1 .. cy+2, air where the world holds nothing (so every
  column is loaded for the planner's snapshot)."
  [w cx cy cz]
  (for [x (range (- cx 2) (+ cx 3)) y (range (dec cy) (+ cy 3)) z (range (- cz 2) (+ cz 3))]
    [x y z (name-at w x y z) (doors/path-props w [x y z])]))

(def boxes-cache
  "[blocks states {[cx cy cz y] boxes}]: the collision boxes near a cell, kept while the world's :blocks and :states are
  the identical maps (any block or state write replaces one)."
  (atom nil))

(defn boxes-at
  [w cx cy cz y]
  (let [[blocks states cache] @boxes-cache
        fresh? (and (identical? blocks (:blocks w)) (identical? states (:states w)))
        k [cx cy cz y]]
    (or (when fresh? (get cache k))
        (let [snapshot (fx/fixture-snapshot {:blocks (local-blocks w cx cy cz)})
              boxes (space/boxes-near snapshot (blocks/default-state-table) cx cy cz y (+ y height))]
          (reset! boxes-cache [(:blocks w) (:states w) (assoc (if fresh? cache {}) k boxes)])
          boxes))))

(defn body-clear?
  "Does the body's box (the planner's half-width, feet at y, 1.8 tall) centred at (px, pz) touch no collision box of the
  blocks around it? Fences are joined to their neighbours as the server joins them."
  [w px y pz]
  (not (space/body-hits? (boxes-at w (floor px) (floor y) (floor pz) y) px pz)))

;; --- the tick loop

(defn sub-of
  "The :sub-pos to keep for body (sub-pos): its true position when its cell's centre is not clear for it, else nil."
  [w {:keys [x y z]}]
  (let [[cx cy cz] [(floor x) (floor y) (floor z)]]
    (when (and (narrow-cell? w cx cy cz) (not (body-clear? w (+ cx 0.5) y (+ cz 0.5))))
      [x cy z])))

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
                              (when (number? (:yaw out))
                                (reset! yaw (:yaw out))
                                ;; the head turns as live steer.mjs's bot.look(yaw, pitch): perception reads it
                                (swap! state assoc :yaw (mod (- 180 (* deg (:yaw out))) 360) :pitch (- (* deg (or (:pitch out) 0)))))
                              (reset! body (step-body @state @body controls @yaw))
                              (swap! state assoc-in [:self :pos]
                                     (if (:body-hitbox @state)
                                       [(:x @body) (floor (:y @body)) (:z @body)]
                                       [(floor (:x @body)) (floor (:y @body)) (floor (:z @body))]))
                              (when-not (:body-hitbox @state)
                                (swap! state assoc :sub-pos (sub-of @state @body)))
                              (when (seq (:wires @state))
                                (swap! state doors/tick-wires (get-in @state [:self :pos])))
                              (js/setImmediate tick))))))]
         (js/setImmediate tick))))))
