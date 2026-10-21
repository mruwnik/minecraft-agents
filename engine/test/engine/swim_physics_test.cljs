(ns engine.swim-physics-test
  "engine.path.executor walking planned routes through water in prismarine-physics, the body's own client physics:
  plans come from the tuned planner over the fake's pathWorld, the body moves by physics over the same blocks (collision
  half-width as every bound bot's). Physics has no breath, no bubble columns and no server; those are live tests."
  (:require [cljs.test :refer [deftest is are]]
            [engine.path.executor :as ex]
            [engine.path.planner-tuned :as planner]
            [engine.test-util :as tu]
            [jobs.lib.walk.world :as wworld]
            [jobs.lib.walk.plan :as wplan]))

(def version "1.21.4")
(def lib (delay {:mc ((tu/require-here "minecraft-data") version)
                 :block ((tu/require-here "prismarine-block") version)
                 :physics (tu/require-here "prismarine-physics")
                 :vec3 (.-Vec3 (tu/require-here "vec3"))}))

(def max-ticks 1200)

(defn box
  "value in every cell of x0..x1, y0..y1, z0..z1, keyed \"x,y,z\"."
  [x0 y0 z0 x1 y1 z1 value]
  (into {} (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))] [(str x "," y "," z) value])))

(defn name-of [v] (if (vector? v) (first v) v))

(defn world-of
  "A prismarine world over blocks (name, or [name metadata] for flowing water); air elsewhere."
  [blocks]
  (let [{:keys [mc block]} @lib
        by-name (.-blocksByName mc)]
    #js {:getBlock (fn [pos]
                     ;; physics reuses one cursor for every cell it reads; the block keeps a copy (its current is read later)
                     (let [cell (.floored pos)
                           v (get blocks (str (.-x cell) "," (.-y cell) "," (.-z cell)) "air")
                           b (new block (.-id (aget by-name (name-of v))) 0 (if (vector? v) (second v) 0))]
                       (set! (.-position b) cell)
                       b))}))

(defn plan
  "The executor's steps from feet cell from to goal over blocks, or nil."
  [blocks [x y z] [gx gy gz]]
  (let [pw (.pathWorld (tu/fake {:blocks (update-vals blocks name-of)}))
        r (planner/plan (.-snapshot pw)
                        #js {:from #js {:x x :y y :z z :px (+ x 0.5) :pz (+ z 0.5)}
                             :goal #js {:kind "near" :x gx :y gy :z gz :range 0}}
                        #js {:table (.-table pw) :space (.-space pw) :weight 1.2
                             :limits (ex/planner-limits ex/policy (wworld/solid-fn pw))})]
    (when (= "found" (.-status r))
      (wplan/plan-steps pw r))))

(defn bot-at
  "A bot at feet cell [x y z], at its middle or at the point px pz inside it."
  [[x y z] [px pz]]
  (let [{:keys [vec3]} @lib]
    #js {:entity #js {:position (new vec3 (or px (+ x 0.5)) y (or pz (+ z 0.5))) :velocity (new vec3 0 0 0) :onGround true :isInWater false
                      :isInLava false :isInWeb false :isCollidedHorizontally false :isCollidedVertically false
                      :elytraFlying false :yaw 0 :pitch 0 :effects #js {} :attributes #js {}}
         :jumpTicks 0 :jumpQueued false :fireworkRocketDuration 0 :version version :inventory #js {:slots #js []}}))

(defn pose-of [bot]
  (let [e (.-entity bot) p (.-position e)]
    {:x (.-x p) :y (.-y p) :z (.-z p) :vy (.-y (.-velocity e)) :on-ground (.-onGround e) :on-climbable false
     :in-water (.-isInWater e) :collided (.-isCollidedHorizontally e)}))

(defn eye-wet? [blocks {:keys [x y z]}]
  (= "water" (name-of (get blocks (str (Math/floor x) "," (Math/floor (+ y 1.62)) "," (Math/floor z))))))

(defn in-block?
  "The body's box as the server has it (0.3 each way, 1.8 high) reaches into a block that is neither air nor water: a
  position the server refuses."
  [blocks {:keys [x y z]}]
  (boolean (some (fn [[cx cy cz]] (not (#{"air" "water"} (name-of (get blocks (str cx "," cy "," cz) "air")))))
                 (for [cx (range (Math/floor (- x 0.3)) (Math/ceil (+ x 0.3)))
                       cy (range (Math/floor y) (Math/ceil (+ y 1.8)))
                       cz (range (Math/floor (- z 0.3)) (Math/ceil (+ z 0.3)))]
                   [cx cy cz]))))

(defn swim
  "Walk the plan from from to goal in physics. {:done :moves :ticks :wet-run :in-block}: the executor's done map (nil when
  it ran out of ticks), the step kinds planned, ticks used, the longest run of ticks with the eye in water, and the ticks
  the body was inside a block (in-block?). :at is the point [px pz] in from the body starts at (default: its middle),
  :half the collision half-width (default: every bound bot's)."
  [blocks from goal & {:keys [at half]}]
  (let [{:keys [mc physics]} @lib
        world (world-of blocks)
        phys ((.-Physics physics) mc world)
        _ (set! (.-playerHalfWidth phys) (or half (.-BODY_HALF_WIDTH (tu/require-here "./js/prim-base.mjs"))))
        steps (plan blocks from goal)
        bot (bot-at from at)]
    (if (nil? steps)
      {:done {:status :no-plan}}
      (loop [t 0 state (ex/start steps 0) run 0 wet-run 0 in-block 0]
        (let [pose (pose-of bot)
              pose (assoc pose :under-water (eye-wet? blocks pose))
              {:keys [done controls yaw pitch] :as r} (ex/tick ex/policy state pose)
              run' (if (eye-wet? blocks pose) (inc run) 0)
              wet-run' (max wet-run run')
              in-block' (cond-> in-block (in-block? blocks pose) inc)]
          (if (or done (>= t max-ticks))
            {:done done :moves (set (map :move steps)) :ticks t :wet-run wet-run' :in-block in-block'}
            (do (set! (.-yaw (.-entity bot)) yaw)
                (set! (.-pitch (.-entity bot)) pitch)
                (.apply (.simulatePlayer phys (new (.-PlayerState physics) bot (clj->js controls)) world) bot)
                (recur (inc t) (:state r) run' wet-run' in-block'))))))))

;; Courses along +x, z -2..4 (walls of water edge to edge, so there is no way round), floor at y 59.

(defn pond
  "Water x 5..(4 + width) from y 60 up to top over a stone floor at 59, banks of stone up to near-top (x 0..4) and far-top
  (10 cells past the water)."
  [top near-top far-top & {:keys [water width] :or {water "water" width 6}}]
  (let [far (+ 5 width)]
    (merge (box 0 59 -2 (+ far 9) 59 4 "stone")
           (box 0 60 -2 4 near-top 4 "stone")
           (box far 60 -2 (+ far 9) far-top 4 "stone")
           (into {} (for [x (range 5 far) y (range 60 (inc top)) z (range -2 5)]
                      [(str x "," y "," z) (if (fn? water) (water z) water)])))))

(def deep (pond 62 62 62))
(def wade (pond 60 60 60))
(def high-drop (pond 62 65 62))
;; a current along +z (flowing water, level 1 at z -2 rising to 7 at z 4: the flow runs to the higher level); physics
;; pushes as hard in any current
(def current (fn [z] ["water" (+ z 3)]))
(def river (pond 61 61 61 :water current))
(def wide-river (pond 61 61 61 :water current :width 20))

(defn crossing
  "What a crossing shows: its end, whether the plan has every kind in kinds, and the longest run of ticks with the eye in
  the water."
  [blocks from goal kinds]
  (let [{:keys [done moves wet-run]} (swim blocks from goal)]
    {:status (:status done) :kinds (every? moves kinds) :wet-run wet-run}))

(deftest crossings-from-a-flush-bank-keep-the-head-out-of-the-water
  (are [blocks from goal kinds] (= {:status :arrived :kinds true :wet-run 0} (crossing blocks from goal kinds))
    deep [1 63 1] [15 63 1] #{:drop :swim :exit}
    wade [1 61 1] [15 61 1] #{:drop :swim}
    river [1 62 1] [15 62 1] #{:drop :swim :exit}
    wide-river [1 62 1] [29 62 1] #{:drop :swim :exit}))

(deftest a-drop-into-the-water-goes-under-only-for-the-splash
  (let [{:keys [status kinds wet-run]} (crossing high-drop [1 66 1] [15 63 1] #{:drop :swim :exit})]
    (is (= [:arrived true] [status kinds]))
    (is (< 0 wet-run 20) "under for the splash, up within a second")))

(deftest crossings-back-arrive
  (are [blocks from goal]
       (= :arrived (:status (:done (swim blocks from goal))))
    deep [15 63 1] [1 63 1]
    wade [15 61 1] [1 61 1]
    river [15 62 1] [1 62 1]))

(deftest a-body-on-the-bottom-swims-up-and-out
  (let [{:keys [done moves]} (swim deep [7 60 1] [15 63 1])]
    (is (= :arrived (:status done)))
    (is (contains? moves :swim-up))))

(deftest a-goal-in-the-water-is-reached-floating
  (let [{:keys [done]} (swim deep [1 63 1] [8 62 1])]
    (is (= :arrived (:status done)))))

(deftest a-goal-on-the-bottom-is-reached-by-diving
  (let [{:keys [done moves]} (swim deep [1 63 1] [8 60 1])]
    (is (= :arrived (:status done)))
    (is (contains? moves :swim-down))))

(defn shifted
  "blocks moved dx along x."
  [blocks dx]
  (into {} (map (fn [[k v]] (let [[x y z] (map js/parseInt (.split k ","))] [(str (+ x dx) "," y "," z) v]))) blocks))

(deftest a-body-leaving-shallow-water-never-enters-the-bank
  ;; the water's edge is the bank's face at x = edge. A width that is no binary fraction (0.31) puts the pressed box one
  ;; rounding step past a face on a power of two: the clip's tolerance holds it there
  (are [edge at half] (= {:status :arrived :in-block 0}
                         (let [{:keys [done in-block]} (swim (shifted wade (- edge 5)) [edge 60 1] [(- edge 4) 61 1] :at at :half half)]
                           {:status (:status done) :in-block in-block}))
    5 nil nil
    16 nil nil
    16 [16.5 1.67] nil
    4 nil nil
    512 nil nil
    16 nil 0.31
    16 [16.5 1.67] 0.31
    4 nil 0.31
    512 nil 0.31))

(def tunnel
  "A water tunnel two high and one wide (x 5..16, y 60..61, z 1) in stone."
  (merge (box 3 59 -1 18 62 3 "stone") (box 5 60 1 16 61 1 "water")))

(deftest a-submerged-tunnel-is-swum-not-paddled
  ;; a vanilla swim (sprint, eyes under) settles at 3.92 b/s, a paddle at 1.96; a quarter over the swim for the start
  (let [{:keys [done ticks]} (swim tunnel [5 60 1] [16 60 1])]
    (is (= :arrived (:status done)))
    (is (<= ticks (* 1.25 20 (/ 11 3.92))) (str ticks " ticks for 11 cells"))))

(defn ticks-until-done
  "Ticks of the pose sequence (a fn of the tick) until the executor ends the walk on a swim-up from y 58 to a step at y 62, nil when it does not in n."
  [pose-at n]
  (let [steps [{:move :swim :x 0 :y 58 :z 0 :px 0.5 :pz 0.5 :h 0 :swim true}
               {:move :swim-up :x 0 :y 62 :z 0 :px 0.5 :pz 0.5 :h 0 :swim true}]]
    (loop [state (ex/start steps 0) k 0]
      (when (< k n)
        (let [{s :state :keys [done]} (ex/tick ex/policy state (pose-at k))]
          (if done {:ticks (inc k) :status (:status done)} (recur s (inc k))))))))

(deftest a-swim-up-that-rises-is-progress-one-that-does-not-is-stuck
  (let [at (fn [y] {:x 0.5 :z 0.5 :y y :vy 0 :in-water true :under-water true :on-ground false})]
    (is (= {:ticks 61 :status :stuck} (ticks-until-done (constantly (at 58)) 200)) "held in place: 3 s")
    (is (nil? (ticks-until-done #(at (+ 58 (* 0.05 %))) 70)) "rising 1 block a second is not stuck")))
