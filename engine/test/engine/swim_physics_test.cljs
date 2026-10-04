(ns engine.swim-physics-test
  "engine.path.executor walking planned routes through water in prismarine-physics, the body's own client physics:
  plans come from the tuned planner over the fake's pathWorld, the body moves by physics over the same blocks (collision
  half-width 0.31, as every bound bot). Physics has no breath, no bubble columns and no server; those are live tests."
  (:require [cljs.test :refer [deftest is are]]
            [engine.path.executor :as ex]
            [engine.path.planner-tuned :as planner]
            [engine.test-util :as tu]
            [jobs.debug.walk-plan :as walk-plan]))

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
                             :limits (ex/planner-limits ex/policy (walk-plan/solid-fn pw))})]
    (when (= "found" (.-status r))
      (walk-plan/plan-steps pw r))))

(defn bot-at [[x y z]]
  (let [{:keys [vec3]} @lib]
    #js {:entity #js {:position (new vec3 (+ x 0.5) y (+ z 0.5)) :velocity (new vec3 0 0 0) :onGround true :isInWater false
                      :isInLava false :isInWeb false :isCollidedHorizontally false :isCollidedVertically false
                      :elytraFlying false :yaw 0 :pitch 0 :effects #js {} :attributes #js {}}
         :jumpTicks 0 :jumpQueued false :fireworkRocketDuration 0 :version version :inventory #js {:slots #js []}}))

(defn pose-of [bot]
  (let [e (.-entity bot) p (.-position e)]
    {:x (.-x p) :y (.-y p) :z (.-z p) :vy (.-y (.-velocity e)) :on-ground (.-onGround e) :on-climbable false
     :in-water (.-isInWater e) :collided (.-isCollidedHorizontally e)}))

(defn eye-wet? [blocks {:keys [x y z]}]
  (= "water" (name-of (get blocks (str (Math/floor x) "," (Math/floor (+ y 1.62)) "," (Math/floor z))))))

(defn swim
  "Walk the plan from from to goal in physics. {:done :moves :ticks :wet-run}: the executor's done map (nil when it ran out
  of ticks), the step kinds planned, ticks used, and the longest run of ticks with the eye in water."
  [blocks from goal]
  (let [{:keys [mc physics]} @lib
        world (world-of blocks)
        phys ((.-Physics physics) mc world)
        _ (set! (.-playerHalfWidth phys) 0.31)
        steps (plan blocks from goal)
        bot (bot-at from)]
    (if (nil? steps)
      {:done {:status :no-plan}}
      (loop [t 0 state (ex/start steps 0) run 0 wet-run 0]
        (let [pose (pose-of bot)
              {:keys [done controls yaw] :as r} (ex/tick ex/policy state pose)
              run' (if (eye-wet? blocks pose) (inc run) 0)
              wet-run' (max wet-run run')]
          (if (or done (>= t max-ticks))
            {:done done :moves (set (map :move steps)) :ticks t :wet-run wet-run'}
            (do (set! (.-yaw (.-entity bot)) yaw)
                (.apply (.simulatePlayer phys (new (.-PlayerState physics) bot (clj->js controls)) world) bot)
                (recur (inc t) (:state r) run' wet-run'))))))))

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
