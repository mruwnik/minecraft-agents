(ns engine.stairs-physics-test
  "engine.path.executor walking planned routes over stairs and slabs in prismarine-physics, the body's own client
  physics: plans come from the tuned planner over a fixture snapshot (block states with their properties), the body moves
  by physics over the same blocks (collision half-width 0.31, as every bound bot)."
  (:require [cljs.test :refer [deftest is are]]
            [engine.path.executor :as ex]
            [engine.path.fixture :as fx]
            [engine.path.planner-tuned :as planner]
            [engine.test-util :as tu]
            [engine.path.walk :as walk]))

(def version "26.1")
(def lib (delay {:mc ((tu/require-here "minecraft-data") version)
                 :block ((tu/require-here "prismarine-block") version)
                 :physics (tu/require-here "prismarine-physics")
                 :vec3 (.-Vec3 (tu/require-here "vec3"))
                 :blocks (tu/require-here "./js/path/blocks.mjs")
                 :space (tu/require-here "./js/path/space.mjs")}))

(def max-ticks 1200)

;; A world is a vector of fills [x0 y0 z0 x1 y1 z1 name props]; a later fill overrides an earlier one, air elsewhere.

(defn cell? [[x0 y0 z0 x1 y1 z1]] (and (= x0 x1) (= y0 y1) (= z0 z1)))

(defn fill-index
  "A lookup fn [x y z] -> fill over fills: single-cell fills by key, boxes by scan; cells win over boxes."
  [fills]
  (let [cells (into {} (map (fn [[x y z :as f]] [[x y z] f])) (filter cell? fills))
        boxes (remove cell? fills)]
    (fn [x y z]
      (or (get cells [x y z])
          (last (filter (fn [[x0 y0 z0 x1 y1 z1]] (and (<= x0 x x1) (<= y0 y y1) (<= z0 z z1))) boxes))))))

(defn world-of
  "A prismarine world over fills, with full block state properties."
  [fills]
  (let [{:keys [mc block]} @lib
        by-name (.-blocksByName mc)
        fill-at (fill-index fills)
        state-of (memoize
                  (fn [name props]
                    (let [b (aget by-name name)
                          defaults (.getProperties (.fromStateId block (.-defaultState b) 0))]
                      (.-stateId (.fromProperties block name (js/Object.assign #js {} defaults (clj->js props)) 0)))))]
    #js {:getBlock (fn [pos]
                     ;; physics reuses one cursor for every cell it reads; the block keeps a copy
                     (let [cell (.floored pos)
                           [_ _ _ _ _ _ name props] (fill-at (.-x cell) (.-y cell) (.-z cell))
                           b (.fromStateId block (state-of (or name "air") props) 0)]
                       (set! (.-position b) cell)
                       b))}))

(defn path-world
  "The planner's view of fills: {:snapshot :table :space} as walk-plan reads a pathWorld."
  [fills]
  (let [{:keys [blocks space]} @lib]
    #js {:snapshot (fx/fixture-snapshot {:fill fills})
         :table (.defaultStateTable blocks)
         :space space}))

(defn plan
  "The executor's steps from feet cell from to goal over fills, or nil."
  [fills [x y z] [gx gy gz]]
  (let [pw (path-world fills)
        r (planner/plan (.-snapshot pw)
                        #js {:from #js {:x x :y (Math/floor y) :z z :px (+ x 0.5) :pz (+ z 0.5)}
                             :goal #js {:kind "near" :x gx :y gy :z gz :range 0}}
                        #js {:table (.-table pw) :space (.-space pw) :weight 1.2
                             :limits (ex/planner-limits ex/policy (walk/solid-fn pw))})]
    (when (= "found" (.-status r))
      (walk/plan-steps pw r))))

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

(defn walk
  "Walk the plan from from to goal in physics. {:done :steps :ticks :pose :i}: the executor's done map (nil when it ran out
  of ticks), the planned steps (nil: no plan), ticks used, the last pose and the step index."
  [fills from goal]
  (let [{:keys [mc physics]} @lib
        world (world-of fills)
        phys ((.-Physics physics) mc world)
        _ (set! (.-playerHalfWidth phys) 0.31)
        steps (plan fills from goal)
        bot (bot-at from)]
    (if (nil? steps)
      {:done {:status :no-plan}}
      (loop [t 0 state (ex/start steps 0)]
        (let [pose (pose-of bot)
              {:keys [done controls yaw] :as r} (ex/tick ex/policy state pose)]
          (if (or done (>= t max-ticks))
            {:done done :steps steps :ticks t :pose pose :i (:i (:state r))}
            (do (set! (.-yaw (.-entity bot)) yaw)
                (.apply (.simulatePlayer phys (new (.-PlayerState physics) bot (clj->js controls)) world) bot)
                (recur (inc t) (:state r)))))))))

;; Courses along +x on a stone floor (top at y 64), z -2..6, x 0..24.

(def floor-fill [0 63 -2 24 63 6 "stone" {}])

(defn status-of [fills from goal] (:status (:done (walk fills from goal))))

(defn stairs
  "A stairs block (or a row of them over z zs, a fill) at x y."
  ([x y z facing] (stairs x y z facing "bottom"))
  ([x y z facing half] (stairs x y z z facing half))
  ([x y z1 z2 facing half] [x y z1 x y z2 "oak_stairs" {:facing facing :half half :shape "straight"}]))

(defn flight
  "n stairs going up in +x from x0, the first at y 64, over stone, spanning z -2..6, and a landing of stone after them."
  [x0 n facing]
  (vec (concat (mapcat (fn [k] [[(+ x0 k) 64 -2 (+ x0 k) (+ 63 k) 6 "stone" {}]
                                (stairs (+ x0 k) (+ 64 k) -2 6 facing "bottom")])
                       (range n))
               [[(+ x0 n) 64 -2 (+ x0 n 4) (+ 63 n) 6 "stone" {}]])))

(def staircase (into [floor-fill] (flight 8 4 "east")))

(def corridor [[0 64 0 24 66 0 "stone" {}] [0 64 2 24 66 2 "stone" {}]])

(defn roof
  "A pitched roof along x: stairs up from x 8 (y 64) facing east to the ridge pair at x 11 and 12 (y 67), down to x 15."
  []
  (vec (cons floor-fill
             (mapcat (fn [x y f] [[x 64 -2 x (dec y) 6 "stone" {}] (stairs x y -2 6 f "bottom")])
                     [8 9 10 11 12 13 14 15] [64 65 66 67 67 66 65 64]
                     ["east" "east" "east" "east" "west" "west" "west" "west"]))))

(deftest a-goal-on-a-stairs-block-is-walked-to-from-any-side
  (are [from goal] (= :arrived (status-of [floor-fill (stairs 8 64 1 "east")] from goal))
    [1 64 1] [8 65 1]
    [15 64 1] [8 65 1]
    [8 64 4] [8 65 1]))

(deftest a-goal-on-a-slab-is-walked-to
  (is (= :arrived (status-of [floor-fill [8 64 1 8 64 1 "oak_slab" {:type "bottom"}]] [1 64 1] [8 64 1]))))

(deftest a-goal-in-the-low-half-of-a-stairs-block-has-no-plan
  (is (= :no-plan (status-of [floor-fill (stairs 8 64 1 "east")] [1 64 1] [8 64 1]))))

(deftest a-staircase-of-four-is-climbed-and-descended
  (are [from goal] (= :arrived (status-of staircase from goal))
    [2 64 1] [13 68 1]
    [13 68 1] [2 64 1]))

(deftest a-staircase-facing-the-other-way-is-climbed
  (is (= :arrived (status-of (into [floor-fill] (flight 8 4 "west")) [2 64 1] [13 68 1]))))

(deftest a-stairs-block-or-slab-on-the-way-is-crossed
  (are [block] (= :arrived (status-of (into [floor-fill] (conj corridor block)) [1 64 1] [15 64 1]))
    (stairs 8 64 1 "east")
    (stairs 8 64 1 "west")
    (stairs 8 64 1 "north")
    (stairs 8 64 1 "east" "top")
    [8 64 1 8 64 1 "oak_slab" {:type "bottom"}]
    [8 64 1 8 64 1 "oak_slab" {:type "top"}]))

(deftest a-pitched-stairs-roof-is-walked-from-both-sides
  (are [from goal] (= :arrived (status-of (roof) from goal))
    [2 64 1] [10 67 1]
    [2 64 1] [11 68 1]
    [2 64 1] [12 68 1]
    [2 64 1] [14 66 1]
    [20 64 1] [10 67 1]
    [20 64 1] [11 68 1]
    [20 64 1] [9 66 1]))

;; A diagonal jump up one block past a corner block: the body presses on the corner and slides out of its column while it
;; is in the air. It can when the corner block is no higher than the landing's floor; past a higher one the slide is not
;; done by the time it falls back (live: onto an oak leaf block beside another leaf block).

(def corner-floor [-2 60 -2 40 63 40 "stone" {}])
(def corner-landing [3 64 3 3 64 3 "stone" {}])
(def corner-hole [2 62 3 2 63 3 "air" {}])
;; walls on the landing's other two sides: the diagonal past the corner is the only way up
(def corner-walls [[4 64 3 4 66 3 "stone" {}] [3 64 4 3 66 4 "stone" {}]])

(defn corner-wall [height] [3 64 2 3 (+ 63 height) 2 "stone" {}])

(deftest a-corner-jump-past-a-low-corner-block-arrives
  (is (= :arrived (status-of (into [corner-floor (corner-wall 1) corner-hole corner-landing] corner-walls) [2 64 2] [3 65 3]))))

(deftest a-corner-jump-past-a-high-corner-block-is-not-planned
  (are [height] (= :no-plan (status-of (into [corner-floor (corner-wall height) corner-hole corner-landing] corner-walls) [2 64 2] [3 65 3]))
    2
    3))

;; A step up of 1/16 (a path block, 15/16, to a grass block) into a place with a ceiling 0.26 above the head: prismarine-physics
;; lifts the body 0.6 where it stands, then moves it, and refuses when the lifted body meets the ceiling over the step
;; (the ceiling is not over the body yet, so the lift is not shortened). Only a jump gets over the edge.

(def low-ceiling-path
  [[0 63 -2 12 63 6 "stone" {}]
   [0 64 -2 5 64 6 "dirt_path" {}]
   [6 64 -2 12 64 6 "grass_block" {:snowy false}]
   [6 67 -2 9 67 6 "stone" {}]])

(deftest a-small-step-up-under-a-low-ceiling-is-walked
  (is (= :arrived (status-of low-ceiling-path [1 64 1] [10 65 1]))))
