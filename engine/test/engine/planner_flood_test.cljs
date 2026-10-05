(ns engine.planner-flood-test
  "engine.path.planner-tuned's goal flood (the early pass, options.preFlood, and the late one): a goal it calls :goal-enclosed
  must really be walled in. A shut door, gate or trapdoor the search can open is a way in, and so is a cell the flood cannot
  see (unloaded, or out of the search's span)."
  (:require [cljs.test :refer [deftest is are]]
            [engine.path.planner-tuned :as planner]
            [engine.test-util :as tu :refer [box floor]]))

(defn plan-over
  "Plan from (0 64 0) to goal [x y z] over a fake world spec; {:status :reason}."
  ([spec goal] (plan-over spec goal {}))
  ([spec [x y z] options]
   (let [pw (.pathWorld (tu/fake spec))
         r (planner/plan (.-snapshot pw)
                         #js {:from #js {:x 0 :y 64 :z 0} :goal #js {:kind "near" :x x :y y :z z :range 0}}
                         (js/Object.assign #js {:table (.-table pw) :space (.-space pw)} (clj->js options)))]
     {:status (.-status r) :reason (.-reason r)})))

(def flat (floor -2 -3 40 3))

(defn hollow
  "A stone box x0..x1, y0..y1, z0..z1 with its inside left empty."
  [x0 y0 z0 x1 y1 z1]
  (apply dissoc (box x0 y0 z0 x1 y1 z1 "stone") (keys (box (inc x0) (inc y0) (inc z0) (dec x1) (dec y1) (dec z1) "stone"))))

;; a stone room x 5..9, z -2..2, feet y 64..65 inside, a roof at y 67
(def room (hollow 5 63 -2 9 67 2))

(def hatch
  {:blocks (merge flat (assoc (box 2 66 -1 8 66 1 "stone") "3,66,0" "oak_trapdoor")
                  {"3,64,0" "ladder" "3,65,0" "ladder"} (box 3 64 -1 3 65 -1 "stone"))
   :states {"3,64,0" {:facing "south"} "3,65,0" {:facing "south"} "3,66,0" {:open false :half "bottom" :facing "south"}}})

;; the trapdoor faces away from the ladder's facing: the body climbs to the ladder's top edge and jumps off it
(def hatch-facing-away (assoc-in hatch [:states "3,66,0" :facing] "north"))

(def pen
  {:blocks (merge flat (apply dissoc (box 5 64 -2 9 64 2 "oak_fence") (keys (box 6 64 -1 8 64 1 "x"))) {"5,64,0" "oak_fence_gate"})
   :states {"5,64,0" {:open false :facing "east"}}})

(def door-room
  {:blocks (merge flat room {"5,64,0" "oak_door" "5,65,0" "oak_door"})
   :states {"5,64,0" {:open false :half "lower" :facing "east"} "5,65,0" {:open false :half "upper" :facing "east"}}})

(def ladder-deck
  {:blocks (merge flat (dissoc (box 2 66 -1 8 66 1 "stone") "3,66,0")
                  {"3,64,0" "ladder" "3,65,0" "ladder" "3,66,0" "ladder"} (box 3 64 -1 3 66 -1 "stone"))
   :states {"3,64,0" {:facing "south"} "3,65,0" {:facing "south"} "3,66,0" {:facing "south"}}})

(deftest a-goal-behind-something-the-search-opens-is-found
  (are [spec goal] (= {:status "found" :reason nil} (plan-over spec goal))
    hatch [6 67 0]
    hatch-facing-away [6 67 0]
    pen [7 64 0]
    door-room [7 64 0]
    ladder-deck [6 67 0]))

(deftest a-goal-behind-something-the-search-opens-is-not-enclosed-early
  (are [spec goal] (not= "goal-enclosed" (:reason (plan-over spec goal {:maxNodes 1})))
    hatch [6 67 0]
    hatch-facing-away [6 67 0]
    pen [7 64 0]
    door-room [7 64 0]))

;; ---- every kind of cell the search passes, the flood passes: a false :goal-enclosed is a goal go-to never tries ----

(defn way-in
  "The room with its west wall's cells (5,64,0) and (5,65,0) cleared and `blocks` put in."
  ([blocks] (way-in blocks {}))
  ([blocks states] {:blocks (merge flat (dissoc room "5,64,0" "5,65,0") blocks) :states states}))

(defn up-to-deck
  "A deck at y 66 over the floor, its only way up the column (3, 64..66, 0) against a wall, holding `blocks`."
  [name]
  {:blocks (merge flat (dissoc (box 2 66 -1 8 66 1 "stone") "3,66,0") (box 3 64 0 3 66 0 name) (box 3 64 -1 3 66 -1 "stone"))})

(def iron-door-room
  {"5,64,0" "iron_door" "5,65,0" "iron_door"})
(def iron-door-states
  {"5,64,0" {:open false :half "lower" :facing "east"} "5,65,0" {:open false :half "upper" :facing "east"}})

(def passable
  [[(way-in {"5,64,0" "oak_fence_gate"} {"5,64,0" {:open true :facing "east"}}) [7 64 0]]
   [(way-in {"5,64,0" "oak_fence_gate"} {"5,64,0" {:open false :facing "east"}}) [7 64 0]]
   [(update (way-in {"5,64,0" "stone_slab"}) :blocks dissoc "5,66,0") [7 64 0]]
   [(way-in (merge iron-door-room {"4,64,0" "stone_pressure_plate"}) iron-door-states) [7 64 0]]
   [(way-in (merge iron-door-room {"4,65,1" "stone_button"})
            (merge iron-door-states {"4,65,1" {:face "wall" :facing "west" :powered false}})) [7 64 0]]
   [(up-to-deck "vine") [6 67 0]]
   [(up-to-deck "scaffolding") [6 67 0]]
   [(up-to-deck "twisting_vines_plant") [6 67 0]]
   [(up-to-deck "ladder") [6 67 0]]])

(deftest every-kind-of-cell-the-search-passes-is-found
  (doseq [[spec goal] passable]
    (is (= "found" (:status (plan-over spec goal {:goalFlood 0 :preFlood 0}))) (pr-str goal (keys (:states spec))))))

(deftest every-kind-of-cell-the-search-passes-is-not-enclosed
  (doseq [[spec goal] passable
          options [{:maxNodes 1} {:preFlood 0 :floodAfter 0}]]
    (is (not= "goal-enclosed" (:reason (plan-over spec goal options))) (pr-str goal options))))

;; water is a way the flood cannot follow (a drop into it starts higher than it looks): any water it meets is a leak
(deftest water-on-the-way-in-is-never-enclosed
  (are [spec goal options] (not= "goal-enclosed" (:reason (plan-over spec goal options)))
    (way-in {"5,64,0" "water"}) [7 64 0] {:maxNodes 1}
    (way-in {"5,64,0" "water"}) [7 64 0] {:preFlood 0 :floodAfter 0}
    (up-to-deck "bubble_column") [6 67 0] {:maxNodes 1}
    (up-to-deck "bubble_column") [6 67 0] {:preFlood 0 :floodAfter 0}))

;; the search never enters cobweb or powder snow (AVOID): behind them the goal is walled in, and the flood says so
(deftest a-way-in-the-search-refuses-is-enclosed
  (are [name options] (= ["goal-enclosed" "exhausted"]
                         [(:reason (plan-over (way-in {"5,64,0" name}) [7 64 0] options))
                          (:reason (plan-over (way-in {"5,64,0" name}) [7 64 0] {:goalFlood 0 :preFlood 0}))])
    "cobweb" {:maxNodes 1}
    "cobweb" {:preFlood 0 :floodAfter 0}
    "powder_snow" {:maxNodes 1}
    "powder_snow" {:preFlood 0 :floodAfter 0}))

;; a gap jump up is a way in: the goal is the top of a pillar (y 65) one empty cell past the floor's edge at x 4
(def pillar {:blocks (merge (floor -2 -3 4 3) (box 6 50 0 6 64 0 "stone"))})

(deftest a-goal-only-a-gap-jump-up-reaches-is-not-enclosed
  (is (= "found" (:status (plan-over pillar [6 65 0] {:goalFlood 0 :preFlood 0}))))
  (is (not= "goal-enclosed" (:reason (plan-over pillar [6 65 0] {:preFlood 0 :floodAfter 0})))))

;; with doors never opened the search walks every way it has: its verdict, not a found path
(deftest a-door-room-with-doors-never-opened-is-not-found
  (are [options] (not= "found" (:status (plan-over door-room [7 64 0] (assoc options :limits {:kinds planner/AVOID-OPEN}))))
    {}
    {:preFlood 0 :floodAfter 0}))

;; A 2-wide roofed corridor x x0..x0+4, z 0..1, shut at its west end and open at its east one, with no way in from the
;; start's floor: what lies east of it decides whether the goal is walled in.
(defn corridor [x0]
  (let [x1 (+ x0 4)]
    (merge (floor -2 -3 3 3)
           (floor x0 0 x1 1)
           (box x0 64 -1 x1 66 -1 "stone") (box x0 64 2 x1 66 2 "stone")
           (box (dec x0) 64 -1 (dec x0) 66 2 "stone")
           (box x0 66 0 x1 66 1 "stone"))))

;; the fake world loads only the 16-block columns its blocks touch: the corridor's open end at x 15 faces the unloaded
;; column x 16..31
(def into-unloaded {:blocks (corridor 11)})

;; the search's span ends 2048 blocks from the start: x 2048 and on are out of it, a floor carries on there
(def into-out-of-span
  {:blocks (merge (corridor 2043) (floor 2048 -3 2060 4))})

;; a one-cell pocket (10,64,1) on the edge of the loaded columns z 0..2, open only towards z -1 (unloaded) over the column
;; x 10 z 0, which holds `between`, with no floor under it
(defn pocket [between]
  {:blocks (merge (dissoc (floor -2 0 11 2) "10,63,0") (box 9 64 0 9 65 1 "stone") (box 11 64 0 11 65 1 "stone")
                  (box 10 64 2 10 65 2 "stone") {"10,66,1" "stone"} between)})

;; the early flood by default, the late one alone with preFlood 0 and floodAfter 0
(deftest a-goal-whose-only-way-out-is-unseen-is-not-enclosed
  (are [spec goal options] (not= "goal-enclosed" (:reason (plan-over spec goal options)))
    into-unloaded [12 64 0] {}
    into-out-of-span [2045 64 0] {}
    into-unloaded [12 64 0] {:preFlood 0 :floodAfter 0}
    into-out-of-span [2045 64 0] {:preFlood 0 :floodAfter 0}
    hatch [6 67 0] {:preFlood 0 :floodAfter 0}
    hatch-facing-away [6 67 0] {:preFlood 0 :floodAfter 0}
    ;; a gap jump from the unloaded z -1 over the hole at z 0, level
    (pocket {}) [10 64 1] {:preFlood 0 :floodAfter 0}
    ;; over a cactus (a block one high nothing stands on) at z 0: only from a takeoff one block up
    (pocket {"10,64,0" "cactus"}) [10 64 1] {}
    (pocket {"10,64,0" "cactus"}) [10 64 1] {:preFlood 0 :floodAfter 0}))

(deftest a-walled-goal-is-still-enclosed-at-once
  (are [spec goal] (= "goal-enclosed" (:reason (plan-over spec goal {:maxNodes 1})))
    {:blocks (merge flat room)} [7 64 0]
    {:blocks (merge flat room {"5,64,0" "iron_door" "5,65,0" "iron_door"})
     :states {"5,64,0" {:open false :half "lower" :facing "east"} "5,65,0" {:open false :half "upper" :facing "east"}}}
    [7 64 0]
    ;; a one-cell pocket on the edge of the loaded columns: a gap jump from the unloaded z -1 cannot come over its wall
    {:blocks (merge (floor -2 0 11 2) (box 9 64 1 9 65 1 "stone") (box 11 64 1 11 65 1 "stone")
                    (box 10 64 0 10 65 0 "stone") (box 10 64 2 10 65 2 "stone") {"10,66,1" "stone"})}
    [10 64 1]))

;; a pen of fence (or wall) with no gate: a fence cell is tight, its two strips (inside and outside the line) separate
;; regions the search never joins, so the flood must not join them either (live: a breed walk at a gateless 12-deep pen
;; searched the whole wide box, ~25 s a round, while the body's API went unanswered)
(defn ring-pen
  "A ring of name round x 10..16, z -3..3 at y 64 over a floor; gap: a cell of the ring left empty."
  [name gap]
  {:blocks (merge (floor -2 -6 40 6)
                  (apply dissoc (box 10 64 -3 16 64 3 name) (cond-> (keys (box 11 64 -2 15 64 2 "x")) gap (conj gap))))})

(deftest a-gateless-fence-or-wall-pen-is-enclosed
  (are [name options] (= "goal-enclosed" (:reason (plan-over (ring-pen name nil) [13 64 0] options)))
    "oak_fence" {:preFlood 0 :floodAfter 0}
    "oak_fence" {:preFlood 4000 :maxNodes 1}
    "cobblestone_wall" {:preFlood 0 :floodAfter 0}
    "nether_brick_fence" {:preFlood 0 :floodAfter 0}))

(deftest a-fence-pen-with-a-gap-is-not-enclosed
  (are [name options] (= {:status "found" :reason nil} (plan-over (ring-pen name "10,64,0") [13 64 0] options))
    "oak_fence" {:preFlood 0 :floodAfter 0}
    "oak_fence" {:preFlood 4000}
    "cobblestone_wall" {:preFlood 0 :floodAfter 0}))

;; ---- a flood that runs out of budget grows; a search that runs out floods once more ----

(defn result-over
  "Plan from [x y z] to the near goal [x y z] (range 0) over a fake world spec; the planner's result as cljs data."
  [spec [fx fy fz] [x y z] options]
  (let [pw (.pathWorld (tu/fake spec))]
    (-> (planner/plan (.-snapshot pw)
                      #js {:from #js {:x fx :y fy :z fz} :goal #js {:kind "near" :x x :y y :z z :range 0}}
                      (js/Object.assign #js {:table (.-table pw) :space (.-space pw)} (clj->js options)))
        (js->clj :keywordize-keys true))))

;; a stone deck x 10..30, z 10..30 at y 70 (441 cells, no way up) over a floor of 63 x 63 cells (live: a sealed platform
;; at y 151 whose flood needed ~12000 nodes searched the whole wide box, 50-190 s a give-up)
(def high-deck {:blocks (merge (floor -2 -2 60 60) (box 10 70 10 30 70 30 "stone"))})

(deftest a-flood-out-of-budget-grows-until-it-proves-the-goal-walled-in
  (let [r (result-over high-deck [0 64 0] [20 71 20] {:preFlood 0 :floodAfter 20 :goalFlood 100})]
    (is (= "goal-enclosed" (:reason r)))
    (is (> (get-in r [:stats :flooded]) 100) "the flood grew past its first budget")
    (is (< (:expanded r) 3969) "before the search ran out of floor")))

;; a 5 x 5 deck at y 70 (x 4..8) over a 9 x 9 floor, all well inside the loaded columns (a gap jump from unloaded land
;; onto it is a way the flood cannot rule out): the search runs out of floor (81 cells) before the late flood is due
;; (live: a gateless pen on a platform ended :one-way at the platform's edge after a walk to the fence)
(def small-floor-deck {:blocks (merge (floor -2 -2 6 6) (box 4 70 4 8 70 8 "stone"))})

(deftest a-search-that-runs-out-before-the-flood-floods-at-the-end
  (is (= "goal-enclosed" (:reason (result-over small-floor-deck [0 64 0] [6 71 6] {}))))
  (is (= "exhausted" (:reason (result-over small-floor-deck [0 64 0] [6 71 6] {:goalFlood 0})))))

;; ---- the frontier: where the searched land runs on into unloaded land ----

;; a walkway (stone at y 79, feet 80) x 18..xe at z 8 over a floor x 0..47, z 0..15: columns x 0..47 are loaded, x 48 on
;; are not (live: a walled walkway at y 100 whose only way down lay past the loaded chunks)
(defn walkway [xe] {:blocks (merge (floor 0 0 47 15) (box 18 79 8 xe 79 8 "stone"))})

(defn cell [m] (mapv m [:x :y :z]))

(deftest a-search-that-runs-out-beside-unloaded-land-names-the-frontier
  (let [r (result-over (walkway 47) [18 80 8] [10 64 8] {})]
    (is (= ["none" "exhausted"] [(:status r) (:reason r)]))
    (is (= [46 80 8] (cell (:frontier r))))
    (is (= [46 80 8] (cell (last (get-in r [:frontier :path :steps])))))))

(deftest a-search-that-runs-out-within-loaded-land-has-no-frontier
  (are [spec options] (nil? (:frontier (result-over spec [18 80 8] [10 64 8] options)))
    (walkway 40) {}
    ;; the frontier is 36 from the goal
    (walkway 47) {:frontierReach 20}))
