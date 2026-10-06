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

;; reasons that are a false proof for a goal the search can reach
(def false-proofs #{"goal-enclosed" "goal-cut-off"})

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
  (are [spec goal] (not (false-proofs (:reason (plan-over spec goal {:maxNodes 1}))))
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
    (is (not (false-proofs (:reason (plan-over spec goal options)))) (pr-str goal options))))

;; water is a way in the flood follows (see a-sealed-platform-holding-a-pool-is-cut-off); a bubble column it cannot, and a
;; wall cell of water is a way in the search takes
(deftest water-on-the-way-in-is-never-enclosed
  (are [spec goal options] (not (false-proofs (:reason (plan-over spec goal options))))
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
  (is (not (false-proofs (:reason (plan-over pillar [6 65 0] {:preFlood 0 :floodAfter 0}))))))

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
  (are [spec goal options] (not (false-proofs (:reason (plan-over spec goal options))))
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
    (is (= "goal-cut-off" (:reason r)))
    (is (> (get-in r [:stats :flooded]) 100) "the flood grew past its first budget")
    (is (< (:expanded r) 3969) "before the search ran out of floor")))

;; a 5 x 5 deck at y 70 (x 4..8) over a 9 x 9 floor, all well inside the loaded columns (a gap jump from unloaded land
;; onto it is a way the flood cannot rule out): the search runs out of floor (81 cells) before the late flood is due
;; (live: a gateless pen on a platform ended :one-way at the platform's edge after a walk to the fence)
(def small-floor-deck {:blocks (merge (floor -2 -2 6 6) (box 4 70 4 8 70 8 "stone"))})

(deftest a-search-that-runs-out-before-the-flood-floods-at-the-end
  (is (= "goal-cut-off" (:reason (result-over small-floor-deck [0 64 0] [6 71 6] {}))))
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

;; the reach counts from the start: a frontier past frontierReach of the goal but within it of the start is named
(deftest a-frontier-near-the-start-counts-past-the-reach-of-the-goal
  (is (= [46 80 8] (cell (:frontier (result-over (walkway 47) [40 80 8] [10 64 8] {:frontierReach 20}))))))

;; ---- the late flood in slices ----

(defn query-of [[fx fy fz] [x y z]]
  #js {:from #js {:x fx :y fy :z fz} :goal #js {:kind "near" :x x :y y :z z :range 0}})

(defn sliced-over
  "create-plan stepped n expansions at a time to the end: [the result as cljs data, the most cells the late flood grew by
  in one step]."
  [spec from goal options n]
  (let [pw (.pathWorld (tu/fake spec))
        options (js/Object.assign #js {:table (.-table pw) :space (.-space pw)} (clj->js options))
        ^js search (@#'planner/new-search (.-snapshot pw) (query-of from goal) options)]
    (.init search)
    (loop [most 0]
      (let [before (some-> (.-lf-seen search) .-size)
            done (.step search n)
            grew (- (or (some-> (.-lf-seen search) .-size) 0) (or before 0))]
        (if done
          [(js->clj (planner/plan (.-snapshot pw) (query-of from goal) options) :keywordize-keys true) (max most grew)]
          (recur (max most grew)))))))

(defn answer [r] [(:status r) (:reason r) (:expanded r) (get-in r [:stats :flooded]) (some-> (:path r) :steps last (select-keys [:x :y :z]))])

(defn sliced-answer
  "The answer of create-plan stepped n at a time."
  [spec from goal options n]
  (let [pw (.pathWorld (tu/fake spec))
        ^js p (planner/create-plan (.-snapshot pw) (query-of from goal)
                                   (js/Object.assign #js {:table (.-table pw) :space (.-space pw)} (clj->js options)))]
    (loop [] (when-not (.step p n) (recur)))
    (answer (js->clj (.result p) :keywordize-keys true))))

;; a late flood that grows goes on from where the last one stopped: a slice floods at most about its own share of cells,
;; and the answer, the stats included, is the one of a search in one go
(deftest a-late-flood-runs-in-the-slices-of-the-search
  (let [options {:preFlood 0 :floodAfter 20 :goalFlood 100}
        [whole most] (sliced-over high-deck [0 64 0] [20 71 20] options 50)]
    (is (= "goal-cut-off" (:reason whole)))
    (is (> (get-in whole [:stats :flooded]) 400) "the flood grew twice")
    (is (< most 150) "no slice flooded much more than its 50")))

(deftest a-search-in-slices-answers-as-in-one-go
  (are [spec from goal options] (= (answer (result-over spec from goal options))
                                   (sliced-answer spec from goal options 7)
                                   (sliced-answer spec from goal options 1000))
    high-deck [0 64 0] [20 71 20] {:preFlood 0 :floodAfter 20 :goalFlood 100}
    high-deck [0 64 0] [20 71 20] {}
    small-floor-deck [0 64 0] [6 71 6] {}
    (walkway 47) [18 80 8] [10 64 8] {}
    flat [0 64 0] [30 64 2] {:floodAfter 5 :goalFlood 10}))

;; ---- the flood schedule (cards 29912d63, 59551eba) ----

;; a 12 x 12 ring of fence round x 10..21, z -6..5 (100 cells inside) over a floor (live: a gateless 12 x 12 pen on a
;; platform; the early flood's 24 nodes never covered it, and the body walked to a post)
(def big-pen
  {:blocks (merge (floor -2 -8 40 8)
                  (apply dissoc (box 10 64 -6 21 64 5 "oak_fence") (keys (box 11 64 -5 20 64 4 "x"))))})

(deftest a-12-by-12-gateless-pen-is-enclosed-before-the-search
  (is (= "goal-enclosed" (:reason (plan-over big-pen [15 64 0] {:maxNodes 1})))))

;; small-floor-deck with a ladder against a stone post at (0, 64..67, -2) whose rung at y 65 is missing: the search turns
;; the ladder away at the gap (ladder-gap) and runs out of floor; the goal on the deck is walled in all the same
(def deck-and-gappy-ladder
  {:blocks (merge (:blocks small-floor-deck) (box 0 64 -2 0 67 -2 "stone") {"0,64,-1" "ladder" "0,66,-1" "ladder"})
   :states {"0,64,-1" {:facing "south"} "0,66,-1" {:facing "south"}}})

(deftest a-ladder-gap-elsewhere-does-not-hide-a-walled-in-goal
  (is (= "ladder-gap" (:reason (result-over deck-and-gappy-ladder [0 64 0] [6 71 6] {:goalFlood 0}))) "the search saw the gap")
  (is (= "goal-cut-off" (:reason (result-over deck-and-gappy-ladder [0 64 0] [6 71 6] {})))))

;; a sealed 45 x 45 deck (2025 cells) over a 93 x 93 floor: the floods of 100, 400 and 1600 cells run out, and the next
;; (6000) was due after 10240 expansions, past the 6000 nodes the search may make: it must come before them
(def wide-deck {:blocks (merge (floor -2 -2 90 90) (box 10 70 10 54 70 54 "stone"))})

(deftest a-sealed-area-is-proved-before-max-nodes
  (let [r (result-over wide-deck [0 64 0] [30 71 30] {:preFlood 0 :floodAfter 20 :goalFlood 100 :maxNodes 6000})]
    (is (= "goal-cut-off" (:reason r)))
    (is (> (get-in r [:stats :flooded]) 1600))))

;; a bubble column beside the goal: the flood can prove nothing once it meets one, so it stops there (not at the start, 30 away)
(deftest a-flood-that-leaks-stops
  (let [r (result-over {:blocks (assoc flat "31,64,0" "bubble_column" "31,63,0" "stone")} [0 64 0] [30 64 0]
                       {:preFlood 0 :floodAfter 0 :goalFlood 100000})]
    (is (= "found" (:status r)))
    (is (< (get-in r [:stats :flooded]) 50))))

;; ---- the flood kept over searches toward one goal (options.goalFloodMemo) ----

(defn rounds
  "Searches toward goal over spec, one a round of n expansions, each from the next start of froms (cycled), as go-to
  begins one after each walk; memo the options.goalFloodMemo they share (nil: none). A search with no progress to walk
  goes on in the next round, as go-to's does. The reason of the first that ends within k rounds, else :unfinished."
  [spec froms goal options n k memo]
  (let [pw (.pathWorld (tu/fake spec))
        search (fn [i] (planner/create-plan (.-snapshot pw) (query-of (nth (cycle froms) i) goal)
                                            (js/Object.assign #js {:table (.-table pw) :space (.-space pw) :goalFloodMemo memo}
                                                              (clj->js options))))]
    (loop [i 0
           ^js p (search 0)]
      (cond
        (>= i k) :unfinished
        (.step p n) (let [r (.result p)] (or (.-reason r) (.-status r)))
        (nil? (.progress p)) (recur (inc i) p)
        :else (recur (inc i) (search (inc i)))))))

(def small-floods {:preFlood 0 :floodAfter 20 :goalFlood 100})

(deftest a-flood-goes-on-over-the-searches-of-one-goal
  (is (= :unfinished (rounds high-deck [[0 64 0] [2 64 0]] [20 71 20] small-floods 300 12 nil))
      "each search floods afresh and never gets to the flood that proves it")
  (is (= "goal-cut-off" (rounds high-deck [[0 64 0] [2 64 0]] [20 71 20] small-floods 300 12 #js {}))))

(deftest a-kept-flood-of-another-goal-is-not-used
  (let [memo #js {}]
    (is (= :unfinished (rounds high-deck [[0 64 0]] [20 71 20] small-floods 300 2 memo)))
    (set! (.-after memo) 0) ; the kept flood would go on at once
    (is (= "found" (rounds high-deck [[0 64 0]] [40 64 40] (assoc small-floods :floodAfter 0) 100000 1 memo)))))

;; a search that begins inside the kept flood can reach the goal: the flood must not call the goal walled in
(deftest a-start-inside-the-kept-flood-meets-it
  (let [memo #js {}]
    (is (= :unfinished (rounds high-deck [[0 64 0]] [15 71 15] small-floods 300 2 memo)))
    (is (some? (.-queue memo)) "a flood is kept")
    (set! (.-after memo) 0) ; the kept flood goes on at once
    (is (= "found" (rounds high-deck [[16 71 15]] [15 71 15] (assoc small-floods :floodAfter 0) 100000 1 memo)))))

;; the deck of up-to-deck whose ladder lacks its rung at y 65: the ladder is the way in, so the goal is not walled in, early
;; or late, and the answer names the gap
(def gappy-ladder-deck {:blocks (dissoc (:blocks (up-to-deck "ladder")) "3,65,0")
                        :states {"3,64,0" {:facing "south"} "3,66,0" {:facing "south"}}})

(deftest a-goal-whose-way-in-is-a-ladder-with-a-gap-is-not-enclosed
  (is (not (false-proofs (:reason (plan-over gappy-ladder-deck [6 67 0] {:maxNodes 1})))))
  (is (= "ladder-gap" (:reason (plan-over gappy-ladder-deck [6 67 0] {}))))
  (is (= "ladder-gap" (:reason (plan-over gappy-ladder-deck [6 67 0] {:preFlood 0 :floodAfter 0})))))

;; ---- a kept flood read an older world: its enclosed answer is checked by a fresh flood ----

;; a stone room x 5..18, z -7..6 (12 x 12 inside, feet y 64), sealed; the way in its west wall at z 0 cleared in opened-room
(def sealed-room {:blocks (merge flat (hollow 5 63 -7 18 67 6))})
(def opened-room {:blocks (merge flat (dissoc (hollow 5 63 -7 18 67 6) "5,64,0" "5,65,0"))})

;; the room on a floor round it, opened (far-opened) in its east wall at z 0, away from the start: the flood proves the
;; goal (17 64 0) walled in before the search gets round
(def round-room (merge (floor -2 -10 24 10) (hollow 5 63 -7 18 67 6)))
(def far-sealed {:blocks round-room})
(def far-opened {:blocks (dissoc round-room "18,64,0" "18,65,0")})

(defn search-over [spec from goal options memo]
  (let [pw (.pathWorld (tu/fake spec))]
    (planner/create-plan (.-snapshot pw) (query-of from goal)
                         (js/Object.assign #js {:table (.-table pw) :space (.-space pw) :goalFloodMemo memo} (clj->js options)))))

(defn reason-of [^js p] (let [r (.result p)] (or (.-reason r) (.-status r))))

;; the goal beside the way in: the kept flood expanded its cells while they were walled
(deftest a-kept-flood-of-a-world-since-opened-does-not-call-the-goal-enclosed
  (let [memo #js {}
        a (search-over far-sealed [0 64 0] [17 64 0] small-floods memo)]
    (is (false? (.step a 60)) "the first search is still going when the world changes")
    (is (some? (.-queue memo)))
    (let [b (search-over far-opened [0 64 0] [17 64 0] small-floods memo)]
      (.step b 100000)
      (is (= "found" (reason-of b))))))

(deftest a-kept-flood-that-proved-the-goal-enclosed-is-checked-in-the-new-world
  (let [memo #js {}
        a (search-over sealed-room [0 64 0] [6 64 0] small-floods memo)]
    (.step a 100000)
    (is (= "goal-enclosed" (reason-of a)))
    (let [b (search-over opened-room [0 64 0] [6 64 0] small-floods memo)]
      (.step b 100000)
      (is (= "found" (reason-of b))))
    (let [c (search-over sealed-room [0 64 0] [6 64 0] small-floods memo)]
      (.step c 100000)
      (is (= "goal-enclosed" (reason-of c)) "a sealed room is still proved"))))

;; a pit x 8..19, z -6..5, feet y 61, 3 below the floor round it: a drop of 3 is the only way in
(def pit {:blocks (apply dissoc (merge (floor -2 -8 24 8) (box 7 60 -7 20 63 6 "stone")) (keys (box 8 61 -6 19 63 5 "x")))})

;; the memo's flood was made with maxDrop 2 (the pit walled in for it); a search that may drop 3 must not take it
(deftest a-kept-flood-of-other-moves-is-not-used
  (let [memo #js {}
        a (search-over pit [0 64 0] [13 61 0] (assoc small-floods :maxDrop 2) memo)]
    (.step a 100000)
    (is (= "goal-enclosed" (reason-of a)))
    (let [other #js {}]
      (.step (search-over pit [0 64 0] [13 61 0] small-floods other) 1)
      (is (not= (.-goal memo) (.-goal other)) "the memo's key names the moves"))
    (let [b (search-over pit [0 64 0] [13 61 0] small-floods memo)]
      (.step b 100000)
      (is (= "found" (reason-of b))))))

;; ---- pockets up to the early flood's 256 cells: every way in the search takes, the flood takes ----

(defn big-way-in
  "The 12 x 12 room (sealed-room) with its west wall's cells (5,64,0) and (5,65,0) cleared and `blocks` put in."
  ([blocks] (big-way-in blocks {}))
  ([blocks states] {:blocks (merge (:blocks opened-room) blocks) :states states}))

;; a 13 x 13 attic (deck y 66, x 2..14, z -6..6, walls and a roof round it) whose only way in is a ladder up to a trapdoor
(defn attic [trapdoor-state]
  {:blocks (merge flat
                  (assoc (box 2 66 -6 14 66 6 "stone") "3,66,0" "oak_trapdoor")
                  (apply dissoc (box 1 66 -7 15 68 7 "stone") (keys (box 2 66 -6 14 68 6 "x")))
                  (box 1 69 -7 15 69 7 "stone")
                  {"3,64,0" "ladder" "3,65,0" "ladder"} (box 3 64 -1 3 65 -1 "stone"))
   :states {"3,64,0" {:facing "south"} "3,65,0" {:facing "south"} "3,66,0" trapdoor-state}})

;; the pit with water 2 deep (y 55..56) at the bottom of an 8-deep drop and a dry ledge (x 17..19, feet 57) for the goal
(def water-pit
  {:blocks (merge (apply dissoc (merge (floor -2 -8 24 8) (box 7 54 -7 20 63 6 "stone")) (keys (box 8 55 -6 19 63 5 "x")))
                  (box 8 55 -6 16 56 5 "water") (box 17 55 -6 19 56 5 "stone"))})

(def big-ways-in
  [[(big-way-in {"5,64,0" "oak_door" "5,65,0" "oak_door"}
                {"5,64,0" {:open false :half "lower" :facing "east"} "5,65,0" {:open false :half "upper" :facing "east"}}) [12 64 0]]
   [(big-way-in {"5,64,0" "oak_door" "5,65,0" "oak_door"}
                {"5,64,0" {:open true :half "lower" :facing "east"} "5,65,0" {:open true :half "upper" :facing "east"}}) [12 64 0]]
   [(big-way-in {"5,64,0" "oak_fence_gate"} {"5,64,0" {:open false :facing "east"}}) [12 64 0]]
   [(big-way-in (merge iron-door-room {"4,64,0" "stone_pressure_plate"}) iron-door-states) [12 64 0]]
   [(attic {:open false :half "bottom" :facing "south"}) [8 67 0]]
   [(attic {:open true :half "bottom" :facing "south"}) [8 67 0]]
   [pit [13 61 0]]
   [(big-way-in {"5,64,0" "water"}) [12 64 0]]
   [water-pit [18 57 0]]])

(deftest every-way-into-a-big-pocket-the-search-takes-is-found
  (doseq [[spec goal] big-ways-in]
    (is (= "found" (:status (plan-over spec goal {:goalFlood 0 :preFlood 0}))) (pr-str goal (:states spec)))))

(deftest every-way-into-a-big-pocket-the-search-takes-is-not-enclosed
  (doseq [[spec goal] big-ways-in
          options [{} {:maxNodes 1} {:preFlood 0 :floodAfter 0}]]
    (is (not (false-proofs (:reason (plan-over spec goal options)))) (pr-str goal (:states spec) options))))

(deftest a-big-pocket-with-no-way-in-is-enclosed-at-once
  (are [spec goal options] (= "goal-enclosed" (:reason (plan-over spec goal options)))
    sealed-room [12 64 0] {:maxNodes 1}
    pit [13 61 0] {:maxNodes 1 :maxDrop 2}))

;; ---- water on a sealed platform: the flood follows the one move into water it does not enumerate (a fall off an edge) ----

;; the small deck with one stone cell and a water cell on it (the pool is part of the deck, nothing leads into it from below)
(def pool-deck {:blocks (merge (:blocks small-floor-deck) {"4,69,4" "stone" "4,70,4" "water"})})

(deftest a-sealed-platform-holding-a-pool-is-cut-off
  (is (= "goal-cut-off" (:reason (result-over pool-deck [0 64 0] [6 71 6] {})))))

(def pooled-high-deck
  {:blocks (merge (:blocks high-deck) (box 14 68 14 16 68 15 "stone") (box 14 69 14 16 70 15 "water"))})

(deftest a-flood-through-a-pool-proves-the-goal-cut-off-early
  (is (= "goal-cut-off"
         (:reason (result-over pooled-high-deck [0 64 0] [20 71 20] {:maxNodes 1 :preFlood 0 :floodAfter 0 :goalFlood 100000})))))

;; a pool beside a ladder tower: the path climbs to y 80 and falls into the pool, so the pool is a way in the flood must find
(deftest a-pool-a-fall-from-a-tower-reaches-is-not-enclosed
  (let [spec {:blocks (merge (:blocks high-deck)
                             (box 11 68 14 12 68 15 "stone") (box 11 69 14 12 70 15 "water")
                             (box 10 64 13 10 79 15 "stone")
                             (apply merge (for [y (range 64 81)] {(str "9," y ",14") "ladder"})))
              :states (into {} (for [y (range 64 81)] [(str "9," y ",14") {:facing "east"}]))}]
    (is (= "found" (:status (result-over spec [0 64 0] [11 70 14] {:goalFlood 0 :preFlood 0}))))
    (is (not (false-proofs
              (:reason (result-over spec [0 64 0] [11 70 14] {:maxNodes 300 :preFlood 0 :floodAfter 0 :goalFlood 100000})))))))

;; a two-high shelter (interior x -1..1, z 0..2, roof y 66) on grass; the first stair cell (0 63 1) is dug out of the floor
;; in front of the body at (0 64 0): one step down, the table and chest beside it
(def shelter-stair
  {:blocks (-> (merge (box -6 62 -6 6 63 8 "stone")
                      (hollow -2 63 -1 2 66 3)
                      {"-1,64,2" "crafting_table" "1,64,0" "chest"})
               (dissoc "0,63,1")
               (assoc "0,64,-1" "oak_door" "0,65,-1" "oak_door"))
   :states {"0,64,-1" {:open false :half "lower" :facing "north"} "0,65,-1" {:open false :half "upper" :facing "north"}}})

(deftest a-first-stair-cell-in-a-shelter-is-not-enclosed
  (are [options] (= {:status "found" :reason nil} (plan-over shelter-stair [0 63 1] options))
    {}
    {:preFlood 0 :floodAfter 0}))
