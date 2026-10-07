(ns engine.planner-water-test
  "engine/js/path/planner-water.test.mjs against the ClojureScript planner: portals, drops into and out of tight
  cells, water (ponds, drops, columns, breath, exits), the water courses, and magma."
  (:require [cljs.test :refer [deftest is are]]
            [engine.path.blocks :as blocks]
            [engine.path.courses :as courses]
            [engine.path.fixture :as fx]
            [engine.planner-fixture :as pf]))

(def table @pf/table)
(def WATER blocks/WATER)
(def PORTAL blocks/PORTAL)
(def MOVE pf/MOVE)
(def costs pf/default-costs)

(defn state-id [name] (fx/state-id name))
(defn state-at [snapshot x y z] (.stateAt ^js snapshot x y z))
(defn kind-at [snapshot x y z] (aget (.-kind ^js table) (state-at snapshot x y z)))
(defn hazard-at [snapshot x y z] (aget (.-hazard ^js table) (state-at snapshot x y z)))
(defn bubble-at [snapshot x y z] (aget (.-bubble ^js table) (state-at snapshot x y z)))

(defn verdict [r] (str (:status r) (some->> (:reason r) (str "/"))))
(defn near
  ([x y z] (near x y z 1))
  ([x y z range] {:kind "near" :x x :y y :z z :range range}))
(defn run
  ([snapshot from goal] (run snapshot from goal {}))
  ([snapshot from goal options] (pf/plan snapshot {:from from :goal goal} options)))
(defn found? [r] (= "found" (:status r)))
(defn steps [r] (get-in r [:path :steps]))
(defn last-step [r] (peek (steps r)))
(defn xyz [s] [(:x s) (:y s) (:z s)])
(defn moves [r] (mapv :move (steps r)))
(defn summary [r] (get-in r [:path :summary]))
(defn cost [r k] (get-in r [:path :cost k]))
(defn plan-course
  ([name] (plan-course name {}))
  ([name options] (pf/course-plan name options)))

;; stone to y 63: feet cells are y 64 (the JS tests' floor reaches x 60)
(defn world
  ([] (world [] []))
  ([fill] (world fill []))
  ([fill blocks] (pf/snapshot {:fill (into [[-2 60 -2 60 63 40 "stone"]] fill) :blocks blocks})))
(def from2 {:x 2 :y 64 :z 2})

;; ---- portals ----

(deftest course-portal-path-goes-around
  (let [snapshot (:snapshot (courses/course-snapshot "portal"))
        r (pf/course-plan "portal")]
    (is (= "found" (verdict r)))
    (is (every? (fn [s] (and (not= PORTAL (hazard-at snapshot (:x s) (:y s) (:z s)))
                             (not= PORTAL (hazard-at snapshot (:x s) (+ 1 (:y s)) (:z s)))))
                (steps r)))))

(def portal-props {"nether_portal" {:axis "x"}})
(defn portal-at [name] [8 64 2 8 65 2 name (portal-props name)])
(defn corridor [name] (world [[8 64 -2 8 70 40 "stone"] (portal-at name)]))

(deftest portal-filling-the-only-way-not-found
  (doseq [name ["nether_portal" "end_portal" "end_gateway"]]
    (let [r (run (corridor name) from2 (near 14 64 2))]
      (is (not= "found" (:status r)) name)
      (is (every? #(< (:x %) 8) (steps r)) name))))

(deftest same-corridor-with-portal-removed-is-passable
  (let [open (world [[8 64 -2 8 70 40 "stone"] [8 64 2 8 65 2 "air"]])]
    (is (= "found" (:status (run open from2 (near 14 64 2)))))))

(deftest goal-in-a-portal-is-found-last-step-in-it
  (are [name goal] (let [r (run (world [(portal-at name)]) from2 goal)]
                     (and (found? r) (= [8 2] [(:x (last-step r)) (:z (last-step r))])))
    "nether_portal" (near 8 64 2 0)
    "end_gateway" (near 8 64 2 0)))

(deftest goal-beside-portal-portal-cell-not-entered
  (let [open (world [[8 64 2 8 65 2 "nether_portal" {:axis "x"}]])
        r (run open from2 (near 8 64 3 0))]
    (is (found? r))
    (is (every? #(not (and (= 8 (:x %)) (= 2 (:z %)))) (steps r)))))

;; ---- drops into and out of tight cells ----

(defn mound [bamboo]
  (world (into [[0 64 0 4 65 4 "stone"]]
               (map (fn [[x z]] [x 64 z x 70 z "bamboo"]))
               bamboo)))
(def on-mound {:x 2 :y 66 :z 2})

(deftest drop-of-2-into-a-tight-cell-found-by-a-drop
  (are [what bamboo]
       (let [r (run (mound bamboo) on-mound (near 5 64 2 0))]
         (and (found? r)
              (some #{(:drop MOVE)} (moves r))
              (= [5 64 2] (xyz (last-step r)))))
    "one bamboo beside the landing" [[6 2]]
    "bamboo on both sides of the landing" [[6 1] [6 3]]
    "bamboo diagonal to the landing" [[6 3]]))

(deftest drop-into-a-tight-cell-lands-on-a-point-bamboo-leaves-free
  (let [last (last-step (run (mound [[6 2]]) on-mound (near 5 64 2 0)))]
    (is (or (>= (js/Math.abs (- (:px last) 6.5)) (- (+ 0.31 0.0625) 1e-9))
            (>= (js/Math.abs (- (:pz last) 2.5)) (- (+ 0.31 0.0625) 1e-9))))))

(deftest drop-out-of-a-tight-cell-is-found
  (let [w (world [[0 64 0 4 65 4 "stone"] [3 66 2 3 70 2 "bamboo"]])
        r (run w {:x 4 :y 66 :z 2} (near 5 64 2 0))]
    (is (found? r))
    (is (some #{(:drop MOVE)} (moves r)))))

(defn gap-world [bamboo]
  (world (into [[5 60 -2 5 63 40 "air"]]
               (map (fn [[x z]] [x 64 z x 70 z "bamboo"]))
               bamboo)))
(defn row [x] (mapv (fn [k] [x (- k 2)]) (range 43)))

(deftest gap-jumps-from-or-into-a-tight-cell-stay-refused
  (are [what bamboo from goal statuses]
       (contains? statuses (:status (run (gap-world bamboo) from goal)))
    "into a tight cell" (row 7) from2 (near 6 64 2 0) #{"partial" "none"}
    "out of a tight cell" (row 3) {:x 4 :y 64 :z 2} (near 7 64 2 0) #{"partial" "none"}
    "between plain cells (control)" [] from2 (near 7 64 2 0) #{"found"}))

;; ---- water: a pond ----

(def lake (world [[10 61 -2 20 63 40 "water"]]))

(deftest swimming-across-a-pond
  (let [r (run lake from2 (near 26 64 2))]
    (is (found? r))
    (is (re-find #"swims \d+" (summary r)))
    (is (> (cost r :waterSeconds) 4))
    (is (= (:airSupply costs) (cost r :airMin)))
    (is (some #(= WATER (kind-at lake (:x %) (:y %) (:z %))) (steps r)))))

(deftest swimming-costs-swimh-seconds-a-block
  (let [base (cost (run lake from2 (near 26 64 2)) :seconds)
        slow (cost (run lake from2 (near 26 64 2) {:costs {:swimH 5}}) :seconds)]
    (is (> slow (+ base 10)))))

(deftest default-water-costs-are-the-stated-ones
  (is (= {:swimH 0.5 :swimUp 0.3 :swimDown 0.35 :exit 0.6 :current 0.3 :bubbleUp 0.08 :bubbleDown 0.12
          :airSupply 15 :airLimit 12 :maxWaterDrop 64 :dripleaf 0.2}
         (select-keys costs [:swimH :swimUp :swimDown :exit :current :bubbleUp :bubbleDown :airSupply :airLimit
                             :maxWaterDrop :dripleaf]))))

(deftest start-in-the-water-way-out-is-an-exit
  (let [r (run lake {:x 15 :y 63 :z 2} (near 2 64 2))]
    (is (found? r))
    (is (some #{(:exit MOVE)} (moves r)))))

(deftest goal-in-the-water-is-standable-and-reached
  (let [r (run lake from2 (near 15 62 2 0))]
    (is (found? r))
    (is (= [15 62 2] (xyz (last-step r))))))

;; ---- water: drops ----

(defn drop-world [high deep]
  (world [[0 64 0 4 (+ 63 high) 4 "stone"] [5 (- 64 deep) -2 20 63 40 "water"]]))

(deftest drop-into-water-is-a-drop-with-no-risk
  (are [high deep]
       (let [r (run (drop-world high deep) {:x 2 :y (+ 64 high) :z 2} (near 12 63 2 1))]
         (and (found? r)
              (some #{(:drop MOVE)} (moves r))
              (= 0 (cost r :risk))
              (= 0 (cost r :maxDrop))
              (= (inc high) (cost r :waterDrop))))
    3 1
    8 1
    8 3
    20 1
    40 2
    60 4))

(deftest drop-summary-says-it-lands-in-water
  (let [r (run (drop-world 8 2) {:x 2 :y 72 :z 2} (near 12 63 2 1))]
    (is (re-find #"drops 9 into water" (summary r)))))

(deftest drop-higher-than-max-water-drop-is-refused
  (let [r (run (drop-world 20 2) {:x 2 :y 84 :z 2} (near 12 63 2 1) {:costs {:maxWaterDrop 10}})]
    (is (not (found? r)))))

(deftest drop-onto-land-of-same-height-is-limited-by-max-drop
  (let [r (run (world [[0 64 0 4 71 4 "stone"]]) {:x 2 :y 72 :z 2} (near 8 64 2 0) {:goalFlood 0})]
    (is (not (found? r)))))

(defn roofed [roof]
  (world (cond-> [[0 64 0 4 71 4 "stone"] [5 61 -2 20 63 40 "water"]]
           roof (conj [5 66 -2 6 66 40 "stone"]))))

(deftest roof-over-the-landing-columns-stops-the-fall-into-water
  (let [found (fn [roof] (found? (run (roofed roof) {:x 2 :y 72 :z 2} (near 12 63 2 1) {:goalFlood 0})))]
    (is (= [false true] [(found true) (found false)]))))

;; ---- water: columns ----

(defn column
  ([depth] (column depth {}))
  ([depth {:keys [bottom fluid props] :or {bottom "stone" fluid "water" props {}}}]
   (world [[10 64 0 12 (+ 63 depth) 2 "stone"]
           [11 63 1 11 63 1 bottom]
           [11 64 1 11 (+ 63 depth) 1 fluid props]
           [10 64 1 10 65 1 "air"]
           [13 (+ 63 depth) 0 16 (+ 63 depth) 2 "stone"]])))
(def bubble-up {:fluid "bubble_column" :props {:drag false} :bottom "soul_sand"})
(def bubble-down {:fluid "bubble_column" :props {:drag true} :bottom "magma_block"})
(defn up [depth] [from2 (near 14 (+ 64 depth) 1 1)])
(defn top-of [depth] {:x 12 :y (+ 64 depth) :z 1})
(defn down [depth] [(top-of depth) (near 5 64 1 1)])
(defn run-up [snapshot depth & [options]] (apply run snapshot (concat (up depth) (when options [options]))))
(defn run-down [snapshot depth] (apply run snapshot (down depth)))

(deftest water-column-20-up
  (let [r (run-up (column 20) 20)]
    (is (found? r))
    (is (re-find #"water column up (18|19|20)" (summary r)))
    (is (< (cost r :airMin) (:airSupply costs)))
    (is (re-find #"lowest air \d+ s" (summary r)))))

(deftest water-column-40-up-is-refused-for-air
  (let [r (run-up (column 40) 40)]
    (is (not (found? r)))
    (is (= "air" (:reason r)))))

(deftest water-column-40-with-higher-air-limit-is-passable
  (is (found? (run-up (column 40) 40 {:costs {:airLimit 14}}))))

(deftest bubble-column-40-up-refills-the-air
  (let [r (run-up (column 40 bubble-up) 40)]
    (is (found? r))
    (is (re-find #"bubble lift up 3\d" (summary r)))
    (is (= (:airSupply costs) (cost r :airMin)))))

(deftest bubble-column-rises-at-bubble-up-seconds-a-block
  (let [base (cost (run-up (column 20 bubble-up) 20) :seconds)
        slow (cost (run-up (column 20 bubble-up) 20 {:costs {:bubbleUp 1.08}}) :seconds)]
    (is (< (js/Math.abs (- slow base 19)) 1.5))))

(deftest magma-drag-column-cannot-be-swum-up
  (is (not (found? (run-up (column 20 bubble-down) 20)))))

(deftest magma-column-down-is-found
  (let [r (run-down (column 20 bubble-down) 20)]
    (is (found? r))
    (is (re-find #"magma column down 1\d" (summary r)))))

(deftest soul-sand-lift-column-cannot-be-swum-down
  (is (not (found? (run-down (column 20 bubble-up) 20)))))

(deftest plain-water-column-20-down
  (let [r (run-down (column 20) 20)]
    (is (found? r))
    (is (re-find #"water column down" (summary r)))))

(deftest plain-column-down-costs-swim-down-a-block-a-drag-column-bubble-down
  (let [plain (cost (run-down (column 20) 20) :seconds)
        magma (cost (run-down (column 20 bubble-down) 20) :seconds)]
    (is (> (- plain magma) (- (* 19 (- (:swimDown costs) (:bubbleDown costs))) 2)))))

;; ---- water: breath ----

(defn tunnel [len shaft]
  (let [mid (+ 11 (bit-shift-right len 1))]
    (world (cond-> [[10 64 -2 (+ 12 len) 68 40 "stone"]
                    [11 64 1 (+ 10 len) 65 1 "water"]
                    [10 64 1 10 65 1 "air"]
                    [(+ 11 len) 64 1 (+ 12 len) 65 1 "air"]]
             shaft (into [[mid 66 1 mid 68 1 "air"] [mid 66 1 mid 67 1 "water"]])))))

(deftest submerged-channel-breath
  (are [len shaft reachable reason]
       (let [r (run (tunnel len shaft) from2 (near (+ 13 len) 64 1 0) {:goalFlood 0})]
         (= [reachable reason] [(found? r) (:reason r)]))
    10 false true nil
    30 false false "air"
    30 true true nil))

(deftest breath-in-the-shaft-refills-the-air
  (let [r (run (tunnel 30 true) from2 (near 43 64 1 0) {:goalFlood 0})]
    (is (> (cost r :airMin) (- (:airSupply costs) (:airLimit costs))))
    (is (> (cost r :waterSeconds) 15))))

;; ---- water: exits ----

(defn bank-world [bank]
  (world [[10 61 -2 14 63 40 "water"] [15 64 -2 20 (+ 63 bank) 40 "stone"]]))

(deftest bank-above-the-flush-stand-height
  (are [bank reachable]
       (= reachable (found? (run (bank-world bank) from2 (near 18 (+ 64 bank) 2 0) {:goalFlood 0 :margin 8})))
    0 true
    1 false
    2 false))

(defn closed-lake [bank]
  (pf/snapshot {:fill [[-2 50 0 30 63 6 "stone"] [10 61 0 14 63 6 "water"] [15 64 0 30 (+ 63 bank) 6 "stone"]]}))

(deftest lake-walled-in-banks-above-flush
  (are [bank status reason]
       (let [r (run (closed-lake bank) {:x 5 :y 64 :z 3} (near 20 (+ 64 bank) 3 0) {:goalFlood 0})]
         (= [status reason] [(:status r) (:reason r)]))
    0 "found" nil
    1 "partial" "exhausted"
    2 "partial" "exhausted"))

(defn wade [rise]
  (world [[8 64 -2 12 64 40 "water"] [13 64 -2 20 (+ 63 rise) 40 "stone"]]))

(defn step-out [r]
  (let [ss (steps r)]
    (->> (map-indexed vector ss)
         (some (fn [[k s]] (when (and (pos? k) (not (:swim s)) (:swim (ss (dec k)))) s))))))

(deftest wading-in-water-1-deep-onto-a-block-or-level-ground
  (are [rise move]
       (let [r (run (wade rise) from2 (near 14 (+ 64 rise) 2 0) {:goalFlood 0 :margin 8})]
         (and (found? r) (= move (:move (step-out r)))))
    1 (:jump MOVE)
    0 (:walk MOVE)))

(deftest wading-a-block-two-over-the-floor-is-not-climbed
  (is (not (found? (run (wade 2) from2 (near 14 66 2 0) {:goalFlood 0 :margin 8})))))

;; ---- courses: water ----

(deftest courses-found
  (doseq [name ["bubble-up" "magma-down" "water20-up" "water20-down" "water2-up" "waterfall-up" "waterfall-down"
                "dropshaft-1deep" "dropshaft-2deep" "drop8-open" "drop3-water" "lake20-flush"]]
    (is (= "found" (verdict (plan-course name))) name)))

(deftest course-drop8-open
  (is (re-find #"drops 8 into water" (summary (plan-course "drop8-open")))))

(deftest course-water20-up
  (is (re-find #"water column up" (summary (plan-course "water20-up")))))

(deftest course-bubble-up
  (let [r (plan-course "bubble-up")]
    (is (re-find #"bubble lift up" (summary r)))
    (is (= (:airSupply costs) (cost r :airMin)))))

(deftest course-magma-down
  (is (re-find #"magma column down" (summary (plan-course "magma-down")))))

(deftest course-waterfall-up
  (let [r (plan-course "waterfall-up")
        slow (plan-course "waterfall-up" {:costs {:current 0}})]
    (is (re-find #"water column up" (summary r)))
    (is (> (cost r :seconds) (+ 1 (cost slow :seconds))))))

(deftest courses-crossed-without-a-land-route
  (doseq [name ["farm-channels" "lilypads" "coral" "dripleaf" "lake20-wade" "stream3" "swamp" "frozen-river"
                "dropshaft-1deep-in" "drop8-water"]]
    (is (= "found" (verdict (plan-course name))) name)))

(deftest courses-sealed-no-way-across
  (doseq [name ["lake20" "lake20-high" "river-current"]]
    (is (not= "found" (:status (plan-course name))) name)))

(def leaf-pond
  (world [[10 61 -2 14 63 40 "water"]
          [10 63 -2 14 63 40 "big_dripleaf" {:tilt "none" :waterlogged false :facing "east"}]]))

(deftest pond-crossed-on-five-big-dripleaf-leaves
  (let [goal (near 16 64 2 0)
        base (:cost (:path (run leaf-pond from2 goal)))
        slow (:cost (:path (run leaf-pond from2 goal {:costs {:dripleaf 3.2}})))]
    (is (= 2.5 (:risk base)))
    (is (< (js/Math.abs (- (:seconds slow) (:seconds base) (* 5 3))) 1e-6))))

(deftest moves-into-flowing-water-cost-current-more
  (let [base (cost (plan-course "waterfall-up") :seconds)
        dear (cost (plan-course "waterfall-up" {:costs {:current 0.4}}) :seconds)]
    (is (> dear base))))

;; ---- water: dominance of the surface over open-water diving ----

(defn deep-lake [fill]
  (pf/snapshot {:fill (into [[-2 50 -2 40 63 14 "stone"] [10 58 -2 29 63 14 "water"]] fill)}))

(deftest lake-6-deep-20-wide-is-crossed-at-the-surface
  (let [r (run (deep-lake []) {:x 2 :y 64 :z 6} (near 36 64 6))]
    (is (found? r))
    (is (< (:expanded r) 700) (str "expanded " (:expanded r)))))

(deftest underwater-tunnel-under-a-stone-ceiling-is-still-found
  (let [snapshot (pf/snapshot {:fill [[-2 50 -2 40 75 14 "stone"]
                                      [10 58 4 14 63 8 "water"] [22 58 4 26 63 8 "water"] [15 62 6 21 63 6 "water"]
                                      [10 64 4 14 70 8 "air"] [22 64 4 26 70 8 "air"]]})
        r (run snapshot {:x 12 :y 63 :z 6} (near 24 63 6 0))]
    (is (found? r))
    (is (some #(and (= 18 (:x %)) (<= (:y %) 63)) (steps r)))))

;; ---- magma: where a route may end, and the columns beside a lift ----

(defn on-magma? [snapshot s]
  (or (= 2 (bubble-at snapshot (:x s) (:y s) (:z s)))
      (= (state-id "magma_block") (state-at snapshot (:x s) (dec (:y s)) (:z s)))))

(deftest route-never-ends-on-magma
  (are [what snapshot goal status]
       (let [r (run snapshot from2 goal {:goalFlood 0})]
         (and (= status (:status r)) (not (on-magma? snapshot (last-step r)))))
    "a goal range 1 around a magma block with stone beside it" (world [[9 63 2 10 63 2 "magma_block"]]) (near 10 64 2 1) "found"
    "a goal on a magma block" (world [[10 63 2 10 63 2 "magma_block"]]) (near 10 64 2 0) "partial"
    "a goal in a magma bubble column" (column 20 bubble-down) (near 11 74 1 0) "partial"))

(def beside
  (world [[-2 64 1 40 66 1 "stone"] [-2 64 3 40 66 3 "stone"]
          [10 63 3 10 63 3 "magma_block"] [10 64 3 10 66 3 "bubble_column" {:drag true}]]))

(deftest walking-past-a-magma-bubble-column
  (are [what besideCosts risk]
       (let [r (run beside from2 (near 18 64 2 0) {:goalFlood 0 :costs besideCosts})]
         (and (found? r) (= risk (cost r :risk))))
    "by default: 1 risk" {} 1
    "costs.besideMagmaColumn 0" {:besideMagmaColumn 0} 0
    "costs.besideMagmaColumn 3" {:besideMagmaColumn 3} 3))

(defn pools
  ([] (pools []))
  ([extra]
   (world (into [[8 64 3 18 70 3 "stone"] [8 64 2 18 80 2 "stone"] [8 64 4 18 80 4 "stone"]
                 [9 63 3 9 63 3 "soul_sand"] [9 64 3 9 70 3 "bubble_column" {:drag false}]
                 [11 63 3 15 63 3 "magma_block"] [11 64 3 15 70 3 "bubble_column" {:drag true}]]
                extra))))
(def in-lift {:x 9 :y 64 :z 3 :px 9.5 :pz 3.5})

(deftest after-leaving-the-lift-sideways-path-does-not-drop-into-the-magma-pool
  (let [snapshot (pools)
        r (run snapshot in-lift (near 17 71 3 0) {:goalFlood 0})]
    (is (not (found? r)))
    (is (every? #(not= 2 (bubble-at snapshot (:x %) (:y %) (:z %))) (steps r)))))

(deftest step-after-the-exit-may-enter-the-magma-column-going-down
  (let [r (run (pools [[16 64 3 17 65 3 "air"]]) in-lift (near 17 64 3 0) {:goalFlood 0})]
    (is (found? r))
    (is (re-find #"magma column down" (summary r)))))

;; ---- a drag column across a channel ----

(defn channel
  "A 1-wide water channel x 3..9 (y 64..66, z 1) in stone, air over it, no bank to climb out onto; a bubble column (drag as given) across it at x 6."
  [drag]
  (world [[2 64 0 10 69 2 "stone"]
          [3 64 1 9 66 1 "water"]
          [3 67 1 9 69 1 "air"]
          [6 63 1 6 63 1 (if drag "magma_block" "soul_sand")]
          [6 64 1 6 66 1 "bubble_column" {:drag drag}]]))

(deftest a-drag-column-across-a-channel-is-never-swum-sideways-into
  (let [goal (near 9 66 1 0)
        from {:x 3 :y 66 :z 1}]
    (is (not (found? (run (channel true) from goal))) "a dragging column blocks the way")
    (is (found? (run (channel false) from goal)) "a lifting column is crossed")))
