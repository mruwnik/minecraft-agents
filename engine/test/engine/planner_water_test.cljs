(ns engine.planner-water-test
  "engine/js/path/planner-water.test.mjs against the ClojureScript planner: portals, drops into and out of tight
  cells, water (ponds, drops, columns, breath, exits), the water courses, and magma."
  (:require [cljs.test :refer [deftest is are]]
            [engine.path.blocks :as blocks]
            [engine.path.courses :as courses]
            [engine.path.fixture :as fx]
            [engine.path.planner-tuned :as planner]
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

;; Respiration n drains 1/(n+1) air a second; a turtle helmet or Water Breathing leaves airGrace free seconds
(deftest respiration-lets-a-dive-twice-as-long-pass
  (is (not (found? (run-up (column 40) 40 {:costs {:airDrain 1}}))))
  (is (found? (run-up (column 40) 40 {:costs {:airDrain 0.5}}))))

(deftest water-breathing-grace-lets-a-long-dive-pass
  (is (not (found? (run-up (column 40) 40))))
  (is (found? (run-up (column 40) 40 {:costs {:airGrace 10}}))))

;; the grace's seconds are free seconds, not air: under Respiration they do not stretch. A dive of B seconds passes when
;; (B - grace) * drain <= limit, not B * drain <= limit + grace.
(deftest grace-seconds-are-not-scaled-by-the-drain
  (let [b (- (:airSupply costs) (cost (run-up (column 40) 40 {:costs {:airLimit 100}}) :airMin)) ; the dive's air seconds
        drain 0.25 grace 10
        right (fn [limit] (run-up (column 40) 40 {:costs {:airLimit limit :airDrain drain :airGrace grace}}))]
    (is (< grace b) "the dive is longer than the grace")
    (is (found? (right (+ (* (- b grace) drain) 0.5))) "just enough air after the free seconds")
    (is (not (found? (right (- (* (- b grace) drain) 0.5)))) "the free seconds are not worth grace / drain of air")))

;; a body that plans again mid-dive: costs.airUsed is the air it has already used (seconds). Start at the foot of an open
;; 6-deep shaft (x 11); a sealed tunnel (x 12..23, y 64..65) leads to a second shaft (x 24) beside the goal, and the way up
;; walks round by z 20 to it
(def dive-or-up
  (world [[10 64 -2 26 72 22 "stone"]
          [11 64 1 11 69 1 "water"] [11 70 1 11 71 1 "air"]
          [12 64 1 23 65 1 "water"]
          [24 64 1 24 69 1 "water"] [24 70 1 25 71 1 "air"]
          [11 70 2 11 71 20 "air"] [12 70 20 24 71 20 "air"] [24 70 2 24 71 19 "air"]]))
(def dive-start {:x 11 :y 64 :z 1})
(def dive-goal (near 25 70 1 0))

(deftest a-plan-started-mid-dive-counts-the-air-already-used
  (let [fresh (run dive-or-up dive-start dive-goal {:goalFlood 0})
        low (run dive-or-up dive-start dive-goal {:goalFlood 0 :costs {:airUsed 10}})]
    (is (found? fresh))
    (is (re-find #"lowest air" (summary fresh)) "full air: the tunnel")
    (is (> (- (:airSupply costs) (cost fresh :airMin)) 5) "the dive takes more than 5 s of air")
    (is (found? low) "5 s of air left: the way up")
    (is (> (cost low :seconds) (cost fresh :seconds)))
    (is (>= (cost low :airMin) 2) "it keeps the margin")))

;; go-to-air-budget's world (live: no plan, the air ran out meanwhile): a 6 deep shaft (x 4), a sealed 12 cell tunnel
;; to a second shaft (x 17), land over all, a bedrock ridge (x 11) with a gap far south. With 8 s of air used, go-to's
;; weighted search (weight 1.2) reached the shafts and the tunnel again and again with air a second apart; a new record
;; was checked against the cell's newest record only, so one an older record beat came back round and round: 200k
;; records, no plan ("budget"). Each record of the cell is checked now.
(def shafts
  (pf/snapshot {:fill [[-4 60 -4 40 63 70 "stone"] [0 64 0 31 69 63 "stone"]
                       [4 64 8 4 69 8 "water"] [5 64 8 16 65 8 "water"] [17 64 8 17 69 8 "water"]
                       [11 70 0 11 73 58 "bedrock"]]}))

(deftest a-weighted-plan-mid-dive-ends-within-the-node-budget
  (let [r (run shafts {:x 4 :y 64 :z 8} (near 18 70 8 1) {:weight 1.2 :costs {:airUsed 7.875}})]
    (is (found? r) (str (verdict r) ", expanded " (:expanded r)))))

(deftest a-plan-started-past-the-air-limit-still-swims-up
  (is (found? (run dive-or-up dive-start dive-goal {:goalFlood 0 :costs {:airUsed 14}}))))

;; a sealed tunnel (x 11..20, head in water) to an air pocket at x 21: 4.5 s of swimming. A body with 13 s of air used
;; drowns for the last 3 s of it (the exit counts)
(def drown-tunnel (world [[10 64 -2 23 68 4 "stone"] [11 64 1 20 65 1 "water"] [21 64 1 21 65 1 "air"]]))
(def drown-start {:x 11 :y 64 :z 1})

(deftest a-plan-to-air-from-under-water-prices-drowning-not-refuses
  (let [r (run drown-tunnel drown-start (near 21 64 1 0) {:goalFlood 0 :costs {:airUsed 13}})]
    (is (found? r) "the swim to air is planned")
    (is (< 4 (cost r :drown) 7) "2 hp a second past the supply (about 3 s)")
    (is (zero? (cost r :risk)) "drowning is its own cost, not risk")))

(deftest a-plan-to-air-has-no-drowning-with-the-air-to-spare
  (is (zero? (cost (run drown-tunnel drown-start (near 21 64 1 0) {:goalFlood 0 :costs {:airUsed 2}}) :drown))))

;; the goal's head cell is a ladder above the air pocket: dry, or waterlogged (the eyes are in water, the air drains)
(defn ladder-head [waterlogged]
  (world [[10 64 -2 23 68 4 "stone"] [11 64 1 20 65 1 "water"] [21 64 1 21 65 1 "air"]
          [21 65 1 21 65 1 "ladder" {:facing "west" :waterlogged waterlogged}]]))

(deftest a-goal-whose-head-cell-is-a-dry-ladder-is-air
  (is (found? (run (ladder-head false) drown-start (near 21 64 1 0) {:goalFlood 0 :costs {:airUsed 13}}))))

(deftest a-goal-whose-head-cell-is-a-waterlogged-ladder-is-not-air
  (is (not (found? (run (ladder-head true) drown-start (near 21 64 1 0) {:goalFlood 0 :costs {:airUsed 13}})))))

;; a ladder shaft (x 11, from y 64) up to a ledge at x 12: dry, or waterlogged (the head is in water the whole climb)
(defn ladder-shaft [height waterlogged]
  (world [[10 64 -2 13 (+ 66 height) 4 "stone"]
          [11 64 1 11 (+ 63 height) 1 "ladder" {:facing "west" :waterlogged waterlogged}]
          [11 (+ 64 height) 1 12 (+ 65 height) 1 "air"]]))
(defn shaft-top [height] (near 12 (+ 64 height) 1 0))

(deftest a-climb-up-a-waterlogged-ladder-drains-the-air
  (let [r (run (ladder-shaft 16 true) drown-start (shaft-top 16) {:goalFlood 0 :costs {:airUsed 13}})]
    (is (found? r) "the way to air")
    (is (> (cost r :drown) 4) "16 blocks of climbing (about 7 s) with 2 s of air left drowns")))

(deftest a-climb-up-a-dry-ladder-breathes
  (is (zero? (cost (run (ladder-shaft 16 false) drown-start (shaft-top 16) {:goalFlood 0 :costs {:airUsed 13}}) :drown))))

(deftest a-waterlogged-ladder-shaft-past-the-air-limit-is-refused-for-air
  (let [wet (run (ladder-shaft 40 true) drown-start (shaft-top 40) {:goalFlood 0})]
    (is (not (found? wet)))
    (is (= "air" (:reason wet))))
  (is (found? (run (ladder-shaft 40 false) drown-start (shaft-top 40) {:goalFlood 0}))))

;; a turtle helmet's grace seconds drown nothing: a dive within limit + grace is safe
(deftest a-dive-within-the-grace-does-not-drown
  (let [r (run-up (column 56) 56 {:costs {:airGrace 10}})]
    (is (found? r))
    (is (not (found? (run-up (column 56) 56))) "the dive is longer than the air limit")
    (is (zero? (cost r :drown)))))

(deftest a-plan-to-air-is-found-over-the-damage-budget
  (is (found? (run drown-tunnel drown-start (near 21 64 1 0) {:goalFlood 0 :damageBudget 1 :costs {:airUsed 13}}))
      "no way fits the budget: the fastest way to air still comes back"))

(deftest a-plan-to-a-goal-under-water-keeps-the-air-refusal
  (is (not (found? (run drown-tunnel drown-start (near 20 64 1 0) {:goalFlood 0 :costs {:airUsed 13}})))))

;; a go-to to the tunnel's stone ceiling (range 2): its head cell is stone, not air
(deftest a-goal-under-water-with-stone-above-is-not-air
  (is (found? (run drown-tunnel drown-start (near 20 66 1 2) {:goalFlood 0 :costs {:airUsed 2}})))
  (is (not (found? (run drown-tunnel drown-start (near 20 66 1 2) {:goalFlood 0 :costs {:airUsed 13}})))))

(deftest a-goal-under-water-is-air-only-when-its-head-cell-breathes
  (doseq [[head ok] [["stone" false] ["lava" false] ["water" false] ["air" true]]]
    (let [w (world [[10 64 -2 23 68 4 "stone"] [11 64 1 20 65 1 "water"] [20 66 1 20 66 1 head]])]
      (is (= ok (found? (run w drown-start (near 20 65 1 0) {:goalFlood 0 :costs {:airUsed 13}}))) head))))

(deftest a-goal-whose-head-cell-is-not-seen-is-not-air
  (is (not (found? (run drown-tunnel drown-start (near 21 64 1 0) {:goalFlood 0 :costs {:airUsed 13} :airSeen (fn [_ _ _] false)}))))
  (is (found? (run drown-tunnel drown-start (near 21 64 1 0) {:goalFlood 0 :costs {:airUsed 13} :airSeen (fn [_ _ _] true)}))))

;; go-to-air-budget drowning-body-swims-to-seen-air: a 2 high water tunnel (feet 64) with an air pocket in its roof (x 11..13,
;; y 66); the body 6 cells west has seen the pocket's east cell only (a 1 high slit seen edge-on through the water). The goal
;; (12 65 8, range 1) has its head in that pocket: a cell of the goal's range with seen air over it is the way to air
(def roof-pocket
  (pf/snapshot {:fill [[-4 60 -4 30 63 20 "stone"] [3 64 7 22 68 9 "stone"] [4 64 8 20 65 8 "water"] [11 66 8 13 66 8 "air"]]}))

(deftest a-goal-whose-range-has-seen-air-over-it-is-a-way-to-air
  (let [plan (fn [seen] (run roof-pocket {:x 5 :y 64 :z 8} (near 12 65 8 1) {:costs {:airUsed 12.375} :airSeen seen}))]
    (is (found? (plan (fn [x y z] (= [x y z] [13 66 8])))) "the pocket's east cell seen")
    (is (not (found? (plan (fn [_ _ _] false)))) "nothing of the pocket seen")))

(deftest past-the-air-limit-no-swim-to-a-goal-under-water
  (is (found? (run drown-tunnel drown-start (near 12 64 1 0) {:goalFlood 0 :costs {:airUsed 11}})))
  (is (not (found? (run drown-tunnel drown-start (near 12 64 1 0) {:goalFlood 0 :costs {:airUsed 14}})))))

(deftest a-plan-to-air-with-a-range-ends-with-the-head-in-air
  (let [r (run drown-tunnel drown-start (near 21 64 1 1) {:goalFlood 0 :costs {:airUsed 13}})]
    (is (found? r))
    (is (= [21 64 1] (xyz (last-step r))))))

;; at health 10 (an hp costs 20 s): a 40-block sealed tunnel (x 12..51) from the start's shaft (x 11) to the goal's (x 52)
;; drowns the body even after a breath on the ledge at its top (7 s past the supply, 14 hp); the walkway round by z 20
;; is safe but a toll makes it some 500 s dearer
(defn lethal-dive
  ([walkway] (lethal-dive walkway 52))
  ([walkway far] ; far: the x of the goal's shaft (a shorter tunnel drowns less)
   (world (cond-> [[10 64 -2 56 72 22 "stone"]
                   [11 64 1 11 69 1 "water"] [11 70 1 11 71 1 "air"]
                   [12 64 1 (dec far) 65 1 "water"]
                   [far 64 1 far 69 1 "water"] [far 70 1 (inc far) 71 1 "air"]
                   [11 70 2 11 71 2 "air"]]
            walkway (into [[11 70 2 11 71 20 "air"] [12 70 20 52 71 20 "air"] [52 70 2 52 71 19 "air"]])))))
(defn toll-walk [] {:cells (js/Map. #js [#js [(planner/cell-key 30 70 20) 3000]])})
(def at-health-10 {:goalFlood 0 :health 10 :damageWeight 20 :costs {:airUsed 12.5 :lethalAir 1}})

(deftest a-lethal-way-to-air-loses-to-a-long-safe-one
  (let [r (run (lethal-dive true) dive-start (near 53 70 1 0) (assoc at-health-10 :tolls (toll-walk)))]
    (is (found? r))
    (is (some #(= 20 (:z %)) (steps r)) "the long way round")
    (is (some? (cost r :drown)) "the plan reports its drowning")
    (is (< (cost r :drown) 10) "not drowned")))

(deftest a-lethal-way-to-air-is-planned-when-it-is-the-only-one
  (let [r (run (lethal-dive false) dive-start (near 53 70 1 0) at-health-10)]
    (is (found? r))
    (is (>= (cost r :drown) 10) "it drowns: still the only way")
    (is (< (cost r :drown) 20) "the least drowning: a breath at the top first")))

(deftest a-lethal-way-to-air-is-refused-unless-the-caller-allows-it
  (let [r (run (lethal-dive false) dive-start (near 53 70 1 0) (assoc-in at-health-10 [:costs :lethalAir] 0))]
    (is (not (found? r)))
    (is (= "air-lethal" (:reason r)))))

(deftest the-default-lethal-refusal-still-finds-the-long-safe-way
  (let [r (run (lethal-dive true) dive-start (near 53 70 1 0) (-> at-health-10 (update :costs dissoc :lethalAir) (assoc :tolls (toll-walk))))]
    (is (found? r))
    (is (some #(= 20 (:z %)) (steps r)) "the long way round")
    (is (some? (cost r :drown)) "the plan reports its drowning")
    (is (< (cost r :drown) 10) "not drowned")))

(def no-lethal-air #(update % :costs dissoc :lethalAir))

(deftest drowning-counts-its-hits-with-a-margin
  ;; 17.5 hp of drowning is 8.75 s past the supply: 8 exact 2 hp hits, but the counter may already be below 0 (a body at
  ;; oxygen 0) or the swim slower than planned, so it counts ceil(8.75 + drownSlack 1) = 10 hits (20 hp)
  (let [lethal (fn [opts] (run (lethal-dive false) dive-start (near 53 70 1 0) (-> at-health-10 no-lethal-air (merge opts))))]
    (is (= "air-lethal" (:reason (lethal {:health 17}))) "9 hits can kill 17 hp")
    (is (= "air-lethal" (:reason (lethal {:health 20}))) "the margin's 10 hits kill 20 hp")
    (is (found? (lethal {:health 19 :costs {:airUsed 12.5 :drownSlack 0}})) "drownSlack 0: ceil(8.75) = 9 hits leave 1 hp")
    (is (= "air-lethal" (:reason (lethal {:health 18 :costs {:airUsed 12.5 :drownSlack 0}}))))))

(deftest a-drowning-with-a-real-margin-is-still-planned
  ;; a 30-block tunnel: about 3.75 s past the supply, counted as ceil(3.75 + 1) = 5 hits (10 hp)
  (let [r (run (lethal-dive false 42) dive-start (near 43 70 1 0) (-> at-health-10 no-lethal-air (assoc :health 12)))]
    (is (found? r))
    (is (< 0 (cost r :drown) 10) "it drowns some, short of the counted 10 hp")))

(deftest drain-scales-the-lowest-air-reading
  (let [full (cost (run-up (column 20) 20) :airMin)
        half (cost (run-up (column 20) 20 {:costs {:airDrain 0.5}}) :airMin)]
    (is (> half full))))

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
    30 true false "air"))

;; a stretch of open water (feet cell water, air over it) in a 30-block tunnel: the head breathes there for the seconds
;; the body spends in it, 4 s of air a second
(defn tunnel-with-surface [open]
  (let [mid (+ 11 (bit-shift-right 30 1))]
    (world [[10 64 -2 42 68 40 "stone"]
            [11 64 1 40 65 1 "water"]
            [10 64 1 10 65 1 "air"]
            [41 64 1 42 65 1 "air"]
            [(- mid (bit-shift-right open 1)) 65 1 (+ mid (bit-shift-right open 1)) 65 1 "air"]])))

(deftest a-long-surface-swim-refills-the-air-a-short-touch-does-not
  (are [open reachable] (= reachable (found? (run (tunnel-with-surface open) from2 (near 43 64 1 0) {:goalFlood 0})))
    14 true
    1 false))

(deftest a-surfacing-in-the-shaft-gives-back-4-s-a-second-not-a-breath
  (let [r (run (tunnel 30 true) from2 (near 43 64 1 0) {:goalFlood 0})]
    (is (= "air" (:reason r)))))

;; a sealed tunnel 3 high (y 64..66) whose middle cell is a dragging column over magma: the head breathes in it, but only
;; for the moments the body spends there
(defn column-tunnel [len]
  (let [mid (+ 11 (bit-shift-right len 1))]
    (world [[10 64 -2 (+ 12 len) 68 40 "stone"]
            [11 64 1 (+ 10 len) 66 1 "water"]
            [10 64 1 10 65 1 "air"]
            [(+ 11 len) 64 1 (+ 12 len) 65 1 "air"]
            [mid 63 1 mid 63 1 "magma_block"]
            [mid 64 1 mid 66 1 "bubble_column" {:drag true}]])))

(deftest a-short-drag-column-is-no-air-station
  (let [goal (near 49 64 1 0)]
    (is (= "air" (:reason (run (column-tunnel 36) from2 goal {:goalFlood 0})))
        "two 9 s dives either side of a column ridden a block: the column gives back 4 s of air a second, not a full breath")
    (is (found? (run (column-tunnel 36) from2 goal {:goalFlood 0 :costs {:airLimit 24}})))))

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

(deftest wading-out-is-priced-by-the-gait
  (let [seconds (fn [costs] (cost (run (wade 0) {:x 12 :y 64 :z 2} (near 13 64 2 0) {:goalFlood 0 :margin 8 :costs costs}) :seconds))]
    (is (> (seconds {:walkS 5 :sprintS 5}) 4.5))))

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
  (are [what snapshot goal status reason]
       (let [r (run snapshot from2 goal {:goalFlood 0})]
         (and (= [status reason] [(:status r) (:reason r)]) (not (on-magma? snapshot (last-step r)))))
    "a goal range 1 around a magma block with stone beside it" (world [[9 63 2 10 63 2 "magma_block"]]) (near 10 64 2 1) "found" nil
    "a goal on a magma block: no cell in range a route may end on" (world [[10 63 2 10 63 2 "magma_block"]]) (near 10 64 2 0) "none" "goal-on-magma"
    "a goal in a magma bubble column" (column 20 bubble-down) (near 11 74 1 0) "none" "goal-on-magma"
    "a goal range 1 in a magma bubble column" (column 20 bubble-down) (near 11 74 1 1) "none" "goal-on-magma"))

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

(deftest step-after-the-exit-may-enter-the-magma-column-to-come-back-up-beside-it
  (let [r (run (pools [[16 64 3 16 65 3 "air"] [17 64 3 17 70 3 "water"]]) in-lift (near 17 70 3 0) {:goalFlood 0})]
    (is (found? r) "down the magma column, out at its bottom, up the water beside it")
    (is (re-find #"magma column down" (summary r)))))

;; ---- a drag column: a fast forced descent to its bottom ----

(defn channel
  "A 1-wide water channel x 3..9 (y 64..63+depth, z 1) in stone, air over it, no bank to climb out onto; a bubble column
  (drag as given) across it at x 6."
  ([drag] (channel drag 3))
  ([drag depth]
   (let [top (+ 63 depth)]
     (world [[2 64 0 10 (+ top 3) 2 "stone"]
             [3 64 1 9 top 1 "water"]
             [3 (inc top) 1 9 (+ top 3) 1 "air"]
             [6 63 1 6 63 1 (if drag "magma_block" "soul_sand")]
             [6 64 1 6 top 1 "bubble_column" {:drag drag}]]))))

(defn in-column [r] (filterv #(= [6 1] [(:x %) (:z %)]) (steps r)))
(defn leaves-column-at [r] (some (fn [[a b]] (when (and (= [6 1] [(:x a) (:z a)]) (not= [6 1] [(:x b) (:z b)])) [a b]))
                                 (partition 2 1 (steps r))))

(deftest a-drag-column-across-a-channel-is-ridden-to-its-bottom
  (let [r (run (channel true) {:x 3 :y 66 :z 1} (near 9 66 1 0))
        [out next] (leaves-column-at r)]
    (is (found? r) "with no other way the dragging column is crossed")
    (is (= 64 (:y out) (:y next)) "the body leaves it only at its bottom, over the magma")
    (is (= [66 65 64] (mapv :y (in-column r))) "pulled down a block at a time, never sideways or up inside it")
    (is (re-find #"magma column down" (summary r)))))

(deftest a-drag-column-descent-costs-bubble-down-a-block
  (let [from {:x 3 :y 66 :z 1}
        goal (near 9 66 1 0)
        ridden (run (channel true) from goal)
        dear (run (channel true) from goal {:costs {:bubbleDown 1.12}})]
    (is (< (cost ridden :seconds) 15) "no flat price")
    (is (= [66 65 64] (mapv :y (in-column ridden))) "a fast descent is ridden")
    (is (= [64] (mapv :y (in-column dear))) "a dear one is swum down beside it, the column crossed at its bottom")))

(deftest a-body-in-a-drag-column-goes-down-first
  (let [r (run (channel true) {:x 6 :y 66 :z 1} (near 9 66 1 0))]
    (is (found? r))
    (is (= 64 (:y (first (leaves-column-at r)))))))

(deftest a-drag-column-whose-swim-back-up-is-out-of-breath-is-not-crossed
  (let [from {:x 3 :y 108 :z 1}
        goal (near 9 108 1 0)]
    (is (not (found? (run (channel true 45) from goal))) "44 blocks back up under water is over the air limit")
    (is (found? (run (channel true 45) from goal {:costs {:airLimit 20}})) "a caller with more breath crosses it")
    (is (found? (run (channel false 45) from goal)) "a lifting column is swum across")))

(defn shelf-channel
  "channel, but west of the column the water is 1 deep (y 66) on stone: the only way east is into the column's top."
  []
  (world [[2 64 0 10 69 2 "stone"]
          [3 66 1 5 66 1 "water"]
          [7 64 1 9 66 1 "water"]
          [3 67 1 9 69 1 "air"]
          [6 63 1 6 63 1 "magma_block"]
          [6 64 1 6 66 1 "bubble_column" {:drag true}]]))

(deftest a-way-into-a-drag-column-above-its-bottom-is-one-way
  (let [r (run (shelf-channel) {:x 3 :y 66 :z 1} (near 20 64 1 0) {:goalFlood 0})
        ow (:oneWay r)]
    (is (= "partial" (:status r)))
    (is (= [6 66 1] [(:x ow) (:y ow) (:z ow)]) "the swim into the column's top cannot be undone")
    (is (every? #(< (:x %) 6) (steps r)) "the partial path stops before it")))

(deftest a-returnable-search-crosses-a-drag-column-only-at-its-bottom
  (let [r (run (channel true) {:x 3 :y 66 :z 1} (near 9 66 1 0) {:returnable true})]
    (is (found? r))
    (is (= [64] (mapv :y (in-column r))))))

(defn wide-channel
  "channel 3 wide (z 0..2), a dragging column in its middle row only at x 6."
  []
  (world [[2 64 -1 10 69 3 "stone"]
          [3 64 0 9 66 2 "water"]
          [3 67 0 9 69 2 "air"]
          [6 63 1 6 63 1 "magma_block"]
          [6 64 1 6 66 1 "bubble_column" {:drag true}]]))

(deftest a-drag-column-with-a-way-round-is-swum-round
  (let [r (run (wide-channel) {:x 3 :y 66 :z 1} (near 9 66 1 0))]
    (is (found? r))
    (is (not-any? #(= [6 1] [(:x %) (:z %)]) (steps r)) "never in the dragging column")))

(defn drag-corner
  "Water x 3..5 at z 0 and x 6..9 at z 1 in stone, air over the water; a dragging column at (6, z 0): the only ways from one
  to the other enter it or brush its corner on a diagonal."
  []
  (world [[2 64 -1 10 69 2 "stone"]
          [3 64 0 6 66 0 "water"]
          [6 64 1 9 66 1 "water"]
          [3 67 0 6 69 0 "air"]
          [6 67 1 9 69 1 "air"]
          [6 63 0 6 63 0 "magma_block"]
          [6 64 0 6 66 0 "bubble_column" {:drag true}]]))

(deftest a-diagonal-swim-never-brushes-a-drag-column
  (let [r (run (drag-corner) {:x 3 :y 66 :z 0} (near 9 66 1 0))
        pairs (partition 2 1 (steps r))]
    (is (found? r))
    (is (not-any? (fn [[a b]] (and (= [5 0] [(:x a) (:z a)]) (= [6 1] [(:x b) (:z b)]))) pairs) "no diagonal past the column")
    (is (some #(= [6 64 0] (xyz %)) (steps r)) "the way is down the column and out at its bottom")))

(deftest a-drop-into-a-drag-column-top-rides-it-to-the-bottom
  (let [snapshot (world [[2 64 0 10 72 2 "stone"]
                         [3 64 1 9 66 1 "water"]
                         [3 67 1 9 72 1 "air"]
                         [5 64 1 5 68 1 "stone"]
                         [6 63 1 6 63 1 "magma_block"]
                         [6 64 1 6 66 1 "bubble_column" {:drag true}]])
        r (run snapshot {:x 5 :y 69 :z 1} (near 9 66 1 0))]
    (is (found? r) "off the pillar into the column's top")
    (is (= [66 65 64] (mapv :y (in-column r))))
    (is (= 64 (:y (first (leaves-column-at r)))))))

;; a 3 s air supply: three dives of 6 water blocks (3.5 s each, from airUsed 0.5 and after 3-block surface windows that
;; give the air back), each 0.5 s past the supply (1 hp). Each air-out is its own count of 2 hp hits, ceil(0.5 + slack):
;; with drownSlack 1, 3 x 2 hits = 12 hp, not ceil(1.5 + 1) = 3 hits of the summed 3 hp
(def air-outs
  (world [[10 64 -2 37 68 4 "stone"]
          [11 64 1 34 65 1 "water"]
          [17 65 1 19 65 1 "air"] [26 65 1 28 65 1 "air"]
          [35 64 1 35 65 1 "air"]]))
(defn run-air-outs [health slack]
  (run air-outs drown-start (near 35 64 1 0) {:goalFlood 0 :health health :costs {:airSupply 3 :airUsed 0.5 :drownSlack slack}}))

(deftest each-air-out-counts-its-own-hits
  (let [r (run-air-outs 13 1)]
    (is (found? r))
    (is (= 3 (cost r :drown)) "three air-outs of 1 hp"))
  (is (= "air-lethal" (:reason (run-air-outs 12 1))) "3 x ceil(0.5 + 1) hits = 12 hp")
  (is (= "air-lethal" (:reason (run-air-outs 6 0))) "3 x ceil(0.5) hits = 6 hp")
  (is (found? (run-air-outs 7 0))))

;; a 3 s air supply: a 6-block dive to an air pocket (x 17) from airUsed 1 drowns 1 s (2 hp, counted ceil(1 + 1) hits =
;; 4 hp), then a 5-block drop out of the pocket takes 2 hp of fall damage: 6 hp certain on the plan
(def drown-then-drop
  (world [[10 64 -2 22 68 4 "stone"] [16 50 -2 24 58 4 "stone"]
          [11 64 1 16 65 1 "water"] [17 64 1 17 65 1 "air"] [18 59 1 20 65 1 "air"]]))
(defn run-drown-then-drop [health]
  (run drown-then-drop drown-start (near 19 59 1 0) {:goalFlood 0 :maxDrop 6 :health health :costs {:airSupply 3 :airUsed 1}}))

(deftest certain-damage-on-the-plan-adds-to-the-lethal-drowning-count
  (let [r (run-drown-then-drop 7)]
    (is (found? r))
    (is (= 2 (cost r :drown)))
    (is (= 2 (cost r :damage)) "the fall"))
  (is (= "air-lethal" (:reason (run-drown-then-drop 6))) "4 counted hp of drowning and the 2 hp fall"))

;; a lethal plan loses to any other whatever the caller's prices: a toll or a danger on the walkway dearer than LETHAL-S
(deftest a-lethal-way-to-air-loses-to-a-safe-one-at-any-toll
  (let [r (run (lethal-dive true) dive-start (near 53 70 1 0)
               (assoc at-health-10 :tolls {:cells (js/Map. #js [#js [(planner/cell-key 30 70 20) 1e9]])}))]
    (is (found? r))
    (is (some #(= 20 (:z %)) (steps r)) "the long way round")))

(deftest a-lethal-way-to-air-loses-to-a-safe-one-at-any-danger-rate
  (let [r (run (lethal-dive true) dive-start (near 53 70 1 0)
               (assoc at-health-10 :dangerCap 1e9 :dangers [{:x 30.5 :y 70 :z 20.5 :close 1 :radius 2 :rate 1e9}]))]
    (is (found? r))
    (is (some #(= 20 (:z %)) (steps r)) "the long way round")))

;; a waterlogged ladder shaft (x 11) with a ledge (x 12..13, y 70) whose first cell stands on magma (1 hp): from the
;; ladder at y 70 with 14.9 s of air used, the step off it onto the magma (head in water at its start) is the plan's first
;; drowning move and takes the hp itself: counted 4 hp of drowning (ceil(t + 1) hits) and 1 hp
(def magma-ledge
  (world [[10 64 -2 14 80 4 "stone"]
          [11 64 1 11 75 1 "ladder" {:facing "east" :waterlogged true}]
          [12 70 1 13 71 1 "air"] [12 69 1 12 69 1 "magma_block"]]))
(defn run-magma-ledge [health]
  (run magma-ledge {:x 11 :y 70 :z 1} (near 13 70 1 0) {:goalFlood 0 :health health :costs {:airUsed 14.9}}))

(deftest the-first-drowning-move-counts-its-own-damage
  (let [r (run-magma-ledge 6)]
    (is (found? r))
    (is (pos? (cost r :drown)))
    (is (= 1 (cost r :damage)) "the magma"))
  (is (= "air-lethal" (:reason (run-magma-ledge 5))) "4 counted hp of drowning and the magma's 1"))

;; drown-then-drop with a way round the 5-block drop: a 3-block drop at z 2 and a 2-block one into the shaft, no damage.
;; Damage is all but free (riskWeight 0.01), so the drop is the plan until the drowning and its 2 hp reach health; with lethalAir 1
;; the drop is then priced lethal and loses to the way round
(def drown-then-drop-or-round
  (world [[10 64 -2 22 68 4 "stone"] [16 50 -2 24 58 4 "stone"]
          [11 64 1 16 65 1 "water"] [17 64 1 17 65 1 "air"] [18 59 1 20 65 1 "air"]
          [17 61 2 17 65 2 "air"] [18 61 2 18 62 2 "air"]]))
(defn run-drop-or-round [health]
  (run drown-then-drop-or-round drown-start (near 19 59 1 0)
       {:goalFlood 0 :maxDrop 6 :health health :riskWeight 0.01 :costs {:airSupply 3 :airUsed 1 :lethalAir 1}}))

(deftest with-lethal-air-certain-damage-makes-a-plan-lethal
  (is (= 2 (cost (run-drop-or-round 7) :damage)) "4 counted hp and the 2 hp drop: not lethal")
  (let [r (run-drop-or-round 6)]
    (is (found? r))
    (is (= 0 (cost r :damage)) "the way round")))

;; a toll of Infinity on a cell off the way (a direct planner caller; go-to's spec refuses it) leaves the lethal plan priced
(deftest a-lethal-plan-is-found-beside-an-infinite-toll
  (let [r (run (lethal-dive false) dive-start (near 53 70 1 0)
               (assoc at-health-10 :tolls {:cells (js/Map. #js [#js [(planner/cell-key 30 70 20) js/Infinity]])}))]
    (is (found? r))
    (is (>= (cost r :drown) 10))))

(deftest lethal-seconds-covers-the-dearest-drowning-second
  (is (>= (planner/lethal-seconds 2 2 8 0 nil 0 0 #js [0 1e3 2e3]) (* 1e6 2 1e3)) "2 hp a second at 1000 s an hp")
  (is (js/isFinite (planner/lethal-seconds 2 2 js/Infinity 0 (js/Map. #js [#js [1 js/Infinity]]) 0 0 nil)))
  (is (js/isFinite (planner/lethal-seconds 0 0 js/NaN 0 nil 0 0 nil)) "riskWeight 0: a NaN danger cap"))

(def bush-bank (world [[10 61 -2 20 63 40 "water"] [21 64 -2 21 64 40 "sweet_berry_bush"]]))

(deftest a-swim-exit-into-a-berry-bush-counts-its-damage
  (let [taken (run bush-bank from2 (near 26 64 2) {:damageBudget 5 :damageWeight 1})
        refused (run bush-bank from2 (near 26 64 2) {:damageBudget 0 :damageWeight 1})]
    (is (found? taken))
    (is (pos? (cost taken :damage)))
    (is (not (found? refused)))))

(deftest a-drag-column-is-ridden-from-a-platform-and-left-at-its-foot-into-a-pocket
  ;; the go-to-bubble-column fixture's drag case (plot y 0 = 64): the pocket beside the column foot is reached only by the ride
  (let [snapshot (world [[0 64 14 3 69 18 "stone"]
                         [4 63 14 8 69 18 "stone"]
                         [3 63 16 3 63 16 "magma_block"]
                         [3 64 16 3 69 16 "bubble_column" {:drag true}]
                         [2 64 16 2 65 16 "water"]])
        r (run snapshot {:x 6 :y 70 :z 16} (near 2 64 16 0))]
    (is (found? r))
    (is (re-find #"magma column down" (summary r)))
    (is (= [2 64 16] (xyz (last-step r))))))
