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
          :airSupply 15 :airLimit 12 :airDrain 1 :airGrace 0 :maxWaterDrop 64 :dripleaf 0.2}
         (select-keys costs [:swimH :swimUp :swimDown :exit :current :bubbleUp :bubbleDown :airSupply :airLimit :airDrain :airGrace
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

(deftest a-plan-started-past-the-air-limit-still-swims-up
  (is (found? (run dive-or-up dive-start dive-goal {:goalFlood 0 :costs {:airUsed 14}}))))

;; a sealed tunnel (x 11..20, head in water) to an air pocket at x 21: 4.5 s of swimming. A body with 13 s of air used
;; drowns for the last 3 s of it (the exit counts)
(def drown-tunnel (world [[10 64 -2 23 68 4 "stone"] [11 64 1 20 65 1 "water"] [21 64 1 21 65 1 "air"]]))
(def drown-start {:x 11 :y 64 :z 1})

(deftest a-plan-to-air-from-under-water-prices-drowning-not-refuses
  (let [r (run drown-tunnel drown-start (near 21 64 1 0) {:goalFlood 0 :costs {:airUsed 13}})]
    (is (found? r) "the swim to air is planned")
    (is (<= 4 (cost r :risk) 7) "2 hp of risk a second past the supply (about 3 s)")))

(deftest a-plan-to-air-has-no-drowning-risk-with-the-air-to-spare
  (is (zero? (cost (run drown-tunnel drown-start (near 21 64 1 0) {:goalFlood 0 :costs {:airUsed 2}}) :risk))))

(deftest a-plan-to-air-is-found-over-the-damage-budget
  (is (found? (run drown-tunnel drown-start (near 21 64 1 0) {:goalFlood 0 :damageBudget 1 :costs {:airUsed 13}}))
      "no way fits the budget: the fastest way to air still comes back"))

(deftest a-plan-to-a-goal-under-water-keeps-the-air-refusal
  (is (not (found? (run drown-tunnel drown-start (near 20 64 1 0) {:goalFlood 0 :costs {:airUsed 13}})))))

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
