(ns engine.planner-climb-test
  "engine/js/path/planner-climb.test.mjs against the ClojureScript planner: ladders, vines, scaffolding and trapdoors
  over ladders, on the live courses and on small hand-built worlds."
  (:require [cljs.test :refer [deftest is are]]
            [engine.planner-fixture :as pf :refer [world near plan]]))

(defn course [name] (.courseSnapshot ^js @pf/courses name))
(defn course-goal [name] (js->clj (.-goal ^js (course name)) :keywordize-keys true))
(defn plan-course
  ([name] (plan-course name {}))
  ([name options] (pf/course-plan name options)))
(defn verdict [r] (str (:status r) (some->> (:reason r) (str "/"))))
(defn move-set [r] (set (map :move (get-in r [:path :steps]))))
(defn cost [r] (get-in r [:path :cost]))
(defn summary [r] (get-in r [:path :summary]))
(defn steps [r] (get-in r [:path :steps]))
(defn near1 [x y z] (pf/near x y z 1))
(defn close? [a b] (< (js/Math.abs (- a b)) 1e-6))
(defn m [k] (get pf/MOVE k))

(deftest courses-found
  (are [name]
       (let [r (plan-course name)
             goal (course-goal name)
             end (peek (steps r))]
         (and (= "found" (verdict r))
              (<= (js/Math.hypot (- (:x end) (:x goal)) (- (:z end) (:z goal))) 1.5)
              (<= (js/Math.abs (- (:y end) (:y goal))) 1)))
    "ladder-up" "ladder-down" "vine-up" "vine-down" "ladder6" "vines6"
    "lad-shaft20-up" "lad-shaft20-down" "lad-wall20-up" "lad-wall20-down"
    "lad-raised1" "lad-trap-closed" "twisting-up" "scaffold-up" "scaffold-down" "weeping"))

(deftest courses-use-move
  (are [name k] (contains? (move-set (plan-course name)) (m k))
    "lad-raised1" :jump-climb
    "lad-trap-closed" :open
    "ladder-up" :climb-up
    "vine-up" :climb-up
    "twisting-up" :climb-up
    "scaffold-up" :climb-up
    "ladder-down" :climb-down
    "vine-down" :climb-down
    "scaffold-down" :climb-down
    "lad-wall20-down" :climb-down))

(deftest closed-trapdoor-over-ladder-is-one-open-with-block-position
  (let [r (plan-course "lad-trap-closed")
        opens (filter #(= (m :open) (:move %)) (steps r))]
    (is (= 1 (count opens)))
    (is (= [{:x 2880 :y 166 :z 3216}] (:opens (first opens))))
    (is (= 1 (:opens (cost r))))))

;; hand-built worlds: stone floor to y 63, so feet cells are y 64
(def from5 {:x 2 :y 64 :z 5})
(defn run5 [snapshot from goal options] (plan snapshot {:from from :goal goal} options))

(defn trap-shaft
  "a one-wide shaft with a ladder (facing west) up to a trapdoor ceiling; the cap beside it is the goal's floor"
  [trap props]
  (world {:fill [[8 64 4 10 72 6 "stone"]
                 [9 64 5 9 72 5 "air"]
                 [8 64 5 8 65 5 "air"]
                 [11 72 4 15 72 6 "stone"]
                 [9 64 5 9 71 5 "ladder" {:facing "west"}]
                 [9 72 5 9 72 5 trap (merge {:half "top"} props)]]}))
(defn trap-over [props] (trap-shaft "oak_trapdoor" props))

(deftest open-trapdoor-over-ladder-facing-differently-climbed-without-open
  (let [r (run5 (trap-over {:open true :facing "east"}) from5 (near 13 73 5) {})]
    (is (= 0 (:opens (cost r))))
    (is (>= (:climbed (cost r)) 5))))

(deftest course-lad-trap-open-not-found
  (is (not= "found" (:status (plan-course "lad-trap-open")))))

(deftest cost-vector-counts-blocks-climbed
  (is (= 5 (:climbed (cost (plan-course "ladder-up")))))
  (is (= 4 (:climbed (cost (plan-course "ladder-down" {:maxDrop 1})))))
  (is (= 0 (:climbed (cost (plan-course "weeping"))))))

(deftest course-summaries
  (are [name pattern] (re-find pattern (summary (plan-course name)))
    "ladder-up" #"ladder up 5"
    "vine-down" #"vines down"
    "scaffold-up" #"scaffolding up"
    "lad-trap-closed" #"opens 1 trapdoor"))

(deftest climbed-block-costs-climb-seconds-overridable
  (let [base (cost (plan-course "ladder-up"))
        slow (cost (plan-course "ladder-up" {:costs {:climbUp 5}}))
        down (cost (plan-course "ladder-down"))
        slow-down (cost (plan-course "ladder-down" {:costs {:climbDown 3}}))]
    (is (close? (:seconds slow)
                (+ (:seconds base) (* (:climbed base) (- 5 (:climbUp pf/default-costs))))))
    (is (close? (:seconds slow-down)
                (+ (:seconds down) (* (:climbed down) (- 3 (:climbDown pf/default-costs))))))))

(deftest open-costs-costs-open-seconds
  (let [base (:seconds (cost (plan-course "lad-trap-closed")))
        dear (:seconds (cost (plan-course "lad-trap-closed" {:costs {:open 11}})))]
    (is (close? dear (+ base (- 11 (:open pf/default-costs)))))))

(deftest default-costs-are-vanilla-rates
  (is (= {:up 0.43 :down 0.33 :open 1.0}
         {:up (:climbUp pf/default-costs) :down (:climbDown pf/default-costs) :open (:open pf/default-costs)})))

(deftest lad-midlanding-tunnel-entered-at-170
  (let [r (plan-course "lad-midlanding")
        ss (steps r)
        into (first (keep-indexed (fn [i s] (when (and (= 2881 (:x s)) (= 171 (:y s))) i)) ss))
        before (nth ss (dec (or into 0)) nil)]
    (is (= "found" (verdict r)))
    (is (re-find #"ladder up" (summary r)))
    (is (and (some? into) (> into 0)))
    (is (= [2880 170] [(:x before) (:y before)]))))

(deftest ladder-down-by-default-steps-off-into-fall-of-3
  (let [r (plan-course "ladder-down")]
    (is (= "found" (verdict r)))
    (is (= 0 (:risk (cost r))))
    (is (= 3 (:maxDrop (cost r))))))

(deftest lad-gap-going-up-refused-with-own-reason
  (let [r (plan-course "lad-gap")]
    (is (not= "found" (:status r)))
    (is (= "ladder-gap" (:reason r)))))

(deftest lad-gap-going-down-caught-by-ladder-below
  (let [snapshot (.-snapshot (course "lad-gap"))
        r (plan snapshot {:from {:x 2886 :y 171 :z 3216 :px 2886.5 :pz 3216.5} :goal (near 2860 161 3216)})
        ss (vec (steps r))]
    (is (= "found" (verdict r)))
    (is (some true? (map-indexed (fn [k s] (and (= (m :drop) (:move s)) (pos? k)
                                                (= 2880 (:x (ss (dec k)))) (= 165 (:y (ss (dec k))))))
                                 ss)))))

(def shaft
  "a ladder up the west face of a stone block 10 high, exit at the top"
  (world {:fill [[10 64 0 20 73 10 "stone"]
                 [9 64 5 9 73 5 "ladder" {:facing "west"}]]}))

(deftest ladder-up-wall-10-high
  (let [r (run5 shaft from5 (near1 15 74 5) {})]
    (is (= "found" (:status r)))
    (is (>= (:climbed (cost r)) 9))
    (is (re-find #"ladder up (9|10)" (summary r)))))

(deftest closed-trapdoor-over-ladder-by-material
  (are [trap statuses] (contains? statuses (:status (run5 (trap-shaft trap {:open false :facing "east"}) from5 (near 13 73 5) {})))
    "oak_trapdoor" #{"found"}
    "copper_trapdoor" #{"found"}
    "iron_trapdoor" #{"partial" "none"}))

(deftest trapdoor-over-ladder-facing-west
  (are [props status opens]
       (let [r (run5 (trap-over props) from5 (near 13 73 5) {:goalFlood 0})]
         (and (= status (:status r))
              (= opens (count (filter #(= (m :open) (:move %)) (steps r))))))
    {:open true :facing "east"} "found" 0
    {:open true :facing "south"} "found" 0
    {:open true :facing "west"} "partial" 0
    {:open false :facing "east"} "found" 1
    {:open false :facing "north"} "found" 1
    {:open false :facing "west"} "partial" 0))

(def tower6
  (world {:fill [[9 64 5 9 69 5 "scaffolding" {:bottom false :waterlogged false :stability_distance 0}]]}))

(deftest scaffolding-tower
  (are [from goal options pattern]
       (let [r (run5 tower6 from goal (merge {:goalFlood 0} options))]
         (and (= "found" (:status r))
              (re-find pattern (summary r))
              (>= (:climbed (cost r)) 5)))
    {:x 5 :y 64 :z 5} (near 9 70 5 0) {} #"scaffolding up"
    {:x 9 :y 70 :z 5 :px 9.5 :pz 5.5} (near 5 64 5 0) {:maxDrop 0} #"scaffolding down"))

(deftest scaffolding-wall-walked-through-at-ground-level
  (let [wall (world {:fill [[8 64 -2 8 66 40 "scaffolding" {:bottom false :waterlogged false :stability_distance 0}]]})
        r (run5 wall from5 (near 14 64 5 0) {:goalFlood 0})]
    (is (= "found" (:status r)))
    (is (some #(and (= 8 (:x %)) (= 64 (:y %))) (steps r)))
    (is (= 0 (:climbed (cost r))))))

(def vine-shaft
  "weeping vines from the top (y 69) to y 65, floor under them at y 64"
  (world {:fill [[5 64 0 14 69 10 "stone"]
                 [13 64 5 13 69 5 "air"]
                 [14 64 5 14 65 5 "air"]
                 [13 65 5 13 68 5 "weeping_vines_plant"]
                 [13 69 5 13 69 5 "weeping_vines"]]}))

(deftest weeping-vines-down-a-shaft
  (let [r (run5 vine-shaft {:x 8 :y 70 :z 5} (near1 18 64 5) {})]
    (is (= "found" (:status r)))
    (is (>= (:climbed (cost r)) 4))
    (is (re-find #"vines down" (summary r)))))

(deftest same-vines-going-up
  (let [r (run5 vine-shaft {:x 18 :y 64 :z 5} (near1 8 70 5) {})]
    (is (= "found" (:status r)))
    (is (re-find #"vines up" (summary r)))))

(def sides {:front [8 5] :north [9 4] :south [9 6]})
(defn tower
  "a ladder 12 high up the west face of a stone wall; one standing block on the ladder's front or side at height h"
  [[ix iz] h]
  (world {:fill [[10 64 0 14 80 10 "stone"]
                 [9 64 5 9 75 5 "ladder" {:facing "west"}]
                 [ix (+ 63 h) iz ix (+ 63 h) iz "stone"]]}))

(defn up-query [[ix iz] h]
  (let [island {:x ix :y (+ 64 h) :z iz}]
    [{:x 2 :y 64 :z 5} (assoc island :kind "near" :range 0)]))
(defn down-query [[ix iz] h]
  [{:x ix :y (+ 64 h) :z iz :px (+ ix 0.5) :pz (+ iz 0.5)} (near 2 64 5 0)])

(deftest ladder-shaft-12-high-stepping-off-at-height
  (are [query side h pattern]
       (let [[from goal] (query (sides side) h)
             r (plan (tower (sides side) h) {:from from :goal goal} {:goalFlood 0 :maxDrop 0})]
         (and (= "found" (:status r))
              (>= (:climbed (cost r)) (dec h))
              (re-find pattern (summary r))))
    up-query :front 4 #"ladder up"
    down-query :front 4 #"ladder down"
    up-query :front 8 #"ladder up"
    down-query :front 8 #"ladder down"
    up-query :north 4 #"ladder up"
    down-query :north 4 #"ladder down"
    up-query :north 8 #"ladder up"
    down-query :north 8 #"ladder down"
    up-query :south 4 #"ladder up"
    down-query :south 4 #"ladder down"
    up-query :south 8 #"ladder up"
    down-query :south 8 #"ladder down"))
