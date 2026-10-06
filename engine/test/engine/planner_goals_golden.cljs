(ns engine.planner-goals-golden
  "Goal sets in the planner (query.goals): one search to the nearest by walking cost of many goals. The goal test is
  'in any goal's area', the heuristic the least over the goals, and the result's goal names the goal reached."
  (:require [cljs.test :refer [deftest is]]
            [engine.planner-bench-golden :as bench]
            [engine.planner-fixture :as pf :refer [near xz last-cell]]))

(defn cost [r] (when (= "found" (:status r)) (+ (get-in r [:path :cost :seconds]) (* 2 (get-in r [:path :cost :risk])))))

(defn close? [a b] (< (js/Math.abs (- a b)) 1e-6))

;; a cell walled in on its four sides and roofed: no move enters it
(defn sealed [x z]
  [[(dec x) 64 z (dec x) 65 z "stone"] [(inc x) 64 z (inc x) 65 z "stone"]
   [x 64 (dec z) x 65 (dec z) "stone"] [x 64 (inc z) x 65 (inc z) "stone"] [x 66 z x 66 z "stone"]])

(def walled-near (pf/world {:fill (sealed 6 2)}))

(deftest one-goal-in-the-set-plans-as-the-single-goal
  (let [g (near 20 64 9 1)
        single (pf/run walled-near g {:goalFlood 0})
        multi (pf/plan walled-near {:from pf/start :goals [g]} {})]
    (is (= "found" (:status single) (:status multi)))
    (is (= (:path single) (:path multi)))
    (is (= (:expanded single) (:expanded multi)))
    (is (= 0 (:goal multi)))))

(deftest a-walled-off-nearest-goal-is-passed-over-for-a-reachable-farther-one
  (let [r (pf/plan walled-near {:from pf/start :goals [(near 6 64 2 0) (near 20 64 2 0)]} {})]
    (is (= "found" (:status r)))
    (is (= 1 (:goal r)))
    (is (= [20 64 2] (last-cell r)))))

(deftest the-nearest-by-walking-cost-wins-over-the-nearest-by-straight-line
  ;; a wall across z = 6 from x -2 to 30: the goal at z 9 is 7 blocks off in a line but ~60 to walk; the one at x 22 wins
  (let [world (pf/world {:fill [[-2 64 6 30 66 6 "stone"]]})
        r (pf/plan world {:from pf/start :goals [(near 2 64 9 0) (near 22 64 2 0)]} {})]
    (is (= "found" (:status r)))
    (is (= 1 (:goal r)))))

(deftest goal-shapes-mix-in-one-set
  (let [r (pf/plan walled-near {:from pf/start :goals [(near 6 64 2 0) (xz 14 2 1)]} {})]
    (is (= "found" (:status r)))
    (is (= 1 (:goal r)))
    (is (<= (js/Math.hypot (- (first (last-cell r)) 14) (- (nth (last-cell r) 2) 2)) 1))))

(deftest every-goal-walled-off-ends-on-the-node-cap-or-exhausted-never-found
  (let [goals [(near 6 64 2 0) (near 30 64 30 0)]
        world (pf/world {:fill (concat (sealed 6 2) (sealed 30 30))})
        capped (pf/plan world {:from pf/start :goals goals} {:maxNodes 300})
        whole (pf/plan world {:from pf/start :goals goals} {})]
    (is (= "budget" (:reason capped)))
    (is (not= "found" (:status capped)))
    (is (nil? (:goal capped)))
    (is (= "exhausted" (:reason whole)))
    (is (not= "found" (:status whole)))))

;; ---- agreement with one search per goal on the frozen bench world ----

(def node-cap 20000)

(defn bench-sets
  "[{:id :snapshot :from :goals}]: every 4th bench query's start with the goals (up to 6) of the queries of its set whose
  goal lies within 64 blocks of it."
  []
  (let [qs (bench/world-queries)]
    (->> qs
         (take-nth 4)
         (keep (fn [{:keys [id snapshot query]}]
                 (let [{:keys [from]} query
                       set-name (first (re-find #"^([a-z-]+)-" id))
                       goals (->> qs
                                  (filter #(.startsWith (:id %) set-name))
                                  (map (comp :goal :query))
                                  (filter #(<= (js/Math.hypot (- (:x %) (:x from)) (- (:z %) (:z from))) 64))
                                  distinct
                                  (take 6)
                                  vec)]
                   (when (>= (count goals) 2) {:id id :snapshot snapshot :from from :goals goals}))))
         vec)))

(defn disagreement
  "nil when one search over the goal set reaches a goal of the least single-search cost at that cost, else what differs.
  Weight 1 (exact A*), no goal flood and the same node cap for both; a set where some single search ran out of nodes is
  left out (its true cost is not known): :unknown."
  [{:keys [id snapshot from goals]}]
  (let [opts {:goalFlood 0 :maxNodes node-cap}
        singles (mapv #(pf/plan snapshot {:from from :goal %} opts) goals)
        multi (pf/plan snapshot {:from from :goals goals} opts)
        costs (mapv cost singles)
        best (reduce (fn [m c] (if (and c (or (nil? m) (< c m))) c m)) nil costs)]
    (cond
      (some #(= "budget" (:reason %)) singles) :unknown
      (nil? best) (when (= "found" (:status multi)) [id :found-but-no-single-did])
      (not= "found" (:status multi)) [id :not-found best]
      (not (close? best (cost multi))) [id :cost (cost multi) best]
      (not (some-> (nth costs (:goal multi)) (close? best))) [id :goal (:goal multi) costs]
      :else nil)))

(deftest one-search-over-a-goal-set-agrees-with-one-search-per-goal-on-the-bench
  (let [sets (bench-sets)
        answers (map disagreement sets)
        checked (count (remove #(= :unknown %) answers))]
    (is (or (bench/skip-world?) (>= checked 20)) (str "only " checked " bench goal sets checked"))
    (is (= [] (vec (remove #(or (nil? %) (= :unknown %)) answers))))))
