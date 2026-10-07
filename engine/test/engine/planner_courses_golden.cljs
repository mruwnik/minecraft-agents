(ns engine.planner-courses-golden
  "engine/js/path/planner-courses.test.mjs against the ClojureScript planner: the live tester's courses planned on their
  fixture terrain, the verdicts and the paths through tight gaps."
  (:require [cljs.test :refer [deftest is are]]
            [engine.path.courses :as courses]
            [engine.planner-fixture :as pf :refer [plan]]))

(def GAP-Z 3217)
(def POD-REACH 0.127)
(def EPS 1e-9)

(defn course [name] (courses/course-snapshot name))
(defn lane [family cmds] (courses/lane-snapshot family cmds))
(defn plan-course [name] (pf/course-plan name))
(defn verdict [r] (str (:status r) (some->> (:reason r) (str "/"))))
(defn steps [r] (get-in r [:path :steps]))

(def found-courses
  ["cocoa-a0-both-feet" "cocoa-a1-both-feet" "cocoa-a2-both-feet"
   "cocoa-a0-both-feethead" "cocoa-a1-both-feethead" "cocoa-a2-both-feethead"
   "cocoa-a2-one-feethead" "cocoa-a2-both-head" "cocoa-a2-both-feethead-diag"
   "cocoa-open-a2-both-feethead" "cocoa-open-a2-one-feethead" "cocoa-farm-across" "cocoa-farm-along"
   "checker-we" "rand50-we" "rand50-ew" "target-in-rand50"
   "fence-diag" "trap-ceil-top"])

(deftest courses-found
  (doseq [name found-courses]
    (let [r (plan-course name)
          goal (:goal (course name))
          last (peek (steps r))]
      (is (= "found" (verdict r)) name)
      (is (<= (js/Math.hypot (- (:x last) (:x goal)) (- (:z last) (:z goal))) 1.5) name))))

(deftest tunnel-stairs-has-no-way-up-under-the-2-high-ceiling
  (is (= "partial" (:status (plan-course "tunnel-stairs")))))

(deftest steps-carry-px-pz-the-cell-centre-for-ordinary-cells
  (let [ss (steps (plan-course "trap-ceil-top"))
        s0 (first ss)]
    (is (every? (fn [{:keys [px pz x z]}] (and (number? px) (number? pz) (<= x px (inc x)) (<= z pz (inc z)))) ss))
    (is (= [(+ (:x s0) 0.5) (+ (:z s0) 0.5)] [(:px s0) (:pz s0)]))))

(deftest narrow-courses-cross-the-wall-within-pod-reach-of-the-gap-boundary
  (doseq [name ["cocoa-a2-both-feet" "cocoa-a2-both-feethead" "cocoa-a2-both-head" "cocoa-a2-both-feethead-diag"]]
    (let [ss (steps (plan-course name))
          in-wall (filter #(= 2880 (:x %)) ss)
          crossings (filter #(and (some? (:cx %)) (#{2880 2881} (:cx %))) ss)]
      (is (>= (count in-wall) 1) name)
      (is (every? #(<= (js/Math.abs (- (:pz %) GAP-Z)) (+ POD-REACH EPS)) in-wall) name)
      (is (>= (count crossings) 2) name)
      (is (every? #(<= (js/Math.abs (- (:cz %) GAP-Z)) (+ POD-REACH EPS)) crossings) name))))

(defn widest [name]
  (apply max (map #(js/Math.abs (- (:pz %) GAP-Z)) (filter #(= 2880 (:x %)) (steps (plan-course name))))))

(deftest cocoa-age-0-pods-leave-a-wider-crossing-than-age-2
  (is (<= (widest "cocoa-a0-both-feethead") 0.5))
  (is (<= (widest "cocoa-a2-both-feethead") (+ POD-REACH EPS))))

(def logs-north "fill 2880 161 3209 2880 163 3215 jungle_log")
(def logs-south "fill 2880 161 3217 2880 163 3223 jungle_log")
(def one-wide
  (delay (lane "tricky" [logs-north logs-south
                         "setblock 2880 161 3216 cocoa[age=2,facing=north]"
                         "setblock 2880 162 3216 cocoa[age=2,facing=north]"])))
(def across-goal {:kind "near" :x 2902 :y 161 :z 3216 :range 1})
(def across-from {:x 2857 :y 161 :z 3216})

(deftest one-wide-gap-with-an-age-2-pod-has-no-path-through
  (let [r (plan @one-wide {:from across-from :goal across-goal})]
    (is (not= "found" (:status r)))
    (is (not-any? #(>= (:x %) 2880) (steps r)))))

(deftest the-same-one-wide-gap-with-the-pod-removed-is-passable
  (let [free (lane "tricky" [logs-north logs-south])]
    (is (= "found" (:status (plan free {:from across-from :goal across-goal}))))))

(deftest trap-ceil-bottom-found-by-opening-the-trapdoors-in-the-way
  (let [r (plan-course "trap-ceil-bottom")]
    (is (= "found" (:status r)))
    (is (re-find #"opens \d+ trapdoors" (get-in r [:path :summary])))
    (is (>= (get-in r [:path :cost :opens]) 10))))

(deftest the-walk-through-the-gap-costs-a-plain-walk-plus-tenth-per-tight-cell
  (let [r (plan-course "cocoa-a2-both-feethead")
        plain (* (dec (count (steps r))) 0.1781895937277263)] ; (a straight run sprints)
    (is (>= (get-in r [:path :cost :seconds]) (- (+ plain 0.3) 1e-6)))))

(defn gap-start [z pz] {:x 2880 :y 161 :z z :px 2880.9 :pz pz})
(def after-wall {:kind "near" :x 2890 :y 161 :z 3217 :range 1})

(deftest starting-inside-the-pod-wall-keeps-the-starts-region
  (let [pod-wall (:snapshot (course "cocoa-a2-both-feethead"))]
    (doseq [[name from] [["the south edge of the north gap cell" (gap-start 3216 3216.95)]
                         ["the north edge of the south gap cell" (gap-start 3217 3217.05)]]]
      (let [r (plan pod-wall {:from from :goal after-wall})]
        (is (= "found" (:status r)) name)
        (is (<= (js/Math.abs (- (:pz (first (steps r))) GAP-Z)) (+ POD-REACH EPS)) name)))))

(deftest starting-in-a-cell-where-no-position-fits-is-not-standable
  (let [r (plan @one-wide {:from {:x 2880 :y 161 :z 3216} :goal after-wall})]
    (is (= ["none" "start-not-standable"] [(:status r) (:reason r)]))))

(deftest the-search-reports-its-tight-cell-statistics
  (let [{:keys [tightCells masks tightMasks regions maskMs]} (:stats (plan-course "cocoa-a2-both-feethead"))]
    (is (and (>= tightCells 3) (>= masks tightMasks) (> tightMasks 0) (>= regions tightMasks) (>= maskMs 0)))))

;; Independent check at a finer grid than the planner's: flood the body-centre positions and ask whether the far side
;; of the course is reachable.
(defn reachable? [snapshot [x0 z0 x1 z1] [sx sz] [tx tz] res]
  (let [boxes-near (.-boxesNear ^js @pf/space)
        body-hits (.-bodyHits ^js @pf/space)
        cache (atom {})
        boxes (fn [cx cz]
                (let [k [cx cz]]
                  (or (@cache k)
                      (let [b (boxes-near snapshot @pf/table cx 161 cz 161 162.8)]
                        (swap! cache assoc k b)
                        b))))
        free? (fn [i j] (not (body-hits (boxes (js/Math.floor (/ i res)) (js/Math.floor (/ j res))) (/ i res) (/ j res))))
        start [(js/Math.round (* sx res)) (js/Math.round (* sz res))]]
    (loop [queue (conj cljs.core/PersistentQueue.EMPTY start)
           seen #{start}]
      (let [[i j :as cur] (peek queue)]
        (cond
          (nil? cur) false
          (< (js/Math.hypot (- i (* tx res)) (- j (* tz res))) (* res 0.6)) true
          :else
          (let [nexts (->> [[1 0] [-1 0] [0 1] [0 -1]]
                           (map (fn [[di dj]] [(+ i di) (+ j dj)]))
                           (remove (fn [[a b :as k]]
                                     (or (< a (* x0 res)) (> a (* x1 res)) (< b (* z0 res)) (> b (* z1 res))
                                         (seen k) (not (free? a b))))))]
            (recur (into (pop queue) nexts) (into seen nexts))))))))

(deftest planner-and-finer-flood-agree-on-whether-the-body-fits
  (are [name area start target res fits status]
       (do (is (= fits (reachable? (:snapshot (course name)) area start target res)) name)
           (is (= status (:status (plan-course name))) name))
    "fence-diag" [2870 3205 2890 3225] [2879.5 3217.5] [2882.5 3216.5] 32 true "found"
    "wall-diag" [2870 3205 2890 3225] [2879.5 3217.5] [2882.5 3216.5] 32 false "partial"
    ;; the body fits through a solid block of offset bamboo only by weaving inside cells: the plan carries the bends
    ;; (engine.courses-physics-test walks it)
    "full-walled" [2855 3209 2905 3224] [2857.5 3216.5] [2902.5 3216.5] 16 true "found"))
