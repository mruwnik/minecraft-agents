(ns engine.floor-sight-pin-test
  "Which floors the body can see from where it stands, through the real ray code (engine.perception.rays/visible-now?):
  the table the planner's options.unknown seeDepth and seeLit rest on (card c51cd6c2, design v3.2 section 0). A cliff
  course: the takeoff's feet cell is (0 64 0) on stone, the landing column x 1 is air down to the landing floor
  (1 63-d 0); a cell counts as seen by any face point (sight-kernel cellLight), so the planner's see depths are lower bounds:
  a row that reads true beyond them is fine, one that reads false inside them is a bug; the eye stands at x ex in the takeoff cell, z 0.5, 1.62 over the feet."
  (:require [cljs.test :refer [deftest is]]
            [engine.fake.raw-world :as fake-raw]
            [engine.path.fixture :as fx]
            [engine.perception :as perception]
            [engine.perception.rays :as rays]))

(def stone (delay (fx/default-state "stone")))

(def light-of {:dark 0 :torch 0 :lit (bit-shift-left 15 4)})

(defn cliff
  "{[x y z] id}: stone under the takeoff (x 0) down to y 40, the landing floor at 63-d under x 1 and 2 (x 2 too, at y 63,
  for the level floor two ahead)"
  [d]
  (into {} (for [[x top] [[0 63] [1 (- 63 d)] [2 (- 63 d)]]
                 y (range 40 (inc top))]
             [[x y 0] @stone])))

(defn raw
  "A rawWorld over the cells, every cell lit at `light` (packed sky << 4 | block), a torch in the off-hand for :torch."
  [cells light-kind]
  #js {:stateAt (fn [x y z] (get cells [x y z] 0))
       :lightAt (fn [_ _ _] (light-of light-kind))
       :sightTable (fn [] @fake-raw/sight)
       :sky (fn [] #js {:timeOfDay 1000 :rain 0 :thunder 0})
       :offHand (fn [] (when (= light-kind :torch) "torch"))
       :heldItem (fn [] nil)
       :stateInfo (fn [_] nil)})

(defn seen?
  "visible-now? of the floor cell [x y z] from the eye at (ex 65.62 ez); pitch in degrees (down negative), or :aimed at the
  cell's centre; the yaw faces the cell along x z."
  [cells light-kind [ex ez] pitch [x y z]]
  (let [per (perception/create (raw cells light-kind) {})
        vx (- (+ x 0.5) ex) vy (- (+ y 0.5) 65.62) vz (- (+ z 0.5) ez)
        h (js/Math.hypot vx vz)
        rad (/ js/Math.PI 180)
        eye #js {:x ex :y 65.62 :z ez :yaw (js/Math.atan2 (- vx) (- vz))
                 :pitch (if (= pitch :aimed) (js/Math.atan2 vy h) (* pitch rad))}]
    (boolean (rays/visible-now? per (:st per) eye x y z (get cells [x y z] 0)))))

(def entry 0.05)
(def centre 0.5)
(def edge 0.8)
(def peek-x 0.95)

;; [d eye-x pitch light seen?]: the straight step-down (d 0: the level floor ahead) onto (1 63-d 0)
(def straight-rows
  [[0 entry -20 :dark false]
   [0 entry -30 :dark true]
   [0 entry 0 :lit false]
   [1 entry :aimed :lit true]
   [1 centre :aimed :dark true]
   [2 centre :aimed :lit true]
   [1 peek-x :aimed :dark true]
   [1 peek-x -30 :lit false]
   [2 peek-x :aimed :dark false]
   [2 peek-x :aimed :lit true]
   [4 peek-x :aimed :torch true]
   [5 peek-x :aimed :torch false]
   [4 edge :aimed :lit true]
   [15 peek-x :aimed :lit true]
   [16 peek-x :aimed :lit true]
   ;; review round 4, m1: a peek that stops at x .87 sees lit floors to d 5 only
   [5 0.87 :aimed :lit true]
   [6 0.87 :aimed :lit true]
   [10 0.87 :aimed :lit true]])

(deftest straight-step-down-floors
  (doseq [[d ex pitch light expected] straight-rows]
    (is (= expected (seen? (cliff d) light [ex 0.5] pitch [1 (- 63 d) 0]))
        (str "d " d " eye x " ex " pitch " pitch " " light))))

(deftest diagonal-level-floor-from-the-entry
  (let [cells (assoc (cliff 0) [1 63 1] @stone)]
    (doseq [pitch [-20 -30]]
      (is (true? (seen? cells :dark [entry entry] pitch [1 63 1])) (str "pitch " pitch)))))

(deftest the-level-floor-two-ahead-is-seen-by-a-face-corner-over-the-next
  (is (true? (seen? (cliff 0) :lit [entry 0.5] :aimed [2 63 0]))))

(deftest a-block-behind-a-wall-stays-unseen
  (let [cells (into (cliff 0) (for [y (range 64 70) z (range -3 4)] [[2 y z] @stone]))
        cells (assoc cells [4 65 0] @stone)]
    (is (false? (seen? cells :lit [0.5 0.5] :aimed [4 65 0])))))
