(ns engine.planner-lava-rim-test
  "The lava pool BaseMiner burned at (card cd97ec8b; events seq 60606-60636, 60782-60814): a cave floor at y -55 with a
  source-lava pool, the walkable rim one block above it. Rebuilt from the body's recorded chunks (x 112..131, y -55..-51,
  z 46..64; everything else deepslate). A pocket on the rim joins the cave only by corner slides whose open side is a
  hole onto the lava; the slide carries the body wholly over that hole. Such a slide costs a large risk: the planner
  takes it only when it is the sole way (a body in the pocket still gets out), never when another way exists."
  (:require [cljs.test :refer [deftest is are]]
            [engine.path.fixture :as fx]
            [engine.planner-fixture :as pf]))

(def cave
  [[112 -56 46 131 -50 64 "deepslate"]
   [126 -55 48 128 -55 48 "lava"]
   [125 -55 49 129 -55 49 "lava"]
   [124 -55 50 129 -55 50 "lava"]
   [124 -55 51 127 -55 51 "lava"]
   [123 -55 52 126 -55 52 "lava"]
   [123 -55 53 125 -55 53 "lava"]
   [124 -55 54 124 -55 54 "lava"]
   [125 -54 48 130 -54 48 "air"]
   [124 -54 49 131 -54 49 "air"]
   [123 -54 50 130 -54 50 "air"]
   [123 -54 51 129 -54 51 "air"]
   [122 -54 52 127 -54 52 "air"]
   [122 -54 53 126 -54 53 "air"]
   [121 -54 54 125 -54 54 "air"]
   [121 -54 55 124 -54 55 "air"]
   [120 -54 56 124 -54 56 "air"]
   [120 -54 57 123 -54 57 "air"]
   [119 -54 58 122 -54 58 "air"]
   [119 -54 59 121 -54 59 "air"]
   [118 -54 60 121 -54 60 "air"]
   [117 -54 61 120 -54 61 "air"]
   [118 -54 62 120 -54 62 "air"]
   [128 -53 47 128 -53 47 "air"]
   [130 -53 47 130 -53 47 "air"]
   [126 -53 48 131 -53 48 "air"]
   [124 -53 49 131 -53 49 "air"]
   [123 -53 50 131 -53 50 "air"]
   [123 -53 51 130 -53 51 "air"]
   [122 -53 52 127 -53 52 "air"]
   [121 -53 53 126 -53 53 "air"]
   [121 -53 54 125 -53 54 "air"]
   [120 -53 55 125 -53 55 "air"]
   [120 -53 56 124 -53 56 "air"]
   [119 -53 57 124 -53 57 "air"]
   [118 -53 58 123 -53 58 "air"]
   [118 -53 59 122 -53 59 "air"]
   [117 -53 60 122 -53 60 "air"]
   [113 -53 61 121 -53 61 "air"]
   [112 -53 62 121 -53 62 "air"]
   [112 -53 63 120 -53 63 "air"]
   [118 -53 64 118 -53 64 "air"]
   [130 -52 47 130 -52 47 "air"]
   [127 -52 48 131 -52 48 "air"]
   [126 -52 49 131 -52 49 "air"]
   [124 -52 50 131 -52 50 "air"]
   [123 -52 51 126 -52 51 "air"]
   [128 -52 51 131 -52 51 "air"]
   [122 -52 52 126 -52 52 "air"]
   [122 -52 53 125 -52 53 "air"]
   [121 -52 54 125 -52 54 "air"]
   [120 -52 55 125 -52 55 "air"]
   [120 -52 56 124 -52 56 "air"]
   [119 -52 57 124 -52 57 "air"]
   [118 -52 58 123 -52 58 "air"]
   [118 -52 59 122 -52 59 "air"]
   [112 -52 60 122 -52 60 "air"]
   [112 -52 61 121 -52 61 "air"]
   [112 -52 62 121 -52 62 "air"]
   [112 -52 63 120 -52 63 "air"]
   [112 -52 64 119 -52 64 "air"]
   [129 -51 48 131 -51 48 "air"]
   [129 -51 49 131 -51 49 "air"]
   [129 -51 50 131 -51 50 "air"]
   [130 -51 51 131 -51 51 "air"]
   [124 -51 53 124 -51 53 "air"]
   [122 -51 54 124 -51 54 "air"]
   [121 -51 55 124 -51 55 "air"]
   [120 -51 56 123 -51 56 "air"]
   [119 -51 57 123 -51 57 "air"]
   [119 -51 58 123 -51 58 "air"]
   [118 -51 59 122 -51 59 "air"]
   [112 -51 60 121 -51 60 "air"]
   [112 -51 61 121 -51 61 "air"]
   [112 -51 62 120 -51 62 "air"]
   [112 -51 63 119 -51 63 "air"]
   [112 -51 64 118 -51 64 "air"]])

(defn lava? [snap x y z] (= "lava" (:name (fx/block-at snap x y z))))
(defn open? [snap x y z] (contains? #{"air" "cave_air"} (:name (fx/block-at snap x y z))))

(defn over-lava
  "The cells of the path over a lava floor the body stands in or slides over: each step cell, and for a corner slide
  (a diagonal step with one side column blocked at the step's height) the open side column. The slide presses the
  body on the blocked side and carries it wholly over the open one (live: BaseMiner at x 124.29 z 50.53, over the
  lava at [124 -55 50], when it caught fire)."
  [snap r]
  (let [cells (pf/cells r)
        stand (for [[x y z] cells :when (lava? snap x (dec y) z)] [x y z])
        sides (for [[[x0 _ z0] [x1 y1 z1]] (partition 2 1 cells)
                    :when (and (not= x0 x1) (not= z0 z1))
                    :let [a [x1 z0] b [x0 z1]
                          open-a (open? snap (first a) y1 (second a))
                          open-b (open? snap (first b) y1 (second b))]
                    :when (not= open-a open-b)
                    :let [[sx sz] (if open-a a b)]
                    :when (lava? snap sx (dec y1) sz)]
                [sx y1 sz])]
    (vec (distinct (concat stand sides)))))

(defn slides-over-lava [from goal]
  (let [snap (pf/snapshot {:fill cave})
        r (pf/plan snap {:from from :goal goal})]
    {:status (:status r) :last (pf/last-cell r) :over (over-lava snap r)
     :risk (get-in r [:path :cost :risk])}))

(deftest a-body-in-the-lava-rim-pocket-still-gets-out
  ;; the pocket's only ways out are the two slides over the lava: the plan takes them (at their risk), it does not trap
  ;; the body; j759/j771: from where steer left the body, back down the cave to the west
  (let [{:keys [status last over risk]} (slides-over-lava {:x 124 :y -54 :z 49} (pf/near 112 -53 63))]
    (is (= "found" status))
    (is (= [112 -53 63] last))
    (is (seq over) "the only way out slides over the lava")
    (is (>= risk 10) (str "the slide over the lava costs its risk, got " risk))))

(deftest a-goal-in-the-lava-rim-pocket-is-still-reached
  ;; j758/j769: the way in, up the cave to the rim; the pocket is only reached by the slides
  (let [{:keys [status last risk]} (slides-over-lava {:x 118 :y -53 :z 59} (pf/near 124 -54 49))]
    (is (= "found" status))
    (is (= [124 -54 49] last))
    (is (>= risk 10) (str "the slide over the lava costs its risk, got " risk))))

;; A corner slide with a way round: start [2 64 2], goal [3 64 3], a pillar at [2 64..65 3] to slide along, the open
;; side [3 64 2] over the floor cell [3 63 2]. Round the pillar on the other side is three diagonals on stone.
(defn corner-world [floor-cell]
  (pf/world {:fill [[2 64 3 2 65 3 "stone"]
                    [3 63 2 3 63 2 floor-cell]]}))

(defn slid? [r] (boolean (some #{[[2 64 2] [3 64 3]]} (partition 2 1 (pf/cells r)))))

(deftest a-corner-slide-over-a-damaging-hole-is-avoided-when-there-is-a-way-round
  (are [floor-cell slides] (let [r (pf/run (corner-world floor-cell) (pf/near 3 64 3))]
                             (is (= "found" (:status r)))
                             (is (= [3 64 3] (pf/last-cell r)))
                             (is (= slides (slid? r)) (str floor-cell " path " (pf/cells r))))
    "lava" false
    "fire" false
    ;; a plain hole (stone one further down) is no danger: the slide stays the short way
    "air" true))

(deftest a-rim-cell-beside-the-pocket-is-reached-without-sliding-over-the-lava
  ;; [122 -54 53] lies beside the pocket's mouth: the walk along the rim reaches it with no slide over the lava (the
  ;; live fixture's along-the-rim-no-slide case)
  (let [{:keys [status last over]} (slides-over-lava {:x 118 :y -53 :z 59} (pf/near 122 -54 53))]
    (is (= "found" status))
    (is (= [122 -54 53] last))
    (is (= [] over))))
