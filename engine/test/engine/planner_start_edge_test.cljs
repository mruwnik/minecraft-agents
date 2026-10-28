(ns engine.planner-start-edge-test
  "The planner's start cell for a body on the edge of a block: the floored cell may have nothing under it while the
  0.6-wide hitbox stands on a neighbour; the start is then the overlapped cell that supports the body."
  (:require [cljs.test :refer [deftest is are]]
            [engine.path.executor :as ex]
            [engine.planner-fixture :as pf :refer [world near run cells]]))

(def hole "the floor under (2,64,4) dug out down to y 60" [2 60 4 2 63 4 "air"])

(defn status+first [r] [(:status r) (:reason r) (first (cells r))])

(deftest start-on-the-edge-of-a-hole-plans-from-the-supporting-cell
  (are [from first-cell] (= ["found" nil first-cell]
                            (status+first (run (world {:fill [hole]}) (near 8 64 8) {} from)))
    ;; z .91 over the hole: the hitbox reaches z 5.21, the block under (2,64,5) holds the body
    {:x 2 :y 64 :z 4 :px 2.5 :py 64 :pz 4.91} [2 64 5]
    ;; z .6926 (trial): the client box (half width .30999) reaches z 5.0026 and the block under (2,64,5) holds it,
    ;; though the server's 0.3 box would not reach
    {:x 2 :y 64 :z 4 :px 2.5 :py 64 :pz 4.6926} [2 64 5]
    ;; z .09 the other way: the block under (2,64,3)
    {:x 2 :y 64 :z 4 :px 2.5 :py 64 :pz 4.09} [2 64 3]
    ;; x edge: the block under (3,64,4)
    {:x 2 :y 64 :z 4 :px 2.8 :py 64 :pz 4.5} [3 64 4]
    ;; corner: (2,5) and (3,4) overlap the hitbox alike; the lower x wins
    {:x 2 :y 64 :z 4 :px 2.95 :py 64 :pz 4.95} [2 64 5]
    ;; a floored cell that stands is kept
    {:x 2 :y 64 :z 5 :px 2.5 :py 64 :pz 5.5} [2 64 5]))

(deftest start-with-nothing-under-the-hitbox-is-not-standable
  (are [fill from] (= ["none" "start-not-standable"]
                      ((juxt :status :reason) (run (world {:fill fill}) (near 8 64 8) {} from)))
    ;; the hitbox wholly over the hole
    [hole] {:x 2 :y 64 :z 4 :px 2.5 :py 64 :pz 4.5}
    ;; the neighbour stands, but half a block higher than the body's feet (a slab): it does not hold the body
    [hole [2 64 5 2 64 5 "stone_slab"]] {:x 2 :y 64 :z 4 :px 2.5 :py 64 :pz 4.91}
    ;; no hitbox position given: only the floored cell
    [hole] {:x 2 :y 64 :z 4}))

(deftest executor-walks-to-a-start-cell-beside-the-body-first
  (let [steps [{:x 2 :y 64 :z 5 :h 0 :move :start :px 2.5 :pz 5.5}
               {:x 3 :y 64 :z 5 :h 0 :move :walk :px 3.5 :pz 5.5}]]
    (is (= 0 (:i (ex/start steps 0 {:x 2.5 :y 64 :z 4.91}))) "body beside the start cell: step 0 first")
    (is (= 0 (:i (ex/start steps 0 {:x 2.5 :y 64 :z 4.6926}))) "body whose client box just reaches the start cell: step 0 first")
    (is (= 1 (:i (ex/start steps 0 {:x 2.5 :y 64 :z 5.5}))) "body in the start cell")
    (is (= 1 (:i (ex/start steps 0 {:x 2.5 :y 64 :z 3.5}))) "body further away: unchanged")
    (is (= 1 (:i (ex/start steps 0))) "no pose: unchanged")))

(def fire-start {:x 2 :y 64 :z 4 :px 2.5 :py 64 :pz 4.5})

(deftest start-in-a-fire-cell-plans-out-of-it
  (is (= ["found" nil] ((juxt :status :reason) (run (world {:fill [[2 64 4 2 64 4 "fire"]]}) (near 8 64 8) {} fire-start)))))

(deftest start-in-fire-ringed-by-lava-is-enclosed-not-unstandable
  ;; a start ringed by lava is enclosed (own land sealed); go-to escalates on it as on goal-enclosed or goal-cut-off, never on start-not-standable
  (is (= ["none" "start-enclosed"]
         ((juxt :status :reason)
          (run (world {:fill [[2 64 4 2 64 4 "fire"] [3 64 4 3 64 4 "lava"] [1 64 4 1 64 4 "lava"] [2 64 5 2 64 5 "lava"] [2 64 3 2 64 3 "lava"]]})
               (near 8 64 8) {} fire-start)))))

;; extinguish live (four-lava): the open lava source next to a filled pit flows sideways into the body's cell, replacing
;; the fire at its feet; a body standing in flowing lava must still plan out of it
(def lava-feet [2 64 4 "lava" {"level" "2"}])

(deftest start-in-flowing-lava-plans-out-of-it
  (are [fill goal want]
       (= want ((juxt :status :reason) (run (world {:fill fill :blocks [lava-feet]}) goal {} fire-start)))
    ;; three covers and one open source: up onto a cover and on to the stone beyond
    [[1 64 4 1 64 4 "cobblestone"] [3 64 4 3 64 4 "cobblestone"] [2 64 5 2 64 5 "cobblestone"] [2 64 3 2 64 3 "lava"]
     [4 64 4 4 64 4 "stone"]]
    (near 4 65 4) ["found" nil]
    ;; lava on all four sides: the start is sealed in, so go-to escalates
    [[1 64 4 1 64 4 "lava"] [3 64 4 3 64 4 "lava"] [2 64 5 2 64 5 "lava"] [2 64 3 2 64 3 "lava"]]
    (near 8 64 8) ["none" "start-enclosed"]))

(def fence-line "a fence line along z 6, x 10..16: each cell's post leaves a free sliver north and south of it" [10 64 6 16 64 6 "oak_fence"])

(defn side-of-line
  "which side of the post the path's points in the line's cells (x 10..16) and beside it lie: :north (z < 6.375), :south (z > 6.625)"
  [r]
  (set (for [{:keys [x z pz]} (get-in r [:path :steps]) :when (and (<= 10 x 16) (<= 5 z 7))] (cond (< pz 6.375) :north (> pz 6.625) :south :else :post))))

(deftest start-in-a-fence-cell-plans-from-the-sliver-the-body-is-in
  ;; the body in a fence cell stands on one side of the post; a plan from the other sliver walks it into the post
  (are [pz side] (= ["found" #{side}] ((juxt :status side-of-line) (run (world {:fill [fence-line]}) (near 4 64 3) {} {:x 13 :y 64 :z 6 :px 13.5 :py 64 :pz pz})))
    6.9375 :south
    6.0625 :north))

(def stair [2 64 4 2 64 4 "cherry_stairs" {:facing "east" :half "bottom" :shape "straight"}])

(deftest start-on-a-stair-with-no-ground-beside-it-plans-from-the-cell-above-it
  (is (= ["found" nil [2 65 4]]
         (status+first (run (world {:fill [stair [1 64 4 1 64 4 "stone"]]}) (near 8 64 4) {} {:x 2 :y 64 :z 4 :px 2.2 :py 64.5 :pz 4.5})))))

(deftest start-on-the-low-half-of-a-stair-steps-down-off-it
  (are [from first-cell] (= ["found" nil first-cell] (status+first (run (world {:fill [stair]}) (near 8 64 4) {} from)))
    ;; the low half (x .2, the box reaches the cell to the west): the walk starts from that cell, not hovering over the stair
    {:x 2 :y 64 :z 4 :px 2.2 :py 64.5 :pz 4.5} [1 64 4]))

(deftest start-a-hair-above-farmland-plans-from-the-farmland-cell
  ;; feet 1/16 over the 15/16-tall floor (held up by a full block beside it, as at a field edge)
  (is (= ["found" nil [2 63 4]]
         (status+first (run (world {:fill [[2 63 4 2 63 4 "farmland"]]}) (near 8 64 8) {} {:x 2 :y 64 :z 4 :px 2.5 :py 64 :pz 4.5})))))

(defn- pod-run [fill props from]
  (run (world {:fill (into [[2 64 3 2 65 3 "jungle_log"]] fill)
               :blocks [[2 64 4 "cocoa" props]]})
       (near 8 64 8) {} (merge {:x 2 :y 64 :z 4 :px 2.6 :py 64.75 :pz 4.4} from)))

(deftest start-on-a-cocoa-pod
  (are [fill props from first-cell] (= ["found" nil first-cell]
                                       (status+first (pod-run fill props from)))
    [] {:age "2" :facing "north"} {} [2 64 4]
    [hole] {:age "0" :facing "north"} {:px 2.9 :pz 4.9} [2 64 5]))

(deftest an-airborne-body-beside-a-stand-does-not-start-from-the-side-cell
  (is (= ["none" "start-not-standable"]
         ((juxt :status :reason) (pod-run [hole] {:age "2" :facing "north"} {:py 65.5})))))
