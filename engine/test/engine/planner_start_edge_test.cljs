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
