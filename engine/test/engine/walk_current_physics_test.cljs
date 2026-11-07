(ns engine.walk-current-physics-test
  "A walk through flowing water in prismarine-physics, the body's own client physics: jobs.lib.walk/decide-tick steers
  against the current's pull (against-current), so the body keeps to its plan where a straight aim drifts off it."
  (:require [cljs.test :refer [deftest is]]
            [engine.path.executor :as ex]
            [engine.path.planner.current :as current]
            [engine.stairs-physics-test :as sp]
            [jobs.lib.walk :as walk]
            [jobs.lib.walk.world :as wworld]))

(def max-ticks 1200)

;; A field of water 1 deep on a stone floor (top y 64), x 0..12, z 0..6, flowing toward +z: level z + 1 (lower water
;; downstream), so every cell pulls the body along +z. Walls at z -1 and 7 leave no dry way round: the walk crosses it
;; along x at z 3.
(def field
  (into [[-2 63 -2 16 63 10 "stone" {}] [-2 64 -1 16 65 -1 "stone" {}] [-2 64 7 16 65 7 "stone" {}]]
        (for [z (range 7)] [0 64 z 12 64 z "water" {:level (inc z)}])))

(def from [-1 64 3])
(def goal [13 64 3])

(defn run
  "Walk the field from from to goal with decide-tick, flow-at from the fills (steer? true) or none."
  [steer?]
  (let [{:keys [mc physics]} @sp/lib
        world (sp/world-of field)
        phys ((.-Physics physics) mc world)
        _ (set! (.-playerHalfWidth phys) (sp/body-half))
        steps (sp/plan field from goal)
        bot (sp/bot-at from)
        env (cond-> {:policy ex/policy :steps steps}
              steer? (assoc :flow-at (walk/flow-fn (sp/path-world field))))]
    (loop [t 0 state (walk/start-state steps (sp/pose-of bot)) poses []]
      (let [pose (sp/pose-of bot)
            [state' {:keys [done controls yaw pitch]}] (walk/decide-tick env state pose)]
        (if (or done (>= t max-ticks))
          {:done done :steps steps :poses poses}
          (do (set! (.-yaw (.-entity bot)) yaw)
              (set! (.-pitch (.-entity bot)) (or pitch 0))
              (.apply (.simulatePlayer phys (new (.-PlayerState physics) bot (clj->js controls)) world) bot)
              (recur (inc t) state' (conj poses pose))))))))

(defn drift [poses] (apply max (map #(js/Math.abs (- (:z %) 3.5)) poses)))

(def in-row
  "The most a body's centre may stray from its row's middle with its box still inside the row's cells."
  (- 0.5 (:body-half ex/policy)))

(deftest a-walk-across-a-current-keeps-its-box-in-its-row
  (let [{:keys [done steps poses]} (run true)]
    (is (every? #(= 3 (:z %)) steps) (pr-str steps))
    (is (= :arrived (:status done)) (pr-str done))
    (is (< (drift poses) in-row) (drift poses))))

(deftest a-straight-aim-across-the-same-current-is-pulled-out-of-its-row
  (is (> (drift (:poses (run false))) in-row)))

(deftest the-field-pulls-along-its-fall
  (let [flow-at (walk/flow-fn (sp/path-world field))
        f (flow-at 5 64 3)]
    (is (= [0 0 1] (vec f)))
    (is (nil? (flow-at 5 65 3)))
    (is (< (js/Math.abs (- current/PUSH (aget (walk/pull flow-at {:x 5.5 :y 64 :z 3.5}) 1))) 1e-9))))

;; A 4 x 4 field whose level rises with x + z (water lower toward +x +z), beside a stone wall at x -1 on its west row: the
;; flows differ in direction cell by cell, diagonal and against a wall.
(def slope
  (into [[-2 63 -2 8 63 8 "stone" {}] [-1 64 0 -1 65 3 "stone" {}]]
        (for [x (range 4) z (range 4)] [x 64 z x 64 z "water" {:level (inc (+ x z))}])))

(def idle {:forward false :back false :left false :right false :jump false :sprint false :sneak false})

(defn pull-by-physics
  "The unit horizontal direction a body at rest in the cell x z of fills is pushed in over one physics tick."
  [fills x z]
  (let [{:keys [physics]} @sp/lib
        world (sp/world-of fills)
        phys ((.-Physics physics) (:mc @sp/lib) world)
        bot (sp/bot-at [x 64 z])]
    (set! (.-playerHalfWidth phys) (sp/body-half))
    (.apply (.simulatePlayer phys (new (.-PlayerState physics) bot (clj->js idle)) world) bot)
    (let [v (.-velocity (.-entity bot)) n (js/Math.hypot (.-x v) (.-z v))]
      [(/ (.-x v) n) (/ (.-z v) n)])))

(deftest the-ported-flow-points-where-the-physics-pushes
  (let [flow-at (walk/flow-fn (sp/path-world slope))]
    (doseq [[x z] [[0 0] [1 1] [2 1] [1 2] [3 3] [0 2] [2 0]]
            :let [f (flow-at x 64 z)
                  n (js/Math.hypot (aget f 0) (aget f 2))
                  [px pz] (pull-by-physics slope x z)]]
      (is (> (+ (* px (/ (aget f 0) n)) (* pz (/ (aget f 2) n))) 0.999) (str [x z] " " (vec f) " vs " [px pz])))))

(deftest a-body-with-no-path-world-asks-for-one-once-per-epoch
  (let [calls (atom 0)]
    (with-redefs [wworld/path-world (fn ([_] (swap! calls inc) nil) ([_ _] (swap! calls inc) nil))]
      (let [flow-at (walk/live-flow-at {:primitives #js {}})]
        (dotimes [_ 5] (flow-at 0 64 0))))
    (is (= 1 @calls))))
