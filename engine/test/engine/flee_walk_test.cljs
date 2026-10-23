(ns engine.flee-walk-test
  "jobs.lib.flee-walk: what is wet, the open cells along a direction, the way back up the body's stairs (pure, over
  block-at fns)."
  (:require [cljs.test :refer [deftest is]]
            [jobs.lib.reach :as reach]
            [jobs.survival.respond.cover :as cover]
            [jobs.lib.flee-walk :as walk]))

(defn pond-world
  "Stone up to y 62; at y 63 water in the band x >= 1, |z| <= 2 (a pond straight ahead of a body fleeing +x), stone
  elsewhere; air above."
  [{:keys [x y z]}]
  (cond
    (<= y 62) "stone"
    (= y 63) (if (and (>= x 1) (<= (js/Math.abs z) 2)) "water" "stone")
    :else "air"))

(deftest a-cell-is-wet-when-the-feet-or-the-floor-are-water
  (is (walk/wet? pond-world {:x 3 :y 63 :z 0}) "feet in water")
  (is (walk/wet? pond-world {:x 3 :y 64 :z 0}) "on water: the body sinks into it")
  (is (not (walk/wet? pond-world {:x 0 :y 64 :z 5}))))

(def flat-stone (fn [{:keys [y]}] (if (<= y 63) "stone" "air")))
(def logged-band (fn [{:keys [x y z]}] (and (= y 63) (>= x 1) (<= (js/Math.abs z) 2))))

(deftest a-waterlogged-block-is-wet
  (is (walk/wet? flat-stone logged-band {:x 3 :y 64 :z 0}) "on a waterlogged floor")
  (is (not (walk/wet? flat-stone logged-band {:x 3 :y 64 :z 5})))
  (is (not (walk/wet? flat-stone {:x 3 :y 64 :z 0})) "unread, the band is dry"))

(deftest water-plants-and-a-bubble-column-are-wet
  (doseq [plant ["kelp" "kelp_plant" "seagrass" "tall_seagrass" "bubble_column"]]
    (is (walk/wet? (fn [{:keys [y]}] (if (= y 64) plant "stone")) {:x 0 :y 64 :z 0}) plant)))

(defn leaf-wall-world
  "Flat stone floor at y 63; an oak_leaves wall at x 3, y 64-66, z -3..3."
  [x y z]
  (cond
    (<= y 63) :solid
    (and (= x 3) (<= 64 y 66) (<= -3 z 3)) (reach/arrow-kind-of #js {:name "oak_leaves"})
    :else (reach/arrow-kind-of #js {:name "air"})))

(deftest leaves-cover-a-body-from-arrows
  (let [skeleton {:x 0.5 :y 64 :z 0.5}]
    (is (reach/out-of-fire? leaf-wall-world [skeleton] {:x 5 :y 64 :z 0}) "behind the leaves")
    (is (not (reach/out-of-fire? leaf-wall-world [skeleton] {:x 0 :y 64 :z 6})) "in the open beside them")))

(def stair-floors (vec (for [i (range 1 9)] [(- 10 i) (- 70 i) 0])))

(deftest a-flight-takes-the-bodys-own-stair-up-away-from-the-mobs
  (let [at (fn [i] {:x (+ 0.5 (- 10 i)) :y (- 71 i) :z 0.5})
        mob {:x 0.5 :y 62 :z 0.5}]
    (is (= {:x 9 :y 70 :z 0} (walk/way-target (at 6) [mob] stair-floors 12))
        "the highest stair cell within reach, toward the entry, that is no nearer the mob")
    (is (nil? (walk/way-target (at 1) [mob] [] 12)) "no stair, no way target")
    (is (nil? (walk/way-target (at 8) [{:x 9.5 :y 70 :z 0.5}] stair-floors 12))
        "every stair cell above the body lies nearer the mob than the body: none")))

(defn deep-lake
  "Stone up to y 59, water at y 60 and 61 for x >= 0, air above: a lake two deep."
  [{:keys [x y]}]
  (cond (<= y 59) "stone" (<= 60 y 61) (if (>= x 0) "water" "air") :else "air"))

(deftest a-walk-over-deep-water-stays-at-the-surface
  (is (= [61 61 61 61] (mapv :y (walk/walk-cells deep-lake {:x 0.5 :y 61 :z 0.5} [1 0] 4)))
      "a swimmer is not walked down to the lake floor with its head under water"))

(deftest the-way-up-is-not-taken-through-a-mob-standing-in-its-mouth
  (let [body {:x 0.5 :y 41 :z 0.5}
        floors [[-2 52 -6]]]
    (is (nil? (walk/way-target body [{:x -0.5 :y 41 :z 0.5}] floors 20))
        "the stair top is far in 3D, but the mob stands on the line to it")
    (is (some? (walk/way-target body [{:x 1.5 :y 41 :z 3.5}] floors 20)) "a mob behind the body is not in the way")))

(deftest the-places-recent-walks-began-are-ways-back-like-stairs
  (let [c {:view (fn [] {:now 1000 :data {:entries {:moved [{:t 900 :data {:from {:x 4.5 :y 15 :z -2.2} :to {:x 1 :y 3 :z 1}}}
                                                           {:t 950 :data {:from {:x 4.7 :y 15 :z -2.4} :to {:x 1 :y 3 :z 1}}}]}
                                           :policies {:moved {:cap 20 :ttl 600000}}}})}]
    (is (= #{[4 14 -3]} (cover/trail-floors c)) "the floor under each start, once")))

(def ledge-world
  "Stone up to y 63 west of x 1; east of it stone only to y 60 (a three-block ledge), lava under a second ledge at z >= 4."
  (fn [{:keys [x y z]}]
    (cond
      (<= y 60) "stone"
      (< x 1) (if (<= y 63) "stone" "air")
      (and (>= z 4) (= y 60)) "lava"
      :else "air")))

(deftest a-flight-jumps-down-a-three-block-ledge-it-sees-the-floor-of
  (is (= 3 (walk/open-cells ledge-world {:x 0.5 :y 64 :z 0.5} [1 0] 3)) "dropped onto the stone three blocks down, not blocked at the lip"))
