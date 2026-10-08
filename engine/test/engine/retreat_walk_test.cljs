(ns engine.retreat-walk-test
  "jobs.survival.retreat-walk's choice of a flight target: water is slow, cover from a ranged mob comes first (pure,
  over block-at fns)."
  (:require [cljs.test :refer [deftest is]]
            [jobs.lib.reach :as reach]
            [jobs.survival.retreat-walk :as walk]))

(defn pond-world
  "Stone up to y 62; at y 63 water in the band x >= 1, |z| <= 2 (a pond straight ahead of a body fleeing +x), stone
  elsewhere; air above."
  [{:keys [x y z]}]
  (cond
    (<= y 62) "stone"
    (= y 63) (if (and (>= x 1) (<= (js/Math.abs z) 2)) "water" "stone")
    :else "air"))

(def from {:x 0.5 :y 64 :z 0.5})
(def threat {:x -5 :y 64 :z 0.5})

(deftest a-flight-takes-a-full-step-on-dry-ground-over-a-swim-straight-away
  (is (= {:x 0 :y 64 :z 6} (walk/choose-target pond-world from [threat] nil [] 6 {:swim-factor 2.2}))
      "sideways on dry ground beats swimming the pond")
  (is (= {:x 6 :y 63 :z 0} (walk/choose-target pond-world from [threat] nil [] 6 {:swim-factor 1}))
      "with water as fast as ground the way straight away is kept"))

(deftest a-flight-without-options-keeps-the-preferred-direction
  (is (= {:x 6 :y 63 :z 0} (walk/choose-target pond-world from [threat] nil [] 6))))

(deftest a-cell-is-wet-when-the-feet-or-the-floor-are-water
  (is (walk/wet? pond-world {:x 3 :y 63 :z 0}) "feet in water")
  (is (walk/wet? pond-world {:x 3 :y 64 :z 0}) "on water: the body sinks into it")
  (is (not (walk/wet? pond-world {:x 0 :y 64 :z 5}))))

(deftest a-flight-from-a-ranged-mob-takes-a-covered-end-first
  (let [flat (fn [{:keys [y]}] (if (<= y 63) "stone" "air"))
        covered? (fn [end] (neg? (:z end)))]
    (is (= {:x 6 :y 64 :z 0} (walk/choose-target flat from [threat] nil [] 6 {:swim-factor 2.2}))
        "no cover asked: straight away")
    (is (neg? (:z (walk/choose-target flat from [threat] nil [] 6 {:swim-factor 2.2 :covered? covered?})))
        "the first direction whose end is out of the line of fire")))

(defn leaf-wall-world
  "Flat stone floor at y 63; an oak_leaves wall at x 3, y 64-66, z -3..3."
  [x y z]
  (cond
    (<= y 63) :solid
    (and (= x 3) (<= 64 y 66) (<= -3 z 3)) (reach/arrow-kind-of #js {:name "oak_leaves"})
    :else (reach/arrow-kind-of #js {:name "air"})))

(deftest leaves-cover-a-body-from-arrows
  (let [skeleton {:x 0.5 :y 64 :z 0.5}]
    (is (walk/out-of-fire? leaf-wall-world [skeleton] {:x 5 :y 64 :z 0}) "behind the leaves")
    (is (not (walk/out-of-fire? leaf-wall-world [skeleton] {:x 0 :y 64 :z 6})) "in the open beside them")))
