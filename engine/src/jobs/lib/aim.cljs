(ns jobs.lib.aim
  "Whether an eye can aim at a block, as a player must to dig it, place against it or use it: a clear line from the eye
  to the block's centre or to a point just inside one of its faces, over the cells the body has seen (a cell never seen
  is in nobody's way). go-to's :see and jobs.lib.blocks/in-reach? ask it."
  (:require [clojure.string :as str]
            [engine.game :as game]
            [engine.sight :as sight]
            [jobs.lib.reach :as reach]
            [jobs.lib.util :as u]))

(def face-inset "How far inside a face the eye aims (a point on the face floors into the cell beside it)." 0.02)

(defn sight-points
  "The points of the block at cell [x y z] an eye aims at: its centre, then the middle of each face, face-inset inside."
  [[x y z]]
  (let [cx (+ x 0.5) cy (+ y 0.5) cz (+ z 0.5) lo face-inset hi (- 1 face-inset)]
    [[cx cy cz] [(+ x lo) cy cz] [(+ x hi) cy cz] [cx (+ y lo) cz] [cx (+ y hi) cz] [cx cy (+ z lo)] [cx cy (+ z hi)]]))

(defn seen-kind-at
  "(fn [x y z]) -> reach/kind-of of the block the body of primitives p sees or remembers there (never seen: :open). Lava
  is :open: a player's aim passes through fluids, and lava flowing into the cell between the eye and a source must not
  hide the source from the seal."
  [p]
  (fn [x y z] (let [b (u/seen-block p {:x x :y y :z z})]
                (if (= "lava" (some-> b .-name)) :open (reach/kind-of b)))))

(defn eye-at
  "The eye [x y z] of a body whose feet are at pos {:x :y :z}."
  [{:keys [x y z]}]
  [x (+ y game/eye-height) z])

(defn sees-cell?
  "Whether the eye [x y z] has a line to one of the sight-points of the block at cell [x y z] that crosses no :solid cell
  of kind-at (the eye's and the block's own cells do not count)."
  [kind-at [ex ey ez] cell]
  (boolean (some (fn [[x y z]] (sight/line-clear ex ey ez x y z (fn [cx cy cz] (keyword-identical? :solid (kind-at cx cy cz)))))
                 (sight-points cell))))

(defn first-blocker
  "The first cell [x y z] a straight line from the eye [x y z] to the centre of the block at cell crosses that kind-at
  gives :solid (the nearest to the eye), nil when the line is clear."
  [kind-at [ex ey ez] [x y z]]
  (let [hit (volatile! nil)
        [tx ty tz] (first (sight-points [x y z]))]
    (sight/line-clear ex ey ez tx ty tz
                      (fn [cx cy cz] (when (keyword-identical? :solid (kind-at cx cy cz)) (vreset! hit [cx cy cz]) true)))
    @hit))

(defn stand-sees-fn
  "(fn [x y z]) -> whether a body standing in the feet cell x y z (at its middle) sees the block at cell, over what the
  body of primitives p has seen: the planner's goalSeen for go-to's :see."
  [p cell]
  (let [kind-at (seen-kind-at p)]
    (fn [x y z] (sees-cell? kind-at (eye-at {:x (+ x 0.5) :y y :z (+ z 0.5)}) cell))))

(defn body-eye
  "The eye of the job's body where it stands now (a whole x or z is a cell: the body stands at its middle)."
  [c]
  (let [{:keys [x z] :as pos} (u/self-pos c)
        mid #(if (= % (js/Math.floor %)) (+ % 0.5) %)]
    (eye-at (assoc pos :x (mid x) :z (mid z)))))

(defn body-sees?
  "Whether the job's body, where it stands now, sees the block at cell [x y z]."
  [c cell]
  (sees-cell? (seen-kind-at (:primitives c)) (body-eye c) cell))

(defn over-slabs
  "kind-at with a bottom slab :open to a line that stays at or above its top (y at or above the cell's middle)."
  [kind-at p ey ty]
  (fn [x y z]
    (let [k (kind-at x y z)]
      (if (and (keyword-identical? :solid k) (>= (min ey ty) (+ y 0.5))
               (let [b (u/seen-block p {:x x :y y :z z})]
                 (and (some-> b .-name (str/ends-with? "_slab")) (= "bottom" (some-> b .-properties .-type)))))
        :open
        k))))

(defn body-sees-entity?
  "Whether the job's body, where it stands now, has a line from its eye to the body (middle) or head of an entity of
  height h standing at pos {:x :y :z}: a player aims over a bottom slab at a head that rises above it."
  [c pos h]
  (let [p (:primitives c)
        [ex ey ez] (body-eye c)
        {:keys [x y z]} pos]
    (boolean (some (fn [ty] (let [kind-at (over-slabs (seen-kind-at p) p ey ty)]
                              (sight/line-clear ex ey ez x ty z (fn [cx cy cz] (keyword-identical? :solid (kind-at cx cy cz))))))
                   [(+ y (* 0.5 h)) (+ y (* 0.85 h))]))))
