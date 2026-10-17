(ns jobs.lib.aim
  "Whether an eye can aim at a block, as a player must to dig it, place against it or use it: a clear line from the eye
  to the block's centre or to a point just inside one of its faces, over the cells the body has seen (a cell never seen
  is in nobody's way). go-to's :see and jobs.lib.blocks/in-reach? ask it."
  (:require [engine.game :as game]
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
  "(fn [x y z]) -> reach/kind-of of the block the body of primitives p sees or remembers there (never seen: :open)."
  [p]
  (fn [x y z] (reach/kind-of (u/seen-block p {:x x :y y :z z}))))

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

(defn stand-sees-fn
  "(fn [x y z]) -> whether a body standing in the feet cell x y z (at its middle) sees the block at cell, over what the
  body of primitives p has seen: the planner's goalSeen for go-to's :see."
  [p cell]
  (let [kind-at (seen-kind-at p)]
    (fn [x y z] (sees-cell? kind-at (eye-at {:x (+ x 0.5) :y y :z (+ z 0.5)}) cell))))

(defn body-sees?
  "Whether the job's body, where it stands now, sees the block at cell [x y z]. A whole x or z is a cell, and a body
  stands at the middle of its cell (the same convention as go-to's stand-sees-fn)."
  [c cell]
  (let [{:keys [x z] :as pos} (u/self-pos c)
        mid #(if (= % (js/Math.floor %)) (+ % 0.5) %)]
    (sees-cell? (seen-kind-at (:primitives c)) (eye-at (assoc pos :x (mid x) :z (mid z))) cell)))
