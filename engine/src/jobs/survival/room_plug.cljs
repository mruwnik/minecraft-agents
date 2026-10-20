(ns jobs.survival.room-plug
  "The closed room a body stands in, read for the shelter: the roof hole to mend (room-plug) so the room is closed again.
  Used by jobs.survival.enter-room and the night's ways."
  (:require [jobs.lib.click :as click]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in-cells :as dig-cells]))

(def room-limit
  "Most cells a closed room may have (the flood from the feet stops there and the room counts as open)."
  256)

(def room-reach
  "Farthest a closed room's cell may be from the feet along any axis."
  8)

(defn room-wall?
  "Whether a cell bounds a room against mobs: sealed?, and not an openable door, gate or trapdoor standing open."
  [p cell]
  (let [b (u/seen-block p cell)]
    (and (dig-cells/sealed? p cell)
         (not (and (= :openable (click/kind-of (.-name b))) (click/reached? :open (click/props-of b)))))))

(defn room-cells
  "The cells reachable from the feet cell through cells that are not room-wall? (six neighbours, the cell plug counted
  as a wall) when they form a closed room: at most room-limit of them, none farther than room-reach from the feet on
  any axis, none unloaded. nil when they do not."
  [p {fx :x fy :y fz :z :as feet} plug]
  (loop [todo [feet] seen #{feet}]
    (if-let [{:keys [x y z] :as cell} (peek todo)]
      (cond
        (> (count seen) room-limit) nil
        (< room-reach (max (js/Math.abs (- x fx)) (js/Math.abs (- y fy)) (js/Math.abs (- z fz)))) nil
        (nil? (u/seen-block p cell)) nil
        :else (let [next (for [d dig-cells/neighbours
                               :let [n (dig-cells/shift cell d)]
                               :when (and (not (seen n)) (not= n plug) (not (room-wall? p n)))]
                           n)]
                (recur (into (pop todo) next) (into seen next))))
      seen)))

(defn door-of?
  "Whether a cell next to one of the room's cells holds a door, gate or trapdoor a hand opens: the room's way out."
  [p cells]
  (boolean (some (fn [cell] (some #(= :openable (click/kind-of (u/seen-name p (dig-cells/shift cell %)))) dig-cells/neighbours)) cells)))

(defn room-plug
  "In a closed room with a door whose roof has a hole over the body: the cell to mend, the lowest of y+2 ..
  y+roof-height straight above the feet that is not solid and has a solid side neighbour to be placed against, when
  once filled it leaves the feet in a closed room (room-cells) with a door, gate or trapdoor in its walls (door-of?).
  nil when there is none (open ground, a room with an open door or doorway, a shaft or pit with no door): walls or a
  pit then."
  [p {:keys [x y z] :as feet} roof-height]
  (let [column (map (fn [dy] {:x x :y (+ y dy) :z z}) (range 2 (inc roof-height)))
        plug (first (filter #(and (not (sh-sense/solid-at? p %)) (dig-cells/supported? p %)) column))
        cells (when plug (room-cells p feet plug))]
    (when (and cells (door-of? p cells))
      plug)))
