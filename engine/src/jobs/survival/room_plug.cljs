(ns jobs.survival.room-plug
  "The closed room a body stands in, read for the shelter: the roof hole to mend (room-plug) so the room is closed again.
  Used by jobs.survival.enter-room and the night's ways."
  (:require [jobs.lib.reach :as reach]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in-cells :as dig-cells]))

(defn room-with-plug
  "The closed room (reach-room/room-from, open doors passable) a walker at the feet cell reaches with the plug cell solid, nil when
  open or it meets an unseen cell."
  [p {fx :x fy :y fz :z} {px :x py :y pz :z}]
  (let [kind-at (reach-room/room-kind-at p (reach-room/memo-kind-at (fn [x y z] (reach/kind-of (u/seen-block p {:x x :y y :z z})))))]
    (reach-room/room-from p (fn [x y z] (if (and (= x px) (= y py) (= z pz)) :solid (kind-at x y z))) fx fy fz)))

(defn room-plug
  "In a closed room with a door whose roof has a hole over the body: the cell to mend, the lowest of y+2 ..
  y+roof-height straight above the feet that is not solid and has a solid side neighbour to be placed against, when
  once filled it leaves the feet in a closed room (room-with-plug) with a door or gate in its walls.
  nil when there is none (open ground, a room with an open door or doorway, a shaft or pit with no door): walls or a
  pit then."
  [p {:keys [x y z] :as feet} roof-height]
  (let [column (map (fn [dy] {:x x :y (+ y dy) :z z}) (range 2 (inc roof-height)))
        plug (first (filter #(and (not (sh-sense/solid-at? p %)) (dig-cells/supported? p %)) column))
        room (when plug (room-with-plug p feet plug))]
    (when (seq (:doors room))
      plug)))
