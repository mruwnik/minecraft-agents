(ns jobs.lib.spawn-light
  "Where mobs may spawn on a walk or a stay, by the light the body knows (Q334): the chance a second at a cell
  (chance-at), the seconds a walk spends where one could spawn (dark-seconds) and the mobs met spawning on timed cells
  (spawns-over) or held at one (spawns-at). The one spawn count jobs.lib.walk-risk, the flee search, a way out of a
  shelter and a held spot price by (jobs.lib.cost/death-chance's :spawns). A roofed dark cell counts only in a dark hall
  (hall-at?, Q345)."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]
            [jobs.lib.look :as look]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::surface-band {:default 4 :spec (a/num-in 0 nil)
                   :doc "Blocks below the body's feet that unseen ground still counts as open surface; deeper is underground, where only hostiles count."}
   ::step {:default 4 :spec (a/num-in 1 nil)
           :doc "Blocks between the cells of the way whose light is judged; a way longer than ::max-cells steps is judged at that many cells, spaced wider."}
   ::max-cells {:default 256 :spec (a/int-in 1 nil)
                :doc "Most cells of the way whose light is judged, so a far goal costs a bounded number of reads."}})

(defn hall-cell
  "The open cell the body would stand in at `to`: the feet when `to` is solid rock or unseen. A torch there ends a hall-at?."
  [p to]
  (let [to (update-vals to js/Math.floor)]
    (if (or (nil? (:y to)) (sh-sense/solid-at? p to) (not (sh-sense/seen-cell? p to))) (sh-sense/feet p) to)))

(defn hall-at?
  "Whether the dark place `to` is in a dark hall (sh-inside/dark-hall-at?), judged from its hall-cell."
  [p to]
  (sh-inside/dark-hall-at? p (hall-cell p to)))

(defn deep?
  "Whether an unseen cell is guessed a cave, not open surface: the body is underground, or the cell lies below the surface band."
  [cell feet-y underground?]
  (or underground? (< (:y cell) (- feet-y (settings/get settings ::surface-band)))))

(defn seen-light
  "The block light of cell, nil when the body has not seen it and cannot see it by turning to it."
  [p cell]
  (when (or (sh-sense/seen-cell? p cell) (look/glimpse? p cell)) (sh-sense/block-light-at p cell)))

(defn chance-at
  "The spawn chance per second at `cell` for a body at feet: a seen cell by its light (sky reaching it down its column; a roofed cell is dark only in a dark hall), an unseen one
  at the worse case (Q16): by the open-sky guess when it lies within the surface band below the feet and the body is not
  underground, else as a dark cave (a goal above the body is no sign that the ground at its own level is a cave), none
  under caves-free?."
  [p cell feet-y underground? caves-free?]
  (let [night (not (look/lit-light? 0 15 (sh-sense/sky-subtract-now p)))
        sky (if night 0 15)
        cell (if (sh-sense/solid-at? p cell) (update cell :y inc) cell)
        block (seen-light p cell)]
    (cond
      block (cost/spawn-chance [(:x cell) (:y cell) (:z cell)]
                               {:light (constantly {:block block :sky (cond (sh-sense/sky-reaches-from-above? p cell) sky (and (< block (look/dark-light)) (hall-at? p cell)) 0 :else 15)})})
      (deep? cell feet-y underground?) (if caves-free? 0 (cost/spawn-chance [(:x cell) (:y cell) (:z cell)] {:light (constantly {:block 0 :sky 0})}))
      :else (cost/spawn-chance [(:x cell) (:y cell) (:z cell)] {:light (constantly {:block 0 :sky sky})}))))

(defn cave-guess?
  "Whether chance-at prices cell only by the unseen worst case of a dark cave (caves-free? undoes it) that a look may yet
  show lit: one a look would show dark (look/dark-in-sight?) is no guess."
  [p cell feet-y underground?]
  (let [cell (if (sh-sense/solid-at? p cell) (update cell :y inc) cell)]
    (and (deep? cell feet-y underground?)
         (nil? (seen-light p cell))
         (pos? (cost/spawn-chance [(:x cell) (:y cell) (:z cell)] {:light (constantly {:block 0 :sky 0})}))
         (not (look/dark-in-sight? p cell)))))

(defn dark-seconds
  "Seconds of the walk over cells where a mob could spawn (chance-at), times (seconds from the start at each cell): every
  k-th cell and the last stand for the stretch walked since the one before, k the larger of ::step and the cells over
  ::max-cells."
  [p cells times feet-y underground? caves-free?]
  (let [n (count cells)
        k (js/Math.ceil (max (settings/get settings ::step) (/ n (settings/get settings ::max-cells))))
        ends (distinct (concat (range k n k) [(dec n)]))]
    (reduce + (for [[a b] (map vector (cons 0 ends) ends)
                    :when (pos? (chance-at p (nth cells b) feet-y underground? caves-free?))]
                (- (times b) (times a))))))

(defn spawns-over
  "The mobs (an expected count) met spawning on the timed cells ([{:x :y :z :t}], :t seconds from the start, the first
  where the body stands) of a walk: cost/spawn-chance a second over its dark-seconds."
  [p cells]
  (if (next cells)
    (let [feet (sh-sense/feet p)]
      (* (cost/spawn-chance [0 0 0])
         (dark-seconds p (vec cells) (mapv :t cells) (:y feet) (sh-sense/buried? p) false)))
    0))

(defn spawns-at
  "The mobs (an expected count) met spawning in secs seconds held at cell (chance-at of the body's place)."
  [p cell secs]
  (* secs (chance-at p (update-vals (select-keys cell [:x :y :z]) js/Math.floor) (:y (sh-sense/feet p)) false false)))

(defn walked
  "Cells ({:x :y :z} or [x y z]) walked one after another from `from` at cost/walk-blocks-per-s, timed for spawns-over:
  from first at :t 0."
  [from cells]
  (let [cells (into [(select-keys from [:x :y :z])] (map #(if (vector? %) (zipmap [:x :y :z] %) %)) cells)]
    (mapv #(assoc %1 :t (/ %2 (cost/walk-blocks-per-s))) cells (reductions + 0 (map u/dist cells (rest cells))))))
