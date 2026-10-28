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
  {::step {:default 4 :spec (a/num-in 1 nil)
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

(defn seen-light
  "The block light of cell, nil when the body has not seen it and cannot see it by turning to it."
  [p cell]
  (when (or (sh-sense/seen-cell? p cell) (look/glimpse? p cell)) (sh-sense/block-light-at p cell)))

(defn- night? [p] (not (look/lit-light? 0 15 (sh-sense/sky-subtract-now p))))

(defn- chance-by
  "chance-at with the sky darkened as now (night) or not at all (an unseen cell guessed by full day)."
  [p cell caves-free? night]
  (let [darkening (if night (sh-sense/sky-subtract-now p) 0)
        cell (if (sh-sense/solid-at? p cell) (update cell :y inc) cell)
        block (seen-light p cell)
        reaches (sh-sense/sky-reaches-from-above? p cell)
        sky-light (sh-sense/sky-light-at p cell)
        ;; a sky-reached cell with no sky light data is priced as unseen, not as dark
        known (and block (or (not reaches) sky-light))]
    (cond
      known (let [sky (if (look/lit-light? 0 (or sky-light 0) darkening) 15 0)]
              (cost/spawn-chance [(:x cell) (:y cell) (:z cell)]
                                 {:light (constantly {:block block :sky (cond reaches sky (and (< block (look/dark-light)) (hall-at? p cell)) 0 :else 15)})}))
      :else (let [now (look/unseen-lit-chance p cell (when-not night 0))
                  ;; caves-free? frees the dark full day leaves too: the chance left is that of dark only the sky lifts
                  dark (if caves-free? (max 0 (- (look/unseen-lit-chance p cell 0) now)) (- 1 now))]
              (cost/spawn-chance [(:x cell) (:y cell) (:z cell)] {:light (constantly {:dark dark})})))))

(defn chance-at
  "The spawn chance per second at `cell`: a seen cell by its light (sky reaching it down its column; a roofed cell is dark
  only in a dark hall), an unseen one at the chance it is dark (look/unseen-lit-chance); under caves-free? an unseen one
  only at the chance full day would light it and now does not."
  [p cell caves-free?]
  (chance-by p cell caves-free? (night? p)))

(defn dawn-lowers?
  "Whether day would lower chance-at at cell: it is night and the cell's light comes from the sky."
  [p cell]
  (and (night? p) (< (chance-by p cell false false) (chance-at p cell false))))

(defn dark-guess?
  "Whether chance-at prices cell only by the guess that it is dark (caves-free? undoes it) that a look may yet show lit:
  an unseen cell even full day would not light (look/unseen-lit? by day: guessed out of the sky's reach, where only a
  light it has not seen would light it). One dark by the night sky alone, or one a look would show dark
  (look/dark-in-sight?), is no guess."
  [p cell]
  (let [cell (if (sh-sense/solid-at? p cell) (update cell :y inc) cell)]
    (and (nil? (seen-light p cell))
         (not (look/unseen-lit? p cell 0))
         (pos? (cost/spawn-chance [(:x cell) (:y cell) (:z cell)] {:light (constantly {:block 0 :sky 0})}))
         (not (look/dark-in-sight? p cell)))))

(defn dark-share
  "chance-at of cell as a share of that of a cell certainly dark: 1 dark, 0 lit, the chance of dark between."
  [p cell caves-free?]
  (let [dark (cost/spawn-chance [0 0 0])]
    (if (pos? dark) (/ (chance-at p cell caves-free?) dark) 0)))

(defn dark-seconds
  "Seconds of the walk where a mob could spawn, times (seconds from the start at each cell): every k-th cell and the
  last stand for the stretch walked since the one before, at its dark-share, k the larger of ::step and the cells over
  ::max-cells."
  [p cells times caves-free?]
  (let [n (count cells)
        k (js/Math.ceil (max (settings/get settings ::step) (/ n (settings/get settings ::max-cells))))
        ends (distinct (concat (range k n k) [(dec n)]))]
    (reduce + (for [[a b] (map vector (cons 0 ends) ends)]
                (* (- (times b) (times a)) (dark-share p (nth cells b) caves-free?))))))

(defn spawns-over
  "The mobs (an expected count) met spawning on the timed cells ([{:x :y :z :t}], :t seconds from the start, the first
  where the body stands) of a walk: cost/spawn-chance a second over its dark-seconds."
  [p cells]
  (if (next cells)
    (* (cost/spawn-chance [0 0 0]) (dark-seconds p (vec cells) (mapv :t cells) false))
    0))

(defn spawns-at
  "The mobs (an expected count) met spawning in secs seconds held at cell (chance-at of the body's place)."
  [p cell secs]
  (* secs (chance-at p (update-vals (select-keys cell [:x :y :z]) js/Math.floor) false)))

(defn walked
  "Cells ({:x :y :z} or [x y z]) walked one after another from `from` at cost/walk-blocks-per-s, timed for spawns-over:
  from first at :t 0."
  [from cells]
  (let [cells (into [(select-keys from [:x :y :z])] (map #(if (vector? %) (zipmap [:x :y :z] %) %)) cells)]
    (mapv #(assoc %1 :t (/ %2 (cost/walk-blocks-per-s))) cells (reductions + 0 (map u/dist cells (rest cells))))))
