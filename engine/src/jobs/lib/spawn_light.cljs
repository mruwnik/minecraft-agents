(ns jobs.lib.spawn-light
  "Where mobs may spawn on a walk or a stay, by the light the body knows (Q334): the chance a second at a cell
  (chance-at), the seconds a walk spends where one could spawn (dark-seconds) and the mobs met spawning on timed cells
  (spawns-over) or held at one (spawns-at), and a walk's exposure for the readiness checks. The one spawn count
  jobs.lib.walk-risk, the flee search, a way out of a shelter, a held spot, readiness and bed-down price by (jobs.lib.cost/death-chance's :spawns). A dark cell counts only where a mob spawns
  beyond 24 blocks and walks to it, roofed or not (Q345, Q45): by the chance it is in a dark hall (hall-chance)."
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
  "The open cell the body would stand in at `to`: the feet when `to` is solid rock or unseen. A torch there ends a dark hall."
  [p to]
  (let [to (update-vals to js/Math.floor)]
    (if (or (nil? (:y to)) (sh-sense/solid-at? p to) (not (sh-sense/seen-cell? p to))) (sh-sense/feet p) to)))

(defn hall-chance
  "The chance a mob spawned farther than 24 blocks walks to the dark place `to` (cost/hall-chance of its
  sh-inside/dark-hall, by the sky's darkening subtract), judged from its hall-cell."
  [p to subtract]
  (cost/hall-chance (sh-inside/dark-hall p (hall-cell p to) subtract)))

(defn seen-light
  "The block light of cell, nil when the body has not seen it and cannot see it by turning to it."
  [p cell]
  (when (or (sh-sense/seen-cell? p cell) (look/glimpse? p cell)) (sh-sense/block-light-at p cell)))

(defn- night? [p] (not (look/lit-light? 0 15 (sh-sense/sky-subtract-now p))))

(defn chance-at
  "The spawn chance per second at `cell`: a seen cell by its light (its own sky light; one that leaves dark is dark by
  the chance it is in a dark hall, hall-chance), an unseen one at the chance it is dark (look/unseen-lit-chance); under caves-free? an unseen one
  only at the chance full day would light it and now does not. subtract: the sky's darkening to judge it by (default:
  now; 0: full day)."
  ([p cell caves-free?] (chance-at p cell caves-free? nil))
  ([p cell caves-free? subtract]
   (let [darkening (or subtract (sh-sense/sky-subtract-now p))
         cell (if (sh-sense/solid-at? p cell) (update cell :y inc) cell)
         block (seen-light p cell)
         reaches (sh-sense/sky-reaches-from-above? p cell)
         sky-light (sh-sense/sky-light-at p cell)
         ;; a sky-reached cell with no sky light data is priced as unseen, not as dark
         known (and block (or (not reaches) sky-light))]
     (cond
       known (let [at-sky #(cost/spawn-chance [(:x cell) (:y cell) (:z cell)] {:light (constantly {:block block :sky %})})]
               (if (or (look/lit-light? 0 (or sky-light 0) darkening) (>= block (look/dark-light)))
                 (at-sky 15)
                 (let [hall (hall-chance p cell darkening)]
                   (+ (* hall (at-sky 0)) (* (- 1 hall) (at-sky 15))))))
       :else (let [now (look/unseen-lit-chance p cell darkening)
                   ;; caves-free? frees the dark full day leaves too: the chance left is that of dark only the sky lifts
                   dark (if caves-free? (max 0 (- (look/unseen-lit-chance p cell 0) now)) (- 1 now))]
                (cost/spawn-chance [(:x cell) (:y cell) (:z cell)] {:light (constantly {:dark dark})}))))))

(defn night-chance
  "chance-at of a stay at cell through the night: the sky darkened as at a clear midnight, or more."
  [p cell]
  (chance-at p cell false (max (sh-sense/sky-subtract-now p) (look/sky-subtract-at 18000 0 0))))

(defn dawn-lowers?
  "Whether day would lower chance-at at cell: it is night and the cell's light comes from the sky."
  [p cell]
  (and (night? p) (< (chance-at p cell false 0) (chance-at p cell false))))

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
  "chance-at of cell (under the sky's darkening subtract, default now) as a share of that of a cell certainly dark: 1 dark, 0 lit, the chance of dark between."
  [p cell caves-free? & [subtract]]
  (let [dark (cost/spawn-chance [0 0 0])]
    (if (pos? dark) (/ (chance-at p cell caves-free? subtract) dark) 0)))

(defn share-of
  "dark-share of cells {:x :y :z} (caves-free?, subtract as there), each cell read once and their spawn-regions
  flooded once (sh-inside/*regions*, the caller's when bound): for the walks of one decision."
  [p caves-free? subtract]
  (let [kept (js/Map.)
        regions (or sh-inside/*regions* (js/Map.))]
    (fn [{:keys [x y z] :as cell}]
      (let [k (str x "," y "," z)]
        (if (.has kept k)
          (.get kept k)
          (let [v (binding [sh-inside/*regions* regions] (dark-share p (select-keys cell [:x :y :z]) caves-free? subtract))] (.set kept k v) v))))))

(defn dark-seconds
  "Seconds of the walk where a mob could spawn, times (seconds from the start at each cell): every k-th cell and the
  last stand for the stretch walked since the one before, at its dark-share, k the larger of ::step and the cells over
  ::max-cells; subtract: the sky's darkening to judge them by (default now); share: a share-of to read them by."
  [p cells times caves-free? & [subtract share]]
  (let [n (count cells)
        k (js/Math.ceil (max (settings/get settings ::step) (/ n (settings/get settings ::max-cells))))
        ends (distinct (concat (range k n k) [(dec n)]))
        share (or share (share-of p caves-free? subtract))]
    (reduce + (for [[a b] (map vector (cons 0 ends) ends)]
                (* (- (times b) (times a)) (share (nth cells b)))))))

(defn spawns-over
  "The mobs (an expected count) met spawning on the timed cells ([{:x :y :z :t}], :t seconds from the start, the first
  where the body stands) of a walk: cost/spawn-chance a second over its dark-seconds (read by share, a share-of not
  caves-free?, nil: its own)."
  [p cells share]
  (if (next cells)
    (* (cost/spawn-chance [0 0 0]) (dark-seconds p (vec cells) (mapv :t cells) false nil share))
    0))

(defn spawns-at
  "The mobs (an expected count) met spawning in secs seconds held at cell (chance-at of the body's place)."
  [p cell secs]
  (* secs (chance-at p (update-vals (select-keys cell [:x :y :z]) js/Math.floor) false)))

(defn walked
  "Cells ({:x :y :z} or [x y z]) walked one after another from `from` at cost/walk-speed, timed for spawns-over:
  from first at :t 0."
  [from cells]
  (let [cells (into [(select-keys from [:x :y :z])] (map #(if (vector? %) (zipmap [:x :y :z] %) %)) cells)]
    (mapv #(assoc %1 :t (/ %2 cost/walk-speed)) cells (reductions + 0 (map u/dist cells (rest cells))))))

(defn exposure
  "{:chance :lowered-by} of the walk of p to `to` ({:x :y :z}, a column {:x :z} at the feet's height; floored): chance-at
  the judged cell and what lowers it (data the readiness checks read; empty: nothing foreseeable does). A solid `to` is
  judged by the first seen open cell over it (the air over a pile, not the unseen logs between), else the cell over it.
  A cell inside the shelter (sh-inside/walk-leaves-shelter?, with the body's latest :shelter entry data when given) that
  p has not made out is lowered by #{:look}, with :look-at [[x y z]]; else #{:light} where dawn-lowers? it (a cell the
  sky reaches, or any outside the shelter), with :light-in, the seconds until light."
  ([p to] (exposure p to nil))
  ([p to shelter]
   (let [to (update-vals (assoc to :y (or (:y to) (:y (sh-sense/feet p)))) js/Math.floor)
         solid? (sh-sense/solid-at? p to)
         over (fn [y] (assoc to :y y))
         open-over (when solid? (first (filter #(and (sh-sense/seen-cell? p (over %)) (not (sh-sense/solid-at? p (over %)))) (range (inc (:y to)) (+ (:y to) 32)))))
         at (over (or open-over (if solid? (inc (:y to)) (:y to))))
         cell [(:x at) (:y at) (:z at)]
         leaves? (sh-inside/walk-leaves-shelter? p shelter to 1)
         chance (chance-at p at false)
         wait (when (and (or leaves? (sh-sense/sky-reaches-from-above? p at)) (dawn-lowers? p at))
                (if leaves? (sh-sense/sky-in p) (sh-sense/light-in p at)))]
     (cond
       (and (not leaves?) (nil? (seen-light p at))) {:chance chance :lowered-by #{:look} :look-at [cell]}
       (and wait (pos? wait)) {:chance chance :lowered-by #{:light} :light-in wait}
       :else {:chance chance :lowered-by #{}}))))
