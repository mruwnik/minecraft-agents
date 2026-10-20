(ns jobs.lib.pillar
  "What the pillar job shares with the jobs that stand on or beside one (bridge, fell-tree, respond.refuge): the blocks
  it builds with, the cell a free cell is, the cell the body stands in, what is carried."
  (:require [jobs.lib.block-sets :as bs]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.util :as u]))

(defn clear?
  "A cell the body's feet or head can move into: air or a non-fluid replaceable plant."
  [n]
  (boolean (and n (bs/replaceable n) (not (bs/fluids n)))))

(defn feet-cell
  "The cell the body's centre is in: the pillar's column is the cell it began in (recentre! walks back to it), not
  the planner's start cell on a block's edge."
  [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]))

(defn carried [p]
  (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} (u/inventory p)))

(def no-hitbox
  "Entities a block is placed through."
  #{"item" "experience_orb" "arrow" "spectral_arrow" "area_effect_cloud" "marker"})

(defn mob-in
  "The first of entities (seen-entities maps; width 0.6 and height 1.8 when not given) whose box overlaps cell, or
  nil: the server refuses a block there."
  [entities [x y z]]
  (first (filter (fn [{:keys [name pos width height]}]
                   (let [hw (/ (or width 0.6) 2)
                         {ex :x ey :y ez :z} pos]
                     (and ex (not (no-hitbox name))
                          (< (- ex hw) (inc x)) (> (+ ex hw) x)
                          (< (- ez hw) (inc z)) (> (+ ez hw) z)
                          (< ey (inc y)) (> (+ ey (or height 1.8)) y))))
                 entities)))
