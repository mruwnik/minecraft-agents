(ns jobs.lib.pillar
  "What the pillar job shares with the jobs that stand on or beside one (bridge, fell-tree, retreat-refuge): the blocks
  it builds with, the cell a free cell is, the cell the body stands in, what is carried."
  (:require [jobs.lib.access.rules :as rules]
            [jobs.lib.util :as u]))

(def default-items ["dirt" "cobblestone"])

(defn clear?
  "A cell the body's feet or head can move into: air or a non-fluid replaceable plant."
  [n]
  (boolean (and n (rules/replaceable n) (not (rules/fluids n)))))

(defn feet-cell
  "The cell the body's centre is in: the pillar's column is the cell it began in (recentre! walks back to it), not
  the planner's start cell on a block's edge."
  [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]))

(defn carried [p]
  (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} (u/inventory p)))
