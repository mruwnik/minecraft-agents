(ns jobs.lib.build.fill
  "Fill under a plan's floor: the dirt column that gives a floor with nothing to click beside it a block to start from, priced as earthwork."
  (:require [jobs.lib.cost :as cost]
            [jobs.lib.places :as places]
            [jobs.lib.placement :as placement]))

(def hollow #{"air" "cave_air" "void_air" "water"})

(def steps [[0 -1 0] [1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [0 1 0]])

(defn islands
  "The lowest layer's cell positions grouped into sets that touch each other sideways."
  [poss]
  (let [all (set poss)
        flood (fn [seen start]
                (loop [seen seen todo [start]]
                  (if-let [[x y z :as pos] (peek todo)]
                    (let [next (remove seen [[(inc x) y z] [(dec x) y z] [x y (inc z)] [x y (dec z)]])]
                      (recur (into seen (filter all next)) (into (pop todo) (filter all next))))
                    seen)))]
    (loop [left all out []]
      (if-let [start (first left)]
        (let [island (flood #{start} start)] (recur (apply disj left island) (conj out island)))
        out))))

(defn max-depth
  "The deepest fill column (blocks) that earthwork (cost/earthwork-s) prices within `worth` seconds, the build's worth; nil worth: no cap."
  [worth]
  (if (nil? worth) ##Inf (long (/ (max 0 worth) (cost/earthwork-s 1)))))

(defn support-column
  "The fill column [pos ...] (highest first) under the cheapest cell of an island with no block beside any of its cells to click, or nil:
  hollow seen cells down to a seen solid one, no longer than depth nor below the world's floor."
  [block-at planned depth island]
  (let [name-at #(:name (block-at %))
        held? (fn [pos] (some #(and (not (planned (mapv + pos %))) (placement/clickable? (block-at (mapv + pos %)))) steps))
        column (fn [[x y z]]
                 (let [below (vec (take-while #(and (not (planned %)) (hollow (name-at %))) (map (fn [dy] [x (- y dy) z]) (range 1 (inc (min depth (- y places/min-y)))))))
                       ground (name-at [x (- y 1 (count below)) z])]
                   (when (and (seq below) ground (not (hollow ground))) below)))]
    (when-not (some held? island)
      (first (sort-by count (keep column island))))))

(defn with-fill
  "cells with the fill column under each island of the lowest layer that has nothing to click (support-column), shallowest first
  while (price n) of all n fill blocks so far stays within worth (nil: no cap), else cells. block-at: pos -> {:name} or nil (unseen)."
  [block-at worth price cells]
  (let [live (remove #(= :clear (:want %)) cells)
        low (when (seq live) (apply min (map #(get (:pos %) 1) live)))
        floor (filter #(= low (get (:pos %) 1)) live)
        planned (set (map :pos cells))
        by-pos (into {} (map (juxt :pos identity)) floor)
        columns (sort-by count (keep #(support-column block-at planned (max-depth worth) %) (islands (keys by-pos))))
        paid (reduce (fn [paid column]
                       (if (or (nil? worth) (<= (price (+ (count (apply concat paid)) (count column))) worth)) (conj paid column) paid))
                     [] columns)
        fill (fn [column] (map (fn [pos] {:pos pos :want "dirt" :part (:part (by-pos (update (first column) 1 inc))) :fill true}) column))]
    (into (vec cells) (mapcat fill) paid)))
