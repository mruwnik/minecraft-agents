(ns jobs.lib.surface
  (:require [jobs.lib.blocks :as b]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.trees :as trees]))

(defn open? "Air, or a plant a hoe's user digs first (grass, flowers): no ground." [n] (or (bs/air n) (contains? b/clearable n)))
(defn canopy? "A log or leaf block: a tree stands on the ground, it is not it." [n] (or (trees/log-name? n) (trees/leaves-name? n)))
(def no-floor (into (into bs/fluids bs/bad-floor) #{"ice" "frosted_ice"}))

(defn column-top
  "The y of the surface of column x z: the highest non-air block scanning from
  (+ from-y depth) down to (- (+ from-y 1) depth), when air lies above it. nil
  for an unloaded cell on the way, a fluid, ice or magma surface, nothing, or a
  covered top; grass, flowers and trees (logs, leaves) are skipped (the top is the ground under them, where till works). name-fn takes {:x :y :z} and returns a block name or nil."
  [name-fn x z from-y depth]
  (let [low (inc (- from-y depth))]
    (loop [y (+ from-y depth)]
      (when (>= y low)
        (let [n (name-fn {:x x :y y :z z})]
          (cond
            (nil? n) nil
            (or (open? n) (canopy? n)) (recur (dec y))
            (no-floor n) nil
            :else (when-let [above (name-fn {:x x :y (inc y) :z z})]
                    (when (or (open? above) (canopy? above)) y))))))))
