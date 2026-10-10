(ns jobs.lib.surface
  (:require [jobs.lib.blocks :as b]))

(def air #{"air" "cave_air" "void_air"})
(defn open? "Air, or a plant a hoe's user digs first (grass, flowers): no ground." [n] (or (air n) (contains? b/clearable n)))
(def no-floor #{"water" "lava" "bubble_column" "ice" "frosted_ice" "magma_block" "powder_snow"})

(defn column-top
  "The y of the surface of column x z: the highest non-air block scanning from
  (+ from-y depth) down to (- (+ from-y 1) depth), when air lies above it. nil
  for an unloaded cell on the way, a fluid, ice or magma surface, nothing, or a
  covered top; grass and flowers lying on the ground are skipped (the top is the ground under them, where till works). name-fn takes {:x :y :z} and returns a block name or nil."
  [name-fn x z from-y depth]
  (let [low (inc (- from-y depth))]
    (loop [y (+ from-y depth)]
      (when (>= y low)
        (let [n (name-fn {:x x :y y :z z})]
          (cond
            (nil? n) nil
            (open? n) (recur (dec y))
            (no-floor n) nil
            :else (when-let [above (name-fn {:x x :y (inc y) :z z})]
                    (when (open? above) y))))))))
