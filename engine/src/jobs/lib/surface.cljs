(ns jobs.lib.surface
  (:require [jobs.lib.blocks :as b]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.trees :as trees]))

(defn open? "Air, or a plant a hoe's user digs first (grass, flowers): no ground." [n] (or (bs/air n) (contains? b/clearable n)))
(defn canopy? "A log or leaf block: a tree stands on the ground, it is not it." [n] (or (trees/log-name? n) (trees/leaves-name? n)))
(def no-floor (into (into bs/fluids bs/bad-floor) #{"ice" "frosted_ice"}))

(defn column-top
  "The y of the surface of column x z: the highest non-air block scanning from
  (+ from-y depth) down to (- (+ from-y 1) depth), when air lies above it. Rock
  or unseen cells at the start of the scan are a roof (a cave ceiling) and are
  skipped down to the first air, so the top is the cave floor. nil for an
  unloaded cell below that air, a fluid, ice or magma surface, nothing; grass,
  flowers and trees (logs, leaves) are skipped (the top is the ground under
  them, where till works). name-fn takes {:x :y :z} and returns a block name or nil."
  [name-fn x z from-y depth]
  (let [low (inc (- from-y depth))
        open-cell? #(or (open? %) (canopy? %))]
    (loop [y (+ from-y depth) roof? true]
      (when (>= y low)
        (let [n (name-fn {:x x :y y :z z})]
          (cond
            (and roof? (or (nil? n) (not (open-cell? n))) (not (no-floor n)))
            (if (and n (some-> (name-fn {:x x :y (inc y) :z z}) open-cell?))
              y
              (recur (dec y) true))
            (nil? n) nil
            (open-cell? n) (recur (dec y) false)
            (no-floor n) nil
            :else (when-let [above (name-fn {:x x :y (inc y) :z z})]
                    (when (open-cell? above) y))))))))
