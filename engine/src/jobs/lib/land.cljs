(ns jobs.lib.land
  "The one test for a cell a body can stand on dry: feet and head air over a solid block that is no fluid, plant or hazard."
  (:require [jobs.lib.blocks :as b]))

(def unsafe-below #{"water" "lava" "fire" "soul_fire" "magma_block" "campfire" "soul_campfire"})

(defn land-cell?
  "Whether cell is a stand cell by name-at (a fn cell -> block name or nil; nil, an unloaded or unknown cell, is not land)."
  [name-at {:keys [x y z]}]
  (let [feet (name-at {:x x :y y :z z})
        head (name-at {:x x :y (inc y) :z z})
        below (name-at {:x x :y (dec y) :z z})]
    (boolean (and feet head below (b/air feet) (b/air head)
                  (not (b/air below)) (not (b/fluids below)) (not (b/clearable below))
                  (not (contains? unsafe-below below))))))
