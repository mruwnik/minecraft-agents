(ns jobs.lib.land
  "The one test for a cell a body can stand on dry: feet and head air or a passable plant, over a solid block that is no fluid, plant or hazard."
  (:require [jobs.lib.block-sets :as bs]
            [jobs.lib.blocks :as b]))

(def unsafe-below (into (into bs/fluids bs/bad-floor) #{"seagrass" "tall_seagrass" "kelp" "kelp_plant"}))

(defn floor?
  "Whether a block name can be stood on: not air, a fluid, a plant or a hazard."
  [below]
  (boolean (and below (not (bs/air below)) (not (bs/fluids below)) (not (b/clearable below))
                (not (contains? unsafe-below below)))))

(defn land-cell?
  "Whether cell is a stand cell by name-at (a fn cell -> block name or nil; nil, an unloaded or unknown cell, is not land).
  The block below is read first: a known non-floor there settles it without reading the feet and head cells."
  [name-at {:keys [x y z]}]
  (and (floor? (name-at {:x x :y (dec y) :z z}))
       (let [feet (name-at {:x x :y y :z z})
             head (name-at {:x x :y (inc y) :z z})]
         (boolean (and feet head (bs/passable? feet) (bs/passable? head))))))
