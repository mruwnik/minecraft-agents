(ns jobs.lib.solid
  "What counts as solid for roofs, walls, floors and arrows, by block name, and the block cell of a position."
  (:require [jobs.lib.block-sets :as bs]))

(def non-solid
  "Block names that are not a roof: air, fluids, plants and fixtures a body can stand in, powder snow (mobs and arrows pass it), and the invisible light block."
  (into (into bs/air bs/fluids)
        #{"short_grass" "grass" "tall_grass" "fern" "large_fern" "torch" "wall_torch" "soul_torch" "soul_wall_torch" "snow" "vine" "dead_bush"
          "seagrass" "tall_seagrass" "fire" "sugar_cane" "powder_snow"}))

(defn solid?
  "Whether a block name counts as solid: not nil (unloaded), not non-solid or walk-through, and not leaves or a
  sapling."
  [name]
  (and (some? name)
       (not (non-solid name))
       (not (re-find bs/walk-through name))
       (not (.endsWith name "_leaves"))
       (not (.endsWith name "_sapling"))))

(defn hides-cells?
  "Whether a block shows nothing of the cells behind it: solid, or powder snow (opaque yet no wall)."
  [name]
  (or (solid? name) (= "powder_snow" name)))

(defn room?
  "Whether a body can stand or step in a block: not nil (unloaded), not solid, and not a bad-floor block (powder snow
  is no wall or roof, yet a body sinks into it)."
  [name]
  (and (some? name) (not (solid? name)) (not (bs/bad-floor name))))

(defn cell
  "The block cell {:x :y :z} containing a position."
  [{:keys [x y z]}]
  {:x (js/Math.floor x) :y (js/Math.floor y) :z (js/Math.floor z)})
