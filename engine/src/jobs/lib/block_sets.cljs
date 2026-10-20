(ns jobs.lib.block-sets
  "The named sets of block names every job reads, each defined once. They differ on purpose: air is only open
  cells; replaceable is what a placement overwrites; passable? is what a body walks through; harms-in-cell and
  bad-floor are what hurts to stand in or on."
  (:require [jobs.lib.solid :as solid]
            [plan.shape :as shape]))

(def air "Open cells (plan.shape/air-names); the invisible light block counts: a body walks through it." (conj shape/air-names "light"))

(def fluids #{"water" "lava" "bubble_column"})

(def replaceable
  "Names a placed block takes the place of: air, fluids (plugging a source, bridging water, sealing a leak) and the
  plants, snow and invisible light blocks a placement overwrites."
  (into (into air fluids) #{"short_grass" "tall_grass" "fern" "large_fern" "dead_bush" "snow" "vine" "glow_lichen" "leaf_litter" "hanging_roots"}))

(def flowers
  "Flowers and small plants a body walks through and a placement does not overwrite."
  #{"dandelion" "poppy" "blue_orchid" "allium" "azure_bluet" "red_tulip" "orange_tulip" "white_tulip" "pink_tulip"
    "oxeye_daisy" "cornflower" "lily_of_the_valley" "torchflower" "pink_petals" "wildflowers" "short_dry_grass"
    "tall_dry_grass" "bush" "firefly_bush" "sunflower" "lilac" "rose_bush" "peony" "pitcher_plant" "wither_rose"})

(def harms-in-cell
  "Blocks that hurt a body standing in or on them."
  #{"lava" "fire" "soul_fire" "magma_block" "campfire" "soul_campfire" "cactus" "sweet_berry_bush" "wither_rose"})

(def bad-floor
  "Names that are no floor to stand on: the harmful blocks and the ones a body sinks into or is held by."
  (into harms-in-cell #{"powder_snow" "cobweb" "tripwire"}))

(def passable-pattern
  "Blocks without a collision box besides the named sets: torches, saplings, tulips, orchids and the solid.cljs walk-through list."
  #"(torch|_sapling|_tulip|_orchid|_flower)$")

(def slows-or-triggers
  "Walk-through blocks a body avoids: cobweb slows it to a crawl, tripwire sets off traps."
  #{"cobweb" "tripwire"})

(defn passable?
  "Whether a block name is one a body walks through (no collision, not a fluid, not cobweb or tripwire): open cells, replaceable plants,
  flowers, rails, signs, carpets, torches, saplings, berry bushes. nil (an unloaded cell) is not passable."
  [name]
  (boolean
   (and name (not (fluids name)) (not (slows-or-triggers name))
        (or (replaceable name) (flowers name) (= "grass" name) (= "sweet_berry_bush" name)
            (re-find passable-pattern name) (re-find solid/walk-through name)))))

(defn passable-or-water?
  "passable?, or water a walker swims through."
  [name]
  (or (= "water" name) (passable? name)))
