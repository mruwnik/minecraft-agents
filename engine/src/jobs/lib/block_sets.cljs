(ns jobs.lib.block-sets
  "The named sets of block names every job reads, each defined once. They differ on purpose: air is only open
  cells; replaceable is what a placement overwrites; passable? is what a body walks through; harms-in-cell and
  bad-floor are what hurts to stand in or on."
  (:require [plan.shape :as shape]))

(def air "Open cells (plan.shape/air-names); the invisible light block counts: a body walks through it." (conj shape/air-names "light"))

(def fluids #{"water" "lava" "bubble_column"})

(def loosened
  "Grounds a hoe first turns into dirt (a second use makes farmland)."
  #{"coarse_dirt" "rooted_dirt"})

(def tillable (into #{"dirt" "grass_block" "dirt_path"} loosened))

(def farmable-ground "The grounds that are farmland or a hoe makes farmland of." (conj tillable "farmland"))

(def replaceable
  "Names a placed block takes the place of: air, fluids (plugging a source, bridging water, sealing a leak) and the
  plants, snow and invisible light blocks a placement overwrites."
  (into (into air fluids) #{"short_grass" "tall_grass" "fern" "large_fern" "dead_bush" "snow" "vine" "glow_lichen" "leaf_litter" "hanging_roots"}))

(defn open-floor?
  "Whether a floor cell holds nothing a placement cannot overwrite: air or a plant (not a fluid)."
  [name]
  (boolean (and name (replaceable name) (not (fluids name)))))

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

(def walk-through
  "Blocks a mob or body walks through or over (signs, banners, rails, plates, buttons, levers, carpets, tripwire,
  cobweb). Not solid: neither roof, wall nor floor."
  #"(_sign|_banner|_carpet|_pressure_plate|_button|_rail)$|^(rail|lever|tripwire|tripwire_hook|string|cobweb|redstone_wire)$")

(def passable-pattern
  "Blocks without a collision box besides the named sets: torches, saplings, tulips, orchids and the solid.cljs walk-through list."
  #"(torch|_sapling|_tulip|_orchid|_flower)$")

(def slows-or-triggers
  "Walk-through blocks a body avoids: cobweb slows it to a crawl, tripwire sets off traps."
  #{"cobweb" "tripwire"})

(def ^{:doc "Whether a block name is one a body walks through (no collision, not a fluid, not cobweb or tripwire): open cells, replaceable plants,
  flowers, rails, signs, carpets, torches, saplings, berry bushes. nil (an unloaded cell) is not passable. Once a name:
  a dig route's search asks it of every cell it weighs."
       :arglists '([name])}
  passable?
  (let [known (js/Map.)]
    (fn [name]
      (let [v (.get known name)]
        (if (some? v)
          v
          (let [v (boolean
                   (and name (not (fluids name)) (not (slows-or-triggers name))
                        (or (replaceable name) (flowers name) (= "grass" name) (= "sweet_berry_bush" name)
                            (re-find passable-pattern name) (re-find walk-through name))))]
            (.set known name v)
            v))))))

(defn passable-or-water?
  "passable?, or water a walker swims through."
  [name]
  (or (= "water" name) (passable? name)))
