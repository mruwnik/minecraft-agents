(ns jobs.lib.solid
  "What counts as solid for roofs, walls, floors and arrows, by block name, and the block cell of a position.")

(def non-solid
  "Block names that are not a roof: air, fluids, and plants and fixtures a body can stand in."
  #{"air" "cave_air" "void_air" "water" "lava" "short_grass" "grass" "tall_grass" "fern" "large_fern"
    "torch" "wall_torch" "snow" "vine" "dead_bush" "seagrass" "tall_seagrass" "fire"})

(def walk-through
  "Blocks a mob or body walks through or over (signs, banners, rails, plates, buttons, levers, carpets, tripwire,
  cobweb). Not solid: neither roof, wall nor floor."
  #"(_sign|_banner|_carpet|_pressure_plate|_button|_rail)$|^(rail|lever|tripwire|tripwire_hook|string|cobweb|redstone_wire)$")

(defn solid?
  "Whether a block name counts as solid: not nil (unloaded), not non-solid or walk-through, and not leaves or a
  sapling."
  [name]
  (and (some? name)
       (not (non-solid name))
       (not (re-find walk-through name))
       (not (.endsWith name "_leaves"))
       (not (.endsWith name "_sapling"))))

(defn cell
  "The block cell {:x :y :z} containing a position."
  [{:keys [x y z]}]
  {:x (js/Math.floor x) :y (js/Math.floor y) :z (js/Math.floor z)})
