(ns engine.triggers.suffocating
  "The suffocating trigger, and the sensing it shares with jobs.survival.breathe.

  Two situations, both read from sensing only:
  - :drowning: in water, oxygen below :min-oxygen, and the head cell is not
    air (a swimmer at the surface breathes and is not drowning).
  - :enclosed: the head cell, one block above the feet, holds a block that
    suffocates. Every block does except the ones in passable: air, water,
    lava, plants, torches, signs, rails, carpets, buttons, ladders, vines,
    cobweb, snow layers and the like. An unloaded cell (blockAt null) is
    read as not enclosed, never as a reason to dig.")

(def default-min-oxygen 12)

(def passable-names
  #{"air" "cave_air" "void_air" "torch" "rail" "water" "lava" "bubble_column" "fire" "soul_fire"
    "grass" "short_grass" "tall_grass" "fern" "large_fern" "seagrass" "tall_seagrass" "kelp" "kelp_plant"
    "dead_bush" "vine" "glow_lichen" "cobweb" "ladder" "snow" "lever" "redstone_wire" "tripwire"
    "tripwire_hook" "light" "structure_void" "sugar_cane" "sweet_berry_bush"})

(def passable-suffixes
  ["_torch" "_sign" "_hanging_sign" "_button" "_pressure_plate" "_carpet" "_rail" "_banner"
   "_sapling" "_flower" "_tulip" "_mushroom" "_fungus" "_roots" "_vines"])

(def passable-flowers
  #{"dandelion" "poppy" "blue_orchid" "allium" "azure_bluet" "oxeye_daisy" "cornflower"
    "lily_of_the_valley" "sunflower" "lilac" "rose_bush" "peony"})

(defn passable?
  "Whether a block of this name lets a head through without suffocating."
  [block-name]
  (or (contains? passable-names block-name)
      (contains? passable-flowers block-name)
      (boolean (some #(.endsWith block-name %) passable-suffixes))))

(defn air? [block-name] (contains? #{"air" "cave_air" "void_air"} block-name))

(defn block-name
  "The block name at cell, or nil when the chunk is not loaded."
  [p cell]
  (some-> (.blockAt p (clj->js cell)) .-name))

(defn head-cell [self]
  {:x (js/Math.floor (.. self -pos -x))
   :y (inc (js/Math.floor (.. self -pos -y)))
   :z (js/Math.floor (.. self -pos -z))})

(defn situation
  "Why the body at p is suffocating, :drowning or :enclosed, or nil."
  [p min-oxygen]
  (let [self (.self p)
        head (block-name p (head-cell self))]
    (cond
      (and (.-inWater self) (< (.-oxygen self) min-oxygen) (not (air? (or head "water")))) :drowning
      (and (some? head) (not (passable? head))) :enclosed
      :else nil)))

(def suffocating
  "Holds when situation says the body is drowning or enclosed. :min-oxygen
  (default 12 of 20) is the drowning threshold. The job's own :min-oxygen is
  set in the entry's :job spec. Persistence is :retry: this reflex must get
  the body back at once, and a spent job fires again while the condition lasts."
  {:name :suffocating
   :when (fn [world _memory args]
           (some? (situation world (:min-oxygen args default-min-oxygen))))
   :job '(jobs.survival.breathe)
   :args {:min-oxygen default-min-oxygen}
   :persistence :retry
   :cooldown-s 0})
