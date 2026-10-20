(ns jobs.lib.pen-blocks
  "What a block is to a penned animal, for jobs.lib.pen: whether it walks through, the hazards it refuses, and the
  collision column {:lo :hi} inside the cell."
  (:require [jobs.lib.block-sets :as bs]))

(def passes
  "Names an animal walks through: no collision."
  (into (into bs/air bs/flowers)
        #{"water" "bubble_column" "short_grass" "tall_grass" "fern" "large_fern" "dead_bush" "leaf_litter"
          "seagrass" "tall_seagrass" "kelp" "kelp_plant" "vine" "glow_lichen" "snow" "cobweb" "structure_void"
          "lily_pad" "sugar_cane" "redstone_wire" "rail" "powered_rail" "detector_rail" "activator_rail" "lever" "tripwire"
          "tripwire_hook" "string" "ladder" "torch" "wall_torch" "redstone_torch" "redstone_wall_torch"
          "wheat" "carrots" "potatoes" "beetroots" "melon_stem" "pumpkin_stem" "nether_wart" "hanging_roots" "bamboo_sapling"}))

(def pass-patterns
  [#"_sapling$" #"_button$" #"_pressure_plate$" #"_sign$" #"_banner$" #"_torch$" #"^attached_.*_stem$" #"_propagule$"])

(def hazards
  "Blocks animals refuse to enter (vanilla gives them a negative path cost)."
  (conj bs/harms-in-cell "powder_snow"))

(def heights
  "Top of the collision box for blocks lower than a full block."
  {"farmland" 0.9375 "dirt_path" 0.9375 "soul_sand" 0.875 "mud" 0.875 "chest" 0.875 "trapped_chest" 0.875
   "ender_chest" 0.875 "enchanting_table" 0.75 "stonecutter" 0.5625 "cake" 0.5 "flower_pot" 0.375
   "daylight_sensor" 0.375})

(defn prop [b k] (some-> b .-properties (aget k)))

(defn on? [b k] (let [v (prop b k)] (or (true? v) (= "true" v))))

(defn ends? [n suffix] (.endsWith n suffix))

(defn passes? [n]
  (or (contains? passes n) (some #(re-find % n) pass-patterns)))

(defn gate? [b] (some-> b .-name (ends? "_fence_gate")))

(defn collision
  "{:lo :hi} (heights inside the cell, plus :hazard?) of block b, or nil when an animal passes through."
  [b]
  (let [n (.-name b)]
    (cond
      (contains? hazards n) {:lo 0 :hi 1 :hazard? true}
      (passes? n) nil
      (ends? n "_carpet") {:lo 0 :hi 0.0625}
      (ends? n "_fence_gate") (when-not (on? b "open") {:lo 0 :hi 1.5})
      (re-find #"_(fence|wall)$" n) {:lo 0 :hi 1.5}
      (ends? n "_slab") (case (prop b "type") "bottom" {:lo 0 :hi 0.5} "top" {:lo 0.5 :hi 1} {:lo 0 :hi 1})
      (ends? n "_trapdoor") (when-not (on? b "open") (if (= "top" (prop b "half")) {:lo 0.8125 :hi 1} {:lo 0 :hi 0.1875}))
      (ends? n "_door") (when-not (on? b "open") {:lo 0 :hi 1})
      (ends? n "_bed") {:lo 0 :hi 0.5625}
      (contains? heights n) {:lo 0 :hi (heights n)}
      :else {:lo 0 :hi 1})))
