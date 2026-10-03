(ns dashboard.blockcolour
  "Block colours for the blueprint page. Ported from tools/dashboard/blueprint.mjs and the world table in map.mjs.
  Every role of one wood or stone is a shade of that material's hue, read off the family table the parser resolves
  {wood:planks} through (src/build/materials.mjs)."
  (:require [clojure.string :as str]))

(def woods ["oak" "spruce" "birch" "jungle" "acacia" "dark_oak" "mangrove" "cherry" "pale_oak" "bamboo" "crimson" "warped"])
(def wood-roles ["planks" "log" "stripped_log" "slab" "stairs" "fence" "fence_gate" "door" "trapdoor" "button" "pressure_plate" "sign"])
(def stones ["cobblestone" "mossy_cobblestone" "stone" "stone_bricks" "mossy_stone_bricks" "bricks" "deepslate_bricks"
             "cobbled_deepslate" "polished_deepslate" "andesite" "polished_andesite" "granite" "polished_granite" "diorite" "polished_diorite"
             "sandstone" "red_sandstone" "blackstone" "polished_blackstone" "polished_blackstone_bricks" "mud_bricks" "tuff" "tuff_bricks"
             "nether_bricks" "end_stone_bricks" "quartz_block" "smooth_stone" "prismarine"])
(def stone-roles ["block" "slab" "stairs" "wall"])
(def dyes ["white" "light_gray" "gray" "black" "brown" "red" "orange" "yellow" "lime" "green" "cyan" "light_blue" "blue" "purple" "magenta" "pink"])
(def dye-roles ["bed" "wool" "carpet" "concrete" "concrete_powder" "terracotta" "glazed_terracotta" "stained_glass" "stained_glass_pane" "banner" "candle" "shulker_box"])

(def log-of {"crimson" "crimson_stem" "warped" "warped_stem" "bamboo" "bamboo_block"})
(def stripped-of {"crimson" "stripped_crimson_stem" "warped" "stripped_warped_stem" "bamboo" "stripped_bamboo_block"})

(defn stone-stem [value]
  (-> value (str/replace #"_block$" "") (str/replace #"bricks$" "brick")))

(defn family-name [family value role]
  (case family
    :wood (case role
            "log" (or (log-of value) (str value "_log"))
            "stripped_log" (or (stripped-of value) (str "stripped_" value "_log"))
            (str value "_" role))
    :stone (if (= role "block") value (str (stone-stem value) "_" role))
    nil))

(def extra-wood {"wood" #(str % "_wood") "stripped_wood" #(str "stripped_" % "_wood")})

(def family-table
  (let [entries (concat
                 (for [v woods r wood-roles] [(family-name :wood v r) {:family :wood :value v :role r}])
                 (for [v woods [r f] extra-wood] [(f v) {:family :wood :value v :role r}])
                 (for [v stones r stone-roles] [(family-name :stone v r) {:family :stone :value v :role r}])
                 (for [v dyes r dye-roles] [(str v "_" r) {:family :dye :value v :role r}]))]
    (into {} entries)))

(defn family-of [name] (get family-table (str name)))

(def wood-tint
  {"oak" "#b08a55" "spruce" "#755433" "birch" "#d7c99a" "jungle" "#a67a4f" "acacia" "#b5652e" "dark_oak" "#4d3620"
   "mangrove" "#7a3a34" "cherry" "#e2a5b3" "pale_oak" "#d9d0c0" "bamboo" "#d1c26a" "crimson" "#7d3c5a" "warped" "#2f8c85"})

(def wood-shade
  {"planks" 1 "log" 0.72 "wood" 0.72 "stripped_log" 1.15 "stripped_wood" 1.15 "slab" 0.93 "stairs" 0.93 "fence" 0.84
   "fence_gate" 0.88 "door" 0.8 "trapdoor" 0.9 "button" 1 "pressure_plate" 1 "sign" 1})

(def stone-tint
  {"cobblestone" "#8a8a8a" "mossy_cobblestone" "#7a8a6a" "stone" "#7a7f88" "stone_bricks" "#6f747c" "mossy_stone_bricks" "#6a7a68" "bricks" "#9a5a4a"
   "deepslate_bricks" "#3e4247" "cobbled_deepslate" "#474b50" "polished_deepslate" "#3a3d42" "andesite" "#8c8e8a" "polished_andesite" "#9a9c98"
   "granite" "#9a6a5a" "polished_granite" "#a87868" "diorite" "#c9c9c6" "polished_diorite" "#d6d6d3" "sandstone" "#d6c99a" "red_sandstone" "#b8653a"
   "blackstone" "#2b2b2e" "polished_blackstone" "#333338" "polished_blackstone_bricks" "#3a3a40" "mud_bricks" "#8a6a4e" "tuff" "#6c6e66" "tuff_bricks" "#767870"
   "nether_bricks" "#4a2a30" "end_stone_bricks" "#d8d9a6" "quartz_block" "#e8e4dc" "smooth_stone" "#9a9ea6" "prismarine" "#5f9a90"})

(def stone-shade {"block" 1 "slab" 0.93 "stairs" 0.93 "wall" 0.86})

(def dye-tint
  {"white" "#e9ecec" "light_gray" "#8e8e86" "gray" "#3e4447" "black" "#1d1d21" "brown" "#835432" "red" "#b02e26" "orange" "#f9801d" "yellow" "#fed83d"
   "lime" "#80c71f" "green" "#5e7c16" "cyan" "#169c9c" "light_blue" "#3ab3da" "blue" "#3c44aa" "purple" "#8932b8" "magenta" "#c74ebd" "pink" "#f38baa"})

;; blocks with a look of their own: lights warm, glass pale, the workstations and containers the browns they are
(def fixed
  {"torch" "#e0a030" "wall_torch" "#e0a030" "soul_torch" "#5fc7c7" "lantern" "#e0a030" "soul_lantern" "#5fc7c7" "jack_o_lantern" "#e08a2e" "campfire" "#e0641e"
   "glowstone" "#f2d16b" "sea_lantern" "#bfe6e0" "glass" "#a9d6e8" "glass_pane" "#a9d6e8" "ladder" "#a67a4f" "chest" "#a0754a" "trapped_chest" "#a0754a"
   "barrel" "#8a6a44" "crafting_table" "#8f6b3f" "furnace" "#6d7076" "smoker" "#5c4a3a" "blast_furnace" "#4f5560" "composter" "#7a5c3a" "bookshelf" "#a0754a"
   "iron_bars" "#8c949c" "iron_block" "#d8d8d8" "iron_door" "#c8c8c8" "iron_trapdoor" "#c8c8c8" "hay_block" "#c9a542" "bell" "#e0b842" "anvil" "#3e4247"
   "@solid" "#4b5462"})

(def world-colours
  {"air" "#14171c" "unloaded" "#2a2f38" "water" "#4a90d9" "lava" "#e0641e"
   "grass_block" "#5c8f4a" "dirt" "#6b5236" "coarse_dirt" "#5e4a34" "farmland" "#4e3a26" "mud" "#4a3f3a" "podzol" "#5a4a2a"
   "sand" "#d8cf9a" "red_sand" "#c2743a" "gravel" "#8c8a86" "clay" "#9aa1ab" "stone" "#7a7f88" "cobblestone" "#8a8a8a"
   "sandstone" "#d6c99a" "snow" "#e8ecf0" "snow_block" "#e8ecf0" "ice" "#9fd3ff"
   "oak_slab" "#a9824e" "oak_planks" "#b08a55" "oak_log" "#6b4f2a" "oak_leaves" "#3f7a2f" "oak_sapling" "#5c8f4a"
   "oak_fence" "#8b7355" "oak_fence_gate" "#a8895f" "spruce_fence" "#6b4f33" "torch" "#e0a030" "wall_torch" "#e0a030"
   "chest" "#a0754a" "composter" "#7a5c3a" "crafting_table" "#a0754a"
   "wheat" "#d9b25f" "carrots" "#e08b3d" "potatoes" "#c9a15f" "beetroots" "#b3435f" "sugar_cane" "#c9c96a"
   "melon_stem" "#7fae3a" "pumpkin_stem" "#d67f2e" "bamboo" "#5c8f4a" "short_grass" "#6fa04f" "tall_grass" "#6fa04f" "fern" "#5f9a48"
   "dandelion" "#e0e060" "poppy" "#d64545"})

(defn hash-name [name]
  (reduce (fn [h ch] (unsigned-bit-shift-right (+ (* h 31) (.charCodeAt ch 0)) 0)) 7 (js/Array.from (str name))))

;; a block the table does not know still gets a colour of its own, the same every time, muted so it never outshines a known one
(defn world-colour [name]
  (or (world-colours name) (str "hsl(" (mod (hash-name name) 360) " 30% 45%)")))

(defn hex2 [n]
  (let [s (.toString (max 0 (min 255 (js/Math.round n))) 16)]
    (if (= 1 (count s)) (str "0" s) s)))

(defn hex? [s] (boolean (re-matches #"#[0-9a-fA-F]{6}" (str s))))

(defn channels [hex] (for [i [1 3 5]] (js/parseInt (subs hex i (+ i 2)) 16)))

(defn shade [hex k]
  (if-not (hex? hex)
    hex
    (apply str "#" (map #(hex2 (* % k)) (channels hex)))))

(defn mix [a b t]
  (apply str "#" (map (fn [x y] (hex2 (+ (* x (- 1 t)) (* y t)))) (channels a) (channels b))))

(defn air? [name] (boolean (re-matches #"air|cave_air|void_air" (str name))))

;; nil for air (drawn as nothing)
(defn block-colour [name]
  (let [{:keys [family value role]} (family-of name)]
    (cond
      (air? name) nil
      (fixed name) (fixed name)
      (= family :wood) (shade (wood-tint value) (get wood-shade role 1))
      (= family :stone) (shade (stone-tint value) (get stone-shade role 1))
      (= family :dye) (dye-tint value)
      :else (world-colour name))))

(def water "#4a90d9")

;; a waterlogged block is mostly water to the eye: a covered channel of slabs reads as the water it carries
(defn alt-colour [{:keys [name states]}]
  (let [base (block-colour name)]
    (if (or (nil? base) (not= "true" (:waterlogged states)) (not (hex? base)))
      base
      (mix base water 0.7))))
