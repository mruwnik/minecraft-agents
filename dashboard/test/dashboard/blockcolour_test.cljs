(ns dashboard.blockcolour-test
  (:require [cljs.test :refer [deftest is]]
            [dashboard.blockcolour :as bc]))

(deftest family-of
  (doseq [[name expected] [["oak_planks" {:family :wood :value "oak" :role "planks"}]
                           ["oak_fence_gate" {:family :wood :value "oak" :role "fence_gate"}]
                           ["stripped_spruce_log" {:family :wood :value "spruce" :role "stripped_log"}]
                           ["crimson_stem" {:family :wood :value "crimson" :role "log"}]
                           ["dark_oak_door" {:family :wood :value "dark_oak" :role "door"}]
                           ["oak_wood" {:family :wood :value "oak" :role "wood"}]
                           ["stripped_oak_wood" {:family :wood :value "oak" :role "stripped_wood"}]
                           ["stone_brick_slab" {:family :stone :value "stone_bricks" :role "slab"}]
                           ["cobblestone" {:family :stone :value "cobblestone" :role "block"}]
                           ["polished_blackstone_brick_wall" {:family :stone :value "polished_blackstone_bricks" :role "wall"}]
                           ["white_bed" {:family :dye :value "white" :role "bed"}]
                           ["light_blue_wool" {:family :dye :value "light_blue" :role "wool"}]
                           ["torch" nil]
                           ["@solid" nil]]]
    (is (= expected (bc/family-of name)) name)))

(deftest block-colour-known
  (doseq [[name expected] [["oak_planks" "#b08a55"] ["cobblestone" "#8a8a8a"] ["white_bed" "#e9ecec"] ["torch" "#e0a030"]
                           ["@solid" "#4b5462"] ["air" nil] ["cave_air" nil] ["farmland" "#4e3a26"]
                           ["oak_log" "#7f633d"] ["stripped_oak_log" "#ca9f62"]]]
    (is (= expected (bc/block-colour name)) name)))

(defn lightness [hex]
  (reduce + (for [i [1 3 5]] (js/parseInt (subs hex i (+ i 2)) 16))))

(deftest wood-roles-are-shades
  (let [[log planks stripped] (map (comp lightness bc/block-colour) ["oak_log" "oak_planks" "stripped_oak_log"])]
    (is (< log planks stripped)))
  (is (not= (bc/block-colour "oak_fence") (bc/block-colour "spruce_fence"))))

(deftest unlisted-block-gets-stable-muted-colour
  (is (= (bc/block-colour "sponge") (bc/block-colour "sponge")))
  (is (re-find #"^hsl\(" (bc/block-colour "sponge")))
  (is (not= (bc/block-colour "sponge") (bc/block-colour "prismarine_shard"))))

(deftest world-colour-hash-is-the-old-one
  (is (= "hsl(297 30% 45%)" (bc/world-colour "sponge"))))

(deftest alt-colour
  (doseq [[alt expected] [[{:name "oak_slab" :states {:waterlogged "true"}} "#658bb0"]
                          [{:name "oak_slab"} "#a4804f"]
                          [{:name "oak_slab" :states {:type "top"}} "#a4804f"]
                          [{:name "air" :states {:waterlogged "true"}} nil]]]
    (is (= expected (bc/alt-colour alt)) (pr-str alt))))

(deftest shade
  (doseq [[hex k expected] [["#808080" 1 "#808080"] ["#808080" 0.5 "#404040"] ["#ffffff" 1.1 "#ffffff"] ["hsl(1 30% 45%)" 0.5 "hsl(1 30% 45%)"]]]
    (is (= expected (bc/shade hex k)))))

(deftest terrain-colour-is-always-a-hex
  (doseq [name ["grass_block" "oak_leaves" "spruce_leaves" "birch_leaves" "sand" "water" "snow" "deepslate" "netherrack" "terracotta"
                "oak_planks" "red_terracotta" "something_new_in_26_1"]]
    (is (bc/hex? (bc/terrain-colour name)) name)))

(deftest terrain-colour-known-blocks-are-not-hash-colours
  (doseq [name ["grass_block" "oak_leaves" "spruce_leaves" "birch_leaves" "jungle_leaves" "acacia_leaves" "dark_oak_leaves" "sand" "red_sand"
                "water" "snow" "snow_block" "ice" "packed_ice" "deepslate" "netherrack" "terracotta" "mycelium" "podzol" "gravel" "stone"
                "andesite" "diorite" "granite" "dirt_path" "moss_block" "lava" "bedrock" "obsidian" "coal_ore" "iron_ore" "kelp" "seagrass"]]
    (is (bc/known? name) name)))
