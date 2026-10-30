(ns jobs.lib.sources
  "The raw sources of items for a recipe plan: what is mined, caught, hunted, cut or dug by hand rather than crafted, and the recipes/plan options that say so."
  (:require [jobs.lib.game-facts :as facts]
            [jobs.lib.blocks :as b]))

(def ore-drops
  "The items mined from ores (raw_iron, coal, diamond ..): gathered, never crafted."
  (set (vals b/ores)))

(def fish-items "Raw fish: caught with a rod (jobs.food.fish), never crafted." #{"cod" "salmon"})

(def mob-drops
  "The items a mob drops, and the mob: gathered by hunting it (a kept pair and penned animals are spared; a hostile kind is hunted armed)."
  {"leather" "cow" "feather" "chicken" "string" "spider" "bone" "skeleton"
   "beef" "cow" "porkchop" "pig" "mutton" "sheep" "chicken" "chicken" "rabbit" "rabbit"})

(def crop-blocks
  "The field crops: item they give, the crop block cut for it (jobs.farm.harvest cuts the ripe ones)."
  {"wheat" "wheat" "carrot" "carrots" "potato" "potatoes" "beetroot" "beetroots"})

(def dirt-blocks
  "The seen blocks digging which by hand gives dirt (grass drops it)."
  #{"dirt" "grass_block"})

(def hand-dug
  "The items dug from the ground by hand (a shovel is faster): the seen blocks that drop one. Flint is the rare drop of gravel."
  {"dirt" dirt-blocks "sand" #{"sand"} "gravel" #{"gravel"} "clay_ball" #{"clay"} "flint" #{"gravel"}})

(defn source-opts
  "The recipes/plan options of the gather and smelt sources: ore drops and hand-dug items (sand for glass) count as gathered, and every furnace product
  (jobs.lib.game-facts/smelt-inputs) is also made by a smelt step."
  []
  {:raw (conj (into ore-drops (concat (keys hand-dug) fish-items (keys mob-drops) (keys crop-blocks))) "obsidian" "bone_meal")
   :smelts facts/smelt-inputs})
