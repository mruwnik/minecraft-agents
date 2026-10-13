(ns jobs.lib.tool-kind
  "Which kind of tool digs a block."
  (:require [clojure.string :as str]))

(def shovel-blocks
  #{"dirt" "grass_block" "sand" "red_sand" "gravel" "clay" "soul_sand" "soul_soil" "mud" "snow_block"
    "coarse_dirt" "rooted_dirt" "podzol" "mycelium" "farmland" "dirt_path"})

(defn tool-kind
  "\"shovel\", \"axe\", \"hoe\", \"shears\" or \"pickaxe\": the tool that digs block-name fastest."
  [block-name]
  (cond
    (shovel-blocks block-name) "shovel"
    (= block-name "cobweb") "shears"
    (and (str/includes? block-name "fence") (not (str/starts-with? block-name "nether_brick"))) "axe"
    (str/ends-with? block-name "_leaves") "hoe"
    (some #(str/ends-with? block-name %) ["_log" "_wood" "_planks"]) "axe"
    :else "pickaxe"))

(defn of-kind?
  "Whether item-name is a tool of kind: \"<material>_<kind>\", or the kind itself (shears)."
  [item-name kind]
  (or (= item-name kind) (str/ends-with? item-name (str "_" kind))))

(def fallback-kind "What digs a block when its own kind is not carried." {"shears" "sword"})
