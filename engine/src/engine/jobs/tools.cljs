(ns engine.jobs.tools
  "Which carried tool suits a block."
  (:require [clojure.string :as str]
            [engine.jobs.combat :as combat]))

(def shovel-blocks
  #{"dirt" "grass_block" "sand" "red_sand" "gravel" "clay" "soul_sand" "soul_soil" "mud" "snow_block"
    "coarse_dirt" "rooted_dirt" "podzol" "mycelium" "farmland" "dirt_path"})

(defn tool-kind
  "\"shovel\", \"axe\" or \"pickaxe\": the tool that digs block-name fastest."
  [block-name]
  (cond
    (shovel-blocks block-name) "shovel"
    (some #(str/ends-with? block-name %) ["_log" "_wood" "_planks"]) "axe"
    :else "pickaxe"))

(defn best-tool
  "The carried tool (of item-names) of the kind that suits block-name with the
  highest material, or nil."
  [item-names block-name]
  (let [suffix (str "_" (tool-kind block-name))
        rank #(get combat/material-rank (first (str/split % #"_")) 0)]
    (->> item-names
         (filter #(str/ends-with? % suffix))
         (sort-by rank >)
         first)))
