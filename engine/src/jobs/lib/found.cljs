(ns jobs.lib.found
  "Found goods: things the body picked up on the way (vanilla auto-pickup of a destroyed chest's drops) that no job
  of its own would gather. Read from the :picked-up memory entries; deposit keeps them out of the body's chests."
  (:require [engine.memory :as mem]
            [engine.settings :as settings]
            [engine.args :as a]))

(a/defargs settings
  {::bulk {:default 256 :spec (a/int-in 1 nil)
           :doc "Picked up this many of one item (within the :picked-up memory) and not a material the body gathers: found goods."}})

(def containers #{"chest" "trapped_chest" "barrel" "hopper" "furnace" "blast_furnace" "smoker" "dropper" "dispenser" "composter" "shulker_box"})

(def gathered
  "Materials and crops the body's own jobs collect in bulk."
  #"^(?:[a-z_]+_(?:log|planks|ore|leaves|sapling|seeds)|raw_[a-z_]+|cobblestone|cobbled_deepslate|stone|deepslate|dirt|grass_block|sand|gravel|andesite|diorite|granite|tuff|netherrack|coal|stick|wheat|carrot|potato|beetroot|melon_slice|pumpkin|cactus|bamboo|kelp|rotten_flesh|bone|arrow|string|feather|leather|[a-z_]+_wool)$")

(defn goods
  "{item {:count n :pos p}}: containers picked up at any count, other items picked up in bulk and not gathered by the
  body's jobs. pos is where the newest pick-up of the item happened."
  [view]
  (let [bulk (settings/get settings ::bulk)
        per (reduce (fn [m {:keys [data]}]
                      (-> m
                          (update-in [(:item data) :count] (fnil + 0) (:count data 0))
                          (assoc-in [(:item data) :pos] (:pos data))))
                    {}
                    (mem/entries view :picked-up))]
    (into {} (filter (fn [[item {n :count}]]
                       (or (contains? containers item)
                           (and (>= n bulk) (not (re-matches gathered item))))))
          per)))
