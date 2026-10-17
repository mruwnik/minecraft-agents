(ns jobs.lib.found
  "Found goods: things the body picked up on the way (vanilla auto-pickup of a destroyed chest's drops) that its own
  digging, harvesting or chest work did not produce. Read from the :picked-up memory entries; deposit keeps them out of the body's chests."
  (:require [engine.memory :as mem]
            [engine.settings :as settings]
            [engine.args :as a]))

(a/defargs settings
  {::bulk {:default 256 :spec (a/int-in 1 nil)
           :doc "Picked up this many of one item (within the :picked-up memory) with no work of its own near: found goods."}})

(def containers #{"chest" "trapped_chest" "barrel" "hopper" "furnace" "blast_furnace" "smoker" "dropper" "dispenser" "composter" "shulker_box"})

(def known-chest-radius 3)

(defn at-known-chest?
  "The pick-up happened within known-chest-radius of a chest the body has booked stock for (:fetch/stock)."
  [stock-cells {:keys [pos]}]
  (some (fn [[x y z]] (<= (Math/hypot (- x (:x pos)) (- y (:y pos)) (- z (:z pos))) known-chest-radius)) stock-cells))

(defn goods
  "{item {:count n :pos p}}: containers picked up at any count, other items picked up in bulk. Pick-ups of the body's
  own work (:own, set by the primitives after its dig, use or chest transfer) and ones at a chest it keeps stock of
  are not found. pos is where the newest pick-up of the item happened."
  [view]
  (let [bulk (settings/get settings ::bulk)
        cells (map (comp :pos :data) (mem/entries view :fetch/stock))
        per (reduce (fn [m {:keys [data]}]
                      (-> m
                          (update-in [(:item data) :count] (fnil + 0) (:count data 0))
                          (assoc-in [(:item data) :pos] (:pos data))))
                    {}
                    (remove #(or (:own (:data %)) (at-known-chest? cells (:data %))) (mem/entries view :picked-up)))]
    (into {} (filter (fn [[item {n :count}]]
                       (or (contains? containers item)
                           (>= n bulk))))
          per)))
