(ns jobs.lib.armour
  "Wearing armour: which carried item goes into which slot, as pure data.
  The slots are mineflayer's equip destinations (head, torso, legs, feet)."
  (:require [clojure.string :as str]
            [jobs.lib.cost.armour :as cost-armour]))

(def slots ["head" "torso" "legs" "feet"])

(def piece-slot
  {"helmet" "head" "chestplate" "torso" "leggings" "legs" "boots" "feet"})

(defn piece-rank
  "Sort key of an armour piece, higher is better: its armour points (jobs.lib.cost.armour), toughness breaking a tie."
  [item]
  (+ (cost-armour/piece-points item)
     (/ (get cost-armour/toughness-by-material (first (str/split item #"_")) 0) 10)))

(defn parse
  "{:slot :rank} of an armour item name (\"iron_helmet\", \"turtle_helmet\"), nil for anything else."
  [item]
  (when (and (string? item) (re-matches #"[a-z]+_(helmet|chestplate|leggings|boots)" item) (pos? (cost-armour/piece-points item)))
    {:slot (piece-slot (peek (str/split item #"_"))) :rank (piece-rank item)}))

(defn rank-of [item] (or (:rank (parse item)) 0))

(defn plan
  "What to put on. worn maps slot -> item name (nil or missing when empty); carried is a seq of item names.
  [{:item :slot}]: the best carried piece for each slot whose worn piece is empty or of a lower rank."
  [worn carried]
  (vec (keep (fn [slot]
               (let [best (->> (distinct carried)
                               (filter #(= slot (:slot (parse %))))
                               (sort-by rank-of >)
                               first)]
                 (when (and best (> (rank-of best) (rank-of (get worn slot))))
                   {:item best :slot slot})))
             slots)))

(defn worn-of
  "slot -> item name from a primitives self's :equipment (JS or cljs)."
  [equipment]
  (let [e (js->clj equipment)]
    (into {} (keep (fn [slot] (when-let [n (get-in e [slot "name"])] [slot n]))) slots)))
