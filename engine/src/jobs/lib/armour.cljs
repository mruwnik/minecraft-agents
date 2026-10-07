(ns jobs.lib.armour
  "Wearing armour: which carried item goes into which slot, as pure data, and the async runner that equips it.
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
  "What to put on. worn maps slot -> item name (nil or missing when empty); carried is a seq of item names. With item:
  {:ok true :put [{:item :slot}]} for that piece, or {:ok false :reason :not-armour|:no-item}. Without: the best
  carried piece for each slot whose worn piece is empty or of a lower rank."
  [worn carried item]
  (if item
    (let [{:keys [slot]} (parse item)]
      (cond
        (not slot) {:ok false :reason :not-armour :item item}
        (not (some #{item} carried)) {:ok false :reason :no-item :item item}
        :else {:ok true :put [{:item item :slot slot}]}))
    {:ok true
     :put (vec (keep (fn [slot]
                       (let [best (->> (distinct carried)
                                       (filter #(= slot (:slot (parse %))))
                                       (sort-by rank-of >)
                                       first)]
                         (when (and best (> (rank-of best) (rank-of (get worn slot))))
                           {:item best :slot slot})))
                     slots))}))

(defn ^:async wear!
  "Equip the plan through equip! (item slot -> promise of a primitive result). Resolves to
  {:ok :worn [{:item :slot}] :reason} : stops at the first piece that is not equipped (:reason :failed, :status)."
  [equip! worn carried item]
  (let [{:keys [ok put] :as p} (plan worn carried item)]
    (if-not ok
      (assoc p :worn [])
      (loop [todo put done []]
        (if-let [{:keys [item slot] :as step} (first todo)]
          (let [r (js->clj (await (equip! item slot)) :keywordize-keys true)]
            (if (= "equipped" (:status r))
              (recur (rest todo) (conj done step))
              {:ok false :reason :failed :status (:status r) :item item :worn done}))
          {:ok true :worn done})))))

(defn worn-of
  "slot -> item name from a primitives self's :equipment (JS or cljs)."
  [equipment]
  (let [e (js->clj equipment)]
    (into {} (keep (fn [slot] (when-let [n (get-in e [slot "name"])] [slot n]))) slots)))
