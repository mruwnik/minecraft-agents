(ns engine.armour
  "Wearing armour: which carried item goes into which slot, as pure data, and the async runner that equips it.
  The slots are mineflayer's equip destinations (head, torso, legs, feet).")

(def slots ["head" "torso" "legs" "feet"])

(def piece-slot
  {"helmet" "head" "chestplate" "torso" "leggings" "legs" "boots" "feet"})

(def material-rank
  "Weakest first. The turtle shell helmet sits between golden and chainmail (its defence is a helmet's 2 points)."
  {"leather" 1 "golden" 2 "turtle" 2.5 "chainmail" 3 "iron" 4 "diamond" 5 "netherite" 6})

(defn parse
  "{:slot :rank} of an armour item name (\"iron_helmet\", \"turtle_helmet\"), nil for anything else."
  [item]
  (when (string? item)
    (let [[_ material piece] (re-matches #"([a-z]+)_(helmet|chestplate|leggings|boots)" item)
          rank (get material-rank material)]
      (when (and rank (or (not= material "turtle") (= piece "helmet")))
        {:slot (piece-slot piece) :rank rank}))))

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
