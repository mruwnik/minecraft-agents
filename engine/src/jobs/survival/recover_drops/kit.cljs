(ns jobs.survival.recover-drops.kit
  "The weapon and armour a known chest holds that a body going to a guarded pile lacks."
  (:require [engine.ctx :as ctx]
            [engine.settings :as settings]
            [engine.game :as game]
            [jobs.lib.armour :as armour]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.stock :as stock]
            [jobs.lib.util :as u]
            [jobs.survival.recover-drops.estimate :as est]))

(defn stock-names
  "The item names the known chests hold (:fetch/stock)."
  [c]
  (into #{} (mapcat keys) (vals (stock/stock-of (ctx/view c)))))

(defn piece-points
  "Armour points of one piece (jobs.lib.cost/armour-stats), 0 for nil."
  [slot item]
  (if item (:points (cost/armour-stats {slot {:name item}})) 0))

(defn kit-problem
  "The need wait of the best weapon, else of the best piece for an empty or weaker armour slot, that a known chest holds
  and the body does not carry (ranked by jobs.lib.cost: damage per attack gap, armour points); nil when none."
  [c]
  (let [p (:primitives c)
        have (set (map :name (u/inventory p)))
        stock (remove have (stock-names c))
        worn (armour/worn-of (.-equipment (.self p)))
        weapons (->> stock (filter #(combat/weapon? combat/default-weapons %)) (sort-by #(/ (cost/weapon-damage %) (cost/attack-gap-ms %)) >))
        pieces (fn [slot] (->> stock
                               (filter #(and (= slot (:slot (armour/parse %))) (> (piece-points slot %) (piece-points slot (get worn slot)))))
                               (sort-by #(piece-points slot %) >)))
        need (fn [names] (when (seq names) {:reason :need :any-of (vec names) :count 1}))]
    (or (when-not (combat/best-weapon p combat/default-weapons) (need weapons))
        (some #(need (pieces %)) armour/slots))))

(defn kit-called-for?
  "Whether the pile is worth the trip were the route safe (its value against the fetch cost without danger and the margin)
  and the killer or the hostiles the body knows make the route dangerous (the estimate names mobs)."
  [c entry]
  (let [{:keys [pos cause]} (:data entry)
        elapsed (settings/ticks->ms (game/ticks-since entry (ctx/view c)))
        e (est/estimate c (:data entry) elapsed)
        safe (cost/fetch-cost {:distance (when pos (u/dist (u/self-pos c) pos)) :danger 0 :elapsed-ms elapsed :cause cause})
        safe-cost (if (js/isFinite (:cost safe)) (:cost safe) js/Infinity)]
    (boolean (and (seq (:top-mobs e)) (> (:value e) (+ safe-cost (:margin (:args c))))))))
