(ns jobs.survival.recover-drops.kit
  "The weapon and armour a known chest holds that a body going to a guarded pile lacks."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [engine.game :as game]
            [jobs.lib.armour :as armour]
            [jobs.lib.child :as child]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.stock :as stock]
            [jobs.lib.util :as u]
            [jobs.survival.recover-drops.config :as cfg]
                        [jobs.survival.recover-drops.deaths :as dth]
            [jobs.survival.recover-drops.estimate :as est]))

(defn on-the-way?
  "Whether a chest at cell costs at most ::kit-detour blocks more than the walk from the body to the pile: the pile
  despawns, so a kit is not worth a trip away from it."
  [c pile cell]
  (let [here (u/self-pos c)
        at (zipmap [:x :y :z] cell)]
    (<= (- (+ (u/dist here at) (u/dist at pile)) (u/dist here pile)) (cfg/kit-detour))))

(defn stock-names
  "The item names the known chests on the way to the pile hold (:fetch/stock)."
  [c pile]
  (into #{} (comp (filter #(on-the-way? c pile (key %))) (mapcat (comp keys val))) (stock/stock-of (ctx/view c))))

(defn piece-points
  "Armour points of one piece (jobs.lib.cost/armour-stats), 0 for nil."
  [slot item]
  (if item (:points (cost/armour-stats {slot {:name item}})) 0))

(defn kit-problem
  "From the chests on the way to the pile, the need wait of the best weapon, else of the best piece for an empty or weaker armour slot, that a known chest holds
  and the body does not carry (ranked by jobs.lib.cost: damage per attack gap, armour points); nil when none."
  [c pile]
  (let [p (:primitives c)
        have (set (map :name (u/inventory p)))
        stock (remove have (stock-names c pile))
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
    (boolean (and (seq (:top-mobs e)) (> (:value e) (+ safe-cost (cfg/margin)))))))

(defn ^:async wear-kit!
  "Put on what the kit fetch brought (jobs.items.equip), once its child has ended."
  [c]
  (await (child/run!* c :wear 'jobs.items.equip {})))

(def kit-fetch-minutes "How long the kit fetch may take." 3)

(defn kit-minutes
  "The kit fetch's limit: kit-fetch-minutes, at most the minutes the pile's despawn window has left."
  [c entry]
  (let [left (/ (settings/ticks->ms (game/despawn-left-ticks entry (ctx/view c))) 60000)]
    (max 0.1 (min left kit-fetch-minutes))))

(defn ^:async kit!
  "Once per run: fetch and wear a weapon and armour when the pile is worth the trip, the danger calls for them and a known
  chest has them (jobs.lib.need, one piece at a time). A fetch child waiting on the world is waited for (declared hold
  :fetching-kit) until it ends, a danger or the despawn window. nil when done, dropped (a fetch that failed: warn with its ending) or
  nothing to do, else :continue for a cut."
  [c entry]
  (let [limits (need/fetch-opts c)]
    (if (or (:kit-done (ctx/mem c)) (not limits) (nil? (kit-problem c (:pos (:data entry)))) (not (kit-called-for? c entry)))
      (do (ctx/update-mem! c assoc :kit-done true) nil)
      (loop []
        (let [wanted (kit-problem c (:pos (:data entry)))
              r (if wanted (await (need/blocks! c (:any-of wanted) 1 (assoc limits :minutes (kit-minutes c entry)))) :done)]
          (cond
            (nil? wanted)
            (do (await (wear-kit! c))
                (ctx/update-mem! c assoc :kit-done true)
                nil)

            (and (not= :continue r) (not= :done (:kind r)))
            (do (ctx/update-mem! c assoc :kit-done true)
                (ctx/emit! c :recover-drops.kit-dropped :warn {:text (str "recover-drops: the kit fetch ended " (name (:kind r)) " " (pr-str (contract/cause-key r)) "; going without a kit")
                                                               :because (contract/for-event r)})
                nil)

            (not= :continue r) (recur)
            (not (ctx/alive? c)) :continue
            (dth/window-closed? c entry) (do (ctx/hold-still! c nil) nil)
            :else (do (await (child/pause! c (cfg/pause-s) :fetching-kit)) (await (pace/pace! c)) (recur))))))))
