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
            [jobs.survival.recover-drops.estimate :as est]
            [jobs.survival.recover-drops.pile :as pl]))

(defn on-the-way?
  "Whether the walk through a chest at cell adds at most budget-s seconds (cost/walk-blocks-per-s) to the walk from the body to
  the pile: the pile's surplus (est/surplus-s) is what a detour may cost before the trip is not worth it."
  [c pile cell budget-s]
  (let [here (u/self-pos c)
        at (zipmap [:x :y :z] cell)]
    (<= (/ (- (+ (u/dist here at) (u/dist at pile)) (u/dist here pile)) (cost/walk-blocks-per-s)) budget-s)))

(defn stock-names
  "The item names the known chests on the way to the pile (a detour of at most budget-s seconds) hold (:fetch/stock)."
  [c pile budget-s]
  (into #{} (comp (filter #(on-the-way? c pile (key %) budget-s)) (mapcat (comp keys val))) (stock/stock-of (ctx/view c))))

(defn piece-points
  "Armour points of one piece (jobs.lib.cost/armour-stats), 0 for nil."
  [slot item]
  (if item (:points (cost/armour-stats {slot {:name item}})) 0))

(defn kit-problem
  "From the chests on the way to the pile, the need wait of the best weapon, else of the best piece for an empty or weaker armour slot, that a known chest holds
  and the body does not carry (ranked by jobs.lib.cost: damage per attack gap, armour points); nil when none. budget-s: see on-the-way?."
  [c pile budget-s]
  (let [p (:primitives c)
        have (set (map :name (u/inventory p)))
        stock (remove have (stock-names c pile budget-s))
        worn (armour/worn-of (.-equipment (.self p)))
        weapons (->> stock (filter #(combat/weapon? combat/default-weapons %)) (sort-by #(/ (cost/weapon-damage %) (cost/attack-gap-ms %)) >))
        pieces (fn [slot] (->> stock
                               (filter #(and (= slot (:slot (armour/parse %))) (> (piece-points slot %) (piece-points slot (get worn slot)))))
                               (sort-by #(piece-points slot %) >)))
        need (fn [names] (when (seq names) {:reason :need :any-of (vec names) :count 1}))]
    (or (when-not (combat/best-weapon p combat/default-weapons) (need weapons))
        (some #(need (pieces %)) armour/slots))))

(defn window-s
  "Seconds left before the drops of death entry despawn."
  [c entry]
  (/ (settings/ticks->ms (game/despawn-left-ticks entry (ctx/view c))) 1000))

(defn ^:async kit-surplus-s
  "Seconds the pile is worth over the fetch cost without danger and the margin, at most the window left over that cost (0: not worth it), when the walk to it is
  dangerous (go-to's quote of it names the mobs it risks, est/estimate); else 0. A kit is called for when it is positive."
  [c entry]
  (let [{:keys [pos]} (:data entry)
        elapsed (est/pile-age-ms c entry)
        e (est/estimate c (:data entry) elapsed (await (est/quote-trip c entry)))
        safe (cost/fetch-cost {:distance (when pos (u/dist (u/self-pos c) pos)) :elapsed-ms elapsed :lost (est/lost c (:data entry)) :back? true :stay-s (pl/collect-s c (:data entry))})
        safe-cost (if (js/isFinite (:cost safe)) (:cost safe) js/Infinity)]
    (if (seq (:seen-mobs e)) (max 0 (min (- (:value e) (+ safe-cost (cfg/margin))) (- (window-s c entry) safe-cost))) 0)))

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
  (let [limits (need/fetch-opts c)
        pile (:pos (:data entry))
        surplus (when (and limits (not (:kit-done (ctx/mem c))) (kit-problem c pile ##Inf)) (await (kit-surplus-s c entry)))]
    (if-not (and surplus (pos? surplus) (kit-problem c pile surplus))
      (do (ctx/update-mem! c assoc :kit-done true) nil)
      (loop []
        (let [wanted (kit-problem c pile surplus)
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
