(ns jobs.survival.night.hold
  "The night's holds: hold still on purpose, eating or cooking first, and the bookkeeping of what the night did."
  (:require [engine.ctx :as ctx]
            [engine.memory :as mem]
            [jobs.lib.waits :as waits]
            [jobs.lib.child :as child]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.foods :as foods]
            [jobs.lib.fuel :as fuel]
            [jobs.lib.furnace :as furnace]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.shelter.inside :as inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.survival.eat :as eat]
            [jobs.survival.night.sites :as sites]
            [jobs.survival.get-food.cook :as cook]))

(defn can-place-furnace?
  "A furnace carried with raw food, fuel and a tool that digs it up again: the hold may put it down in the shelter."
  [c]
  (let [p (:primitives c)
        names (map :name (u/inventory p))]
    (boolean (and (some #{"furnace"} names)
                  (some #(fuel/fuel-per-unit "furnace" %) names)
                  (nil? (tools/harvest-need names (js->clj (.harvestTools p "furnace"))))))))

(defn ^:async pick-up-furnace!
  "Dig the furnace the hold put down (memory :placed-furnace) and take it back: carried gear stays carried. It is forgotten
  only once dug up; a failed dig keeps it, so the next hold retries. True while the dig goes on."
  [c]
  (let [pos (:placed-furnace (ctx/mem c))
        e (waits/waiting-out c (await (ctx/call c :unplace 'jobs.blocks.dig {:pos pos :ways #{:carried}})))]
    (when (= :done (:kind e))
      (ctx/update-mem! c dissoc :placed-furnace :furnace)
      (when (= pos (mem/place (ctx/view c) :furnace))
        (ctx/remember! c :furnace {:gone true :was pos} mem/place-policy)))
    (= :continue e)))

(defn ^:async cooking-in-shelter?
  "Raw food carried and a furnace or smoker seen within cook's ::shelter-cook-reach, else a carried furnace put down
  beside the body once the body is safe from the night (inside/safe-from-night?; an exposed hold cooks only at a furnace it sees; can-place-furnace?, once it stands it is the seen one), or a cook under way, and the cook not rushed
  (cook/rushed?) nor failed in this job: run it (cook/cook!, fetching nothing). The furnace it put down is dug up when
  the cook ends. True while it goes on (do not eat raw), nil when it ended (the cooked food is eaten next) or none is
  possible."
  [c]
  (let [raw? (and (seq (cook/raw-carried c)) (not (:no-cook (ctx/mem c))))
        seen (when raw? (cook/seen-cooker c (cook/shelter-cook-reach)))
        placed (when (and raw? (not seen) (not (:placed-furnace (ctx/mem c))) (can-place-furnace? c) (inside/safe-from-night? (:primitives c)) (not (cook/rushed? c)))
                 (await (furnace/place-furnace! c)))
        _ (when (= :again placed) (ctx/update-mem! c assoc :placed-furnace (:furnace (ctx/mem c))))
        cooker (or seen (:placed-furnace (ctx/mem c)))]
    (cond
      (= :continue placed) true
      (and (or cooker (:cooking (ctx/mem c))) (not (cook/rushed? c)))
      (let [going (= :continue (await (cook/cook! c cooker {:ways #{:carried}})))]
        (if (or going (not (:placed-furnace (ctx/mem c))))
          going
          (await (pick-up-furnace! c))))
      (:placed-furnace (ctx/mem c)) (await (pick-up-furnace! c)))))

(defn ^:async eat-if-hungry!
  "The hungry reflex (below night in the shipped registers) cannot reach a held body, so a hold eats one bite (a
  jobs.survival.eat child) when the hungry trigger's condition holds (hungry?, or eat-now? below 7 hp), after cooking
  raw food at a furnace within reach of the shelter (cooking-in-shelter?). A sleeping body does not eat."
  [c]
  (let [p (:primitives c)
        self (.self p)
        hungry (and (not (sh-sense/sleeping? p)) (foods/wants-meal? self (reserve/spared (ctx/view c) self) {}))
        cooking (when hungry (await (cooking-in-shelter? c)))]
    (when (and hungry (not cooking) (eat/carried-best (assoc c :args {:allow-bad false})))
      (let [e (await (ctx/call c :eat 'jobs.survival.eat {:until 20 :max-bites 1 :get-food? false}))]
        (when (pos? (:ate (:data e) 0))
          (ctx/emit! c :shelter.ate :info {:food (.-food (.self p)) :text "ate while holding the night"}))))))

(defn food-wanted?
  "A starving body with nothing to eat, no get-food give-up in the last foods/starving-rest-ms, and no danger near
  (the same check as get-food's night search): worth a get-food child instead of a hold."
  [c]
  (let [p (:primitives c)
        self (.self p)]
    (and (not (sh-sense/sleeping? p))
         (foods/starving? self {})
         (not (foods/carries-food? self (reserve/spared (ctx/view c) self)))
         (zero? (ctx/count-in c :hungry (foods/starving-rest-ms)))
         (not (danger-q/danger-near? p (danger-q/hostile-radius) (danger-q/ranged-radius))))))

(def in-shelter-ways
  "The get-food ways a sheltered hold allows: they need no walk out of the shelter."
  #{:bake :cook})

(defn ^:async hold!
  "Hold the body still on purpose (reason) for hold-ms, eating first when hungry; a starving body with no food looks
  for some first (food-wanted?, a get-food child, limited to in-shelter-ways while the body is safe from the night): :again."
  [c reason]
  (ctx/hold-still! c reason)
  (await (eat-if-hungry! c))
  (when (food-wanted? c)
    (ctx/hold-still! c nil)
    (await (ctx/call c :food 'jobs.survival.get-food (if (inside/safe-from-night? (:primitives c)) {:ways in-shelter-ways :carry 0} {:carry 0})))
    (ctx/hold-still! c reason))
  (await (child/pause! c (/ (sites/hold-ms) 1000) reason))
  :again)

(defn busy!
  "The night acts again (walks, digs, logs out): no longer holding still."
  [c]
  (ctx/hold-still! c nil))

(defn sheltered! [c how] (ctx/update-mem! c assoc :sheltered how))

