(ns jobs.survival.night.hold
  "The night's holds: hold still on purpose, eating or cooking first, and the bookkeeping of what the night did."
  (:require [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.foods :as foods]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.survival.eat :as eat]
            [jobs.survival.get-food.cook :as cook]))

(def hold-ms
  "How long one hold waits before the night looks again (the wait primitive does not wake a sleeping body)."
  5000)

(defn ^:async cooking-in-shelter?
  "Raw food carried and a furnace or smoker seen within cook's ::shelter-cook-reach, or a cook under way, and the
  cook not rushed (cook/rushed?) nor failed in this job: run it (cook/cook!). True while it goes on (do not eat
  raw), nil when it ended (the cooked food is eaten next) or none is possible."
  [c]
  (let [cooker (when (and (seq (cook/raw-carried c))
                          (not (:no-cook (ctx/mem c))))
                 (cook/seen-cooker c (cook/shelter-cook-reach)))]
    (when (and (or cooker (:cooking (ctx/mem c))) (not (cook/rushed? c)))
      (= :continue (await (cook/cook! c cooker))))))

(defn ^:async eat-if-hungry!
  "The hungry reflex (below night in the shipped registers) cannot reach a held body, so a hold eats one bite (a
  jobs.survival.eat child) when the hungry trigger's condition holds (hungry?, or eat-now? below 7 hp), after cooking
  raw food at a furnace within reach of the shelter (cooking-in-shelter?). A sleeping body does not eat."
  [c]
  (let [p (:primitives c)
        self (.self p)
        hungry (and (not (sh-sense/sleeping? p)) (or (foods/hungry? (.-food self) (.-health self) {}) (foods/eat-now? self {})))
        cooking (when hungry (await (cooking-in-shelter? c)))]
    (when (and hungry (not cooking) (eat/carried-best (assoc c :args {:allow-bad false})))
      (let [e (await (ctx/call c :eat 'jobs.survival.eat {:until 20 :max-bites 1}))]
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

(defn ^:async hold!
  "Hold the body still on purpose (reason) for hold-ms, eating first when hungry; a starving body with no food looks
  for some first (food-wanted?, a get-food child): :again."
  [c reason]
  (ctx/hold-still! c reason)
  (await (eat-if-hungry! c))
  (when (food-wanted? c)
    (ctx/hold-still! c nil)
    (await (ctx/call c :food 'jobs.survival.get-food {}))
    (ctx/hold-still! c reason))
  (await (child/pause! c (/ hold-ms 1000) reason))
  :again)

(defn busy!
  "The night acts again (walks, digs, logs out): no longer holding still."
  [c]
  (ctx/hold-still! c nil))

(defn sheltered! [c how] (ctx/update-mem! c assoc :sheltered how))

