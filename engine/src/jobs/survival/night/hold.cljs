(ns jobs.survival.night.hold
  "The night's holds: hold still on purpose, eating or cooking first, and the bookkeeping of what the night did."
  (:require [engine.ctx :as ctx]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.foods :as foods]
            [jobs.lib.shelter :as sh]
            [jobs.lib.util :as u]
            [jobs.survival.eat :as eat]
            [jobs.survival.get-food :as get-food]))

(def hold-ms
  "How long one hold waits before the night looks again (the wait primitive does not wake a sleeping body)."
  5000)

(defn ^:async cooking-in-shelter?
  "Raw food carried and a furnace or smoker seen within get-food's ::shelter-cook-reach, or a cook under way, and the
  cook not rushed (get-food/rushed?) nor failed lately: run it (get-food/cook!). True while it goes on (do not eat
  raw), nil when it ended (the cooked food is eaten next) or none is possible."
  [c]
  (let [cooker (when (and (seq (get-food/raw-carried c))
                          (zero? (ctx/count-in c :no-cook get-food/no-bake-ms)))
                 (get-food/seen-cooker c (get-food/shelter-cook-reach)))]
    (when (and (or cooker (:cooking (ctx/mem c))) (not (get-food/rushed? c)))
      (= :continue (await (get-food/cook! c cooker))))))

(defn ^:async eat-if-hungry!
  "The hungry reflex (below night in the shipped registers) cannot reach a held body, so a hold eats one carried
  food (jobs.survival.eat's choice) when the hungry trigger's condition holds (hungry?, or eat-now? below 7 hp),
  after cooking raw food at a furnace within reach of the shelter (cooking-in-shelter?). A sleeping body does not eat."
  [c]
  (let [p (:primitives c)
        self (.self p)
        health (.-health self)
        hungry (and (not (sh/sleeping? p)) (or (foods/hungry? (.-food self) health {}) (foods/eat-now? self {})))
        cooking (when hungry (await (cooking-in-shelter? c)))
        best (when (and hungry (not cooking))
               (eat/best-food (u/inventory p) false nil health (.-food self) (foods/reserved (ctx/view c))))]
    (when best
      (await (ctx/act c :equip #js {:item best}))
      (let [r (await (ctx/act c :eat #js {:item best}))]
        (when (= "ate" (.-status r))
          (ctx/emit! c :shelter.ate :info {:item best :food (.-food r) :text (str "ate " best " while holding the night")}))))))

(defn food-wanted?
  "A starving body with nothing to eat, no get-food give-up in the last foods/starving-rest-ms, and no danger near
  (the same check as get-food's night search): worth a get-food child instead of a hold."
  [c]
  (let [p (:primitives c)
        self (.self p)]
    (and (not (sh/sleeping? p))
         (foods/starving? self {})
         (not (foods/carries-food? self (foods/reserved (ctx/view c))))
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
    (await (ctx/call-child c :food 'jobs.survival.get-food {}))
    (ctx/hold-still! c reason))
  (await (ctx/act c :wait #js {:ms hold-ms}))
  :again)

(defn busy!
  "The night acts again (walks, digs, logs out): no longer holding still."
  [c]
  (ctx/hold-still! c nil))

(defn sheltered! [c how] (ctx/update-mem! c assoc :sheltered how))

