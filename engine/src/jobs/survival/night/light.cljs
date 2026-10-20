(ns jobs.survival.night.light
  "The night lights a sealed room that only its dark keeps from being shelter (jobs.build.light-area, called not copied)."
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.choose :as choose]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.torch :as torch]
            [jobs.survival.night.hold :as hold]))

(a/defargs settings
  {::craft-s {:default 5 :doc "Seconds one crafting of up to 4 torches from coal or charcoal is priced at, in light's quote." :spec (a/num-in 0 nil)}})

(def ways
  "The shut-in ways light competes with, and their jobs."
  {:walls 'jobs.survival.wall-in :pit 'jobs.survival.dig-pit :pocket 'jobs.survival.dig-pocket})

(defn ^:async way-cost
  "Seconds the cheapest of the shut-in ways quotes for the night, ##Inf when none can be done here."
  [c]
  (let [args {:for :night :use {:blocks (choose/filler-names c :shelter)}}]
    (loop [[job & more] (vals ways) best ##Inf]
      (if (nil? job)
        best
        (let [q (await (ctx/quote c job args))]
          (recur more (if (contract/ending? q) best (min best (get-in q [:uses :job-time 1] ##Inf)))))))))

(defn ^:async light-cost
  "Seconds light-area over the room's cells quotes, plus crafting the torches that are short from coal (##Inf when the coal
  cannot make them)."
  [c cells]
  (let [p (:primitives c)
        q (await (ctx/quote c 'jobs.build.light-area {:cells cells}))
        need (get-in q [:uses :items "torch" 1] 0)
        short (max 0 (- need (torch/torches-held p)))]
    (cond
      (contract/ending? q) ##Inf
      (> short (* 4 (torch/fuel-held p))) ##Inf
      :else (+ (get-in q [:uses :job-time 1] ##Inf) (* (settings/get settings ::craft-s) (js/Math.ceil (/ short 4)))))))

(defn ^:async wanted?
  "Once a night: the body stands in a sealed room whose only fault is a dark spawnable cell beyond the spawn-free radius, holds
  torches or coal, and lighting quotes no more than the cheapest wall, pit or pocket."
  [c]
  (let [p (:primitives c)]
    (and (not (:light-tried (ctx/mem c)))
         (or (pos? (torch/torches-held p)) (pos? (torch/fuel-held p)))
         (sh-inside/dark-room-at? p (sh-sense/feet p))
         (<= (await (light-cost c (:cells (reach-room/room p (sh-sense/feet p)))))
             (await (way-cost c))))))

(defn ^:async light!
  "jobs.build.light-area over the room's cells, called again while it goes on; once it ends, :light-tried, whatever came of it
  (the next pass finds the room safe, or digs in as before)."
  [c]
  (hold/busy! c)
  (let [p (:primitives c)
        cells (:cells (reach-room/room p (sh-sense/feet p)))
        d (await (ctx/call c :light 'jobs.build.light-area {:cells cells}))]
    (when-not (= :continue d)
      (ctx/update-mem! c assoc :light-tried true))
    :again))
