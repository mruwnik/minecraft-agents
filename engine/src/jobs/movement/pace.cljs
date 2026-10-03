(ns jobs.movement.pace
  (:require [engine.ctx :as ctx]))

(def doc
  "Walk a, b, a, b (:laps times each) per round, a moveTo per leg, stopping
  the round early on any status but arrived; done after :rounds rounds. A
  harmless long round for showing a reflex cut a running job.")

(def args
  {:a {:doc "first point" :default nil}
   :b {:doc "second point" :default nil}
   :laps {:doc "a-b laps per round" :default 3}
   :rounds {:doc "rounds before done" :default 8}
   :range {:doc "moveTo range" :default 1}})

(defn check [_c] true)

(defn ^:async round [c]
  (let [{:keys [a b laps rounds range]} (:args c)
        legs (take (* 2 laps) (cycle [a b]))]
    (loop [legs legs]
      (when-let [pos (first legs)]
        (let [r (await (ctx/act c :moveTo (clj->js {:pos pos :range range})))]
          (when (= "arrived" (.-status r))
            (recur (rest legs))))))
    (let [done-rounds (inc (:rounds-run (ctx/mem c) 0))]
      (ctx/update-mem! c assoc :rounds-run done-rounds)
      (if (>= done-rounds rounds) :done :continue))))
