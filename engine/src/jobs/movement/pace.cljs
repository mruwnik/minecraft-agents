(ns jobs.movement.pace
  (:require [engine.ctx :as ctx]
            [jobs.lib.near :as near]))

(def doc
  "Walk a, b, a, b (:laps times each) per round, one walk per leg (jobs.lib.near/walk-near!, doors :shut).
  Ends after :rounds rounds, or at once with a :leg-unfinished warning (target, status) when a leg does not arrive.
  A harmless long job, for showing that a reflex cuts a running one.")

(def args
  {:a {:doc "first point" :default nil}
   :b {:doc "second point" :default nil}
   :laps {:doc "a-b laps per round" :default 3}
   :rounds {:doc "rounds before done" :default 8}
   :range {:doc "walk range" :default 1}})

(defn check [_c] true)

(defn ^:async walk-legs
  "Walk the legs in order; nil when all arrived, else the first leg's pos and result."
  [c range legs]
  (loop [legs legs]
    (when-let [pos (first legs)]
      (let [r (await (near/walk-near! c pos range))]
        (if (= :there r)
          (recur (rest legs))
          [pos r])))))

(defn ^:async round [c]
  (let [{:keys [a b laps rounds range]} (:args c)
        unfinished (await (walk-legs c range (take (* 2 laps) (cycle [a b]))))]
    (if-let [[pos r] unfinished]
      (do (ctx/emit! c :leg-unfinished :warn {:to pos :status (name r)
                                               :text (str "a leg to " pos " did not arrive: " (name r))})
          :done)
      (let [done-rounds (inc (:rounds-run (ctx/mem c) 0))]
        (ctx/update-mem! c assoc :rounds-run done-rounds)
        (if (>= done-rounds rounds) :done :continue)))))
