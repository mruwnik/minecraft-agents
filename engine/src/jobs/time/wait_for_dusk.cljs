(ns jobs.time.wait-for-dusk
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [jobs.lib.game-facts :as facts]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.cond :as cond]
            [engine.settings :as settings]))

(def idle-ok true)

(def doc
  "Done once it is evening. Waits (condition reason :dusk-not-come) until the day's tick 12000 (dusk), and runs from
  dusk on through the night to dawn, so at once when started at night, then ends :done. Needs nothing. Quote: the
  ticks left to dusk at the live game rate (##Inf most while the server's tick is frozen).")

(def target [])

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds the wait for dusk may take." :spec (a/int-in 1 nil)}})

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? ready/ok)

(def fit? ready/ok)

(def can-run? (cond/because :dusk-not-come (cond/>= (cond/time-of-day) facts/dusk-tick)))

(def can-start? ready/ok)

(defn done?
  "Done when it is evening or later in the day's ticks."
  [c]
  (let [dusk? (>= (.-timeOfDay (.self (:primitives c))) facts/dusk-tick)]
    (if dusk?
      (contract/ok :dusk "it is evening" :units :dusks :done 1 :left 0)
      (contract/later :dusk-not-come "it is not evening yet" :units :dusks :done 0 :left 1))))

(defn quote-of [c]
  (let [left (max 0 (- facts/dusk-tick (.-timeOfDay (.self (:primitives c)))))
        s (/ (settings/ticks->ms left) 1000)]
    {:uses {:job-time [0 s (if (and (pos? left) (settings/frozen?)) ##Inf s)]}}))

(defn ^:async round [c] (ctx/end c :done nil))
