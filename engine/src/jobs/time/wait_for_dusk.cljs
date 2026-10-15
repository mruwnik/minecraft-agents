(ns jobs.time.wait-for-dusk
  (:require [engine.args :as a]
            [jobs.lib.game-facts :as facts]
            [engine.ctx :as ctx]
            [engine.settings :as settings]))

(def idle-ok true)

(def doc
  "Done once it is evening. Waits (condition reason :dusk-not-come) until the day's tick 12000 (dusk), and runs from
  dusk on through the night to dawn, so at once when started at night, then ends :done. Needs nothing. Quote: the
  ticks left to dusk at the live game rate (##Inf most while the server's tick is frozen).")

(def target [])

(defn dusk-not-come [c]
  (when-not (>= (.-timeOfDay (.self (:primitives c))) facts/dusk-tick)
    {:reason :dusk-not-come :dusk facts/dusk-tick}))

(a/defargs settings
  {::time-s {:default 1800 :doc "Seconds the wait for dusk may take." :spec (a/int-in 1 nil)}})

(def defaults {:spend {:time ::time-s}})

(def conditions [[:fn dusk-not-come]])

(defn done?
  "Done when it is evening or later in the day's ticks."
  [c]
  (let [dusk? (>= (.-timeOfDay (.self (:primitives c))) facts/dusk-tick)]
    {:done? dusk? :units :dusks :left (if dusk? 0 1)}))

(defn quote-of [c]
  (let [left (max 0 (- facts/dusk-tick (.-timeOfDay (.self (:primitives c)))))
        s (/ (settings/ticks->ms left) 1000)]
    {:uses {:time [0 s (if (and (pos? left) (settings/frozen?)) ##Inf s)]}}))

(defn ^:async round [c] (ctx/end c :done nil))
