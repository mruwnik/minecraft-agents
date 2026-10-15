(ns jobs.time.wait-for-day
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]))

(def idle-ok true)

(def doc
  "Done once it is day. Waits (condition reason :day-not-come) at night, then ends :done. Needs nothing. Quote: the
  ticks left of the night at the live game rate (##Inf most while the server's tick is frozen).")

(def target [])

(def day-tick "The time of day (game ticks) at which the sun is up again (engine.senses/day-at?)." 23460)

(def night-end-tick "The tick the night ends at most." 24000)

(defn day-not-come [c]
  (when-not (.-isDay (.self (:primitives c))) :day-not-come))

(a/defargs settings
  {::time-s {:default 1800 :doc "Seconds the wait for day may take." :spec (a/int-in 1 nil)}})

(def defaults {:spend {:time ::time-s}})

(def conditions [[:fn day-not-come]])

(defn done?
  "Done when it is day."
  [c]
  (let [day? (boolean (.-isDay (.self (:primitives c))))]
    {:done? day? :units :days :left (if day? 0 1)}))

(defn ticks->s [n] (/ (settings/ticks->ms n) 1000))

(defn quote-of [c]
  (let [self (.self (:primitives c))
        t (.-timeOfDay self)]
    (if (.-isDay self)
      {:uses {:time [0 0 0]}}
      {:uses {:time [0 (ticks->s (max 0 (- day-tick t)))
                     (if (settings/frozen?) ##Inf (ticks->s (max 0 (- night-end-tick t))))]}})))

(defn ^:async round [c] (ctx/end c :done nil))
