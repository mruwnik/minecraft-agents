(ns jobs.time.wait-for-day
  (:require [engine.ctx :as ctx]
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

(def conditions [[:fn day-not-come]])

(defn ticks->s [n] (/ (settings/ticks->ms n) 1000))

(defn quote-of [c]
  (let [self (.self (:primitives c))
        t (.-timeOfDay self)]
    (if (.-isDay self)
      {:uses {:time [0 0 0]}}
      {:uses {:time [0 (ticks->s (max 0 (- day-tick t)))
                     (if (settings/frozen?) ##Inf (ticks->s (max 0 (- night-end-tick t))))]}})))

(defn ^:async round [c] (ctx/end c :done nil))
