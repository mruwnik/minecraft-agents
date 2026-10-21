(ns jobs.lib.shelter.clock
  "The night's clock: day and night ticks, the time left or gone, tonight's start."
  (:require [engine.memory :as mem]
            [engine.settings :as settings]))

(def ms-per-day
  "One in-game day in engine-clock milliseconds (20 minutes). \"A day\" in memory policies and :max-days-awake is
  this long."
  1200000)

(def slept-tonight-ms
  "A :slept entry younger than this counts as \"slept tonight\" (half an in-game day)."
  (/ ms-per-day 2))

(def morning-tick
  "The first time of day (ticks) that self().isDay counts as day again after a night."
  23461)

(def dusk-tick
  "The first time of day (ticks) that self().isDay counts as night."
  12542)

(def dark-tick "The time of day (ticks) by which the open sky is dark enough for monsters to spawn." 13000)

(def home-limit "Farthest home, in blocks, worth the walk (go-home's :limit default, the dusk-home trigger's bound)." 160)

(def day-tick "The time of day (game ticks) at which the sun is up again (engine.senses/day-at?)." 23460)

(def night-end-tick "The tick the night ends at most." 24000)

(defn ticks->s [n] (/ (settings/ticks->ms n) 1000))

(defn night-left-quote
  "The :uses :job-time quote of the night left, from the body's self: [0 mean most] seconds at the live game rate (##Inf
  most while the server's tick is frozen); zero by day."
  [c]
  (let [self (.self (:primitives c))
        t (.-timeOfDay self)]
    (if (.-isDay self)
      {:uses {:job-time [0 0 0]}}
      {:uses {:job-time [0 (ticks->s (max 0 (- day-tick t)))
                     (if (settings/frozen?) ##Inf (ticks->s (max 0 (- night-end-tick t))))]}})))

(def morning-margin-ms
  "Extra time away, so a body that logged out until morning comes back after the night has ended."
  2000)

(defn ms-until-morning
  "Milliseconds (at the live game rate) until the night ends at time of day t (ticks), plus morning-margin-ms; 0 by day."
  [t]
  (if (or (< t dusk-tick) (>= t morning-tick))
    0
    (+ (settings/ticks->ms (- morning-tick t)) morning-margin-ms)))

(defn ms-since-dusk
  "Milliseconds since this night began at time of day t (ticks); 0 by day."
  [t]
  (if (or (< t dusk-tick) (>= t morning-tick)) 0 (settings/ticks->ms (- t dusk-tick))))

(defn tonight
  "The engine time this night began: entries written since are tonight's."
  [p view]
  (- (:now view) (ms-since-dusk (.-timeOfDay (.self p)))))

(defn latest-since [view kind t]
  (let [e (mem/latest view kind)]
    (when (and e (>= (:t e) t)) e)))
