(ns triggers.survival.dusk-home
  "The dusk-home trigger: before night, a body under the open sky whose remembered :home (else :bed) is within :limit
  blocks leaves for it once the day left no longer covers the walk (:slack times the straight line at :speed blocks a
  second) plus :margin-s seconds. It does not hold at night (the night job has it), within :arrive blocks of the place,
  beyond :limit (the body digs in where night finds it), or underground (sh/buried?)."
  (:require [engine.memory :as mem]
            [jobs.lib.shelter :as sh]
            [jobs.lib.util :as u]))

(def defaults {:limit 160 :margin-s 60 :speed 4.3 :slack 1.4 :arrive 4})

(defn target
  "The place to go home to: :home, else :bed."
  [view]
  (or (mem/place view :home) (mem/place view :bed)))

(defn dusk-home
  [p view {:keys [limit margin-s speed slack arrive]}]
  (let [self (.self p)
        left-s (/ (- sh/dusk-tick (.-timeOfDay self)) 20)
        pos (target view)]
    (boolean
     (and pos (pos? left-s) (not (sh/sleeping? p))
          (let [d (u/dist (u/self-pos {:primitives p}) pos)]
            (and (< arrive d) (<= d limit)
                 (<= left-s (+ margin-s (/ (* slack d) speed)))
                 (not (sh/buried? p))))))))
