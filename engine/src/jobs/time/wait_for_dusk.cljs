(ns jobs.time.wait-for-dusk)

(def dusk-tick 12000)

(def doc "Done once it is evening: its check declines until the day's tick 12000 (dusk) and stays true through the night to dawn, so its one round runs from dusk on, and at once when started at night.")

(defn check [c] (>= (.-timeOfDay (.self (:primitives c))) dusk-tick))

(defn ^:async round [_c] :done)
