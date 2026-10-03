(ns jobs.time.wait-for-day)

(def doc "Done once it is day: its check declines at night, so its one round runs by day.")

(defn check [c] (.-isDay (.self (:primitives c))))

(defn ^:async round [_c] :done)
