(ns jobs.time.wait-for-day
  (:require [engine.ctx :as ctx]))

(def doc "Done once it is day: its check declines at night (waiting :day-not-come), so its one round runs by day.")

(defn check [c]
  (or (.-isDay (.self (:primitives c))) (ctx/wait c :day-not-come)))

(defn ^:async round [_c] :done)
