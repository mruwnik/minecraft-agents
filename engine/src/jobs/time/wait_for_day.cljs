(ns jobs.time.wait-for-day
  (:require [engine.ctx :as ctx]))

(def doc "Done once it is day. Waits (check reason :day-not-come) at night.")

(defn check [c]
  (or (.-isDay (.self (:primitives c))) (ctx/wait c :day-not-come)))

(defn ^:async round [_c] :done)
