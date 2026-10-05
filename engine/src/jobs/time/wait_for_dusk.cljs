(ns jobs.time.wait-for-dusk
  (:require [engine.ctx :as ctx]))

(def dusk-tick 12000)

(def doc
  "Done once it is evening. Waits (check) until the day's tick 12000 (dusk), and runs from dusk on through the
  night to dawn, so at once when started at night.")

(defn check [c]
  (or (>= (.-timeOfDay (.self (:primitives c))) dusk-tick) (ctx/wait c {:reason :dusk-not-come :dusk dusk-tick})))

(defn ^:async round [_c] :done)
