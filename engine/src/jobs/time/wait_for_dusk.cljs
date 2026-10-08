(ns jobs.time.wait-for-dusk
  (:require [jobs.lib.game-facts :as facts]
            [engine.ctx :as ctx]))


(def idle-ok true)

(def doc
  "Done once it is evening. Waits (check) until the day's tick 12000 (dusk), and runs from dusk on through the
  night to dawn, so at once when started at night.")

(defn check [c]
  (or (>= (.-timeOfDay (.self (:primitives c))) facts/dusk-tick) (ctx/wait c {:reason :dusk-not-come :dusk facts/dusk-tick})))

(defn ^:async round [_c] :done)
