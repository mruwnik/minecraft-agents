(ns engine.fast-pace
  "Under test the 50 ms timers a job's loop awaits between steps resolve on the next event-loop turn instead (a
  setImmediate: the loop still yields, nothing waits). Loaded by engine.test-util."
  (:require [jobs.movement.go-to :as go-to]
            [jobs.lib.pace :as pace]
            [jobs.lib.child :as child]))

(defn immediate!
  "A promise that resolves on the next event-loop turn."
  []
  (js/Promise. (fn [resolve] (js/setImmediate resolve))))

(set! go-to/pace! immediate!)
(set! pace/pace! immediate!)
(set! child/pace! immediate!)
