(ns engine.fast-pace
  "Under test the 50 ms timers a job's loop awaits between steps resolve on the next event-loop turn instead (a
  setImmediate: the loop still yields, nothing waits), and add their pace-ms to paced-ms, so a test clock
  (test-util/act-clock) counts them as the time they take live. Loaded by engine.test-util."
  (:require [jobs.lib.pace :as pace]))

(def paced-ms "The ms of every pace timer so far." (atom 0))

(defn immediate!
  "A promise that resolves on the next event-loop turn; pace-ms more paced."
  []
  (swap! paced-ms + pace/pace-ms)
  (js/Promise. (fn [resolve] (js/setImmediate resolve))))

(set! pace/timer immediate!)
