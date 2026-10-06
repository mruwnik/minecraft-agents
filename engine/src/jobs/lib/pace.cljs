(ns jobs.lib.pace
  "The timer a job's loop awaits between steps.")

(def pace-ms "The timer awaited between two steps, so a loop with no act never starves the event loop." 50)

(defn pace!
  "A promise that resolves after pace-ms (a timer, never a microtask)."
  []
  (js/Promise. (fn [resolve] (js/setTimeout resolve pace-ms))))
