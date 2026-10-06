(ns jobs.lib.pace
  "The timer a job's loop awaits between steps."
  (:require [engine.ctx :as ctx]))

(def pace-ms "The timer awaited between two steps, so a loop with no act never starves the event loop." 50)

(defn pace!
  "A promise that resolves after pace-ms (a timer, never a microtask)."
  []
  (js/Promise. (fn [resolve] (js/setTimeout resolve pace-ms))))

(defn ^:async steps!
  "A whole attempt made of steps: call (step) again, a pace! between, while it resolves to :again; then resolve to
  what it gave. A cut round resolves to :continue."
  [c step]
  (loop []
    (let [r (await (step))]
      (cond
        (not= :again r) r
        (ctx/alive? c) (do (await (pace!)) (recur))
        :else :continue))))
