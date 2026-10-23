(ns jobs.lib.pace
  "The timer a job's loop awaits between steps."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.waits :as waits]))

(def pace-ms "The timer awaited between two steps, so a loop with no act never starves the event loop." 50)

(defn timer
  "A promise that resolves after pace-ms (a timer, never a microtask)."
  []
  (js/Promise. (fn [resolve] (js/setTimeout resolve pace-ms))))

(defn pace!
  "The pause between two steps of c's loop: its limits are checked (ctx/check-limits!: a loop of steps with no act
  ends on its budget), then the timer."
  [c]
  (ctx/check-limits! c)
  (timer))

(defn ^:async steps!
  "A whole attempt made of steps: call (step) again, a pace! between, while it resolves to :again, the job's goal does
  not hold (ctx/done) and its can-run? holds (ctx/can-run); then resolve to what it gave. Between steps a goal that
  holds ends the job :done, a :never impossible, a :later or :look yields a listed root job (:continue); a reflex or a called child ends condition-failed.
  A cut round resolves to :continue. A reflex job itself (not a child it calls, which yields :continue to its caller)
  never gives :continue: a step's :continue is another step, or, when a
  child waits on the world (child-wait), the condition-failed ending with that reason, as stepping on would spin."
  [c step]
  (loop []
    (let [r (await (step))
          r (if (and (= :continue r) (:reflex c) (empty? (:slots c)) (ctx/alive? c)) (or (waits/reflex-wait-end c) :again) r)]
      (if (or (not= :again r) (not (ctx/alive? c)))
        (if (= :again r) :continue r)
        (if-let [e (ctx/done c)]
          e
          (let [s (ctx/can-run c)]
            (case (first s)
              :ok (do (await (pace! c)) (recur))
              :never (ctx/end c :impossible (contract/status-cause s))
              (if (ctx/returns-on-later? c) (ctx/end c :condition-failed (contract/status-cause s)) :continue))))))))
