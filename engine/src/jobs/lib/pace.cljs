(ns jobs.lib.pace
  "The timer a job's loop awaits between steps."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]))

(def pace-ms "The timer awaited between two steps, so a loop with no act never starves the event loop." 50)

(defn pace!
  "A promise that resolves after pace-ms (a timer, never a microtask)."
  []
  (js/Promise. (fn [resolve] (js/setTimeout resolve pace-ms))))

(defn ^:async steps!
  "A whole attempt made of steps: call (step) again, a pace! between, while it resolves to :again, the job's goal does
  not hold (ctx/done) and its can-run? holds (ctx/can-run); then resolve to what it gave. Between steps a goal that
  holds ends the job :done, a :never impossible, a :later or :look yields a listed root job (:continue); a reflex or a called child ends condition-failed.
  A cut round resolves to :continue."
  [c step]
  (loop []
    (let [r (await (step))]
      (if (or (not= :again r) (not (ctx/alive? c)))
        (if (= :again r) :continue r)
        (if-let [e (ctx/done c)]
          e
          (let [s (ctx/can-run c)]
            (case (first s)
              :ok (do (await (pace!)) (recur))
              :never (ctx/end c :impossible (contract/status-cause s))
              (if (ctx/returns-on-later? c) (ctx/end c :condition-failed (contract/status-cause s)) :continue))))))))

(defn ^:async timed-steps!
  "steps!, but a round that has run round-s seconds by the call's clock gives the body back with :continue after the step in
  hand, so the call's :checks are asked again; every step resumes from the body's cell."
  [c round-s step]
  (let [deadline (+ (ctx/now c) (* 1000 round-s))]
    (steps! c (fn ^:async timed []
                (let [r (await (step))]
                  (if (and (= :again r) (>= (ctx/now c) deadline)) :continue r))))))
