(ns engine.triggers.stuck
  "The stuck trigger and the condition the unstick job shares with it. Both
  read only the :moved entries engine.core/act writes after every moveTo."
  (:require [engine.memory :as mem]))

(def defaults
  {:n 4 :min-move 1.5 :window-ms 60000 :quiet-ms 300000})

(def ok-statuses #{"arrived" "partial"})

(defn dist [a b]
  (js/Math.hypot (- (:x a) (:x b)) (- (:y a) (:y b)) (- (:z a) (:z b))))

(defn bad-move?
  "A :moved entry's data is a bad move when moveTo did not end arrived or
  partial, or the body moved less than min-move blocks. A move that asked for
  a spot within min-move of where the body already stood is a no-op, not bad."
  [min-move {:keys [from to status target]}]
  (or (not (ok-statuses status))
      (and (< (dist from to) min-move)
           (or (nil? target) (>= (dist from target) min-move)))))

(defn stuck?
  "True when the last :n :moved entries are all bad moves and the oldest of
  them is less than :window-ms old. False with fewer than :n entries. Moves
  written before the latest :stuck entry (a failed unstick) are not counted,
  so a body the job gave up on needs :n fresh bad moves to fire again, and
  not even then until that entry is :quiet-ms old."
  [view args]
  (let [{:keys [n min-move window-ms quiet-ms]} (merge defaults args)
        gave-up (or (:t (mem/latest view :stuck)) 0)
        last-n (take-last n (filter #(> (:t %) gave-up) (mem/entries view :moved)))]
    (and (>= (- (:now view) gave-up) quiet-ms)
         (= n (count last-n))
         (< (- (:now view) (:t (first last-n))) window-ms)
         (every? #(bad-move? min-move (:data %)) last-n))))

(def stuck
  "Holds when stuck? does, with :n, :min-move, :window-ms and :quiet-ms from
  the args. The job it starts is (jobs.maintenance.unstick); see its doc for
  how the two interact. After a give-up the :stuck entry silences the trigger
  for :quiet-ms (5 min), so a body still blocked under the resumed job does
  not re-fire it every cooldown; the 60 s cooldown only covers the spell
  that ended well."
  {:name :stuck
   :when (fn [_world memory args] (stuck? memory args))
   :job '(jobs.maintenance.unstick)
   :args defaults
   :persistence :cooldown
   :cooldown-s 60})
