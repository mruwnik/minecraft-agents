(ns triggers.debug.player-joined
  "The player-joined trigger: another player joined the server a moment ago."
  (:require [engine.memory :as mem]))

(def default-window-s 10)

(def defaults {:window-s default-window-s})

(defn player-joined
  "Holds when the latest :player-joined entry (a body event, written when another player's join reaches the body)
  is at most :window-s (args, default 10) old.
  Runs jobs.debug.notify, which reports and writes a :notify entry. The cooldown equals the window, so one join
  fires once. Replace the job in the register entry to greet or follow instead.
  :player-left has no trigger: it is a memory entry only."
  [_world memory args]
  (let [t (:t (mem/latest memory :player-joined))]
    (boolean (and t (<= (- (:now memory) t) (* 1000 (:window-s args default-window-s)))))))
