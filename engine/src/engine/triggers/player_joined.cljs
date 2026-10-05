(ns engine.triggers.player-joined
  "The player-joined trigger: another player joined the server a moment ago."
  (:require [engine.memory :as mem]))

(def default-window-s 10)

(def trigger
  "Holds when the latest :player-joined entry (a body event, written when another player's join reaches the body)
  is at most :window-s (args, default 10) old.
  Runs jobs.debug.notify, which reports and writes a :notify entry. The cooldown equals the window, so one join
  fires once. Replace the job in the register entry to greet or follow instead.
  :player-left has no trigger: it is a memory entry only."
  {:name :player-joined
   :when (fn [_world memory args]
           (let [t (:t (mem/latest memory :player-joined))]
             (boolean (and t (<= (- (:now memory) t) (* 1000 (:window-s args default-window-s)))))))
   :job '(jobs.debug.notify {:text "player joined"})
   :args {:window-s default-window-s}
   :persistence :cooldown
   :cooldown-s default-window-s})
