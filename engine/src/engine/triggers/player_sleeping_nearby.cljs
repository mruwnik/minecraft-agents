(ns engine.triggers.player-sleeping-nearby
  "The player-sleeping-nearby trigger. It holds (sh/log-out-wanted?) when all of these are so:
    it is night
    another player within :player-radius is asleep
    no bed is remembered within :bed-radius
    :offline-allowed is not false
    the last log-out was not unsupported
  The job logs out for 20 s: the sleeper skips the night within seconds of the body leaving.
  The 30 s cooldown spaces log-outs. A roofed body must still log out, so the roof does not matter."
  (:require [engine.jobs.shelter :as sh]))

(def trigger
  {:name :player-sleeping-nearby
   :when (fn [world memory args]
           (sh/log-out-wanted? world memory args))
   :job '(jobs.survival.log-out {:offline-ms 20000})
   :args {:player-radius sh/default-player-radius
          :bed-radius sh/default-bed-radius
          :offline-allowed true}
   :persistence :cooldown
   :cooldown-s 30})
