(ns engine.triggers.player-sleeping-nearby
  "The player-sleeping-nearby trigger. It holds when it is night, another
  player within :player-radius is asleep, no bed is remembered within
  :bed-radius, :offline-allowed is not false and the last log-out was not
  unsupported. The register cooldown (30 s) spaces log-outs, and a log-out
  ending at the reconnect is judged once the body has settled. Whether the
  body is roofed does not matter: a roofed body must still log out so the
  sleeper can skip the night."
  (:require [engine.jobs.shelter :as sh]))

(def trigger
  {:name :player-sleeping-nearby
   :when (fn [world memory args]
           (sh/log-out-wanted? world memory args))
   :job '(jobs.survival.log-out)
   :args {:player-radius sh/default-player-radius
          :bed-radius sh/default-bed-radius
          :offline-allowed true}
   :persistence :cooldown
   :cooldown-s 30})
