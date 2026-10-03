(ns engine.triggers.player-sleeping-nearby
  "The player-sleeping-nearby trigger. It holds when it is night, another
  player within :player-radius is asleep, no bed is remembered within
  :bed-radius, :offline-allowed is not false and the last log-out was not
  unsupported, and the latest :log-out entry is at least :gap-s (default 30)
  seconds old (or there is none). The gap is the trigger's own: the cooldown
  alone does not hold, because right after a return the sleeper is not sensed
  yet, the end counts as cleared and no cooldown starts. Whether the body is roofed does not matter: a roofed body must
  still log out so the sleeper can skip the night."
  (:require [engine.jobs.shelter :as sh]
            [engine.memory :as mem]))

(def default-gap-s 30)

(defn log-out-gap-passed? [memory args]
  (let [last (:t (mem/latest memory :log-out))]
    (or (nil? last)
        (>= (- (:now memory) last) (* 1000 (:gap-s args default-gap-s))))))

(def trigger
  {:name :player-sleeping-nearby
   :when (fn [world memory args]
           (and (log-out-gap-passed? memory args)
                (sh/log-out-wanted? world memory args)))
   :job '(jobs.survival.log-out)
   :args {:player-radius sh/default-player-radius
          :bed-radius sh/default-bed-radius
          :offline-allowed true
          :gap-s default-gap-s}
   :persistence :cooldown
   :cooldown-s 30})
