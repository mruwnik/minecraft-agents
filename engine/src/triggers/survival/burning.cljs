(ns triggers.survival.burning
  "The burning trigger: the body is on fire or in lava and not fire resistant (jobs.lib.body/burning?), or a water pour
  is left to scoop."
  (:require [engine.memory :as mem]
            [jobs.lib.body :as body]))

(defn burning
  "Holds when the body is on fire or in lava and has no fire_resistance
  effect, or when memory holds an unexpired :extinguish-pour (a run cut after
  the pour left its water source to scoop). A danger reflex: no cooldown (and
  extinguish has no backoff), so a job that ends with the body still burning is fired again at once."
  [world memory _args]
  (boolean (or (body/burning? (.self world))
               (mem/latest memory :extinguish-pour))))
