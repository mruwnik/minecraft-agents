(ns triggers.survival.burning
  "The burning trigger: the body is on fire or in lava and not fire resistant (jobs.lib.body/burning?), or a water pour
  is left to scoop."
  (:require [engine.args :as a]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.body :as body]
            [jobs.lib.danger :as danger-q]))

(a/defargs settings
  {::with-hostile {:default false :doc "Whether the trigger may fire for plain flames while a real danger is near." :spec boolean?}})

(defn burning
  "Holds when the body is on fire, in lava or in a fire block and has no fire_resistance
  effect, or when memory holds an unexpired :extinguish-pour (a run cut after
  the pour left its water source to scoop). A danger reflex: no cooldown (and
  extinguish has no backoff), so a job that ends with the body still burning is fired again at once.
  Plain flames (not lava, not a fire block) and a pour to scoop yield while a real danger is near (the hostile-near
  rule at its default radii), so the flight goes on: a burning body hurts less than a zombie in reach. Lava and fire
  blocks always fire. ::with-hostile lifts the yield."
  [world memory _args]
  (boolean (and (or (body/burning? world)
                    (mem/latest memory :extinguish-pour))
                (or (settings/get settings ::with-hostile)
                    (body/in-hazard? world)
                    (not (danger-q/danger-near? world (danger-q/hostile-radius) (danger-q/ranged-radius)))))))
