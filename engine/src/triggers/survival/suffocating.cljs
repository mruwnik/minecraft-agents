(ns triggers.survival.suffocating
  "The suffocating trigger: the body is drowning or enclosed (jobs.lib.breath/situation says which, from sensing only)."
  (:require [jobs.lib.body :as body]
            [jobs.lib.breath :as breath]))

(def defaults {})

(defn suffocating
  "Holds when breath/situation says the body is drowning or enclosed.
  A danger reflex: cooldown 0 and breathe has no backoff, so a spent or declined job fires again at once while the
  danger lasts. Never while dead: the corpse still lies in the water until the respawn, and a run started then
  would take the respawned body for one afloat."
  [world memory args]
  (and (not (body/dead? memory)) (some? (breath/situation world))))
