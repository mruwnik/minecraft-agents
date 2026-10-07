(ns triggers.survival.suffocating
  "The suffocating trigger: the body is drowning or enclosed (jobs.lib.breath/situation says which, from sensing only)."
  (:require [jobs.lib.breath :as breath]))

(def defaults breath/defaults)

(defn suffocating
  "Holds when breath/situation says the body is drowning or enclosed.
  :min-oxygen (default 12 of 20) is the drowning threshold; the job's own :min-oxygen is set in the entry's :job spec.
  A danger reflex: cooldown 0 and breathe has no backoff, so a spent or declined job fires again at once while the
  danger lasts."
  [world _memory args]
  (some? (breath/situation world (:min-oxygen args breath/default-min-oxygen))))
