(ns jobs.lib.warm
  "The tables the first danger would build while a mob is beside the body, built once the body is up (the :body/warm hook)."
  (:require [engine.game :as game]
            [engine.path.blocks :as blocks]
            [engine.settings :as settings]
            [jobs.lib.cost.caught :as caught]
            [jobs.lib.cost.value :as value]))

(defn warm!
  "Build the item worth tables of the body's game version (as item-value and find-worth ask them), the planner's block table and the
  posterior of every counted flight and fight group (caught/prior: the first masked plan's unseen-share prices spawns with them)."
  []
  (value/item-value [{:name "stone"}])
  (value/item-value [{:name "stone"}] :overrides (value/measured-worths @game/version))
  (blocks/default-state-table)
  (doseq [which [::caught/flights ::caught/fights]
          counts (vals (settings/get caught/settings which))]
    (caught/posterior counts))
  nil)
