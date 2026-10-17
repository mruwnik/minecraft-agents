(ns triggers.survival.poisoned
  "The poisoned trigger: a poison or wither costs hp and a cure is carried."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.body :as body]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::below-health {:default 10 :doc "Health (of 20) below which a poisoned body takes its cure." :spec (a/num-in 0 20)}})

(defn poisoned
  "Holds while the body has an hp-costing effect (poison, wither), its health is below :health (default ::below-health, 10) and a
  cure (jobs.lib.body/cures) is carried. Poison alone stops at half a heart, so a healthy body leaves it to run out."
  [world memory args]
  (and (not (body/recovery-pending? memory :poisoned))
       (< (.-health (.self world)) (or (:health args) (settings/get settings ::below-health)))
       (body/poisoned? world)
       (some? (body/cure-carried (u/inventory world) nil))))
