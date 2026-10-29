(ns triggers.survival.poisoned
  "The poisoned trigger: a poison or wither costs hp."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.body :as body]))

(a/defargs settings
  {::below-health {:default 10 :doc "Health (of 20) below which a poisoned body takes its cure." :spec (a/num-in 0 20)}})

(defn poisoned
  "Holds while the body has an hp-costing effect (poison, wither), its health is below :health (default ::below-health, 10) and no regeneration
  runs (a golden apple's does not clear the poison, so the next cure waits for it to end); survival.cure carries a cure or
  prices the fetch of one against waiting the poison out. Poison alone stops at half a heart, so a healthy body leaves it to run out."
  [world memory args]
  (and (not (body/recovery-pending? memory :poisoned))
       (< (.-health (.self world)) (or (:health args) (settings/get settings ::below-health)))
       (body/poisoned? world)
       (not (body/regenerating? world))))
