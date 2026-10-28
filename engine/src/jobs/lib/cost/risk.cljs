(ns jobs.lib.cost.risk
  "How many hp of expected damage a risk may take at a health and food, the one rule (jobs.lib.price/allowed adds a goal's
  worth and the cannot-heal lift): ::risk-share of the health, cut by ::hungry-share at ::hungry-food or under. too-hurt?:
  a risky job's own work (::work-hp) takes more than that and would not at full health. Pure."
  (:require [engine.args :as a]
            [engine.settings :as settings]))

(a/defargs settings
  {::risk-share {:default 0.5 :spec (a/num-in 0 nil)
                 :doc "Share of the health the body has that the expected damage of a walk (spawns met on its dark open stretches, hostiles near its way), of a stay at an exposed place or of a risky job's work may take; above it the start check waits."}
   ::hungry-food {:default 6 :spec (a/int-in 0 20)
                  :doc "Food at or under which the body cannot sprint away and does not heal: the damage it can afford is cut by ::hungry-share."}
   ::hungry-share {:default 0.5 :spec (a/num-in 0 1)
                   :doc "Share of the affordable damage a body at ::hungry-food or under may risk."}
   ::work-hp {:default 3 :spec (a/num-in 0 nil)
              :doc "hp a risky job's own work (a fall, lava, a mob met at the face) is expected to cost before the body can stop it."}})

(defn share-hp
  "hp of expected damage a body at health and food may risk while waiting would heal it: ::risk-share of the health, cut
  while hungry (food unknown: not hungry)."
  [health food]
  (* (settings/get settings ::risk-share) health
     (if (some-> food (<= (settings/get settings ::hungry-food))) (settings/get settings ::hungry-share) 1)))

(defn work-hp [] (settings/get settings ::work-hp))

(defn too-hurt?
  "Whether a risky job's work (::work-hp) is more than a body at health and food may risk (share-hp), and healed it may."
  [health food]
  (let [w (work-hp)]
    (and (> w (share-hp health food)) (<= w (share-hp 20 food)))))
