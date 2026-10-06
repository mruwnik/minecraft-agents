(ns jobs.lib.cost
  "The cost and estimate calculators, in one place. Pure: they take data (equipment, mobs, items, a kind-at lookup) and
  never read the world themselves. They compose: armour under mob threat, mob threat under fight cost, route danger and
  the planner's rate; route danger under fetch cost.

    armour   armour-stats, after-armour, equipment-of: vanilla's per-hit formula, the only one (jobs.lib.cost.armour).
    threat   hostile?, mob-hurt: what one mob does over some seconds, after armour (jobs.lib.cost.threat).
    fight    fight-damage, decide: the damage of a fight with the current kit, :fight or :flee (jobs.lib.cost.fight).
    danger   route-danger, straight-route: the danger of a walk past mobs; stance, danger-rate, danger-list: go-to's
             planner dangers (jobs.lib.cost.danger).
    value    item-value, fetch-cost: what items are worth, what fetching them costs (jobs.lib.cost.value)."
  (:require [jobs.lib.cost.armour :as armour]
            [jobs.lib.cost.danger :as danger]
            [jobs.lib.cost.fight :as fight]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.cost.value :as value]))

(def equipment-of armour/equipment-of)
(def armour-stats armour/armour-stats)
(def after-armour armour/after-armour)

(def hostile? threat/hostile?)
(def mob-hurt threat/mob-hurt)

(def fight-damage fight/fight-damage)
(def decide fight/decide)

(def route-danger danger/route-danger)
(def straight-route danger/straight-route)
(def stance danger/stance)
(def danger-rate danger/danger-rate)
(def danger-list danger/danger-list)
(def max-dangers danger/max-dangers)

(def item-value value/item-value)
(def fetch-cost value/fetch-cost)
