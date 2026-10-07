(ns jobs.lib.cost
  "The cost and estimate calculators, in one place. Pure: they take data (equipment, mobs, items, a kind-at lookup) and
  never read the world themselves. They compose: armour under mob threat, mob threat under fight cost, route danger and
  the planner's rate; route danger under fetch cost.

    armour   armour-stats, after-armour, equipment-of: vanilla's per-hit formula, the only one; fall-damage, fall-profile: a fall's hp and the drops a body takes
             (jobs.lib.cost.armour).
    threat   hostile?, mob-hurt: what one mob does over some seconds, after armour (jobs.lib.cost.threat).
    weapon   attack-gap-ms, weapon-damage, remaining-health, mob-max-health, ranged-mobs: the fight inputs; the one tool-material table
             (tool-tier, weapon-rank, cheapness) (jobs.lib.cost.weapon).
    fight    fight-damage, decide: the damage of a fight with the current kit, :fight or :flee (jobs.lib.cost.fight).
    danger   route-danger, straight-route: the danger of a walk past mobs; stance, danger-rate, danger-list: go-to's
             planner dangers (jobs.lib.cost.danger).
    health   hp-seconds, health-scale, damage-budget, survivable-budget: what an hp costs and how many a walk may spend (jobs.lib.cost.health).
    food     food-reserve: the food a body keeps carried, 3 days of it (jobs.lib.cost.food).
    value    item-value, fetch-cost, walk-cost: what items are worth, what fetching them or walking costs (jobs.lib.cost.value);
             per-dark, dark-factor: the one price of a dark block, which go-to's planner costs.
    tolls    farm-tolls, zone-tolls: go-to's :tolls for planted cells and zone cells to cross only as a last resort
             (jobs.lib.cost.tolls)."
  (:require [jobs.lib.cost.armour :as armour]
            [jobs.lib.cost.danger :as danger]
            [jobs.lib.cost.fight :as fight]
            [jobs.lib.cost.food :as food]
            [jobs.lib.cost.health :as health]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.cost.tolls :as tolls]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.cost.value :as value]))

(def equipment-of armour/equipment-of)
(def armour-stats armour/armour-stats)
(def after-armour armour/after-armour)
(def fall-damage armour/fall-damage)
(def fall-profile armour/fall-profile)

(def hostile? threat/hostile?)
(def mob-hurt threat/mob-hurt)

(def attack-gap-ms weapon/attack-gap-ms)
(def weapon-damage weapon/weapon-damage)
(def remaining-health weapon/remaining-health)
(def ranged-mobs weapon/ranged-mobs)
(def tool-tier weapon/tool-tier)
(def weapon-rank weapon/weapon-rank)
(def cheapness weapon/cheapness)

(def default-reserve fight/default-reserve)
(def fight-damage fight/fight-damage)
(def decide fight/decide)

(def route-danger danger/route-danger)
(def straight-route danger/straight-route)
(def stance danger/stance)
(def danger-rate danger/danger-rate)
(def danger-opts danger/danger-opts)
(def danger-shape-problem danger/danger-shape-problem)
(def danger-list danger/danger-list)
(def max-dangers danger/max-dangers)
(def danger-stances danger/stances)
(def danger-max-rate danger/max-rate)
(def danger-cap danger/danger-cap)
(def danger-default-shape danger/danger-shape)

(def hp-seconds health/hp-seconds)
(def health-scale health/health-scale)
(def damage-budget health/damage-budget)
(def survivable-budget health/survivable-budget)

(def item-value value/item-value)
(def fetch-cost value/fetch-cost)
(def walk-cost value/walk-cost)
(def per-danger value/per-danger)
(def per-dark value/per-dark)
(def dark-factor value/dark-factor)

(def farm-tolls tolls/farm-tolls)
(def zone-tolls tolls/zone-tolls)

(def food-reserve food/food-reserve)
(def food-short food/food-short)
(def food-reserve-points food/reserve-points)
