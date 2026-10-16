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
             planner dangers; reach-first?, outruns-at?: a body's race to a refuge against its chasers (jobs.lib.cost.danger).
    health   hp-seconds, health-scale, drown-prices, damage-budget, survivable-budget: what an hp costs and how many a walk may spend (jobs.lib.cost.health).
    food     food-reserve: the food a body keeps carried, 3 days of it; cook-worth?, cook-gain: whether cooking raw food beats eating it now; meat-factor, meat-animal: raw meat's replacement cost, dear for hunted, 1 with a working pen (jobs.lib.cost.food).
    value    item-value, reserve-cost (using a reserved unit: value x s/(1-s)), fetch-cost, walk-cost, detour-cost: what items are worth, what fetching them, walking or a pick-up detour costs; tour-cost, tour-worth: a tour of
             frontiers against what its goal is worth; light-cost: a torch go-to places; dig-cost: the seconds a block takes to dig (jobs.lib.cost.value);
             per-dark, dark-factor: the one price of a dark block, which go-to's planner costs; spawn-chance: the chance a mob spawns at a cell or
             over an area (a set or {:min :max} box of cells) per second, from the light the body knows (unseen counts as dark); exposure: the chance a walk meets and
             what lowers it ({:chance :lowered-by #{:dawn}}).
    planner  planner-costs-problem, planner-costs, planner-names: go-to's :costs, the per-move prices it may override; landing-problem, planner-landing: go-to's :landing, fall damage by landing block; air-profile, air-used, air-costs: the planner's air drain and grace from the body's helmet, the air a dive has used; swim-profile: its swimH from Depth Strider on the boots; swim-factor: a swim block in walk blocks (a flight's water price); gait-problem, gait-costs: go-to's :gait; unknown-prices: the planner's prices of unseen cells (jobs.lib.cost.planner).
    tolls    farm-tolls, zone-tolls: go-to's :tolls for planted cells and zone cells to cross only as a last resort
             (jobs.lib.cost.tolls).
    base-rates  density, biome-chance, expected-density: the ore a block of ground is expected to hold, from the game's
             own placements, and the chance of a biome on ground the body has not seen (jobs.lib.cost.base-rates)."
  (:require [jobs.lib.cost.armour :as armour]
            [jobs.lib.cost.base-rates :as base-rates]
            [jobs.lib.cost.danger :as danger]
            [jobs.lib.cost.fight :as fight]
            [jobs.lib.cost.food :as food]
            [jobs.lib.cost.health :as health]
            [jobs.lib.cost.planner :as planner]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.cost.tolls :as tolls]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.shelter :as sh]
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
(def reach-first? danger/reach-first?)
(def outruns-at? danger/outruns-at?)
(def outruns? danger/outruns?)

(def hp-seconds health/hp-seconds)
(def health-scale health/health-scale)
(def drown-prices health/drown-prices)
(def damage-budget health/damage-budget)
(def survivable-budget health/survivable-budget)

(def planner-costs-problem planner/planner-costs-problem)
(def planner-costs planner/planner-costs)
(def air-profile planner/air-profile)
(def air-used planner/air-used)
(def swim-profile planner/swim-profile)
(def swim-factor planner/swim-factor)
(def air-costs planner/air-costs)
(def planner-names planner/planner-names)
(def landing-problem planner/landing-problem)
(def planner-landing planner/planner-landing)
(def gaits planner/gaits)
(def gait-problem planner/gait-problem)
(def gait-costs planner/gait-costs)
(def unknown-prices planner/unknown-prices)

(def item-value value/item-value)
(def fetch-cost value/fetch-cost)
(def reserve-cost value/reserve-cost)
(def walk-cost value/walk-cost)
(defn walk-blocks-per-s [] (value/walk-blocks-per-s))
(def dig-cost value/dig-cost)
(def detour-cost value/detour-cost)
(def tour-cost value/tour-cost)
(def tour-worth value/tour-worth)
(defn trip [] (value/trip))
(def light-cost value/light-cost)
(def per-danger value/per-danger)
(def spawn-chance value/spawn-chance)

(defn exposure
  "{:chance :lowered-by} of the walk of p from `from` to `to` ({:x :y :z}): the mob spawn chance per second it meets and
  what lowers it, data the readiness checks read (empty: nothing foreseeable does, so never). A walk that leaves shelter
  (sh/walk-leaves-shelter?, with the body's latest :shelter entry data when given) meets the open-sky night figure, any
  other the spawn-chance at `to` from the light p has seen; :dawn lowers a chance that night alone puts there. A cell p
  has no light data of is guessed dark and lowered by #{:look}, with :look-at [to]. Lava and the like are not spawn
  chance. A crude stub."
  ([p from to] (exposure p from to nil))
  ([p _from to shelter]
   (let [cell [(:x to) (:y to) (:z to)]
         seen (fn [c] (let [[x y z] c
                            at {:x x :y y :z z}]
                        (when-let [block (sh/block-light-at p at)] {:block block :sky (or (sh/sky-light-at p at) 0)})))
         open-sky (fn [_] {:block 0 :sky 15})
         leaves? (sh/walk-leaves-shelter? p shelter to 1)
         [light night?] (if leaves? [open-sky true] [seen (sh/night? p)])
         chance (spawn-chance cell {:light light :night? night?})]
     (if (and (not leaves?) (nil? (seen cell)))
       {:chance chance :lowered-by #{:look} :look-at [to]}
       {:chance chance
        :lowered-by (if (and night? (> chance (spawn-chance cell {:light light :night? false}))) #{:dawn} #{})}))))
(defn per-dark [] (value/per-dark))
(defn dark-factor ([] (value/dark-factor)) ([kit] (value/dark-factor kit)))

(def farm-tolls tolls/farm-tolls)
(def zone-tolls tolls/zone-tolls)

(def food-reserve food/food-reserve)
(def food-short food/food-short)
(def food-reserve-points food/reserve-points)
(def cook-worth? food/cook-worth?)
(def cook-gain food/cook-gain)
(def meat-factor food/meat-factor)
(def meat-animal food/meat-animal)

(def density base-rates/density)
(def biome-chance base-rates/biome-chance)
(def expected-density base-rates/expected-density)
