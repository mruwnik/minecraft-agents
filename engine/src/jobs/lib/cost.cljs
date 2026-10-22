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
    value    item-value, reserve-cost (using a reserved unit: value x s/(1-s)), fetch-cost, walk-cost, detour-cost: what items are worth, what fetching them, walking or a pick-up detour costs; tour-cost, tour-worth, food-worth: a tour of
             frontiers against what its goal is worth; light-cost: a torch go-to places; dig-cost: the seconds a block takes to dig (jobs.lib.cost.value);
             per-dark, dark-factor: the one price of a dark block, which go-to's planner costs; spawn-chance: the chance a mob spawns at a cell or
             over an area (a set or {:min :max} box of cells) per second, from the light the body knows (unseen counts as dark); exposure: the chance a walk meets and
             what lowers it ({:chance :lowered-by #{:light}}).
    planner  planner-costs-problem, planner-costs, planner-names: go-to's :costs, the per-move prices it may override; landing-problem, planner-landing: go-to's :landing, fall damage by landing block; air-profile, water-breathing-s, air-used, air-costs: the planner's air drain and grace from the body's helmet, the air a dive has used; swim-profile: its swimH from Depth Strider on the boots; swim-factor: a swim block in walk blocks (a flight's water price); gait-problem, gait-costs: go-to's :gait; unknown-prices: the planner's prices of unseen cells (jobs.lib.cost.planner).
    tolls    farm-tolls, zone-tolls: go-to's :tolls for planted cells and zone cells to cross only as a last resort
             (jobs.lib.cost.tolls).
    death    experience-cost, death-cost, death-chance, route-death-cost: death as an ordinary cost, the walk back for the items plus the experience; a route pays its chance of dying x that (jobs.lib.cost.death).
    base-rates  density, biome-chance, expected-density, peak-density: the ore a block of ground is expected to hold, from the game's
             own placements, and the chance of a biome on ground the body has not seen (jobs.lib.cost.base-rates)."
  (:require [jobs.lib.cost.armour :as armour]
            [jobs.lib.cost.base-rates :as base-rates]
            [jobs.lib.cost.danger :as danger]
            [jobs.lib.cost.death :as death]
            [jobs.lib.cost.fight :as fight]
            [jobs.lib.cost.food :as food]
            [jobs.lib.cost.health :as health]
            [jobs.lib.cost.planner :as planner]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.cost.tolls :as tolls]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.cost.table :as table]
            [jobs.lib.cost.value :as value]
            [jobs.lib.look :as look]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]))

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
(def leaves-reserve? fight/leaves-reserve?)

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
(def mob-kind danger/mob-kind)

(def hp-seconds health/hp-seconds)
(def walk-hp-seconds health/walk-hp-seconds)
(def health-scale health/health-scale)
(def drown-prices health/drown-prices)
(def damage-budget health/damage-budget)
(def survivable-budget health/survivable-budget)

(def planner-costs-problem planner/planner-costs-problem)
(def planner-costs planner/planner-costs)
(def air-profile planner/air-profile)
(def water-breathing-s planner/water-breathing-s)
(def air-used planner/air-used)
(def swim-profile planner/swim-profile)
(def swim-factor planner/swim-factor)
(def air-costs planner/air-costs)
(def planner-names planner/planner-names)
(def landing-problem planner/landing-problem)
(def planner-landing planner/planner-landing)
(def walk-costs planner/walk-costs)
(def walk-landing planner/walk-landing)
(def walk-drop-cost planner/walk-drop-cost)
(def walk-unknown-prices planner/walk-unknown-prices)
(def gaits planner/gaits)
(def gait-problem planner/gait-problem)
(def gait-costs planner/gait-costs)
(def unknown-prices planner/unknown-prices)

(def item-value value/item-value)
(def fetch-cost value/fetch-cost)
(def reserve-cost value/reserve-cost)
(def walk-cost value/walk-cost)
(defn walk-blocks-per-s [] (value/walk-blocks-per-s))
(def dig-cost table/dig-cost)
(def detour-cost value/detour-cost)
(def tour-cost value/tour-cost)
(def tour-worth value/tour-worth)
(def food-worth value/food-worth)
(defn trip [] (value/trip))
(def light-cost value/light-cost)
(def per-danger value/per-danger)
(def spawn-chance value/spawn-chance)

(defn near-body?
  "Whether the cell `to` lies within sh-inside/spawn-free-radius of the body's feet: no mob spawns that near a player, so dark there is no danger to go to or work at."
  [p to]
  (let [feet (sh-sense/feet p)]
    (<= (u/dist feet (update to :y #(or % (:y feet)))) sh-inside/spawn-free-radius)))

(defn exposure
  "{:chance :lowered-by} of the walk of p to `to` ({:x :y :z}, a column {:x :z} at the feet's height; floored): the mob spawn chance per second it meets and
  what lowers it (with :light-in, the seconds until light, when :light does), data the readiness checks read (empty: nothing foreseeable does, so never). A place outside the shelter
  (sh-inside/walk-leaves-shelter?, with the body's latest :shelter entry data when given) that p has no light data of is guessed under open sky; any
  other the spawn-chance at `to` from the light p has seen; :light lowers a chance that darkness alone puts there and light-in says waiting lifts, but only
  at a cell the sky reaches down its column (under leaves too): a roofed dark cell (a stair, a cave) is no exposure on its own, only threats count. A cell p
  has no light data of is guessed dark and lowered by #{:look}, with :look-at [[x y z]]. A solid `to` is judged by the first seen open cell over it (the air over a pile, not the unseen logs between), else the cell over it (a solid cell has no light that night changes). Lava and the like are not spawn
  chance. A dark cell has none when no dark spawnable cell beyond the spawn-free radius of it is reachable on foot (sh-inside/walled-off-at?: a closed seen region without a way in). A crude stub."
  ([p to] (exposure p to nil))
  ([p to shelter]
   (let [to (update-vals (assoc to :y (or (:y to) (:y (sh-sense/feet p)))) js/Math.floor)
         solid? (sh-sense/solid-at? p to)
         over (fn [y] (assoc to :y y))
         open-over (when solid? (first (filter #(and (sh-sense/seen-cell? p (over %)) (not (sh-sense/solid-at? p (over %)))) (range (inc (:y to)) (+ (:y to) 32)))))
         cell [(:x to) (or open-over (if solid? (inc (:y to)) (:y to))) (:z to)]
         dark (look/dark-light)
         made-out? (fn [at] (or (sh-sense/seen-cell? p at) (look/glimpse? p at)))
         block-of (fn [c] (let [[x y z] c
                                at {:x x :y y :z z}]
                            (when (made-out? at) (sh-sense/block-light-at p at))))
         raw-light (fn [c] (when-let [block (block-of c)] {:block block :sky (or (sh-sense/sky-light-at p (let [[x y z] c] {:x x :y y :z z})) 0)}))
         open-sky (fn [c] {:block (or (block-of c) 0) :sky 15})
         lit-by (fn [raw darkening] (fn [c] (when-let [{:keys [block sky]} (raw c)] {:block block :sky (if (< (- sky darkening) dark) 0 15)})))
         leaves? (sh-inside/walk-leaves-shelter? p shelter to 1)
         raw (if leaves? (fn [c] (or (raw-light c) (open-sky c))) raw-light)
         darkening (sh-sense/sky-subtract-now p)
         at {:x (nth cell 0) :y (nth cell 1) :z (nth cell 2)}
         dark-chance (spawn-chance cell {:light (lit-by raw darkening)})
         chance (if (and (pos? dark-chance) (not leaves?) (raw cell) (sh-inside/walled-off-at? p at)) 0 dark-chance)]
     (if (and (not leaves?) (nil? (raw cell)))
       {:chance chance :lowered-by #{:look} :look-at [cell]}
       (let [open-sky? (or leaves? (sh-sense/sky-reaches-from-above? p {:x (nth cell 0) :y (nth cell 1) :z (nth cell 2)}))
             wait (when (and open-sky? (> chance (spawn-chance cell {:light (lit-by raw 0)})))
                    (if leaves? (sh-sense/sky-in p) (sh-sense/light-in p {:x (nth cell 0) :y (nth cell 1) :z (nth cell 2)})))]
         (if (and wait (pos? wait))
           {:chance chance :lowered-by #{:light} :light-in wait}
           {:chance chance :lowered-by #{}}))))))
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

(def experience-cost death/experience-cost)
(def death-cost death/death-cost)
(def death-chance death/death-chance)
(def route-death-cost death/route-death-cost)

(def density base-rates/density)
(def biome-chance base-rates/biome-chance)
(def expected-density base-rates/expected-density)
(def peak-density base-rates/peak-density)
