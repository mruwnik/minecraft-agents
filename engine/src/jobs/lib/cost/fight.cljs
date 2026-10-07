(ns jobs.lib.cost.fight
  "The cost of a fight with the current kit (fight-damage) and what the hostile reflex makes of it (decide)."
  (:require [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.cost.armour :as armour]
            [jobs.lib.cost.threat :as threat]))

(def walk-speed "Blocks a second a body closes on a mob it walks up to." 4)

(defn fight-damage
  "The damage a body expects to take fighting mobs [{:name :distance :hits :health}] (nearest first) with weapon (item
  name, nil a fist) wearing equipment (jobs.lib.cost.armour).
  The mobs die one at a time, each after ceil(health left / weapon damage) swings at the weapon's attack gap. Every
  mob hurts the body (jobs.lib.cost.threat/mob-hurt, after armour) until it dies. A ranged mob also shoots while the
  body walks up to it (distance less 3, at walk-speed)."
  [{:keys [weapon equipment mobs]}]
  (let [stats (armour/armour-stats equipment)
        dmg (weapon/weapon-damage weapon)
        gap-s (/ (weapon/attack-gap-ms weapon) 1000)
        approach (fn [m] (if (contains? weapon/ranged-mobs (:name m)) (/ (max 0 (- (:distance m 0) 3)) walk-speed) 0))
        kill-s (fn [m] (* gap-s (js/Math.ceil (/ (max 0 (weapon/remaining-health (assoc m :damage dmg))) dmg))))
        ends (rest (reductions + 0 (map #(+ (approach %) (kill-s %)) mobs)))]
    (reduce + 0 (map (fn [m end] (threat/mob-hurt stats (:name m) end)) mobs ends))))

(defn decide
  "Pure: :flee from any creeper, else :fight when the expected damage leaves at least reserve health, else :flee."
  [{:keys [health damage creeper? reserve]}]
  (if (and (not creeper?) (<= damage (- health reserve)))
    :fight
    :flee))
