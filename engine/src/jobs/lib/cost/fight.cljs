(ns jobs.lib.cost.fight
  "The cost of a fight with the current kit (fight-damage) and what the hostile reflex makes of it (decide)."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.cost.armour :as armour]
            [jobs.lib.cost.threat :as threat]))

(def default-reserve "Health a fight must leave: the hostile reflexes' :reserve default and the planner's stance." 4)

(def walk-speed "Blocks a second a body closes on a mob it walks up to." 4)

(a/defargs settings
  {::fist-gap-ms {:default 1300 :doc "Milliseconds between the hits a body with no sword or axe lands: the mob is knocked back and hops, so most of the time goes on walking up to it again." :spec (a/int-in 1 nil)}
   ::hysteresis-health {:default 8 :doc "Above this health a fight already begun against few mobs goes on though one hit made it dearer than reserve." :spec (a/num-in 0 20)}})

(defn swing-gap-ms
  "Milliseconds between the hits weapon lands: its attack gap, ::fist-gap-ms for anything that is no sword or axe."
  [weapon]
  (if (contains? weapon/weapon-damage-by-name weapon)
    (weapon/attack-gap-ms weapon)
    (settings/get settings ::fist-gap-ms)))

(defn fight-damage
  "The damage a body expects to take fighting mobs [{:name :distance :hits :health}] (nearest first) with weapon (item
  name, nil a fist) wearing equipment (jobs.lib.cost.armour).
  The mobs die one at a time, each after ceil(health left / weapon damage) hits swing-gap-ms apart. Every
  mob hurts the body (jobs.lib.cost.threat/mob-hurt, after armour) until it dies. A ranged mob also shoots while the
  body walks up to it (distance less 3, at walk-speed). A creeper costs threat/creeper-exchange per swing (hit-and-back), not its blast."
  [{:keys [weapon equipment mobs]}]
  (let [stats (armour/armour-stats equipment)
        dmg (weapon/weapon-damage weapon)
        gap-s (/ (swing-gap-ms weapon) 1000)
        approach (fn [m] (if (contains? weapon/ranged-mobs (:name m)) (/ (max 0 (- (:distance m 0) 3)) walk-speed) 0))
        swings (fn [m] (js/Math.ceil (/ (max 0 (weapon/remaining-health (assoc m :damage dmg))) dmg)))
        kill-s (fn [m] (* gap-s (swings m)))
        ends (rest (reductions + 0 (map #(+ (approach %) (kill-s %)) mobs)))
        hurt (fn [m end] (if (= "creeper" (:name m))
                           (* (swings m) (threat/creeper-exchange))
                           (threat/mob-hurt stats (:name m) end)))]
    (reduce + 0 (map hurt mobs ends))))

(defn leaves-reserve?
  "Whether a fight costing damage leaves at least reserve of health."
  [{:keys [health damage reserve]}]
  (<= damage (- health reserve)))

(defn decide
  "Pure but for its settings: :flee when creeper? (a creeper the body will not fight hit-and-back), else :fight when the expected damage leaves at least reserve
  health, or when a fight already begun (fighting?) has few mobs in it (few?) above ::hysteresis-health, or
  against a mob the body cannot outrun (cannot-outrun?: the flight would lose as well), else :flee."
  [{:keys [health damage creeper? reserve fighting? few? cannot-outrun?]}]
  (if (and (not creeper?)
           (or (leaves-reserve? {:health health :damage damage :reserve reserve})
               (and fighting? few? (> health (settings/get settings ::hysteresis-health)))
               (and fighting? cannot-outrun?)))
    :fight
    :flee))
