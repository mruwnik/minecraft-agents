(ns jobs.lib.cost.fight
  "The cost of a fight with the current kit (fight-damage, fight-seconds)."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.cost.armour :as armour]
            [jobs.lib.cost.threat :as threat]))

(def default-reserve "Health a fight must leave: the planner's stance and the cornered fight's." 4)

(def walk-speed "Blocks a second a body closes on a mob it walks up to." 4)

(a/defargs settings
  {::fist-gap-ms {:default 1300 :doc "Milliseconds between the hits a body with no sword or axe lands: the mob is knocked back and hops, so most of the time goes on walking up to it again." :spec (a/int-in 1 nil)}
   ::drowned-wet-hits-per-swing {:default 1.51 :spec (a/num-in 0 nil)
                                 :doc "Blows a drowned lands on a body that swims fighting it, per swing the kill needs (swings): the body's swings seldom reach it and knock it off, so the fight drags on. Fitted on 2026-10-23 to the trial bodies' first meets swum in and fought with a sword: the value under which their fights' death chances are likeliest to give the 3 deaths the drowned dealt of n=5 (tools/event-counts.mjs drowned-meets :wet-fight); a land fight's is ::hit-rate x the swing gap, about 0.38."}})

(defn swing-gap-ms
  "Milliseconds between the hits weapon lands: its attack gap, ::fist-gap-ms for anything that is no sword or axe."
  [weapon]
  (if (contains? weapon/weapon-damage-by-name weapon)
    (weapon/attack-gap-ms weapon)
    (settings/get settings ::fist-gap-ms)))

(defn swings
  "Hits weapon needs to kill mob m ({:name :hits :health})."
  [weapon m]
  (let [dmg (weapon/weapon-damage weapon)]
    (js/Math.ceil (/ (max 0 (weapon/remaining-health (assoc m :damage dmg))) dmg))))

(defn mob-ends
  "Seconds from now at which each of mobs dies, one at a time (see fight-damage)."
  [{:keys [weapon mobs]}]
  (let [gap-s (/ (swing-gap-ms weapon) 1000)
        approach (fn [m] (if (threat/ranged? m) (/ (max 0 (- (:distance m 0) 3)) walk-speed) 0))]
    (rest (reductions + 0 (map #(+ (approach %) (* gap-s (swings weapon %))) mobs)))))

(defn fight-seconds
  "Seconds the fight (as fight-damage's argument) takes until its last mob dies."
  [fight]
  (or (last (mob-ends fight)) 0))

(defn fight-damage
  "The damage a body expects to take fighting mobs [{:name :distance :hits :health}] (nearest first) with weapon (item
  name, nil a fist) wearing equipment (jobs.lib.cost.armour), swimming when wet?: a drowned there lands
  ::drowned-wet-hits-per-swing blows per swing its kill needs.
  The mobs die one at a time, each after ceil(health left / weapon damage) hits swing-gap-ms apart. Every
  mob hurts the body (jobs.lib.cost.threat/mob-hurt, after armour) until it dies. A ranged mob also shoots while the
  body walks up to it (distance less 3, at walk-speed). The measured hit rates come from lone sword fights: a group, or a body with no sword or axe, is hit once a second per mob. A creeper costs threat/creeper-exchange per swing of a sword or axe (hit-and-back), not
  its blast; with anything else (too slow to back off between swings) its blast."
  [{:keys [weapon equipment mobs wet?] :as fight}]
  (let [stats (armour/armour-stats equipment)
        swings (partial swings weapon)
        ends (mob-ends fight)
        armed? (contains? weapon/weapon-damage-by-name weapon)
        lone-sword? (and (= 1 (count mobs)) armed?)
        hurt (fn [m end] (cond (and armed? (= "creeper" (:name m)))
                               (* (swings m) (threat/creeper-exchange))
                               (and wet? (= "drowned" (threat/kind m)))
                               (threat/mob-hurt stats "drowned" (* (swings m) (settings/get settings ::drowned-wet-hits-per-swing)) identity 1)
                               :else
                               (threat/mob-hurt stats (threat/kind m) end identity (when lone-sword? (threat/lone-hit-rate (threat/kind m))))))]
    (reduce + 0 (map hurt mobs ends))))

(defn leaves-reserve?
  "Whether a fight costing damage leaves at least reserve of health."
  [{:keys [health damage reserve]}]
  (<= damage (- health reserve)))
