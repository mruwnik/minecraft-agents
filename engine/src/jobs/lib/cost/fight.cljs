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
   ::drowned-wet-hits-per-swing {:default {:k 11.25 :n 12.47} :spec (a/map-with {:k (a/num-in 0 nil) :n (a/num-in 0 nil)})
                                 :doc "Blows a drowned lands on a body that swims fighting it, per swing the kill needs (swings): the body's swings seldom reach it and knock it off, so the kill takes 10 to 20 s where a land fight's takes 2 to 3 (a land fight's blows per swing are ::hit-rate x the swing gap, about 0.38). The blows are melee ones (2 to 2.5 hp; a trident drowned is its own kind). A Gamma posterior (Jeffreys prior): mean (k + 0.5) / n, shape k + 0.5, worth k blows over n swings. Fitted 2026-10-24 to the trial bodies' 5 first meets swum in and fought with a sword, 3 of them deaths the drowned dealt: the posterior over the per-meet death chances, mode 0.86, mean 0.94, 90% in 0.55 to 1.44, moment-matched (tools/event-counts.mjs drowned-meets :wet-fight)."}})

(defn wet-rate
  "{:mean :shape} of the blows a drowned lands per swing on a body that swims fighting it (::drowned-wet-hits-per-swing)."
  []
  (let [{:keys [k n]} (settings/get settings ::drowned-wet-hits-per-swing)]
    {:mean (/ (+ k 0.5) n) :shape (+ k 0.5)}))

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

(defn wet-drowned? [{:keys [wet?]} m] (and wet? (= "drowned" (threat/kind m))))

(defn mob-damages
  "The damage each of the fight's mobs is expected to deal (see fight-damage)."
  [{:keys [weapon equipment mobs] :as fight}]
  (let [stats (armour/armour-stats equipment)
        swings (partial swings weapon)
        armed? (contains? weapon/weapon-damage-by-name weapon)
        lone-sword? (and (= 1 (count mobs)) armed?)
        hurt (fn [m end] (cond (and armed? (= "creeper" (:name m)))
                               (* (swings m) (threat/creeper-exchange))
                               (wet-drowned? fight m)
                               (threat/mob-hurt stats "drowned" (* (swings m) (:mean (wet-rate))) identity 1)
                               :else
                               (threat/mob-hurt stats (threat/kind m) end identity (when lone-sword? (threat/lone-hit-rate (threat/kind m))))))]
    (map hurt mobs (mob-ends fight))))

(defn fight-damage
  "The damage a body expects to take fighting mobs [{:name :distance :hits :health}] (nearest first) with weapon (item
  name, nil a fist) wearing equipment (jobs.lib.cost.armour), swimming when wet?: a drowned there lands wet-rate's mean
  blows per swing its kill needs.
  The mobs die one at a time, each after ceil(health left / weapon damage) hits swing-gap-ms apart. Every
  mob hurts the body (jobs.lib.cost.threat/mob-hurt, after armour) until it dies. A ranged mob also shoots while the
  body walks up to it (distance less 3, at walk-speed). The measured hit rates come from lone sword fights: a group, or a body with no sword or axe, is hit once a second per mob. A creeper costs threat/creeper-exchange per swing of a sword or axe (hit-and-back), not
  its blast; with anything else (too slow to back off between swings) its blast."
  [fight]
  (reduce + 0 (mob-damages fight)))

(defn sum-shape
  "The shape of the negative binomial that matches the mean and variance of a sum of blow counts [[mean shape]] (jobs.lib.cost.death/blow-chance's) (shape
  nil: a Poisson one); nil when none has a shape."
  [parts]
  (let [extra (reduce + 0 (keep (fn [[m s]] (when s (/ (* m m) s))) parts))
        m (reduce + 0 (map first parts))]
    (when (pos? extra) (/ (* m m) extra))))

(defn blow-shape
  "The shape (sum-shape) of the fight's blow count, nil a Poisson one: a wet drowned's blows
  carry wet-rate's width, the other mobs' none."
  [fight]
  (let [parts (map (fn [m d] [d (when (wet-drowned? fight m) (:shape (wet-rate)))]) (:mobs fight) (mob-damages fight))]
    (sum-shape parts)))

(defn leaves-reserve?
  "Whether a fight costing damage leaves at least reserve of health."
  [{:keys [health damage reserve]}]
  (<= damage (- health reserve)))
