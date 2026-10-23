(ns jobs.lib.cost.answer
  "The one price of each answer to hostiles, so the reflexes take the cheapest (decide). Pure.

  An answer is {:damage hp :hit hp :seconds s :then [answer]}: the hp it is expected to cost in blows of about :hit hp
  each, the seconds it takes, and what may follow it (the cheapest of :then is paid too). answer-price: the hp at the
  health each leaves (health/hp-cost), the death the blows risk (death/death-chance x death-cost), the seconds, and the
  cheapest :then at full health.

  fight-answer: a fight with the kit (fight/fight-damage, fight-seconds). A lone creeper fought hit-and-back risks the
  blast of a late step back: its :hit is the blast at the creeper's swell start.
  flee-answer: a walk away from the mobs, nil when one of them keeps up (outruns?: the flight's speed is body-speed over
  ::outrun-factor). A melee mob hits while within its reach, a ranged one while the body is within its bow's range (both
  follow at their chase speed), a
  creeper within its swell start blows at the distance the body reached when its fuse ends. The mob is left alive:
  :then the fight with it, or the same walk once more. A stub until the priced flee search (card ee045b60).
  build-answer: building a refuge (seal or pillar) for some seconds: the blows of the melee mobs that arrive meanwhile.

  Game facts kept as constants: a creeper's explosion power (3; damage falls off to none at twice it), its swell start
  (3 blocks) and fuse (1.5 s), a skeleton's bow range (15 blocks)."
  (:require [jobs.lib.cost.armour :as armour]
            [jobs.lib.cost.danger :as danger]
            [jobs.lib.cost.death :as death]
            [jobs.lib.cost.fight :as fight]
            [jobs.lib.cost.health :as health]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.cost.weapon :as weapon]))

(def blast-power "A creeper's explosion power (vanilla)." 3)
(def swell-start "Blocks within which a creeper starts to swell (vanilla)." 3)
(def fuse-s "Seconds from a creeper's first swell to its blast (vanilla: 30 ticks)." 1.5)
(def bow-range "Blocks within which a skeleton shoots (vanilla)." 15)

(defn blast-damage
  "hp a creeper's blast deals a body distance blocks from it, before armour (vanilla, full exposure: none at twice the power), at the server difficulty."
  [distance]
  (let [impact (- 1 (/ distance (* 2 blast-power)))]
    (if (pos? impact)
      (threat/difficulty-scale (js/Math.floor (+ 1 (* (/ (+ (* impact impact) impact) 2) 7 2 blast-power))))
      0)))

(defn blast-hurt
  "blast-damage at distance after the armour stats."
  [stats distance]
  (armour/after-armour stats :explosion (blast-damage distance) 1))

(defn blow
  "hp one blow of mob-name deals through the armour stats: a creeper's is its blast at its swell start."
  [stats mob-name]
  (if (= "creeper" mob-name)
    (blast-hurt stats swell-start)
    (threat/mob-hurt stats mob-name 1 identity 1)))

(defn answer-price
  "Seconds answer costs body {:health :absorption, death-cost's :respawn :level :carried} at `at` (see the ns doc); :then
  is priced at full health: the body meets the mob again once it has healed."
  [{:keys [health] :as body} {:keys [damage hit seconds then]} at]
  (let [health (or health 20)
        own (+ (health/hp-cost health (min damage health))
               (* (death/death-chance body {:damage damage :hit hit}) (death/death-cost body at))
               seconds)]
    (if (seq then)
      (+ own (apply min (map #(answer-price (assoc body :health 20 :absorption 0) % at) then)))
      own)))

(defn decide
  "{:answer key :prices {key seconds}}: the cheapest of answers {key answer-or-nil} (nil: not open to the body) by
  answer-price; nil when none is open."
  [body answers at]
  (let [prices (into {} (keep (fn [[k a]] (when a [k (answer-price body a at)]))) answers)]
    (when (seq prices)
      {:answer (key (first (sort-by val prices))) :prices prices})))

(defn fight-answer
  "The answer of fight (fight/fight-damage's argument): its damage and seconds, :hit the hardest blow of its mobs."
  [{:keys [equipment mobs] :as fight}]
  (let [stats (armour/armour-stats equipment)]
    {:damage (fight/fight-damage fight)
     :hit (reduce max 0 (map #(blow stats (:name %)) mobs))
     :seconds (fight/fight-seconds fight)}))

(defn flee-mob
  "{:damage :hit :seconds} of a walk away from mob {:name :distance :follow (blocks it keeps chasing out to) :speed (its
  chase speed, default danger/chase-speed of the name)} at speed v through the armour stats, nil when it keeps up."
  [stats v {:keys [name distance follow speed]}]
  (let [d (or distance 0)
        chase (or speed (danger/chase-speed name))
        gain (- v chase)
        escape (when (pos? gain) (/ (max 0 (- (or follow d) d)) gain))]
    (cond
      (nil? escape) nil
      (= "creeper" name)
      (let [hurt (if (<= d swell-start) (blast-hurt stats (+ d (* v fuse-s))) 0)]
        {:damage hurt :hit hurt :seconds escape})
      (contains? weapon/ranged-mobs name)
      {:damage (threat/mob-hurt stats name (/ (max 0 (- bow-range d)) gain) identity (threat/lone-hit-rate name))
       :hit (blow stats name) :seconds escape}
      :else
      {:damage (threat/mob-hurt stats name (/ (max 0 (- (danger/melee-reach) d)) gain) identity (threat/lone-hit-rate name))
       :hit (blow stats name) :seconds escape})))

(defn flee-answer
  "The answer of a walk away from mobs [flee-mob's] by a body with food wearing equipment, the walk there and back (it
  goes back to its work), times 1 + returns (the flights from such mobs lately: each predicts another); :then the answer
  later (the fight with them, nil none) or the walk again. nil when a mob keeps up or none is given."
  [{:keys [food equipment mobs returns later]}]
  (let [stats (armour/armour-stats equipment)
        v (danger/flight-speed (or food 20))
        per (map #(flee-mob stats v %) mobs)
        times (inc (or returns 0))]
    (when (and (seq per) (every? some? per))
      (let [walk {:damage (* times (reduce + (map :damage per)))
                  :hit (reduce max 0 (map :hit per))
                  :seconds (* 2 times (reduce max 0 (map :seconds per)))}]
        (assoc walk :then (cond-> [walk] later (conj later)))))))

(defn build-answer
  "The answer of building a refuge for seconds against mobs [flee-mob's] wearing equipment: the blows of each melee mob
  (no creeper, enderman or shooter) from its arrival (its chase speed) to the end, at its measured hit rate."
  [{:keys [equipment mobs seconds]}]
  (let [stats (armour/armour-stats equipment)
        melee (remove #(or (#{"creeper" "enderman"} (:name %)) (contains? weapon/ranged-mobs (:name %))) mobs)
        left (fn [{:keys [name distance speed]}]
               (max 0 (- seconds (/ (max 0 (- (or distance 0) (danger/melee-reach))) (or speed (danger/chase-speed name))))))]
    {:damage (reduce + 0 (map #(threat/mob-hurt stats (:name %) (left %) identity (threat/lone-hit-rate (:name %))) melee))
     :hit (reduce max 0 (map #(blow stats (:name %)) melee))
     :seconds seconds}))
