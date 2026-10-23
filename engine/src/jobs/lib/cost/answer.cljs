(ns jobs.lib.cost.answer
  "The one price of each answer to hostiles, so the reflexes take the cheapest (decide). Pure.

  An answer is {:damage hp :hit hp :seconds s :then [answer] :caught {:chance :damage :hit}}: the hp it is expected to
  cost in blows of about :hit hp each, the seconds it takes, what may follow it (the cheapest of :then is paid too) and,
  for a walk away, the chance its mobs catch it and the blows they then land. answer-price: the hp at the health each
  leaves (health/hp-cost), the death the blows risk (death-chance x death/death-cost), the seconds, and the cheapest
  :then at full health.

  death-chance: the one chance of dying every route, spot, wait or answer is priced by. A known threat on a route costs
  the death of the answer the body would take to it (decide over fight, walk away and the covers it has).

  fight-answer: a fight with the kit (fight/fight-damage, fight-seconds). A lone creeper fought hit-and-back risks the
  blast of a late step back: its :hit is the blast at the creeper's swell start.
  flee-answer: a walk away from the mobs, nil when one of them keeps up (outruns?: the flight's speed is body-speed over
  ::outrun-factor). A melee mob hits while within its reach, a ranged one while the body is within its bow's range (both
  follow at their chase speed), a creeper that reaches its swell start before the body gets going
  (danger/flight-start-s) blows at the distance the body reached when its fuse ends. The mob is left alive:
  :then the fight with it, or the same walk once more. A stub until the priced flee search (card ee045b60).
  build-answer: building a refuge (seal or pillar) for some seconds: the blows of the melee mobs that arrive meanwhile.
  cover-answer: shutting the body in (walls, pit or pocket) by build-answer, then hidden for the quiet.

  Game facts kept as constants: a creeper's explosion power (3; damage falls off to none at twice it), its swell start
  (3 blocks) and fuse (1.5 s), a skeleton's bow range (15 blocks)."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.cost.armour :as armour]
            [jobs.lib.cost.danger :as danger]
            [jobs.lib.cost.death :as death]
            [jobs.lib.cost.fight :as fight]
            [jobs.lib.cost.health :as health]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.cost.weapon :as weapon]))

(a/defargs settings
  {::caught-chance {:default 0.15 :spec (a/num-in 0 1)
                    :doc "Chance a walk away is caught, its mobs landing their blows over the whole flight: the prior for a flight whose route is unknown, measured from the trial logs on 2026-10-23 (n=63)."}})

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

(defn blows-chance
  "The chance the blows of answer kill body: death/blow-chance of its :damage, or of the :caught blows for its :chance."
  [body {:keys [damage hit caught]}]
  (let [clean (death/blow-chance body damage hit)]
    (if caught
      (+ (* (- 1 (:chance caught)) clean) (* (:chance caught) (death/blow-chance body (:damage caught) (:hit caught))))
      clean)))

(defn expected-damage [{:keys [damage caught]}]
  (if caught (+ (* (- 1 (:chance caught)) damage) (* (:chance caught) (:damage caught))) damage))

(defn healed [body] (assoc body :health 20 :absorption 0))

(defn answer-price
  "Seconds answer costs body {:health :absorption, death-cost's :respawn :level :carried} at `at` (see the ns doc); :then
  is priced at full health: the body meets the mob again once it has healed."
  [{:keys [health] :as body} {:keys [seconds then] :as answer} at]
  (let [health (or health 20)
        own (+ (health/hp-cost health (min (expected-damage answer) health))
               (* (blows-chance body answer) (death/death-cost body at))
               seconds)]
    (if (seq then)
      (+ own (apply min (map #(answer-price (healed body) % at) then)))
      own)))

(defn answer-death-chance
  "The chance answer kills body: its blows (blows-chance), then the cheapest of its :then (answer-price at `at`) at full health."
  [body {:keys [then] :as answer} at]
  (let [own (blows-chance body answer)
        after (when (seq then) (apply min-key #(answer-price (healed body) % at) then))]
    (if after
      (- 1 (* (- 1 own) (- 1 (answer-death-chance (healed body) after at))))
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
  chase speed, default danger/chase-speed of the name)} at speed v through the armour stats, nil when it keeps up. A
  creeper that reaches its swell start within danger/flight-start-s (the body gains nothing yet) blows at the distance
  the body reached when its fuse ends: a blast is not felt before it lands, as other mobs' blows are (respond's felt
  rate)."
  [stats v {:keys [name distance follow speed]}]
  (let [d (or distance 0)
        chase (or speed (danger/chase-speed name))
        gain (- v chase)
        escape (when (pos? gain) (/ (max 0 (- (or follow d) d)) gain))]
    (cond
      (nil? escape) nil
      (= "creeper" name)
      (let [start (danger/flight-start-s)
            swell (/ (max 0 (- d swell-start)) chase)
            hurt (if (< swell start)
                   (blast-hurt stats (+ (min d swell-start) (* v (max 0 (- fuse-s (- start swell))))))
                   0)]
        {:damage hurt :hit hurt :seconds (+ start escape)})
      :else
      (let [r (if (contains? weapon/ranged-mobs name) bow-range (danger/melee-reach))]
        {:damage (threat/mob-hurt stats name (/ (max 0 (- r d)) gain) identity (threat/lone-hit-rate name))
         :hit (blow stats name) :seconds escape}))))

(defn flee-answer
  "The answer of a walk away from mobs [flee-mob's] by a body with food wearing equipment, the walk there and back (it
  goes back to its work), times 1 + returns (the flights from such mobs lately: each predicts another); :then the answer
  later (the fight with them, nil none) or the walk again. :caught: each flight is caught at chance caught (default
  ::caught-chance), its mobs then landing their blows (a creeper its blast) over the flight. A creeper that blows on the
  walk is spent: what follows is the walk from the rest or later (none left: nothing). nil when a mob keeps up or none is given."
  [{:keys [food equipment mobs returns later caught] :as args}]
  (let [stats (armour/armour-stats equipment)
        v (danger/flight-speed (or food 20))
        per (map #(flee-mob stats v %) mobs)
        times (inc (or returns 0))]
    (when (and (seq per) (every? some? per))
      (let [hit (reduce max 0 (map :hit per))
            walk {:damage (* times (reduce + (map :damage per)))
                  :hit hit
                  :seconds (* 2 times (reduce max 0 (map :seconds per)))
                  :caught {:chance (- 1 (js/Math.pow (- 1 (or caught (settings/get settings ::caught-chance))) times))
                           :damage (reduce + 0 (map (fn [m f] (if (= "creeper" (:name m))
                                                                  (blow stats "creeper")
                                                                  (threat/mob-hurt stats (:name m) (:seconds f) identity (threat/lone-hit-rate (:name m)))))
                                                    mobs per))
                           :hit hit}}]
        (let [spent (set (keep (fn [[m f]] (when (and (= "creeper" (:name m)) (pos? (:damage f))) m)) (map vector mobs per)))
              left (remove spent mobs)]
          (cond
            (empty? spent) (assoc walk :then (cond-> [walk] later (conj later)))
            (seq left) (assoc walk :then (cond-> [(flee-answer (assoc args :mobs left))] later (conj later)))
            :else walk))))))

(defn build-answer
  "The answer of building a refuge for seconds against mobs [flee-mob's] wearing equipment: the blows of each melee mob
  from its arrival (its chase speed), and the arrows of each shooter within its bow's range, to the end, at its measured
  hit rate; none from a creeper or an enderman."
  [{:keys [equipment mobs seconds]}]
  (let [stats (armour/armour-stats equipment)
        hitting (remove #(or (#{"creeper" "enderman"} (:name %))
                             (and (contains? weapon/ranged-mobs (:name %)) (> (or (:distance %) 0) bow-range)))
                        mobs)
        left (fn [{:keys [name distance speed]}]
               (if (contains? weapon/ranged-mobs name)
                 seconds
                 (max 0 (- seconds (/ (max 0 (- (or distance 0) (danger/melee-reach))) (or speed (danger/chase-speed name)))))))]
    {:damage (reduce + 0 (map #(threat/mob-hurt stats (:name %) (left %) identity (threat/lone-hit-rate (:name %))) hitting))
     :hit (reduce max 0 (map #(blow stats (:name %)) hitting))
     :seconds seconds}))

(defn cover-answer
  "The answer of shutting the body in against mobs [flee-mob's] wearing equipment by a cover {:open-s :seconds}
  (jobs.survival.shut-in/covers: the seconds a side stays open, the seconds it takes with the quiet after it): the blows
  of build-answer over open-s; then the same cover once more or one of then. nil with a creeper (its blast takes the walls)."
  [{:keys [equipment mobs then]} {:keys [open-s seconds]}]
  (when-not (some #(= "creeper" (:name %)) mobs)
    (let [a (assoc (build-answer {:equipment equipment :mobs mobs :seconds open-s}) :seconds seconds)]
      (assoc a :then (conj (vec then) a)))))

(defn threat-answers
  "The answers {key answer-or-nil} body {:health :food :equipment :weapon :covers} has to mob-name met beside it: the
  fight, the walk away (caught at chance caught, nil the prior) and each of :covers {way {:open-s :seconds}}
  (jobs.survival.shut-in/covers) by cover-answer, as respond prices them."
  [{:keys [food equipment weapon covers]} mob-name caught]
  (let [m {:name mob-name :distance 0 :follow (threat/follow-range mob-name)}
        fight (fight-answer {:weapon weapon :equipment equipment :mobs [m]})]
    (into {:fight fight
           :flee (flee-answer {:food food :equipment equipment :mobs [m] :caught caught :later fight})}
          (map (fn [[way cover]] [way (cover-answer {:equipment equipment :mobs [m]} cover)]))
          covers)))

(defn threat-chance
  "The chance mob-name met beside body kills it at `at`: the answer-death-chance of the answer decide takes of
  threat-answers; a provoked-only mob counts threat/provoked-share."
  [body mob-name caught at]
  (let [answers (threat-answers body mob-name caught)
        {:keys [answer]} (decide body answers at)]
    (* (if (contains? threat/provoked-only mob-name) (threat/provoked-share) 1)
       (answer-death-chance body (get answers answer) at))))

(defn route-hurt
  "hp the known threats of route {:cells :threats :at} (death-chance's) are expected to take off body: for each one whose
  reach the :cells enter, the expected-damage of the answer decide takes of threat-answers at :at (default the first
  cell), times its weight (a provoked-only mob threat/provoked-share more): death-chance in hp, for a walk priced against health."
  [body {:keys [cells threats at]}]
  (let [at (or at (first cells))
        hurt (memoize (fn [mob caught]
                        (let [answers (threat-answers body mob caught)]
                          (* (if (contains? threat/provoked-only mob) (threat/provoked-share) 1)
                             (expected-damage (get answers (:answer (decide body answers at))))))))]
    (reduce + 0 (for [{:keys [mob weight reach caught]} (when (seq cells) threats)
                      :when (pos? (death/exposure-s reach cells))]
                  (* (or weight 1) (hurt mob caught))))))

(defn threats-chance
  "The chance the known threats of route {:cells :threats :at} kill body (death-chance's, the route's class left out): for
  each one whose reach the :cells enter (death/exposure-s), its threat-chance at :at (default the first cell) times its
  weight."
  [body {:keys [cells threats at]}]
  (let [at (or at (first cells))
        chance (memoize (fn [mob caught] (threat-chance body mob caught at)))
        survive (reduce (fn [s {:keys [mob weight reach caught]}]
                          (if (pos? (death/exposure-s reach cells))
                            (* s (- 1 (* (or weight 1) (chance mob caught))))
                            s))
                        1 (when (seq cells) threats))]
    (- 1 survive)))

(defn death-chance
  "The chance of dying on route {:underground? :night? :cells :threats :at}: the chance by its class (underground, surface
  at night or by day: the mobs not yet known) and its threats-chance, for each known threat {:mob :weight :reach :caught}
  (weight 0-1, reach a fn of a cell x y z, its weight 0-1, nil outside: jobs.lib.threat-field; caught the chance a walk
  away from it is caught, nil the prior). body as threat-answers, with answer-price's. An answer {:damage :hit
  :caught}: its blows-chance."
  [body {:keys [underground? night? damage] :as route}]
  (if damage
    (blows-chance body route)
    (let [base (settings/get death/settings (cond underground? ::death/chance-underground night? ::death/chance-surface-night :else ::death/chance-surface-day))]
      (+ base (* (- 1 base) (threats-chance body route))))))

(defn spawns-chance
  "The chance spawns mobs (an expected count, a Poisson one) spawned beside body kill it at `at`: each its threat-chance,
  caught at the prior, over threat/spawn-weights."
  [body spawns at]
  (let [per (/ (reduce + (map (fn [[mob w]] (* w (threat-chance body mob nil at))) threat/spawn-weights))
               (reduce + (vals threat/spawn-weights)))]
    (- 1 (js/Math.exp (- (* spawns per))))))

(defn lethal-hp
  "The hp a death chance of body at `at` weighs beside the hp it risks: chance x death/death-cost, at health/hp-seconds an hp."
  [body chance at]
  (/ (* chance (death/death-cost body at)) health/hp-seconds))

(defn route-death-cost
  "What a route adds to its cost for the death it risks: death-chance x death/death-cost at `at` (where it would die)."
  [body route at]
  (* (death-chance body (assoc route :at at)) (death/death-cost body at)))
