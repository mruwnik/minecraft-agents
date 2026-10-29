(ns jobs.lib.cost.health
  "What an hp costs and how many a walk may spend, the one place. Pure.

  hp-seconds: an hp is about 10 s: healing it takes 6 exhaustion, 60 blocks sprinted (0.1 each), 10 s at 5.6 blocks/s.
  health-scale: the price rises x20/health (at most 4) near zero health (the danger rate's scale too); hp-price: the two together; hp-cost: the price of losing some hp; gain-worth: the worth of gaining some; heal-worth?: whether one more hp is worth the wait and food it costs; risk-s: a walk's risk in seconds.
  drown-prices: the planner's drownPrices, the cumulative price of each hp of drowning at the health it leaves.
  damage-budget: the hp a walk may spend on certain damage (drops over 3 blocks, plants that hurt on touch): the health
  and absorption over the floor (:min-health, default 12) less a margin of 1, at most :max-damage; 0 while burning, poisoned or withered,
  or with a health or absorption that is not a number.
  survivable-budget: the hp a walk that must arrive may spend: all but 1 (the margin dropped), still under
  a caller's :max-damage, and just the damage-budget when the caller set a :min-health (a floor that is never crossed)."
  (:require [engine.args :as a]
            [engine.settings :as settings]))


(a/defargs settings
  {::default-min-health {:default 12 :doc "Health a walk does not spend below." :spec (a/int-in 0 nil)}
   ::margin {:default 1 :doc "hp left over the floor." :spec (a/int-in 0 nil)}
   ::hp-seconds {:default 10 :doc "Seconds one hp costs at full health when the planner weighs a drop, a plant's prick or a danger against a longer way (more at low health)." :spec a/pos-num?}
   ::max-scale {:default 4 :doc "The most the price of an hp rises near zero health." :spec (a/int-in 1 nil)}
   ::heal-s-per-hp {:default 4 :spec (a/num-in 0 nil)
                    :doc "Seconds a body that can heal takes to regain an hp by waiting (natural regeneration at 18+ food: 80 ticks)."}})

(defn walk-hp-seconds "Seconds an hp costs a walk at full health: the setting ::hp-seconds." []
  (settings/get settings ::hp-seconds))

(def spend-down
  "A call's :floor that lets go-to spend all but 1 hp (survivable-budget). 0, not 1: a body under 1 hp is below a floor of 1, so its call would fail at once."
  {:health 0})

(defn default-min-health [] (settings/get settings ::default-min-health))

(defn margin [] (settings/get settings ::margin))

(defn max-scale [] (settings/get settings ::max-scale))

(defn health-scale
  "How many times dearer an hp is at health than at 20, at most max-scale."
  [health]
  (min (max-scale) (/ 20 (max 1 health))))

(defn hp-price
  "Seconds one hp costs at health: walk-hp-seconds x health-scale."
  [health]
  (* (walk-hp-seconds) (health-scale health)))

(defn heal-worth?
  "Whether healing one more hp at health is worth its cost: the hp's hp-price is more than the heal-s seconds of waiting
  for it plus its food (6 exhaustion, the walk-hp-seconds an hp is priced at full health). Never at 20."
  [health heal-s]
  (and (< health 20) (> (hp-price health) (+ (walk-hp-seconds) heal-s))))

(defn heal-s-per-hp [] (settings/get settings ::heal-s-per-hp))

(defn hp-cost
  "Seconds losing damage hp from health costs: each hp at the hp-price of the health it leaves, a part hp pro rata."
  [health damage]
  (let [whole (js/Math.floor (max 0 damage))]
    (+ (reduce + 0 (map #(hp-price (- health %)) (range whole)))
       (* (- damage whole) (hp-price (- health whole))))))

(defn gain-worth
  "Seconds gaining `gain` (whole) hp from health is worth: each at the hp-price of the health it brings, but while the body
  heals by waiting (heals?) at most what waiting costs an hp: heal-s-per-hp and its food (walk-hp-seconds)."
  [health gain heals?]
  (let [cap (if heals? (+ (walk-hp-seconds) (heal-s-per-hp)) js/Infinity)]
    (reduce + 0 (map #(min cap (hp-price (+ health % 1))) (range gain)))))

(defn risk-s
  "Seconds a walk's risk costs a body at health: the hurt hp it expects at hp-cost (at most the health: past it is the
  death) and its death chance in hp (lethal hp, jobs.lib.cost/lethal-hp) at hp-seconds, the rate lethal-hp divided by."
  [health hurt lethal]
  (+ (hp-cost health (min (max 0 hurt) health)) (* (walk-hp-seconds) lethal)))

(defn commute-s
  "Seconds a walk of secs away costs over sheltering where the body stands, which takes shelter-s: for a body in the ground
  (in-ground?, its work goes on through the night) the walk there and back less that time; else 0 (the night stops the
  work either way)."
  [in-ground? secs shelter-s]
  (if in-ground? (- (* 2 secs) shelter-s) 0))

(defn night-lost-s
  "Seconds of work a way through a night with left seconds still to run costs a body busy at it busy-s seconds: for a body in the
  ground (in-ground?, its work goes on after) those seconds, at most the night; else the night left (the night stops the work either way)."
  [in-ground? left busy-s]
  (if in-ground? (min left busy-s) left))

(defn night-way-s
  "Seconds a way through a night with left seconds still to run costs: the work it loses keeping the body busy busy-s
  seconds (night-lost-s), risk-s (its tolls and hurt, no time), open-s (its seconds exposed, priced) and morning-s (what
  it costs by day: a shelter's blocks and dig out, the walk back to where the night found the body)."
  [in-ground? left {:keys [busy-s risk-s open-s morning-s] :or {busy-s 0 risk-s 0 open-s 0 morning-s 0}}]
  (+ (night-lost-s in-ground? left busy-s) risk-s open-s morning-s))

(defn night-walk-bound-s
  "The longest walk whose least night-way-s (busy its seconds and, in the ground, the same seconds back by day: commute-s's walk there
  and back) is within price; -1 when none is."
  [in-ground? left price]
  (cond
    (not in-ground?) (if (<= left price) ##Inf -1)
    (<= (/ price 2) left) (/ price 2)
    :else (- price left)))

(defn drown-prices
  "The seconds the first k hp of drowning cost a body with hp left, k from 0 to n (default: hp, rounded up): each hp at
  hp-seconds x health-scale of the health it is taken from, so the price rises as the hp runs out."
  ([hp hp-s] (drown-prices hp hp-s (js/Math.ceil hp)))
  ([hp hp-s n]
   (vec (reductions + 0 (map #(* hp-s (health-scale (- hp %))) (range n))))))

(def ticking-effects "Effects that keep costing hp." #{"poison" "wither"})

(defn ticking-hp
  "The hp the poison and wither of effects ({:name :amplifier :duration} in ticks) take off a body at health if waited out: one hp
  per 25 ticks of poison (halved each level) down to 1 hp, per 40 of wither (halved each level) down to none."
  [effects health]
  (let [lost (fn [name tick] (transduce (comp (filter #(= name (:name %)))
                                              (map #(/ (:duration %) (max 1 (/ tick (js/Math.pow 2 (:amplifier % 0)))))))
                                        + 0 effects))]
    (min health (+ (min (lost "poison" 25) (max 0 (dec health))) (lost "wither" 40)))))

(defn damage-budget
  "The hp of certain damage a walk may take: see the ns doc. body {:health :absorption :effects (names) :on-fire}
  and settings {:min-health :max-damage} (nil: the defaults)."
  [{:keys [health absorption effects on-fire]} {:keys [min-health max-damage]}]
  (if (or on-fire (some ticking-effects effects) (not (js/isFinite health)) (not (js/isFinite (or absorption 0))))
    0
    (min (max 0 (- (+ health (or absorption 0)) (or min-health (default-min-health)) (margin)))
         (or max-damage js/Infinity))))

(defn survivable-budget
  "The hp a walk may spend and leave the body 1: see the ns doc. Same arguments as damage-budget."
  [{:keys [health absorption on-fire effects] :as body} {:keys [min-health max-damage] :as settings}]
  (cond
    min-health (damage-budget body settings)
    (or on-fire (some ticking-effects effects) (not (js/isFinite health)) (not (js/isFinite (or absorption 0)))) 0
    :else (min (max 0 (dec (+ health (or absorption 0)))) (or max-damage js/Infinity))))
