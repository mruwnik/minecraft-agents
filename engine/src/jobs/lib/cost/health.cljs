(ns jobs.lib.cost.health
  "What an hp costs and how many a walk may spend, the one place. Pure.

  hp-seconds: an hp is about 10 s: healing it takes 6 exhaustion, 60 blocks sprinted (0.1 each), 10 s at 5.6 blocks/s.
  health-scale: the price rises x20/health (at most 4) near zero health (the danger rate's scale too); hp-price: the two together.
  drown-prices: the planner's drownPrices, the cumulative price of each hp of drowning at the health it leaves.
  damage-budget: the hp a walk may spend on certain damage (drops over 3 blocks, plants that hurt on touch): the health
  and absorption over the floor (:min-health, default 12) less a margin of 1, at most :max-damage; less when the food
  would put the hungry line (jobs.lib.foods/hungry?) over the body after the damage; 0 while burning, poisoned or withered,
  or with a health or absorption that is not a number.
  survivable-budget: the hp a walk that must arrive may spend: all but 1 (the food cap and the margin dropped), still under
  a caller's :max-damage, and just the damage-budget when the caller set a :min-health (a floor that is never crossed)."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.foods :as foods]))

(a/defargs settings
  {::default-min-health {:default 12 :doc "Health a walk does not spend below." :spec (a/int-in 0 nil)}
   ::margin {:default 1 :doc "hp left over the floor." :spec (a/int-in 0 nil)}
   ::hp-seconds {:default 10 :doc "Seconds one hp costs at full health when the planner weighs a drop, a plant's prick or a danger against a longer way (more at low health)." :spec a/pos-num?}
   ::max-scale {:default 4 :doc "The most the price of an hp rises near zero health." :spec (a/int-in 1 nil)}})

(def hp-seconds "Seconds one hp costs at full health." 10)

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

(defn commute-s
  "Seconds a walk of secs away costs over sheltering where the body stands, which takes shelter-s: for a body in the ground
  (in-ground?, its work goes on through the night) the walk there and back less that time; else 0 (the night stops the
  work either way)."
  [in-ground? secs shelter-s]
  (if in-ground? (- (* 2 secs) shelter-s) 0))

(defn drown-prices
  "The seconds the first k hp of drowning cost a body with hp left, k from 0 to n (default: hp, rounded up): each hp at
  hp-seconds x health-scale of the health it is taken from, so the price rises as the hp runs out."
  ([hp hp-s] (drown-prices hp hp-s (js/Math.ceil hp)))
  ([hp hp-s n]
   (vec (reductions + 0 (map #(* hp-s (health-scale (- hp %))) (range n))))))

(def ticking-effects "Effects that keep costing hp." #{"poison" "wither"})

(defn food-cap
  "The most hp a body with food and health can lose and still not be hungry, less the margin; nil when the food is
  at or over top-up-food (the line stops rising there)."
  [food health]
  (when (< food foods/top-up-food)
    (let [fine (count (take-while #(not (foods/hungry? food (- health %) {})) (range 0 21)))]
      (max 0 (- fine 1 (margin))))))

(defn damage-budget
  "The hp of certain damage a walk may take: see the ns doc. body {:health :absorption :food :effects (names) :on-fire}
  and settings {:min-health :max-damage} (nil: the defaults)."
  [{:keys [health absorption food effects on-fire]} {:keys [min-health max-damage]}]
  (if (or on-fire (some ticking-effects effects) (not (js/isFinite health)) (not (js/isFinite (or absorption 0))))
    0
    (let [room (max 0 (- (+ health (or absorption 0)) (or min-health (default-min-health)) (margin)))
          cap (when food (food-cap food health))]
      (cond-> (min room (or max-damage js/Infinity))
        cap (min cap)))))

(defn food-limited?
  "Whether the hunger cap (food-cap), not the health floor or :max-damage, sets damage-budget for this body and settings."
  [{:keys [health absorption food]} {:keys [min-health max-damage]}]
  (let [cap (when food (food-cap food health))
        room (max 0 (- (+ health (or absorption 0)) (or min-health (default-min-health)) (margin)))]
    (boolean (and cap (< cap room) (< cap (or max-damage js/Infinity))))))

(defn survivable-budget
  "The hp a walk may spend and leave the body 1: see the ns doc. Same arguments as damage-budget."
  [{:keys [health absorption on-fire effects] :as body} {:keys [min-health max-damage] :as settings}]
  (cond
    min-health (damage-budget body settings)
    (or on-fire (some ticking-effects effects) (not (js/isFinite health)) (not (js/isFinite (or absorption 0)))) 0
    :else (min (max 0 (dec (+ health (or absorption 0)))) (or max-damage js/Infinity))))
