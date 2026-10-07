(ns jobs.lib.cost.health
  "What an hp costs and how many a walk may spend, the one place. Pure.

  hp-seconds: an hp is about 10 s: healing it takes 6 exhaustion, 60 blocks sprinted (0.1 each), 10 s at 5.6 blocks/s.
  health-scale: the price rises x20/health (at most 4) near zero health (the danger rate's scale too).
  drown-prices: the planner's drownPrices, the cumulative price of each hp of drowning at the health it leaves.
  damage-budget: the hp a walk may spend on certain damage (drops over 3 blocks, plants that hurt on touch): the health
  and absorption over the floor (:min-health, default 12) less a margin of 1, at most :max-damage; less when the food
  would put the hungry line (jobs.lib.foods/hungry?) over the body after the damage; 0 while burning, poisoned or withered,
  or with a health or absorption that is not a number.
  survivable-budget: the hp a walk that must arrive may spend: all but 1 (the food cap and the margin dropped), still under
  a caller's :max-damage, and just the damage-budget when the caller set a :min-health (a floor that is never crossed)."
  (:require [engine.settings :as settings]
            [jobs.lib.foods :as foods]))

(def settings
  {::default-min-health {:default 12 :doc "Health a walk does not spend below." :type :int :min 0}
   ::margin {:default 1 :doc "hp left over the floor." :type :int :min 0}
   ::max-scale {:default 4 :doc "The most the price of an hp rises near zero health." :type :int :min 1}})

(def hp-seconds "Seconds one hp costs at full health." 10)

(defn default-min-health [] (settings/get settings ::default-min-health))

(defn margin [] (settings/get settings ::margin))

(defn max-scale [] (settings/get settings ::max-scale))

(defn health-scale
  "How many times dearer an hp is at health than at 20, at most max-scale."
  [health]
  (min (max-scale) (/ 20 (max 1 health))))

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

(defn survivable-budget
  "The hp a walk may spend and leave the body 1: see the ns doc. Same arguments as damage-budget."
  [{:keys [health absorption on-fire effects] :as body} {:keys [min-health max-damage] :as settings}]
  (cond
    min-health (damage-budget body settings)
    (or on-fire (some ticking-effects effects) (not (js/isFinite health)) (not (js/isFinite (or absorption 0)))) 0
    :else (min (max 0 (dec (+ health (or absorption 0)))) (or max-damage js/Infinity))))
