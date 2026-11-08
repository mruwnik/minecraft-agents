(ns jobs.lib.cost.health
  "What an hp costs and how many a walk may spend, the one place. Pure.

  hp-seconds: what regaining a lost hp takes, the same at any health: the food it eats (Q142). A death is no hp: it is priced
  apart, in seconds, by its chance, so low health weighs through the death chance. hp-cost: the price of losing some hp; risk-s: a walk's risk in seconds.
  gain-worth: hp gained at hp-seconds each and the death they avert (Q140).
  damage-budget: the hp a walk may spend on certain damage (drops over 3 blocks, plants that hurt on touch): the health
  and absorption over the floor (:min-health, default 12) less a margin of 1, never more than survivable-budget, at most :max-damage; 0 while burning, poisoned or withered,
  or with a health or absorption that is not a number.
  survivable-budget: the hp a walk that must arrive may spend: all but 1 (the margin dropped), still under
  a caller's :max-damage, and just the damage-budget when the caller set a :min-health (a floor that is never crossed)."
  (:require [engine.args :as a]
            [engine.settings :as settings]))

(a/defargs settings
  {::default-min-health {:default 12 :doc "Health a walk does not spend below." :spec (a/int-in 0 nil)}
   ::margin {:default 1 :doc "hp left over the floor." :spec (a/int-in 0 nil)}
   ::fill-point-s {:default 7.17 :spec a/pos-num?
                   :doc "Seconds a point of food or saturation (what exhaustion drains) got in the field takes: 15444.7 s of 612 hungry get-food calls of the trial bodies that took nothing from a chest, cut and fruitless ones counted, over the 1106 points and 1047.2 saturation their food gave (a body hungry at 7 food eating the most points first: the bar stops at 20 and saturation at the food level), 2026-11-07. Recount: node tools/event-counts.mjs food-gets (:fill-s)."}
   ::heal-s-per-hp {:default 4 :spec (a/num-in 0 nil)
                    :doc "Seconds a body that can heal takes to regain an hp by waiting (natural regeneration at 18+ food: 80 ticks)."}})

(def exhaustion-per-hp "Exhaustion natural regeneration spends on an hp." 6)

(def exhaustion-per-point "Exhaustion that drains a point of saturation or food." 4)

(defn fill-point-s [] (settings/get settings ::fill-point-s))

(defn hp-seconds "Seconds an hp costs: the food that heals it again, exhaustion-per-hp at ::fill-point-s a point." []
  (* (/ exhaustion-per-hp exhaustion-per-point) (settings/get settings ::fill-point-s)))

(def spend-down
  "A call's :floor that lets go-to spend all but 1 hp (survivable-budget). 0, not 1: a body under 1 hp is below a floor of 1, so its call would fail at once."
  {:health 0})

(defn default-min-health [] (settings/get settings ::default-min-health))

(defn margin [] (settings/get settings ::margin))

(defn heal-s-per-hp [] (settings/get settings ::heal-s-per-hp))

(def landing-hp "hp a falling block that lands on the body costs: suffocation, 1 hp a half second, for the second a dig out takes." 2)

(defn hp-cost "Seconds losing damage hp costs: hp-seconds each, a part hp pro rata." [damage]
  (* (max 0 damage) (hp-seconds)))

(defn gain-worth
  "Seconds gaining `gain` (whole) hp from health is worth: each hp-seconds and the death it averts ((averted h), seconds, for
  the hp from h; nil: none), but while the body heals by waiting (heals?) at most what waiting costs an hp: heal-s-per-hp and
  its food (hp-seconds)."
  [health gain heals? averted]
  (let [cap (if heals? (+ (hp-seconds) (heal-s-per-hp)) js/Infinity)]
    (reduce + 0 (map #(min cap (+ (hp-seconds) (if averted (averted (+ health %)) 0))) (range gain)))))

(defn risk-s
  "Seconds a walk's risk costs a body at health: the hurt hp it expects at hp-cost (at most the health: past it is the
  death) and its death chance in hp (lethal hp, jobs.lib.cost/lethal-hp) at hp-seconds, the rate lethal-hp divided by."
  [health hurt lethal]
  (+ (hp-cost (min (max 0 hurt) health)) (* (hp-seconds) lethal)))

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
    ;; a floor and margin of 0 would budget a lethal fall, deeper than the planner's maxDrop (survivable-budget's)
    (min (max 0 (- (+ health (or absorption 0)) (max 1 (+ (or min-health (default-min-health)) (margin)))))
         (or max-damage js/Infinity))))

(defn survivable-budget
  "The hp a walk may spend and leave the body 1: see the ns doc. Same arguments as damage-budget."
  [{:keys [health absorption on-fire effects] :as body} {:keys [min-health max-damage] :as settings}]
  (cond
    min-health (damage-budget body settings)
    (or on-fire (some ticking-effects effects) (not (js/isFinite health)) (not (js/isFinite (or absorption 0)))) 0
    :else (min (max 0 (dec (+ health (or absorption 0)))) (or max-damage js/Infinity))))
