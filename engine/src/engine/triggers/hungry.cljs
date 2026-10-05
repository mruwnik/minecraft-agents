(ns engine.triggers.hungry
  "The hungry trigger, and the condition the get-food job shares with it."
  (:require [engine.foods :as foods]
            [engine.memory :as mem]))

(def default-food 6)

(def default-food-when-hurt 14)

(defn hungry?
  "Food is below :food (default 6 of 20), or below :food-when-hurt (default
  14) while health is below full: a hurt body heals only on a fed stomach."
  [food health args]
  (or (< food (:food args default-food))
      (and (< health 20) (< food (:food-when-hurt args default-food-when-hurt)))))

(def top-up-food
  "Natural regeneration needs food of at least this much (of 20)."
  18)

(def rare-food
  "Foods kept for emergencies; never eaten just to top up."
  foods/precious)

(defn top-up?
  "Health is below full, food is below top-up-food and a common food is
  carried (names: the carried item names): eat to regenerate like a player."
  [food health carried-names]
  (boolean (and (< health 20) (< food top-up-food)
                (some #(and (foods/edible? %) (not (rare-food %))) carried-names))))

(defn carried-names [self]
  (map #(.-name %) (array-seq (.-inventory self))))

(defn eaten-unnamed?
  "Whether a meal that names no item would eat item at health: food, not harmful, not named-only, and precious only
  at low health (the rules of jobs.survival.eat)."
  [health item]
  (and (foods/edible? item)
       (not (contains? foods/named-only item))
       (or (not (contains? foods/precious item)) (< health foods/low-health))))

(defn carries-food?
  "Whether the sensed self carries something a meal would eat."
  [self]
  (boolean (some #(eaten-unnamed? (.-health self) %) (carried-names self))))

(def bake-wheat "Carried wheat enough to bake a loaf." 3)

(defn wheat-carried [self]
  (reduce + 0 (keep #(when (= "wheat" (.-name %)) (.-count %)) (array-seq (.-inventory self)))))

(def default-rest-s
  "How long the trigger rests after get-food found nothing (its :hungry entry), unless food comes to hand; the same
  as get-food's :ask-cooldown-ms."
  600)

(defn resting?
  "Whether the trigger rests: get-food gave up (a :hungry entry, written with the food.none warn) less than :rest-s
  ago, and since then nothing to eat came to hand: no food carried, too little wheat to bake, no :food-source entry
  learned after the give-up."
  [self view args]
  (let [gave-up (:t (mem/latest view :hungry))
        learned (:t (mem/latest view :food-source))]
    (boolean (and gave-up
                  (< (- (:now view) gave-up) (* 1000 (:rest-s args default-rest-s)))
                  (not (carries-food? self))
                  (< (wheat-carried self) bake-wheat)
                  (not (and learned (> learned gave-up)))))))

(def hungry
  "Holds when hungry? says so, or top-up? does (a hurt body at food 14 to 17
  carrying food); args :food and :food-when-hurt. It rests (resting?) for :rest-s (600) after get-food found
  nothing, until food is carried, wheat to bake is, or a food source is learned: a body with nothing at hand is not
  sent searching again every cooldown. The food.none warn says so."
  {:name :hungry
   :when (fn [world memory args]
           (let [self (.self world)]
             (and (or (hungry? (.-food self) (.-health self) args)
                      (top-up? (.-food self) (.-health self) (carried-names self)))
                  (not (resting? self memory args)))))
   :job '(jobs.survival.get-food)
   :args {:food default-food :food-when-hurt default-food-when-hurt :rest-s default-rest-s}
   :persistence :cooldown
   :cooldown-s 90})
