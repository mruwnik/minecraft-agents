(ns engine.triggers.hungry
  "The hungry trigger, and the condition the get-food job shares with it."
  (:require [jobs.survival.eat :as eat]))

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
  #{"golden_carrot" "golden_apple" "enchanted_golden_apple"})

(defn top-up?
  "Health is below full, food is below top-up-food and a common food is
  carried (names: the carried item names): eat to regenerate like a player."
  [food health carried-names]
  (boolean (and (< health 20) (< food top-up-food)
                (some #(and (eat/edible %) (not (rare-food %))) carried-names))))

(defn carried-names [self]
  (map #(.-name %) (array-seq (.-inventory self))))

(def hungry
  "Holds when hungry? says so, or top-up? does (a hurt body at food 14 to 17
  carrying food); args :food and :food-when-hurt."
  {:name :hungry
   :when (fn [world _memory args]
           (let [self (.self world)]
             (or (hungry? (.-food self) (.-health self) args)
                 (top-up? (.-food self) (.-health self) (carried-names self)))))
   :job '(jobs.survival.get-food)
   :args {:food default-food :food-when-hurt default-food-when-hurt}
   :persistence :cooldown
   :cooldown-s 90})
