(ns engine.triggers.hungry
  "The hungry trigger, and the condition the get-food job shares with it.")

(def default-food 6)

(def default-food-when-hurt 14)

(defn hungry?
  "Food is below :food (default 6 of 20), or below :food-when-hurt (default
  14) while health is below full: a hurt body heals only on a fed stomach."
  [food health args]
  (or (< food (:food args default-food))
      (and (< health 20) (< food (:food-when-hurt args default-food-when-hurt)))))

(def hungry
  "Holds when hungry? says so; args :food and :food-when-hurt."
  {:name :hungry
   :when (fn [world _memory args]
           (let [self (.self world)]
             (hungry? (.-food self) (.-health self) args)))
   :job '(jobs.survival.get-food)
   :args {:food default-food :food-when-hurt default-food-when-hurt}
   :persistence :cooldown
   :cooldown-s 90})
