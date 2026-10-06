(ns jobs.lib.cost.food
  "The food a body keeps carried: three days of it. A body loses about 20 hunger points a day (a Minecraft day is
  20 minutes; sprinting, jumping, fighting and healing drain hunger, plain walking and mining hardly do; an
  estimate, not a measure, kept on the safe side), so the reserve is 60 points. Pure over an inventory
  ([{:name :count}]); food is what jobs.lib.foods calls edible, except precious food (golden apples), which is kept
  for emergencies and never counts."
  (:require [jobs.lib.foods :as foods]))

(def days 3)

(def points-per-day 20)

(def reserve-points (* days points-per-day))

(defn reserve-food?
  "Food that counts toward the reserve: edible and not precious."
  [name]
  (and (foods/edible? name) (not (contains? foods/precious name))))

(defn food-reserve
  "{name count}: the carried food to keep, best (most hunger points) first, until it holds reserve-points; the last
  name keeps only what is still needed."
  [inventory]
  (let [totals (reduce (fn [m {:keys [name count]}] (cond-> m (reserve-food? name) (update name (fnil + 0) count)))
                       {} inventory)]
    (loop [names (sort-by (juxt #(- (foods/points %)) identity) (keys totals))
           need reserve-points
           kept {}]
      (if (or (empty? names) (<= need 0))
        kept
        (let [n (first names)
              per (foods/points n)
              take-n (min (totals n) (js/Math.ceil (/ need (max per 1))))]
          (recur (rest names) (- need (* per take-n)) (assoc kept n take-n)))))))

(defn food-short
  "Hunger points the inventory lacks to hold the reserve (0 when it holds it)."
  [inventory]
  (let [held (reduce (fn [n {:keys [name count]}] (cond-> n (reserve-food? name) (+ (* count (foods/points name)))))
                     0 inventory)]
    (max 0 (- reserve-points held))))
