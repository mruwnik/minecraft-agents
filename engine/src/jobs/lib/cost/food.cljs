(ns jobs.lib.cost.food
  "The food a body keeps carried: three days of it. A busy body loses about 12 hunger points a day (a Minecraft day is
  20 minutes; walking, mining and fighting use 4 exhaustion for each point; an estimate, not a measure), so the
  reserve is 36 points. Pure over an inventory ([{:name :count}]); food is what jobs.lib.foods calls edible."
  (:require [jobs.lib.foods :as foods]))

(def days 3)

(def points-per-day 12)

(def reserve-points (* days points-per-day))

(defn food-reserve
  "{name count}: the carried food to keep, best (most hunger points) first, until it holds reserve-points; the last
  name keeps only what is still needed."
  [inventory]
  (let [totals (reduce (fn [m {:keys [name count]}] (cond-> m (foods/edible? name) (update name (fnil + 0) count)))
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
