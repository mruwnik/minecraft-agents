(ns world-test.food-gets
  "tools/event-counts.mjs food-gets: counts the measured prior behind jobs.lib.cost.food ::field-point-s from the bodies'
  logs alone: each hungry reflex's get-food ending (any outcome, fruitless and cut ones too), its :wall-time and the
  hunger points and held saturation (held-saturation: capped at the food level) of the food its report yields (a raw food it cooked counted once, as the cooked), left out when the call
  took food from a chest (food.took-from-chest: home food, not the field's). :fill-s (seconds a point of hunger or saturation, what
  exhaustion drains) is cost.health ::fill-point-s. Pure parts: scan-line, report."
  (:require [jobs.lib.foods :as foods]))

(defn- field [line re] (second (re-find re line)))

(defn meals
  "The foods of yields {item count}, one name for each eaten: a raw food whose cooked food is among them counts once, as the cooked."
  [yields]
  (mapcat (fn [[n k]] (repeat (- k (min k (get yields (foods/cooked-of n) 0))) n))
          (filter (comp foods/food? key) yields)))

(defn yield-points
  "Hunger points (by default; or (of item), e.g. foods/saturation) of yields' meals."
  ([yields] (yield-points yields foods/points))
  ([yields of] (reduce + 0 (map of (meals yields)))))

(defn held-saturation
  "The saturation the meals of yields leave a body that starts hungry (foods/default-food) and eats the most points first: the bar
  stops at full-food and saturation at the food level, so what overflows is lost."
  [yields]
  (second (reduce (fn [[food sat] n]
                    (let [food (min foods/full-food (+ food (foods/points n)))]
                      [food (min food (+ sat (foods/saturation n)))]))
                  [foods/default-food 0]
                  (sort-by (comp - foods/points) (meals yields)))))

(defn scan-line
  "{:call [gen job] :chest? true} of a food.took-from-chest line, {:call :s :points} of a hungry get-food ending, else nil
  (job: the root job id)."
  [line]
  (let [call #(vector (field line #":generation-id \"([^\"]*)\"") (field line #":job-id \"([^\"/]*)"))
        yields (delay (into {} (for [[_ n k] (re-seq #"\"([a-z_]+)\" (\d+)" (or (field line #":yields \{:items \{([^}]*)\}") ""))]
                                 [n (js/parseInt k)])))]
    (cond
      (.includes line ":kind :food.took-from-chest") {:call (call) :chest? true}
      (and (.includes line ":kind :ended") (.includes line "hungry → jobs.survival.get-food: "))
      {:call (call)
       :s (js/parseFloat (or (field line #":wall-time ([\d.]+)") "0"))
       :points (yield-points @yields)
       :saturation (held-saturation @yields)})))

(defn report
  "{:n :s :points :point-s :saturation :fill-s} of the scanned rows' endings whose call took nothing from a chest, nil with none."
  [rows]
  (let [chest (set (map :call (filter :chest? rows)))
        ends (remove #(or (:chest? %) (chest (:call %))) rows)
        s (reduce + (map :s ends))
        points (reduce + (map :points ends))
        fill (+ points (reduce + (map :saturation ends)))
        per #(when (pos? %) (/ (js/Math.round (* 100 (/ s %))) 100))]
    (when (seq ends)
      {:n (count ends) :s (/ (js/Math.round (* 10 s)) 10) :points points :point-s (per points)
       :saturation (/ (js/Math.round (* 10 (- fill points))) 10) :fill-s (per fill)})))
