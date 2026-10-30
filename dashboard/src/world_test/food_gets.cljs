(ns world-test.food-gets
  "tools/event-counts.mjs food-gets: counts the measured prior behind jobs.lib.cost.food ::field-point-s from the bodies'
  logs alone: each hungry reflex's get-food ending (any outcome, fruitless and cut ones too), its :wall-time and the
  hunger points of the food its report yields (a raw food it cooked counted once, as the cooked), left out when the call
  took food from a chest (food.took-from-chest: home food, not the field's). Pure parts: scan-line, report."
  (:require [jobs.lib.foods :as foods]))

(defn- field [line re] (second (re-find re line)))

(defn yield-points
  "Hunger points of yields {item count}: each food at its points, a raw food whose cooked food is among them once, as the cooked."
  [yields]
  (- (reduce + 0 (for [[n k] yields :when (foods/food? n)] (* k (foods/points n))))
     (reduce + 0 (for [[raw cooked] foods/cooked-of :when (and (yields raw) (yields cooked))]
                   (* (min (yields raw) (yields cooked)) (foods/points raw))))))

(defn scan-line
  "{:call [gen job] :chest? true} of a food.took-from-chest line, {:call :s :points} of a hungry get-food ending, else nil
  (job: the root job id)."
  [line]
  (let [call #(vector (field line #":generation-id \"([^\"]*)\"") (field line #":job-id \"([^\"/]*)"))]
    (cond
      (.includes line ":kind :food.took-from-chest") {:call (call) :chest? true}
      (and (.includes line ":kind :ended") (.includes line "hungry → jobs.survival.get-food: "))
      {:call (call)
       :s (js/parseFloat (or (field line #":wall-time ([\d.]+)") "0"))
       :points (yield-points (into {} (for [[_ n k] (re-seq #"\"([a-z_]+)\" (\d+)" (or (field line #":yields \{:items \{([^}]*)\}") ""))]
                                        [n (js/parseInt k)])))})))

(defn report
  "{:n :s :points :point-s} of the scanned rows' endings whose call took nothing from a chest, nil with none."
  [rows]
  (let [chest (set (map :call (filter :chest? rows)))
        ends (remove #(or (:chest? %) (chest (:call %))) rows)
        s (reduce + (map :s ends))
        points (reduce + (map :points ends))]
    (when (seq ends)
      {:n (count ends) :s (/ (js/Math.round (* 10 s)) 10) :points points
       :point-s (when (pos? points) (/ (js/Math.round (* 100 (/ s points))) 100))})))
