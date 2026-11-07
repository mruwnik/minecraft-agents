(ns world-test.food-gets
  "tools/event-counts.mjs food-gets: counts the measured prior behind jobs.lib.cost.food ::field-point-s from the bodies'
  logs alone: each hungry reflex's get-food ending (any outcome, fruitless and cut ones too), its :wall-time and the
  hunger points and saturation of the food its report yields (a raw food it cooked counted once, as the cooked), left out when the call
  took food from a chest (food.took-from-chest: home food, not the field's). :fill-s (seconds a point of hunger or saturation, what
  exhaustion drains) is cost.health ::fill-point-s. Pure parts: scan-line, report."
  (:require [jobs.lib.foods :as foods]))

(defn- field [line re] (second (re-find re line)))

(defn yield-points
  "Hunger points (by default; or (of item), e.g. foods/saturation) of yields {item count}: each food at its points, a raw food
  whose cooked food is among them once, as the cooked."
  ([yields] (yield-points yields foods/points))
  ([yields of]
   (- (reduce + 0 (for [[n k] yields :when (foods/food? n)] (* k (of n))))
      (reduce + 0 (for [[raw cooked] foods/cooked-of :when (and (yields raw) (yields cooked))]
                    (* (min (yields raw) (yields cooked)) (of raw)))))))

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
       :saturation (yield-points @yields foods/saturation)})))

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
