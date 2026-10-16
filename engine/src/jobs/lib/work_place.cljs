(ns jobs.lib.work-place
  "Where a farm or build job works, for its safe? export (ready/safe-at): the cell at the middle of the place its args
  name, nil (here) when they name none."
  (:require [engine.args :as a]
            [jobs.lib.world :as known]))

(defn middle
  "The cell at the middle of the positions ([x y z] vectors), nil for none."
  [ps]
  (when (seq ps)
    (let [[lo hi] [(apply map min ps) (apply map max ps)]
          [x y z] (map #(js/Math.floor (/ (+ %1 %2) 2)) lo hi)]
      {:x x :y y :z z})))

(defn plan-spot
  "The middle of the cells of the plan :plan (of :part, when given), nil when it is not readable."
  [{:keys [plan part]} c]
  (when plan
    (let [answer (known/plan c plan)]
      (when-not (known/unreadable? answer)
        (middle (keep #(when (or (nil? part) (= part (:part %))) (:pos %)) (:cells answer)))))))

(defn corner-vec [p] (let [{:keys [x y z]} (a/cell p)] [x y z]))

(defn spot
  "The place of c's args: :at or :pos, else the middle of :plan, of :box, of :from to :to, else :center."
  [c]
  (let [{:keys [at pos box from to center] :as args} (:args c)]
    (or (a/cell (or at pos))
        (plan-spot args c)
        (when box (middle (map corner-vec [(:min box) (:max box)])))
        (when (and from to) (middle (map corner-vec [from to])))
        (a/cell center))))

