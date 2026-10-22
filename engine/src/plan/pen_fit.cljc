(ns plan.pen-fit
  "The one rule for whether a pen (its inner box, the cells animals stand on) can be herded into. Pure, shared by the plan
  shape check, build.pen, pen-check and herd.")

(def min-depth
  "Pen cells in a straight line from in-1 along the gate's axis that herd needs: the body on in-5 puts a led animal 2.2
  cells inside the gate cell; on in-4 it still overlaps it. A pen is never smaller because a lure run walks in a line."
  5)

(defn box-sides
  "The box's lengths in cells along x and z that a gate's axis could run, only the gate's when it stands beside a side."
  [{:keys [min max]} gate]
  (let [xs (inc (- (:x max) (:x min)))
        zs (inc (- (:z max) (:z min)))]
    (cond
      (and gate (or (< (:x gate) (:x min)) (> (:x gate) (:x max)))) [xs]
      (and gate (or (< (:z gate) (:z min)) (> (:z gate) (:z max)))) [zs]
      :else [xs zs])))

(defn deep-enough? [box gate]
  (>= (apply clojure.core/max (box-sides box gate)) min-depth))

(defn shallow-text
  "Why herd cannot use the inner box (given a gate cell, or nil for any side), or nil when it can."
  [{:keys [min max] :as box} gate]
  (when-not (deep-enough? box gate)
    (str "the pen is " (inc (- (:x max) (:x min))) "x" (inc (- (:z max) (:z min))) " cells; herd needs " min-depth
         " in a line inward from the gate so a led animal clears the gate cell")))
