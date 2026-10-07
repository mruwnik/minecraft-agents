(ns engine.path.planner.danger
  "Search methods: the risk of walking near known dangers (options.dangers, packed by engine.path.planner-tuned)."
  (:require [engine.path.planner.search :refer [Search]]))

(set! *warn-on-infer* true)

(extend-type Search
  Object

  ;; the risk (hp) of dsec seconds spent entering the cell x,y,z: each danger's rate times its weight there (1 within
  ;; close blocks of its point, falling linearly to 0 at radius; the cell's centre at its floor), the sum at most
  ;; danger-cap a second, priced at damage-weight like any damage (the move adds risk-weight times its risk to g, so rates and
  ;; cap are scaled by damage-weight / risk-weight once, when planner-tuned packs them; equal by default). 0 at once for a cell outside every danger's radius.
  (dangerRisk [s x y z dsec]
    (if (or (< x (.-dbx0 s)) (> x (.-dbx1 s)) (< y (.-dby0 s)) (> y (.-dby1 s)) (< z (.-dbz0 s)) (> z (.-dbz1 s)))
      0
      (let [^js a (.-dangers s)
            cx (+ x 0.5)
            cz (+ z 0.5)]
        (loop [i 0 rate 0]
          (if (< i (.-n-dangers s))
            (let [o (* i 6)
                  dx (- cx (aget a o))
                  dy (- y (aget a (+ o 1)))
                  dz (- cz (aget a (+ o 2)))
                  close (aget a (+ o 3))
                  radius (aget a (+ o 4))
                  d2 (+ (* dx dx) (* dy dy) (* dz dz))]
              (cond
                (>= d2 (* radius radius)) (recur (inc i) rate)
                (<= d2 (* close close)) (recur (inc i) (+ rate (aget a (+ o 5))))
                :else (recur (inc i) (+ rate (/ (* (aget a (+ o 5)) (- radius (js/Math.sqrt d2))) (- radius close))))))
            (* dsec (js/Math.min rate (.-danger-cap s)))))))))
