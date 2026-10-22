(ns jobs.lib.lava-risk
  "The risk of seen lava near a cell, in hp: full beside it, a quarter at ::radius, 0 beyond. go-to's route check and
  access.cleanup's scaffold walks use it."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::radius {:default 3 :spec (a/int-in 0 nil)
             :doc "Blocks around a cell of the walk within which seen lava is a risk."}
   ::hp {:default 6 :spec (a/num-in 0 nil)
         :doc "hp of risk for a step right beside seen lava (less the farther it is, down to a quarter at ::radius): a slip there is lethal, so a small chance of it costs this much."}})

(defn radius [] (settings/get settings ::radius))

(defn hp-from
  "::hp scaled by the closeness of the nearest of `lavas` ([x y z] vectors) to the cell [x y z]: full beside it, a quarter at ::radius, 0 beyond."
  [lavas [x y z]]
  (let [r (radius)
        near (for [[lx ly lz] lavas
                   :let [d (max (js/Math.abs (- lx x)) (js/Math.abs (- lz z)))]
                   :when (and (<= d r) (<= -3 (- ly y) 2))]
               d)]
    (if (seq near)
      (* (settings/get settings ::hp) (/ (- (inc r) (max 1 (apply min near))) r))
      0)))

(defn hp
  "The hp of risk of standing at cell (a map or [x y z]) with seen lava near: ::hp scaled by how close the nearest seen lava is, else 0."
  [p cell]
  (let [[x y z] (if (map? cell) [(:x cell) (:y cell) (:z cell)] cell)
        r (radius)]
    (if-not y
      0
      (hp-from (for [dx (range (- r) (inc r)) dz (range (- r) (inc r)) dy (range -3 3)
                     :when (= "lava" (u/seen-name p {:x (+ x dx) :y (+ y dy) :z (+ z dz)}))]
                 [(+ x dx) (+ y dy) (+ z dz)])
               [x y z]))))
