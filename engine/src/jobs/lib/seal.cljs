(ns jobs.lib.seal
  "What a body sealing itself in shares: the cells a hostile's hitbox overlaps (no block goes there), centring the
  body in its own cell."
  (:require [engine.ctx :as ctx]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]))

(def mob-half-width
  "Half the width of a zombie-sized mob's hitbox."
  0.3)

(defn hitbox-cells
  "The feet and head cells a mob standing at pos overlaps (its hitbox is 0.6 wide, so one straddling a cell
  boundary is in both cells: the server refuses a block there)."
  [{:keys [x y z]}]
  (let [span (fn [v] (distinct [(js/Math.floor (- v mob-half-width)) (js/Math.floor (+ v mob-half-width))]))
        fy (js/Math.floor y)]
    (for [cx (span x) cz (span z) cy [fy (inc fy)]] {:x cx :y cy :z cz})))

(defn hostile-cells
  "The cells the hostiles within radius overlap: no block goes there."
  [p radius]
  (set (mapcat #(hitbox-cells (danger-q/mob-pos p %)) (danger-q/known-hostiles p radius {}))))

(defn off-centre?
  "Whether the body's hitbox (0.6 wide) reaches out of its cell into a side cell."
  [p]
  (let [{:keys [x z]} (u/self-pos {:primitives p})
        frac #(- % (js/Math.floor %))]
    (boolean (some #(or (< (frac %) 0.3) (> (frac %) 0.7)) [x z]))))

(defn ^:async centre!
  "Step to the middle of the body's own cell, so the side cells around it can take blocks."
  [c]
  (let [{:keys [x y z]} (sh-sense/feet (:primitives c))]
    ;; raw moveTo kept: a step inside the body's own cell, range 0.15; the planner has no goal finer than a cell.
    (await (ctx/act c :moveTo #js {:pos #js {:x (+ x 0.5) :y y :z (+ z 0.5)} :range 0.15}))))
