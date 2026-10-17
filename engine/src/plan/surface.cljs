(ns plan.surface
  "A plan with :surface true is wanted on the ground as seen: each column of its cells follows the ground there.
  Shared by the build jobs and by plans check, so both judge the same cells."
  (:require [jobs.lib.surface :as spot]))

(def depth "Blocks above and below a planned column's lowest cell that the surface is looked for in." 6)

(def on-ground
  "Names that stand on the ground and are not it: fences, gates, walls and torches."
  #"(_fence|_fence_gate|_wall|torch)$")

(defn top-fn
  "(fn [x z y]) the y of the seen ground top of column x z around y, nil when unknown; name-at takes {:x :y :z} and
  returns the block name seen there, nil when unseen."
  [name-at]
  (let [name-fn (fn [pos] (let [n (name-at pos)] (if (and n (re-find on-ground n)) "air" n)))]
    (fn [x z y] (spot/column-top name-fn x z y depth))))

(defn follow
  "cells ({:pos [x y z]}) with each column [x z] moved up or down as a whole so its lowest cell stands on the top of the
  ground: (top-of x z lowest-y) is that top's y, nil when unknown (the column stays)."
  [cells top-of]
  (let [shift (into {}
                    (for [[col cs] (group-by (fn [{[x _ z] :pos}] [x z]) cells)
                          :let [base (apply min (map (comp second :pos) cs))
                                top (top-of (first col) (second col) base)]
                          :when top]
                      [col (- (inc top) base)]))]
    (mapv (fn [{[x y z] :pos :as cell}]
            (let [d (get shift [x z] 0)] (if (zero? d) cell (assoc cell :pos [x (+ y d) z]))))
          cells)))
