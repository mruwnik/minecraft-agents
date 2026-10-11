(ns jobs.lib.own-pen
  "The wall of the pen the body stands shut in: a closed enclosure (jobs.lib.pen/check from its feet) in the body's own
  area (zone or plan) with livestock inside. A dig out of it frees the animals, so go-to's escalation spares its wall."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.animals :as animals]
            [jobs.lib.apiary :as apiary]
            [jobs.lib.pen :as pen]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::max-cells {:default 400 :doc "Most cells searched for the pen the body is shut in." :spec (a/int-in 1 nil)}
   ::radius {:default 16 :doc "Blocks round the body in which livestock is looked for inside that pen." :spec (a/int-in 1 nil)}})

(defn wall?
  "A fn of a cell [x y z]: whether it is the wall of the own pen holding livestock that the body at feet stands in
  (beside a cell of the pen's inside or above it, up to the rim), else always false. The pen is read on the first call."
  [c feet]
  (let [p (:primitives c)
        wall (delay
               (let [at (zipmap [:x :y :z] feet)
                     answer (when (access/in-own-area? c at)
                              (pen/check {:block-at (apiary/seen-block-at-fn p) :at at :max-cells (settings/get settings ::max-cells)}))]
                 (when (and (:closed? answer) (seq (animals/in-pen-rows p answer (settings/get settings ::radius))))
                   (let [columns (into #{} (map (fn [[x _ z]] [x z])) (:inside answer))
                         floor (apply min (map second (:inside answer)))]
                     (fn [[x y z]]
                       (and (>= y floor)
                            (boolean (some (fn [dx] (some (fn [dz] (contains? columns [(+ x dx) (+ z dz)])) [-1 0 1])) [-1 0 1]))))))))]
    (fn [cell] (boolean (some-> @wall (apply [cell]))))))
