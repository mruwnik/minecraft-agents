(ns jobs.lib.own-pen
  "The wall of the pen the body stands shut in: a closed enclosure (jobs.lib.pen/check from its feet) in the body's own
  area (zone or plan) with livestock inside. A dig out of it frees the animals, so go-to's escalation and clear-box spare its wall."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.animals :as animals]
            [jobs.lib.apiary :as apiary]
            [jobs.lib.pen :as pen]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::max-starts {:default 100 :doc "Most standing cells beside a box tried as the start of a pen check." :spec (a/int-in 1 nil)}
   ::depth {:default 8 :doc "Blocks below a box in which the ground beside it is looked for as a pen floor." :spec (a/int-in 0 nil)}
   ::max-cells {:default 400 :doc "Most cells searched for the pen the body is shut in." :spec (a/int-in 1 nil)}
   ::wall-height {:default 3 :doc "Blocks above the highest cell inside a pen up to which a cell beside it counts as its wall." :spec (a/int-in 0 nil)}
   ::radius {:default 16 :doc "Blocks round the body in which livestock is looked for inside that pen." :spec (a/int-in 1 nil)}})

(defn wall-fn
  "A fn of a cell [x y z]: whether it is the wall of an own pen holding livestock that closes round one of the feet cells
  starts (beside a cell of the pen's inside or above it, up to ::wall-height above the highest inside cell). A start in an own area only; the pen is read once."
  [c starts]
  (let [p (:primitives c)
        block-at (apiary/seen-block-at-fn p)
        pens (reduce (fn [pens [x y z :as feet]]
                       (let [at (zipmap [:x :y :z] feet)]
                         (if (or (some #(contains? (:inside %) feet) pens) (not (access/in-own-area? c at)))
                           pens
                           (let [answer (pen/check {:block-at block-at :at at :max-cells (settings/get settings ::max-cells)})]
                             (if (and (:closed? answer) (seq (animals/in-pen-rows p answer (settings/get settings ::radius))))
                               (conj pens answer)
                               pens)))))
                     [] starts)
        walls (map (fn [{:keys [inside]}]
                     {:columns (into #{} (map (fn [[x _ z]] [x z])) inside) :floor (apply min (map second inside)) :ceiling (+ (apply max (map second inside)) (settings/get settings ::wall-height))})
                   pens)]
    (fn [[x y z]]
      (boolean (some (fn [{:keys [columns floor ceiling]}]
                       (and (<= floor y ceiling)
                            (some (fn [dx] (some (fn [dz] (contains? columns [(+ x dx) (+ z dz)])) [-1 0 1])) [-1 0 1])))
                     walls)))))

(defn wall?
  "A fn of a cell [x y z]: whether it is the wall of the own pen holding livestock that the body at feet stands in,
  else always false. The pen is read on the first call."
  [c feet]
  (let [wall (delay (wall-fn c [feet]))]
    (fn [cell] (@wall cell))))

(defn standing-cell
  "The first feet cell [x y z] going down column x z from top to bottom (air over a seen non-air block), else nil."
  [name-at x z top bottom]
  (let [air? #(contains? #{"air" "cave_air"} %)]
    (first (for [y (range top (dec bottom) -1)
                 :let [here (name-at [x y z]) below (name-at [x (dec y) z])]
                 :when (and (air? here) below (not (air? below)))]
             [x y z]))))

(defn box-wall?
  "A fn of a cell [x y z]: whether it is the wall of an own pen holding livestock that borders the box of cells (maps
  {:x :y :z}): the pens round the first standing cell, going down, of each column in or one step beside the box, as far
  as depth below it. The pens are read on the first call."
  [c cells]
  (let [wall (delay
               (let [p (:primitives c)
                     name-at (fn [[x y z]] (u/seen-name p {:x x :y y :z z}))
                     xs (map :x cells) zs (map :z cells) ys (map :y cells)
                     standing (for [x (range (dec (apply min xs)) (+ 2 (apply max xs)))
                                    z (range (dec (apply min zs)) (+ 2 (apply max zs)))
                                    :let [at (standing-cell name-at x z (apply max ys) (- (apply min ys) (settings/get settings ::depth)))]
                                    :when at]
                                at)]
                 (wall-fn c (take (settings/get settings ::max-starts) standing))))]
    (fn [cell] (@wall cell))))
