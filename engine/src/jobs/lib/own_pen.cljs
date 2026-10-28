(ns jobs.lib.own-pen
  "The wall of the pen the body stands shut in: a closed enclosure (jobs.lib.pen/check from its feet) in the body's own
  area (zone or plan) with livestock inside. A dig out of it frees the animals, so go-to's escalation and clear-cells spare its wall."
  (:require [jobs.lib.block-sets :as bs]
            [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.animals :as animals]
            [jobs.lib.pen :as pen]
            [jobs.lib.pen-world :as pen-world]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::max-starts {:default 100 :doc "Most standing cells beside a box tried as the start of a pen check." :spec (a/int-in 1 nil)}
   ::depth {:default 8 :doc "Blocks below a box in which the ground beside it is looked for as a pen floor." :spec (a/int-in 0 nil)}
   ::max-cells {:default 400 :doc "Most cells searched for the pen the body is shut in." :spec (a/int-in 1 nil)}
   ::wall-height {:default 3 :doc "Blocks above the highest cell inside a pen up to which a cell beside it counts as its wall." :spec (a/int-in 0 nil)}
   ::radius {:default 16 :doc "Blocks round the body in which livestock is looked for inside that pen." :spec (a/int-in 1 nil)}})

(defn escapes-fn
  "A fn of a start {:x :y :z}: whether the walk from it reaches ::max-cells cells, so pen/check from it finds no pen. A
  start whose walk reaches one that escapes escapes too (it reaches all that one reaches), as does every cell on the way:
  the walks from starts on one open field stop a few cells out instead of each filling ::max-cells."
  [world]
  (let [open (volatile! #{})]
    (fn [at]
      (let [seeds (pen-world/seeds-at world at)
            [key] (map :key seeds)]
        (cond
          (empty? seeds) false
          (contains? @open key) true
          :else (let [{:keys [seen truncated? stopped]} (pen/fill* world seeds nil (settings/get settings ::max-cells) #(contains? @open %))
                      from (fn [[x y z]] (:from (.get seen (pen-world/pack (:store world) x y z))))]
                  (cond
                    truncated? (do (vswap! open conj key) true)
                    stopped (do (vswap! open into (take-while some? (iterate from (from stopped)))) true)
                    :else false)))))))

(defn stocked-pens
  "The own pens holding livestock that close round one of the feet cells starts, [{:inside #{cell} :animals rows}]
  (animals/in-pen-rows). A start in an own area only; each pen is read once.
  A pen not fully seen (dark, out of view: only unseen cells left open) counts too, with the livestock standing in the own area as its animals: what cannot be seen is not taken for open."
  [c starts]
  (let [p (:primitives c)
        block-at (u/seen-block-at-fn p)
        blocks (pen-world/store)
        escapes? (escapes-fn (pen-world/make-world block-at #{} blocks))]
    (reduce (fn [pens [x y z :as feet]]
              (let [at (zipmap [:x :y :z] feet)]
                (if (or (some #(contains? (:inside %) feet) pens) (not (access/in-own-area? c at)) (escapes? at))
                  pens
                  (let [answer (pen/check {:block-at block-at :at at :blocks blocks :max-cells (settings/get settings ::max-cells)})
                        unseen? (and (= :unloaded (:reason answer)) (seq (:inside answer)))
                        animals (when (or (:closed? answer) unseen?)
                                  (animals/in-pen-rows p answer (settings/get settings ::radius) #(and unseen? (access/in-own-area? c %))))]
                    (if (seq animals)
                      (conj pens {:inside (:inside answer) :animals animals})
                      pens)))))
            [] starts)))

(defn walls-fn
  "A fn of a cell [x y z]: whether it is the wall of one of pens (stocked-pens): beside a cell of a pen's inside or
  above it, up to ::wall-height above its highest inside cell."
  [pens]
  (let [walls (map (fn [{:keys [inside]}]
                     {:columns (into #{} (map (fn [[x _ z]] [x z])) inside) :floor (apply min (map second inside)) :ceiling (+ (apply max (map second inside)) (settings/get settings ::wall-height))})
                   pens)]
    (fn [[x y z]]
      (boolean (some (fn [{:keys [columns floor ceiling]}]
                       (and (<= floor y ceiling)
                            (some (fn [dx] (some (fn [dz] (contains? columns [(+ x dx) (+ z dz)])) [-1 0 1])) [-1 0 1])))
                     walls)))))

(defn wall-fn
  "A fn of a cell [x y z]: whether it is the wall of an own pen holding livestock that closes round one of the feet cells
  starts (stocked-pens, walls-fn)."
  [c starts]
  (walls-fn (stocked-pens c starts)))

(defn shut-in
  "The own pen holding livestock the body at feet stands in, read on first use: {:wall? (fn of a cell, walls-fn) :herd
  (a delay of its livestock rows, [] when none)}."
  [c feet]
  (let [pens (delay (stocked-pens c [feet]))
        wall (delay (walls-fn @pens))]
    {:wall? (fn [cell] (@wall cell))
     :herd (delay (vec (mapcat :animals @pens)))}))

(defn wall?
  "A fn of a cell [x y z]: whether it is the wall of the own pen holding livestock that the body at feet stands in,
  else always false. The pen is read on the first call."
  [c feet]
  (:wall? (shut-in c feet)))

(defn standing-cell
  "The first feet cell [x y z] going down column x z from top to bottom (air over a seen non-air block), else nil."
  [name-at x z top bottom]
  (let [air? bs/air]
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
