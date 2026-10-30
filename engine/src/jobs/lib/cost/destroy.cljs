(ns jobs.lib.cost.destroy
  "What digging a cell destroys, in seconds of replacing it: the one price every dig route and way adds to a cell's dig.
  Natural terrain nobody built (jobs.lib.access.rules/natural?) and unstripped tree wood costs nothing; any other block is put back; a crop
  loses the growth it had; farmland is put back as dirt and tilled again, and the crop on it lost."
  (:require [engine.game :as game]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.cost.farm :as farm]
            [jobs.lib.cost.table :as table]
            [jobs.lib.cost.value :as value]
            [jobs.lib.cost.ways :as ways]
            [jobs.lib.crops :as crops]))

(defn put-back-s
  "Seconds putting block n back costs (ways/put-back-cost): its placing, and its worth (value/item-value) when its dig
  drops something else; the placing only for nil (not seen)."
  [n]
  (ways/put-back-cost (if (or (nil? n) (some #{n} (get (table/drops-table @game/version) n [n])))
                        0
                        (:value (value/item-value [{:name n :count 1}])))))

(defn crop-lost-s
  "Seconds destroying crop at stage age costs: the growth it had, its ripe worth (value/crop-worth) times the share of
  its stages grown (nil age: ripe, the worse case), and its replanting (farm/step-s :plant)."
  [crop age]
  (let [ripe (get crops/ripe-age crop)]
    (+ (* (value/crop-worth crop) (/ (min ripe (or age ripe)) ripe)) (farm/step-s :plant 1))))

(defn destroyed-s
  "Seconds of replacing what digging block n destroys, over the block `over` it and the age of the crop that dig
  destroys (crop-age, nil not known):
  - a crop: crop-lost-s;
  - farmland: put back as the dirt its dig drops (ways/put-back-cost) and tilled (farm/step-s :till), with a crop on it lost;
  - own? (the body's own build) or a block that is not natural terrain nor replaceable: put-back-s;
  - else 0 (nil n: not seen)."
  [n over crop-age own?]
  (cond
    (contains? crops/ripe-age n) (crop-lost-s n crop-age)
    (= "farmland" n) (+ (ways/put-back-cost 0) (farm/step-s :till 1)
                        (if (contains? crops/ripe-age over) (crop-lost-s over crop-age) 0))
    own? (put-back-s n)
    (or (nil? n) (rules/natural? n) (rules/tree-wood? n) (contains? bs/replaceable n)) 0
    :else (put-back-s n)))

(defn destroyed-fn
  "(fn [n cell own? & [dug]]) destroyed-s of digging block n at cell, the block over it and the crop's age read by
  (sensed cell) (a sensed block or nil: .-name, .-age, .-unknown). dug: the set of cells dug together, default none (a crop
  is an open cell: a route cutting farmland under it does not dig it, so the farmland pays it); a crop over cell that
  is among them is already paid for."
  [sensed]
  (fn [n [x y z :as cell] own? & [dug]]
    (let [above (sensed [x (inc y) z])
          over (when (and above (not (true? (.-unknown above))) (not (some-> dug (contains? [x (inc y) z])))) (.-name above))
          crop (cond (contains? crops/ripe-age n) (sensed cell) (contains? crops/ripe-age over) above)]
      (destroyed-s n over (some-> crop .-age) own?))))
