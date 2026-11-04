(ns jobs.lib.cost.destroy
  "What digging a cell destroys, in seconds of replacing it: the one price every dig route and way adds to a cell's dig.
  Natural terrain nobody built (jobs.lib.access.rules/natural?) and unstripped tree wood that does not look placed costs nothing; any other block is put back; a crop
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

(def built-block #"(?:planks|slab|stairs|glass|glass_pane|door|fence|fence_gate|bricks|cobblestone|ladder|chest|torch|sign|crafting_table|furnace|bed)$")

(defn placed-log?
  "True when the tree wood at cell looks placed: a built block (planks, glass, doors...) touching it, or, with no leaves
  touching it and no vertical log stacked on or under a vertical one, a horizontal axis or a log beside it in a horizontal
  line. Leaves and stacked trunks mark a tree (2x2 trunks, branches, trunks grown together). (sensed cell) is a sensed
  block or nil."
  [sensed [x y z]]
  (let [name-at (fn [c] (let [b (sensed c)] (when-not (or (nil? b) (true? (.-unknown b))) (.-name b))))
        props (some-> (sensed [x y z]) .-properties)
        vertical? (not (contains? #{"x" "z"} (some-> props .-axis)))
        side [[1 0 0] [-1 0 0] [0 0 1] [0 0 -1]]
        up-down [[0 1 0] [0 -1 0]]
        at (fn [[dx dy dz]] (name-at [(+ x dx) (+ y dy) (+ z dz)]))]
    (boolean
     (or (some #(some->> (at %) (re-find built-block)) (concat side up-down))
         (and (not-any? #(some->> (at %) (re-find #"leaves$")) (concat side up-down))
              (not (and vertical? (some #(rules/tree-wood? (at %)) up-down)))
              (or (not vertical?)
                  (some #(rules/tree-wood? (at %)) side)))))))

(def soil #"^(?:dirt|coarse_dirt|grass_block|podzol|mud|clay|sand|red_sand|gravel)$")

(defn placed-roof?
  "True when the soil block at cell looks like a roof someone laid: it sits in a soil run at most 2 thick with known open
  air under and over the run, and a soil neighbour beside it is such a run too, so a thin flat span (the ceiling of a
  tunnel dug through soil has soil over it, and is not one). Natural thin arches are rarer than laid roofs, and a wrong
  guess only prices a put-back. (sensed cell) is a sensed block or nil."
  [sensed [x y z]]
  (let [name-at (fn [c] (let [b (sensed c)] (when-not (or (nil? b) (true? (.-unknown b))) (.-name b))))
        open? (fn [c] (boolean (some-> (name-at c) bs/air)))
        soil? (fn [c] (boolean (some->> (name-at c) (re-find soil))))
        thin? (fn [cx cy cz]
                (and (soil? [cx cy cz])
                     (let [run (fn [dy] (loop [k 1] (cond (open? [cx (+ cy (* dy k)) cz]) k
                                                          (and (< k 3) (soil? [cx (+ cy (* dy k)) cz])) (recur (inc k)))))
                           down (run -1) up (run 1)]
                       (boolean (and down up (<= (+ down up -1) 2))))))]
    (and (thin? x y z)
         (boolean (some (fn [[dx dz]] (or (thin? (+ x dx) y (+ z dz)) (thin? (+ x dx) (dec y) (+ z dz))
                                          (thin? (+ x dx) (inc y) (+ z dz))))
                        [[1 0] [-1 0] [0 1] [0 -1]])))))

(defn destroyed-fn
  "(fn [n cell own?]) destroyed-s of digging block n at cell, the block over it and the crop's age read by
  (sensed cell) (a sensed block or nil: .-name, .-age, .-unknown). A crop over cell is an open cell a route does not
  dig, so the farmland under it pays it. Tree wood that looks placed (placed-log?) and a soil roof over open air (placed-roof?) are priced as a build."
  [sensed]
  (fn [n [x y z :as cell] own?]
    (let [own? (or own? (and (rules/tree-wood? n) (placed-log? sensed cell))
                     (and (some? n) (re-find soil n) (placed-roof? sensed cell)))
          above (sensed [x (inc y) z])
          over (when (and above (not (true? (.-unknown above)))) (.-name above))
          crop (cond (contains? crops/ripe-age n) (sensed cell) (contains? crops/ripe-age over) above)]
      (destroyed-s n over (some-> crop .-age) own?))))
