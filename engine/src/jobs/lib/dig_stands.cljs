(ns jobs.lib.dig-stands
  "The cells a block is dug from, and what standing there costs: a solid block is never in a threat's field, the open
  cells beside it are."
  (:require [engine.game :as game]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.look :as look :refer [cell-of]]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]))

(defn around [{:keys [x y z]} [dx dy dz]] {:x (+ x dx) :y (+ y dy) :z (+ z dz)})

(defn face-names
  "The names of the 6 faces of cell pos, as the body knows them: the last seen block, nil for a cell never seen. The cells
  it stands in are felt, and lava directly on top shows as drips, so those read the world. The cell open (a cell a cut is about to
  dig), when given, reads as air."
  [c pos open]
  (let [p (:primitives c)
        feet (cell-of (u/self-pos c))
        own? #(or (= % feet) (= % (update feet :y inc)))]
    (map (fn [f]
           (let [cell (around pos f)
                 seen (look/seen-block p cell)]
             (cond
               (= cell open) "air"
               (own? cell) (u/seen-name p cell)
               (and (= [0 1 0] f) (= "lava" (u/seen-name p cell))) "lava"
               (not (:unknown seen)) (:name seen))))
         rules/neighbour-deltas)))

(def fires #{"fire" "soul_fire"})

(defn open-name?
  "Whether a block of this seen name can be stood in: a known one, not a fluid or a fire, of no collision shape."
  [c n]
  (and n (not (bs/fluids n)) (not (fires n)) (rules/empty-shape? (game/version-of (:primitives c)) n)))

(defn stand-cells
  "The seen open cells beside pos, where the body can stand to dig it (pos itself when none is seen open)."
  [c pos]
  (let [stands (vec (for [[f n] (map vector rules/neighbour-deltas (face-names c pos nil)) :when (open-name? c n)] (around pos f)))]
    (if (seq stands) stands [pos])))

(defn stands-of
  "[pos] when the block seen at pos is open or not known, else its stand-cells."
  [c pos]
  (let [cell (cell-of pos)
        n (u/seen-name (:primitives c) cell)]
    (if (or (nil? n) (open-name? c n)) [pos] (stand-cells c cell))))

(defn stay-danger
  "threats/stay-danger of the calmest of stands-of pos (nil when one is out of every field or affordable)."
  [c pos stay-s worth]
  (let [danger (threats/stay-danger-fn c)
        ds (map #(danger % stay-s worth) (stands-of c pos))]
    (when (every? some? ds) (apply min-key :risk ds))))

(defn stay-loss-fn
  "(fn [pos]) seconds a stay of stay-s at the calmest of stands-of pos costs (threats/danger-near-fn's :stay-loss), 0 outside
  every known threat's field."
  [c stay-s]
  (let [near (threats/danger-near-fn c)
        at (fn [cell] (if-let [{:keys [stay-loss]} (near cell)] (stay-loss stay-s) 0))]
    (fn [pos] (transduce (map #(at (cell-of %))) min ##Inf (stands-of c pos)))))
