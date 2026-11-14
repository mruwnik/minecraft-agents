(ns jobs.lib.villager-pen
  "The pen a villager is rolled in (jobs.village.pen builds it, jobs.village.roll asks for it): a one-wide cell V beside
  the workstation W, along a side d. V is floored, walled three high on both sides, roofed, and V and W lie under one
  roof; W plugs V's far end with a top slab above it, so a broken W leaves a gap 1.5 high, too low for a villager (1.95)
  and room for a sneaking body (1.5) to reach in. The near end O (the cell beyond V, feet and head) stays open until the
  villager stands in V, then it is plugged. V must hold no other villager. jobs.village.pen lights V while it waits for
  the villager; a pen that holds it is not judged by its light (the villager fills the cell)."
  (:require [jobs.lib.block-sets :as bs]
            [jobs.lib.torch :as torch]
            [jobs.lib.util :as u]
            [jobs.lib.villager :as villager]))

(def sides "The four horizontal sides [dx dz] a pen may run along from the workstation." [[1 0] [-1 0] [0 1] [0 -1]])

(defn layout
  "The cells of the pen at workstation cell [x y z] along side [dx dz]: :cell V, :shut (floor, walls, roof: built first),
  :slab (the cell above W), :plug (O, feet and head), :side."
  [[x y z] [dx dz :as side]]
  (let [[vx vz] [(+ x dx) (+ z dz)]
        walls (for [s [1 -1] dy [0 1 2]] [(+ vx (* s dz)) (+ y dy) (+ vz (* s dx))])]
    {:side side
     :cell [vx y vz]
     :shut (vec (concat [[vx (dec y) vz]] walls [[vx (+ y 2) vz] [x (+ y 2) z]]))
     :slab [x (inc y) z]
     :plug [[(+ vx dx) y (+ vz dz)] [(+ vx dx) (inc y) (+ vz dz)]]}))

(defn name-at "The seen block name at cell [x y z], or nil." [p cell] (u/seen-name p (u/cell-pos cell)))

(defn solid?
  "Whether a seen block name stops a villager: not walked through, no fluid. An unseen cell (nil) is not."
  [n]
  (boolean (and n (not (bs/passable? n)) (not (bs/fluids n)))))

(defn owed
  "The cells of cells not seen solid (unseen ones are owed: the worse case)."
  [p cells]
  (filterv #(not (solid? (name-at p %))) cells))

(defn top-slab?
  "Whether the cell holds a slab in its upper half, seen."
  [p cell]
  (let [b (u/seen-block p (u/cell-pos cell))]
    (boolean (and b (re-find #"_slab$" (.-name b)) (= "top" (some-> (.-properties b) (aget "type")))))))

(defn open?
  "Whether the feet and head cells at [x y z] are seen open for a villager to stand in."
  [p [x y z]]
  (every? #(bs/passable? (name-at p %)) [[x y z] [x (inc y) z]]))

(defn feet-cell [e] (u/cell-vec (.-pos e)))

(defn inside?
  "Whether villager entity e stands in the pen's cell."
  [lay e]
  (= (:cell lay) (feet-cell e)))

(defn others-in
  "The villagers in sight other than id standing in the pen's cell or its open end."
  [p lay id]
  (let [cells (set (cons (:cell lay) (:plug lay)))]
    (filterv #(and (not= id (villager/ident %)) (cells (feet-cell %))) (villager/seen p))))

(defn dark? [p lay] (torch/perceived-dark? p (:cell lay)))

(defn plug-shut?
  "Whether the near end lets no villager through (it needs two open cells over each other): its head cell is solid, or
  its feet cell is and so is the cell over the head (a 1-high gap, as a training cell's back seal has)."
  [p lay]
  (let [[[x y z] head] (:plug lay)
        s? #(solid? (name-at p %))]
    (or (s? head) (and (s? [x y z]) (s? [x (+ y 2) z])))))

(defn penned
  "The layout of the pen at workstation cell ws holding villager entity e (id its ident): shut, the slab on top, the
  near end plugged, no other villager in it; nil when there is none."
  [p ws e]
  (let [id (villager/ident e)]
    (some (fn [side]
            (let [lay (layout ws side)]
              (when (and (inside? lay e)
                         (empty? (owed p (:shut lay)))
                         (plug-shut? p lay)
                         (top-slab? p (:slab lay))
                         (empty? (others-in p lay id)))
                lay)))
          sides)))

(defn to-reopen
  "The plug cells standing solid: dug so a villager outside can walk in."
  [p lay]
  (filterv #(solid? (name-at p %)) (:plug lay)))

(defn work
  "Blocks a pen still needs placed or dug (unseen cells counted as owed): its price in blocks. in?: the villager
  stands in it, so the plug stays."
  [p lay in?]
  (+ (count (owed p (:shut lay)))
     (if (top-slab? p (:slab lay)) 0 1)
     (cond (not in?) (+ (count (:plug lay)) (count (to-reopen p lay)))
           (plug-shut? p lay) 0
           :else (count (owed p (:plug lay))))))

(defn choose
  "The side to pen villager e at workstation cell ws: one whose cell e stands in, else one whose cell is seen open and
  whose near end is seen (open, or solid to dig), the one with the fewest blocks to place or dig (a pen already standing
  costs least). nil when no side has room."
  [p ws e]
  (let [lays (map #(layout ws %) sides)
        in (when e (filter #(inside? % e) lays))
        room (filter #(and (open? p (:cell %))
                           (every? (fn [cell] (let [n (name-at p cell)] (and n (not (bs/fluids n))))) (:plug %)))
                     lays)]
    (if (seq in)
      (:side (first in))
      (:side (first (sort-by #(work p % false) room))))))
