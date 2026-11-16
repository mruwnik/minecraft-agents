(ns jobs.lib.villager-pen
  "The pen a villager is rolled in (jobs.village.pen builds it, jobs.village.roll asks for it): a one-wide cell V beside
  the workstation W, along a side d. V is floored, walled three high on both sides, roofed, and V and W lie under one
  roof; W plugs V's far end with a top slab above it, so a broken W leaves a gap 1.5 high, too low for a villager (1.95)
  and room for a sneaking body (1.5) to reach in. The near end O (the cell beyond V, feet and head) stays open until the
  villager stands in V, then it is plugged. A villager claims a job site it can path to a cell next to (the game's
  AcquirePoi: reach 1, by Manhattan distance), so W's other sides are shut too: the two across d walled at feet height,
  and the body's stand S behind W (opposite V) boxed in, floor, roof and both cells of its three other sides, with the
  body in it. V must hold no other villager and read lit."
  (:require [jobs.lib.block-sets :as bs]
            [jobs.lib.torch :as torch]
            [jobs.lib.util :as u]
            [jobs.lib.villager :as villager]))

(def sides "The four horizontal sides [dx dz] a pen may run along from the workstation." [[1 0] [-1 0] [0 1] [0 -1]])

(defn layout
  "The cells of the pen at workstation cell [x y z] along side [dx dz]: :cell V, :shut (V's floor, walls and roof, W's
  roof, W's sides across d, S's floor and roof: built first), :slab (the cell above W), :plug (O, feet and head), :stand
  S, :box (S's three other sides, feet and head: shut round the body), :entry (the box's side away from W), :floors
  {cell floor}: the floors of O and the entry, which the plug and the box's entry are placed against over a drop."
  [[x y z] [dx dz :as side]]
  (let [[vx vz] [(+ x dx) (+ z dz)]
        [sx sz] [(- x dx) (- z dz)]
        across (fn [cx cz dys] (for [s [1 -1] dy dys] [(+ cx (* s dz)) (+ y dy) (+ cz (* s dx))]))
        entry [[(- sx dx) y (- sz dz)] [(- sx dx) (inc y) (- sz dz)]]
        plug [[(+ vx dx) y (+ vz dz)] [(+ vx dx) (inc y) (+ vz dz)]]
        below (fn [[cx cy cz]] [cx (dec cy) cz])]
    {:side side
     :cell [vx y vz]
     :shut (vec (concat [[vx (dec y) vz]] (across vx vz [0 1 2]) [[vx (+ y 2) vz] [x (+ y 2) z]]
                        (across x z [0]) [[sx (dec y) sz] [sx (+ y 2) sz]]))
     :floors {(first plug) (below (first plug)) (first entry) (below (first entry))}
     :slab [x (inc y) z]
     :plug plug
     :stand [sx y sz]
     :box (vec (concat (across sx sz [0 1]) entry))
     :entry entry}))

(defn name-at "The seen block name at cell [x y z], or nil." [p cell] (u/seen-name p (u/cell-pos cell)))

(defn solid?
  "Whether a seen block name stops a villager: not walked through, no fluid. An unseen cell (nil) is not."
  [n]
  (boolean (and n (not (bs/passable? n)) (not (bs/fluids n)))))

(defn owed
  "The cells of cells not seen solid (unseen ones are owed: the worse case)."
  [p cells]
  (filterv #(not (solid? (name-at p %))) cells))

(defn with-floors
  "Cells owed (owed) with the floor under each first, while that floor is not seen solid. A floor under a cell standing
  solid is never owed: buried, it is never seen and holds nothing up."
  [p lay cells]
  (let [cells (owed p cells)]
    (into (filterv #(not (solid? (name-at p %))) (keep (:floors lay) cells)) cells)))

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

(defn body-cell [p] (feet-cell (.self p)))

(defn at-stand? "Whether the body stands in the stand S." [p lay] (= (:stand lay) (body-cell p)))

(defn way-in?
  "Whether a side of the box is seen open, feet and head, for the body to walk into S through."
  [p lay]
  (boolean (some (fn [pair] (every? #(bs/passable? (name-at p %)) pair)) (partition 2 (:box lay)))))

(defn owed-with-villager-in
  "The shut cells of lay owed (owed) while a villager stands in V, V's floor and roof never seen left out: from the stand
  the top slab hides them, the villager standing there is held up by the floor, and no villager climbs walls three high."
  [p lay]
  (let [[x y z] (:cell lay)
        hidden #{[x (dec y) z] [x (+ y 2) z]}]
    (filterv #(not (and (hidden %) (nil? (name-at p %)))) (owed p (:shut lay)))))

(defn owed-shut
  "The shut cells of lay owed: owed-with-villager-in when an adult villager stands in its cell, else all of owed."
  [p lay]
  (if (some #(inside? lay %) (villager/adults p))
    (owed-with-villager-in p lay)
    (owed p (:shut lay))))

(defn boxed
  "The layout of the pen built at workstation cell ws (its shut cells solid, the top slab on) whose stand the body stands
  boxed in (no side open to walk out by), else nil: a dead end next to a workstation is no pen."
  [p ws]
  (some (fn [side]
          (let [lay (layout ws side)]
            (when (and (at-stand? p lay) (not (way-in? p lay)) (empty? (owed-shut p lay)) (top-slab? p (:slab lay)))
              lay)))
        sides))

(defn exit-cell
  "The first solid cell of the box's entry when the body stands boxed in the stand of a built pen at ws (boxed), else nil."
  [p ws]
  (some->> (boxed p ws) :entry (filter #(solid? (name-at p %))) first))

(defn luring
  "The layout of the pen at ws with the body boxed in its stand (no side open to walk out by), the top slab on, no shut
  cell seen open and the near end open: the body sees out only through the pen, where a villager walks in; else nil.
  Shut cells never seen count for it: from the stand the eye does not reach the pen's walls (jobs.village.pen looks)."
  [p ws]
  (some (fn [side]
          (let [lay (layout ws side)]
            (when (and (at-stand? p lay) (not (way-in? p lay)) (top-slab? p (:slab lay))
                       (not-any? #(name-at p %) (owed p (:shut lay))) (not (plug-shut? p lay)))
              lay)))
        sides))

(defn built?
  "Whether a pen stands at workstation cell ws: a top slab seen where one side's layout puts it."
  [p ws]
  (boolean (some #(top-slab? p (:slab (layout ws %))) sides)))

(defn penned
  "The layout of the pen at workstation cell ws holding villager entity e (id its ident): shut (owed-with-villager-in),
  the slab on top, the near end plugged, no other villager in it, lit, the body boxed in its stand; nil when there is none."
  [p ws e]
  (let [id (villager/ident e)]
    (some (fn [side]
            (let [lay (layout ws side)]
              (when (and (inside? lay e)
                         (empty? (owed-with-villager-in p lay))
                         (plug-shut? p lay)
                         (top-slab? p (:slab lay))
                         (empty? (others-in p lay id))
                         (not (dark? p lay))
                         (at-stand? p lay)
                         (empty? (owed p (:box lay))))
                lay)))
          sides)))

(defn walked-in
  "The adult villagers standing in the cell of a pen built at workstation cell ws (its shut cells solid, the top slab on)."
  [p ws]
  (for [lay (map #(layout ws %) sides)
        :when (and (empty? (owed p (:shut lay))) (top-slab? p (:slab lay)))
        e (villager/adults p)
        :when (inside? lay e)]
    e))

(defn cells
  "Every cell of the pen layout lay: what a fetch for the pen's own blocks must never dig."
  [lay]
  (vec (concat [(:cell lay) (:slab lay) (:stand lay)] (:shut lay) (:plug lay) (:box lay) (vals (:floors lay)))))

(defn solids
  "The cells standing solid (to dig: a plug of an empty pen, the box's entry)."
  [p cells]
  (filterv #(solid? (name-at p %)) cells))

(defn work
  "Blocks a pen still needs placed or dug (unseen cells counted as owed): its price in blocks. in?: the villager
  stands in it, so the plug stays."
  [p lay in?]
  (+ (count (owed p (:shut lay)))
     (count (owed p (:box lay)))
     (if (top-slab? p (:slab lay)) 0 1)
     (cond (not in?) (+ (count (:plug lay)) (count (solids p (:plug lay))))
           (plug-shut? p lay) 0
           :else (count (owed p (:plug lay))))))

(defn choose
  "The side to pen villager e at workstation cell ws: one whose cell e stands in, else one whose cell and stand are
  seen open and whose near end is seen (open, or solid to dig), the one with the fewest blocks to place or dig (a pen
  already standing costs least). nil when no side has room."
  [p ws e]
  (let [lays (map #(layout ws %) sides)
        in (when e (filter #(inside? % e) lays))
        room (filter #(and (open? p (:cell %))
                           (open? p (:stand %))
                           (every? (fn [cell] (let [n (name-at p cell)] (and n (not (bs/fluids n))))) (:plug %)))
                     lays)]
    (if (seq in)
      (:side (first in))
      (:side (first (sort-by #(work p % false) room))))))
