(ns jobs.survival.dig-in-cells
  "The cells a shelter fills and the placing of blocks into them (read from the world, placed, shut): jobs.survival.shut-in,
  its ways (wall-in, dig-pit, dig-pocket, enter-room), plug-way, block-arrow-gap and the retreat jobs use them."
  (:require [engine.args :as a]
            [jobs.lib.block-sets :as bs]
            [engine.settings :as settings]
            [jobs.lib.blocks :as lb]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.seal :as seal]
            [jobs.lib.land :as land]
            [jobs.lib.ledger :as ledger]
            [engine.ctx :as ctx]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.solid :as solid]
            [jobs.lib.util :as u]))

(def sides [[1 0] [-1 0] [0 1] [0 -1]])

(def corners "The diagonal neighbours: a mob standing there still reaches the body." [[1 1] [1 -1] [-1 1] [-1 -1]])

(def hazards
  "Blocks that are or hold a fluid: a cell opened beside or under one fills."
  (into bs/fluids #{"kelp" "kelp_plant" "seagrass" "tall_seagrass"}))

(defn wet?
  "Whether the cell holds a fluid: a hazards block, or a waterlogged one."
  [p cell]
  (let [b (u/seen-facts p cell)]
    (boolean (and b (or (hazards (:name b)) (:waterlogged? b))))))

(defn lateral-fluid
  "The name of the first wet? side neighbour of the cell, or nil."
  [p {:keys [x y z]}]
  (some (fn [[dx dz]]
          (let [cell {:x (+ x dx) :y y :z (+ z dz)}]
            (when (wet? p cell) (u/seen-name p cell))))
        sides))

(def around-deltas "The cell itself and its six neighbours." (cons [0 0 0] rules/neighbour-deltas))

(def beside-deltas "The cell itself and its four side neighbours." (cons [0 0 0] (map (fn [[dx dz]] [dx 0 dz]) sides)))

(defn lava-around
  "The seen lava cells among cell shifted by deltas (around-deltas, beside-deltas)."
  [p cell deltas]
  (filter #(= "lava" (u/seen-name p %))
          (for [[dx dy dz] deltas] (assoc cell :x (+ (:x cell) dx) :y (+ (:y cell) dy) :z (+ (:z cell) dz)))))

(defn ^:async seal-lava!
  "Fill each of the lava cells with a carried block (jobs.lib.blocks/place-cell!, the place child): true when every one
  is filled (or none is given), false when a block is missing or a placement fails; the failure is emitted
  (:shelter_seal_failed, naming the cell and 'no block to seal with' or the place status). Placed with :ignore-zones?
  because this is safety: lava the body's own dig laid open is filled even where it borders another's zone, and the
  block stays in the lava cell the dig exposed."
  [c blocks lavas]
  (let [fail! (fn [lava why]
                (ctx/emit! c :shelter_seal_failed :info {:text (str "lava at " (:x lava) " " (:y lava) " " (:z lava) " not sealed: " why)})
                false)]
    (loop [[lava & more] lavas]
      (if (nil? lava)
        true
        (if-let [item (lb/pick c blocks)]
          (let [st (await (lb/place-cell! c lava item {:ignore-zones? true}))]
            (if (#{:placed :already} st) (recur more) (fail! lava (str "place " (or st "failed")))))
          (fail! lava "no block to seal with"))))))

(defn stand-on?
  "Whether a mob can stand on the top of the block at cell: a full cube or any other floor (slab, stair, farmland, path,
  soul sand, chest, carpet; jobs.lib.land/floor?), not a fence, wall, pane or bars (too tall to stand on) or a cell not loaded."
  [p cell]
  (let [b (u/seen-block p cell)]
    (boolean (and b
                  (not (re-find seal/mob-proof-shapes (.-name b)))
                  (or (.-fullCube b) (land/floor? (.-name b)))))))

(defn fill-cells
  "The cells to fill around the feet cell, in placement order: sides at feet
  height, sides at head height, a support beside the roof cell (a block needs a
  solid face neighbour to be placed against, and the roof cell has none until
  the support exists; none when one of the four is placed already or the roof cell is sealed; else the one farthest from
  threats ({:x :y :z}), the +x one with none), then the roof cell above the head; only those the
  predicate sealed? (cell -> bool) does not hold for. A corner cell (diagonal, feet or head height) is among them only
  when a mob could stand in it: an open one on a floor it stands on (stand-on?, cell -> bool), or above a corner
  that is one; the corners follow the sides."
  [sealed? {:keys [x y z]} threats stand-on?]
   (let [roof {:x x :y (+ y 2) :z z}
         supports (for [[dx dz] sides] {:x (+ x dx) :y (+ y 2) :z (+ z dz)})
         gap (fn [{cx :x cy :y cz :z}]
               (apply min (map #(u/dist {:x (+ cx 0.5) :y (+ cy 0.5) :z (+ cz 0.5)} %) threats)))
         support (if (empty? threats) (first supports) (apply max-key gap supports))
         at (fn [dy [dx dz]] {:x (+ x dx) :y (+ y dy) :z (+ z dz)})
         corner-cells (fn [corner]
                        (let [feet-c (at 0 corner)
                              feet-stand? (and (not (sealed? feet-c)) (stand-on? (at -1 corner)))]
                          (cond-> []
                            feet-stand? (conj feet-c)
                            (or feet-stand? (stand-on? feet-c)) (conj (at 1 corner)))))]
     (filterv #(not (sealed? %))
              (concat (for [dy [0 1] d sides] (at dy d))
                      (mapcat corner-cells corners)
                      (when-not (or (sealed? roof) (some sealed? supports)) [support])
                      [roof]))))

(defn open-cells
  "The fill-cells around the feet cell that are not sealed? in the world; the roof support away from threats."
  ([p feet] (open-cells p feet nil))
  ([p feet threats] (fill-cells #(seal/sealed? p %) feet threats #(stand-on? p %))))

(defn placeable?
  "Whether a block placed in cell has a neighbour (any of the six) whose collision shape fills its cell to be placed against."
  [p {:keys [x y z]}]
  (boolean (some (fn [[dx dy dz]] (:full-cube? (u/seen-facts p {:x (+ x dx) :y (+ y dy) :z (+ z dz)})))
                 rules/neighbour-deltas)))

(a/defargs settings
  {::support-depth {:default 3 :doc "How many cells under a wall cell with nothing beside it a shelter fills to give it a block to be placed against." :spec (a/int-in 0 nil)}})

(defn support-chain
  "The cells to fill, lowest first, so a block can then be placed in cell: [] when cell is placeable? already, the cells
  under it down to one that is (at most depth of them), nil when none is (or one is wet or in avoid, the body's cells)."
  [p cell avoid depth]
  (if (placeable? p cell)
    []
    (let [below (update cell :y dec)]
      (when (and (pos? depth) (not (avoid below)) (not (wet? p below)))
        (some-> (support-chain p below avoid (dec depth)) (conj below))))))

(defn supported-order
  "cells that can be placed in their order: placeable? now, or beside (any of the six) an earlier one that is placed first."
  [p cells]
  (let [beside (fn [{:keys [x y z]}] (map (fn [[dx dy dz]] {:x (+ x dx) :y (+ y dy) :z (+ z dz)}) rules/neighbour-deltas))
        ok (loop [ok #{}]
             (let [more (into #{} (filter #(and (not (ok %)) (or (placeable? p %) (some ok (beside %))))) cells)]
               (if (empty? more) ok (recur (into ok more)))))]
    (filterv ok cells)))

(defn bottom-up
  "The next cells to place from the open cells, in order: the supported-order ones, else the first one with the support-chain
  under it (the cell alone when it has none: the place then says no-support)."
  [p cells feet]
  (let [avoid #{feet (update feet :y inc)}
        ready (supported-order p cells)]
    (if (or (seq ready) (empty? cells))
      ready
      (conj (vec (support-chain p (first cells) avoid (settings/get settings ::support-depth))) (first cells)))))

(defn door
  "The door cells [feet head] of one side among the placed cells: the first side
  with both cells placed, else the first side placed at feet height plus the
  cell above it, else nil."
  [placed {:keys [x y z]}]
  (let [at (fn [[dx dz] dy] {:x (+ x dx) :y (+ y dy) :z (+ z dz)})
        feet-placed (filter #(contains? placed (at % 0)) sides)
        both (first (filter #(contains? placed (at % 1)) feet-placed))
        side (or both (first feet-placed))]
    (when side [(at side 0) (at side 1)])))

(defn note-placed!
  "Job memory: cell is a placed cell, and one more of item is spent (:spent {item count}, which the sealed event reports).
  The block also goes to the scaffold ledger (purpose :shelter) for go-to's goal-enclosed clearing and recover-drops."
  [c cell item]
  (let [at (if (map? cell) [(:x cell) (:y cell) (:z cell)] (vec cell))]
    (ledger/remember! c (-> (ledger/intend (ledger/open-entries (ctx/view c)) {:cell at :item item :before "air" :job (:id c) :purpose :shelter})
                            (ledger/confirm at))))
  (ctx/update-mem! c #(-> % (update :placed (fnil conj #{}) cell) (assoc-in [:placed-items cell] item) (update-in [:spent item] (fnil inc 0)))))

(defn sealed-in?
  "Whether the world shows the body shut in: a roof within roof-height above (a solid block, or a cell that stops a mob,
  sealed?: leaves, glass) and, for walls mode, no open cell around the feet (checked from the blocks, not from what was placed)."
  [p mode roof-height]
  (and (or (sh-sense/roofed? p roof-height)
           (and (= :walls mode)
                (let [{:keys [x y z]} (sh-sense/feet p)]
                  (some #(seal/sealed? p {:x x :y (+ y %) :z z}) (range 1 (inc roof-height))))))
       (or (not= :walls mode) (empty? (open-cells p (sh-sense/feet p))))))

(defn supported?
  "Whether a block placed in cell has a side neighbour to be placed against: one whose collision shape fills its cell
  (full-cube?; the place step needs a collision box, so a flower or a crop is no support)."
  [p {:keys [x y z]}]
  (boolean (some (fn [[dx dz]] (:full-cube? (u/seen-facts p {:x (+ x dx) :y y :z (+ z dz)}))) sides)))

(defn dig-plan
  "{:roof :depth} for a pit dug from start: the roof goes at start when a side of it is solid (the pit is 2 deep), else
  one lower, in the ground layer, when a side of that is solid (3 deep: flat ground has nothing beside the start cell
  to place the roof against). nil when neither can be roofed."
  [p start]
  (let [below (update start :y dec)]
    (cond
      (supported? p start) {:roof start :depth 2}
      (supported? p below) {:roof below :depth 3})))

(defn pit-steps
  "The drops of a pit dug depth deep from start, as {:steps [{:dig cells to open, top first; :to the feet cell dropped
  into; :floor the cell under it}] :plugs :roof}. With no side it goes straight down (each dig is under the feet).
  With side [dx dz] it zigzags between start's column and the side's, so every dig is beside or below-beside the body
  and the floor of each drop shows through the open column before the drop: the body's eye sees a side column's
  cells down to 2 under its feet. :plugs is the side column's cell beside the head at the bottom; :roof is over the head."
  [{:keys [x y z]} side depth]
  (let [[dx dz] side
        col (fn [i yy] (if (odd? i) {:x (+ x dx) :y yy :z (+ z dz)} {:x x :y yy :z z}))
        bottom (- y depth)]
    {:steps (mapv (fn [i] {:dig (if (and side (> i 1)) [(col i (- y i -1)) (col i (- y i))] [(col (if side i 0) (- y i))])
                           :to (col (if side i 0) (- y i))
                           :floor (col (if side i 0) (- y i 1))})
                  (range 1 (inc depth)))
     :plugs (if side [(col (inc depth) (inc bottom))] [])
     :roof (col (if side depth 0) (+ bottom 2))}))

(defn known-solid?
  "Whether the body has sensed cell and it is solid: a floor it may drop onto (a guess never is)."
  [p cell]
  (solid/solid? (u/seen-name p cell)))

(defn open-cell?
  "Whether the body sees cell open: sensed, not solid and dry."
  [p cell]
  (let [n (u/seen-name p cell)]
    (and (some? n) (not (solid/solid? n)) (not (wet? p cell)))))

(defn roof-held?
  "Whether a block placed at roof has a side to be placed against other than open (the pit's open cell beside it): a
  seen full cube, or rock read under the ground."
  [p roof open]
  (boolean (some (fn [[dx dz]]
                   (let [n (assoc roof :x (+ (:x roof) dx) :z (+ (:z roof) dz))]
                     (and (not= n open) (or (:full-cube? (u/seen-facts p n)) (and (nil? (u/seen-name p n)) (dig-look/rock-solid? p n))))))
                 sides)))

(defn pit-shapes
  "The pits that may be dug from feet (pit-steps), straight first: straight down only when the body knows the
  floor under every drop (it cannot see under the block it stands on), else a zigzag over each open side, 2 deep under a
  roof with a side to hold it, else 3."
  [p feet side-ok?]
  (let [straight (when-let [{:keys [depth]} (dig-plan p feet)]
                   (let [shape (pit-steps feet nil depth)]
                     (when (every? #(known-solid? p (:floor %)) (:steps shape)) shape)))
        zigzag (for [[dx dz :as side] sides
                     :let [b (assoc feet :x (+ (:x feet) dx) :z (+ (:z feet) dz))
                           depth (cond (roof-held? p feet b) 2
                                       (roof-held? p (update b :y dec) (update feet :y dec)) 3)]
                     :when (and depth (side-ok? b))]
                 (pit-steps feet side depth))]
    (remove nil? (cons straight zigzag))))

(defn shaft-top
  "When the feet cell is the bottom of a 1x1 shaft (no side open at feet height): the first height up the open column
  with a side open, where the shaft opens out (its :start, the height to climb to); nil when a side is open at feet
  height, or the column meets a solid cell or runs on for 32 cells first."
  [p {:keys [x y z]}]
  (let [open-side? (fn [yy] (some (fn [[dx dz]] (not (sh-sense/solid-at? p {:x (+ x dx) :y yy :z (+ z dz)}))) sides))]
    (when-not (open-side? y)
      (loop [yy (inc y)]
        (cond
          (> yy (+ y 32)) nil
          (sh-sense/solid-at? p {:x x :y yy :z z}) nil
          (open-side? yy) yy
          :else (recur (inc yy)))))))
