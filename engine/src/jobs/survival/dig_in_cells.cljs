(ns jobs.survival.dig-in-cells
  "The cells a shelter fills and the placing of blocks into them (read from the world, placed, shut): jobs.survival.dig-in
  and jobs.survival.retreat use them."
  (:require [jobs.lib.blocks :as lb]
            [jobs.lib.click :as click]
            [jobs.lib.tidy :as tidy]
            [engine.ctx :as ctx]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.shelter :as sh]
            [jobs.lib.solid :as solid]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]))

(def sides [[1 0] [-1 0] [0 1] [0 -1]])

(def hazards
  "Blocks that are or hold a fluid: a cell opened beside or under one fills."
  #{"lava" "water" "bubble_column" "kelp" "kelp_plant" "seagrass" "tall_seagrass"})

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
  is filled (or none is given), false when a block is missing or a placement fails."
  [c blocks lavas]
  (loop [[lava & more] lavas]
    (cond
      (nil? lava) true
      (nil? (lb/pick c blocks)) false
      (not (#{:placed :already} (await (lb/place-cell! c lava (lb/pick c blocks) {:ignore-zones? true})))) false
      :else (recur more))))

(def mob-proof-shapes
  "Blocks that stop a mob although their collision shape does not fill the cell: fences (1.5 high), walls, panes,
  iron bars, gates, doors and trapdoors."
  #"(_fence|_fence_gate|_wall|_pane|_door|_trapdoor)$|^iron_bars$")

(defn open-openable
  "The block at cell when it is a door, gate or trapdoor (wooden, copper or iron) standing open, else nil."
  [p cell]
  (let [b (u/seen-block p cell)]
    (when (and b (#{:openable :iron} (click/kind-of (.-name b))) (click/reached? :open (click/props-of b)))
      b)))

(defn sealed?
  "Whether a cell already stops a mob: its block fills the cell (blockAt's :fullCube: stone, leaves, glass) or is a
  fence, wall, pane, bar, or a gate, door or trapdoor that is shut (one standing open is a way in). Signs, rails,
  plates, buttons, levers, carpets, torches, plants, slabs and stairs let a mob walk or step through (or leave a gap
  it fits through), so they are not sealed; nor is an unloaded cell."
  [p cell]
  (let [b (u/seen-block p cell)]
    (boolean (and b
                  (not (open-openable p cell))
                  (or (.-fullCube b) (re-find mob-proof-shapes (.-name b)))))))

(defn ^:async shut-open!
  "Shut the door, gate or trapdoor standing open at cell with one click of the hand (jobs.lib.click): :shut when
  it is shut now, :open when it stays open (an iron one, or a click that did nothing), nil when the cell holds none.
  A shelter fills such a cell by shutting it, never by placing into it or digging it."
  [c cell]
  (when-let [b (open-openable (:primitives c) cell)]
    (if (= :openable (click/kind-of (.-name b)))
      (let [r (await (click/click! c cell :closed (.-name b)))]
        (if (= :changed (:outcome r)) :shut :open))
      :open)))

(defn fill-cells
  "The cells to fill around the feet cell, in placement order: sides at feet
  height, sides at head height, a support beside the roof cell (a block needs a
  solid face neighbour to be placed against, and the roof cell has none until
  the support exists; none when the roof cell is sealed already), then the roof cell above the head; only those the
  predicate sealed? (cell -> bool) does not hold for."
  [sealed? {:keys [x y z]}]
  (let [roof {:x x :y (+ y 2) :z z}]
    (filterv #(not (sealed? %))
             (concat (for [dy [0 1] [dx dz] sides] {:x (+ x dx) :y (+ y dy) :z (+ z dz)})
                     (when-not (sealed? roof) [{:x (inc x) :y (+ y 2) :z z}])
                     [roof]))))

(defn open-cells
  "The fill-cells around the feet cell that are not sealed? in the world."
  [p feet]
  (fill-cells #(sealed? p %) feet))

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

(defn full-cube? [c cell]
  (boolean (some-> (u/seen-block (:primitives c) cell) .-fullCube)))

(defn ^:async place-all!
  "Place blocks at cells in order. Resolves to :ok, or the first status that is not placed or occupied.
  A cell occupied by a block that fills it is sealed already and goes in :occupied.
  A cell occupied by a block a mob walks through is dug once and placed again.
  If that second try is occupied too, or the dig fails, the cell goes in :occupied and is not tried again.
  A cell that is sealed by now (the other half of a door just shut) is skipped.
  An open door, gate or trapdoor is shut (shut-open!), never placed into or dug. One that stays open goes in :occupied."
  [c blocks cells]
  (loop [cells cells]
    (let [item (lb/pick c blocks)
          cell (first cells)]
      (cond
        (empty? cells) :ok
        (sealed? (:primitives c) cell) (recur (rest cells))
        (nil? item) "no-item"
        :else (let [door (await (shut-open! c cell))
                    r (when-not door (await (tidy/place! c cell item true)))
                    status (some-> r .-status)]
                (cond
                  (= :shut door) (recur (rest cells))

                  (= :open door)
                  (do (ctx/update-mem! c update :occupied (fnil conj #{}) cell)
                      (recur (rest cells)))

                  (= "placed" status)
                  (do (ctx/update-mem! c update :placed (fnil conj #{}) cell)
                      (recur (rest cells)))

                  (not= "occupied" status)
                  (do (when (not= "no-item" status)
                        (ctx/update-mem! c update-in [:refused cell] (fnil inc 0)))
                      status)

                  (or (full-cube? c cell) (contains? (:cleared (ctx/mem c) #{}) cell))
                  (do (ctx/update-mem! c update :occupied (fnil conj #{}) cell)
                      (recur (rest cells)))

                  :else
                  (let [_ (await (tools/equip-for! c (u/seen-name (:primitives c) cell) {:fast true}))
                        d (await (tidy/dig! c cell true))]
                    (ctx/update-mem! c update :cleared (fnil conj #{}) cell)
                    (when (not= "dug" (.-status d))
                      (ctx/update-mem! c update :occupied (fnil conj #{}) cell))
                    (recur (if (= "dug" (.-status d)) cells (rest cells))))))))))

(defn sealed-in?
  "Whether the world shows the body shut in: a roof within roof-height above and, for walls mode, no open cell around
  the feet (checked from the blocks, not from what was placed)."
  [p mode roof-height]
  (and (sh/roofed? p roof-height)
       (or (not= :walls mode) (empty? (open-cells p (sh/feet p))))))

(defn ^:async collect-drops!
  "Pick up the placeable blocks a dig dropped."
  [c blocks drops]
  (loop [ds (filter #(some #{(.-name %)} blocks) (array-seq drops))]
    (when (seq ds)
      (await (ctx/act c :collect #js {:id (.-id (first ds))}))
      (recur (rest ds)))))

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

(def room-limit
  "Most cells a closed room may have (the flood from the feet stops there and the room counts as open)."
  256)

(def room-reach
  "Farthest a closed room's cell may be from the feet along any axis."
  8)

(defn room-wall?
  "Whether a cell bounds a room against mobs: sealed?, and not an openable door, gate or trapdoor standing open."
  [p cell]
  (let [b (u/seen-block p cell)]
    (and (sealed? p cell)
         (not (and (= :openable (click/kind-of (.-name b))) (click/reached? :open (click/props-of b)))))))

(def neighbours [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])

(defn shift [{:keys [x y z]} [dx dy dz]] {:x (+ x dx) :y (+ y dy) :z (+ z dz)})

(def rock-depth "How far behind a seen solid face a cell the body has not sensed is taken for rock." 3)

(defn rock-name
  "The block name at cell as a player reads rock: what the body sees or remembers, else \"stone\" for a loaded cell it has
  not sensed when a seen solid block lies behind it within rock-depth cells along some axis and no neighbour is seen
  open (an open neighbour would have shown it). nil when it is not loaded or no seen face is near. The shelter plans
  (pit, pocket, niche) read the rock they dig through this way, then dig to see (jobs.lib.dig-look)."
  [p cell]
  (or (u/seen-name p cell)
      (when (some-> (u/sensed p cell) .-unknown)
        (let [name-at (fn [d k] (u/seen-name p (shift cell (mapv #(* k %) d))))]
          (when (and (every? #(or (nil? %) (solid/solid? %)) (map #(name-at % 1) neighbours))
                     (some (fn [d] (some solid/solid? (take 1 (remove nil? (map #(name-at d %) (range 1 (inc rock-depth)))))))
                           neighbours))
            "stone")))))

(defn rock-solid?
  "Whether cell is solid rock as the body reads it (rock-name)."
  [p cell]
  (solid/solid? (rock-name p cell)))

(defn room-cells
  "The cells reachable from the feet cell through cells that are not room-wall? (six neighbours, the cell plug counted
  as a wall) when they form a closed room: at most room-limit of them, none farther than room-reach from the feet on
  any axis, none unloaded. nil when they do not."
  [p {fx :x fy :y fz :z :as feet} plug]
  (loop [todo [feet] seen #{feet}]
    (if-let [{:keys [x y z] :as cell} (peek todo)]
      (cond
        (> (count seen) room-limit) nil
        (< room-reach (max (js/Math.abs (- x fx)) (js/Math.abs (- y fy)) (js/Math.abs (- z fz)))) nil
        (nil? (u/seen-block p cell)) nil
        :else (let [next (for [d neighbours
                               :let [n (shift cell d)]
                               :when (and (not (seen n)) (not= n plug) (not (room-wall? p n)))]
                           n)]
                (recur (into (pop todo) next) (into seen next))))
      seen)))

(defn door-of?
  "Whether a cell next to one of the room's cells holds a door, gate or trapdoor a hand opens: the room's way out."
  [p cells]
  (boolean (some (fn [cell] (some #(= :openable (click/kind-of (u/seen-name p (shift cell %)))) neighbours)) cells)))

(defn room-plug
  "In a closed room with a door whose roof has a hole over the body: the cell to mend, the lowest of y+2 ..
  y+roof-height straight above the feet that is not solid and has a solid side neighbour to be placed against, when
  once filled it leaves the feet in a closed room (room-cells) with a door, gate or trapdoor in its walls (door-of?).
  nil when there is none (open ground, a room with an open door or doorway, a shaft or pit with no door): walls or a
  pit then."
  [p {:keys [x y z] :as feet} roof-height]
  (let [column (map (fn [dy] {:x x :y (+ y dy) :z z}) (range 2 (inc roof-height)))
        plug (first (filter #(and (not (sh/solid-at? p %)) (supported? p %)) column))
        cells (when plug (room-cells p feet plug))]
    (when (and cells (door-of? p cells))
      plug)))

(defn shaft-top
  "When the feet cell is the bottom of a 1x1 shaft (no side open at feet height): the first height up the open column
  with a side open, where the shaft opens out (its :start, the height to climb to); nil when a side is open at feet
  height, or the column meets a solid cell or runs on for 32 cells first."
  [p {:keys [x y z]}]
  (let [open-side? (fn [yy] (some (fn [[dx dz]] (not (sh/solid-at? p {:x (+ x dx) :y yy :z (+ z dz)}))) sides))]
    (when-not (open-side? y)
      (loop [yy (inc y)]
        (cond
          (> yy (+ y 32)) nil
          (sh/solid-at? p {:x x :y yy :z z}) nil
          (open-side? yy) yy
          :else (recur (inc yy)))))))
