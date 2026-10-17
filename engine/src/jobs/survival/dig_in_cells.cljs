(ns jobs.survival.dig-in-cells
  "The cells a shelter fills and the placing of blocks into them (read from the world, placed, shut): jobs.survival.dig-in
  and jobs.survival.retreat use them."
  (:require [engine.args :as a]
            [clojure.string :as string]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as lb]
            [jobs.lib.click :as click]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.tidy :as tidy]
            [engine.ctx :as ctx]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.shelter :as sh]
            [jobs.lib.solid :as solid]
            [jobs.lib.tools :as tools]
            [jobs.lib.look :as look]
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

(defn placeable?
  "Whether a block placed in cell has a neighbour (any of the six) whose collision shape fills its cell to be placed against."
  [p {:keys [x y z]}]
  (boolean (some (fn [[dx dy dz]] (:full-cube? (u/seen-facts p {:x (+ x dx) :y (+ y dy) :z (+ z dz)})))
                 rules/neighbour-deltas)))

(a/defargs settings
  {::mob-wait-ticks {:default 200 :doc "Game ticks a shelter's place refused for a mob in its cell is retried before the shelter gives up."
                     :spec (a/int-in 0 nil)}
   ::pickup-reach {:default 1 :doc "How far from the body, along the ground, an item is picked up without a walk." :spec (a/int-in 0 nil)}
   ::support-depth {:default 3 :doc "How many cells under a wall cell with nothing beside it a shelter fills to give it a block to be placed against." :spec (a/int-in 0 nil)}
   ::rock-depth {:default 3 :doc "How far behind a seen solid face a cell the body has not sensed is taken for rock." :spec (a/int-in 0 nil)}})

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

(defn mob-wait-ms [] (settings/ticks->ms (settings/get settings ::mob-wait-ticks)))

(defn refusing-mobs
  "The names of the entities a refused place reports near its cell (the mob in the way), nil when none."
  [r]
  (seq (map #(or (.-name %) (.-kind %)) (some-> r .-refusal .-entities))))

(defn mob-text
  "', <names> stays in the cell' for a refused place r that named mobs, else ''."
  [r]
  (if-let [mobs (refusing-mobs r)] (str ", " (string/join ", " mobs) " stays in the cell") ""))

(defn keep-waiting!
  "Whether a place refused for a mob (r) is still within mob-wait-ms of the first such refusal, kept in job memory under
  key k (cleared by forget-wait!); false when r names no mob or the wait is over."
  [c k r]
  (let [now (ctx/now c)
        since (or (k (ctx/mem c)) now)]
    (boolean (when (and (refusing-mobs r) (<= (- now since) (mob-wait-ms)))
               (ctx/update-mem! c assoc k since)
               true))))

(defn forget-wait! [c k] (ctx/update-mem! c dissoc k))

(defn full-cube? [c cell]
  (boolean (some-> (u/seen-block (:primitives c) cell) .-fullCube)))

(defn note-placed!
  "Job memory: cell is a placed cell, and one more of item is spent (:spent {item count}, which the sealed event reports).
  The block also goes to the scaffold ledger (purpose :shelter) for go-to's goal-enclosed clearing and recover-drops."
  [c cell item]
  (let [at (if (map? cell) [(:x cell) (:y cell) (:z cell)] (vec cell))]
    (ledger/remember! c (-> (ledger/intend (ledger/open-entries (ctx/view c)) {:cell at :item item :before "air" :job (:id c) :purpose :shelter})
                            (ledger/confirm at))))
  (ctx/update-mem! c #(-> % (update :placed (fnil conj #{}) cell) (assoc-in [:placed-items cell] item) (update-in [:spent item] (fnil inc 0)))))

(defn bed?
  "Whether a bed stands at cell: a shelter never digs one (the spawn bed would go with it)."
  [p cell]
  (string/ends-with? (or (u/seen-name p cell) "") "_bed"))

(defn ^:async clear-over!
  "A place was answered occupied at cell: dig out a block a mob walks through (a torch, a sapling) once, so the place can be
  tried again. True when it was dug. Else cell goes in occupied-key and is not tried again: a full block, a bed, one
  already dug once (cleared-key), one in another's area, a dig that fails."
  [c cell cleared-key occupied-key]
  (let [p (:primitives c)
        leave! #(do (ctx/update-mem! c update occupied-key (fnil conj #{}) cell) false)]
    (if (or (nil? (u/seen-block p cell)) (full-cube? c cell) (bed? p cell) (contains? (get (ctx/mem c) cleared-key) cell)
            (access/trespass-refusal c :dig cell))
      (leave!)
      (let [_ (await (tools/equip-for! c (u/seen-name p cell) {:fast true}))
            d (await (tidy/dig! c cell true))]
        (ctx/update-mem! c update cleared-key (fnil conj #{}) cell)
        (if (= "dug" (.-status d)) true (leave!))))))

(defn ^:async place-all!
  "Place blocks at cells in order. Resolves to :ok, :wait (a mob stands in the cell: the place was refused and is retried
  for mob-wait-ms, the caller yields in between), or the first status that is not placed or occupied.
  A cell occupied by a block that fills it is sealed already and goes in :occupied.
  A cell occupied by a block a mob walks through is dug once and placed again, never a bed (it goes in :occupied).
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
                    status (some-> r .-status)
                    _ (when-not (refusing-mobs r) (forget-wait! c :mob-since))]
                (cond
                  (= :shut door) (recur (rest cells))

                  (= :open door)
                  (do (ctx/update-mem! c update :occupied (fnil conj #{}) cell)
                      (recur (rest cells)))

                  (= "placed" status)
                  (do (note-placed! c cell item)
                      (forget-wait! c :mob-since)
                      (recur (rest cells)))

                  (and (not= "occupied" status) (keep-waiting! c :mob-since r))
                  :wait

                  (not= "occupied" status)
                  (do (when (not= "no-item" status)
                        (ctx/update-mem! c update-in [:refused cell] (fnil inc 0)))
                      (ctx/update-mem! c assoc :mob-text (mob-text r))
                      status)

                  :else
                  (if (await (clear-over! c cell :cleared :occupied))
                    (recur cells)
                    (recur (rest cells)))))))))

(defn sealed-in?
  "Whether the world shows the body shut in: a roof within roof-height above (a solid block, or a cell that stops a mob,
  sealed?: leaves, glass) and, for walls mode, no open cell around the feet (checked from the blocks, not from what was placed)."
  [p mode roof-height]
  (and (or (sh/roofed? p roof-height)
           (and (= :walls mode)
                (let [{:keys [x y z]} (sh/feet p)]
                  (some #(sealed? p {:x x :y (+ y %) :z z}) (range 1 (inc roof-height))))))
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

(declare rock-solid?)

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
                     (and (not= n open) (or (:full-cube? (u/seen-facts p n)) (and (nil? (u/seen-name p n)) (rock-solid? p n))))))
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

(defn pickup-reach [] (settings/get settings ::pickup-reach))

(defn in-reach-ids
  "The ids of the item entities among ids that lie within pickup-reach of the body (a step or less above or below)."
  [c ids]
  (let [p (:primitives c)
        at (u/self-pos c)
        wanted (set ids)]
    (->> (look/seen-items p {:radius 4 :max 64})
         (filter #(wanted (.-id %)))
         (filter (fn [e] (let [q (u/pos-of (.-pos e))]
                           (and (<= (js/Math.hypot (- (:x q) (:x at)) (- (:z q) (:z at))) (pickup-reach))
                                (<= (js/Math.abs (- (:y q) (:y at))) 1)))))
         (mapv #(.-id %)))))

(defn ^:async collect-pit-drops!
  "Pick up the placeable drops of the pit's digs (memory :pit-drops) that fell into the body's column at or over its
  feet and lie within its pickup reach: a drop that drifted further is left, so taking drops never walks the body off
  the floor it stands on."
  [c {:keys [x y z]}]
  (let [here? (fn [{cell :cell}] (and (= x (:x cell)) (= z (:z cell)) (>= (:y cell) y)))
        drops (filter here? (:pit-drops (ctx/mem c)))]
    (loop [[id & more] (in-reach-ids c (map :id drops))]
      (when id
        (await (ctx/act c :collect #js {:id id}))
        (recur more)))
    (ctx/update-mem! c update :pit-drops #(vec (remove here? %)))))

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

(def neighbours rules/neighbour-deltas)

(defn shift [{:keys [x y z]} [dx dy dz]] {:x (+ x dx) :y (+ y dy) :z (+ z dz)})

(defn rock-depth [] (settings/get settings ::rock-depth))

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
                     (some (fn [d] (some solid/solid? (take 1 (remove nil? (map #(name-at d %) (range 1 (inc (rock-depth))))))))
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
