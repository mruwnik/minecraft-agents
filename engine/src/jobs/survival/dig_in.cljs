(ns jobs.survival.dig-in
  (:require [engine.jobs.tidy :as tidy]
            [engine.ctx :as ctx]
            [engine.jobs.access :as access]
            [engine.jobs.shelter :as sh]
            [engine.jobs.util :as u]))

(def doc
  "Roof the body in for the night. Check: it is night and nothing solid is
  within :roof-height blocks above. With enough :blocks carried to fill every
  open cell, it walls a 1x1 shelter: the four sides at feet height, the four at
  head height, a support cell beside the roof cell, then one above the head, at
  most :max-places placements per round. Walls mode converges: every round it
  computes the open cells from the body's current feet cell, so a body that
  was moved is walled in where it now stands. With fewer blocks it digs down
  two, but only while the block under each one is solid and no lateral water
  or lava borders the descent cell (collecting the blocks it digs), and places one
  above, at the cell the body stood in, from a carried or dug block. If a dig
  drops no placeable block and none is carried it stops at once (a
  dig_in_failed warn, \"nothing to roof the pit with\") instead of leaving the
  body in a roofless pit, and remembers the column's start cell as a
  :dig-in-futile entry (cap 5, 10 minutes). Its check declines while it carries
  no placeable block and such an entry lies within 8 blocks of the body, so a
  re-fired reflex does not dig a deeper pit every time. Fluid-adjacent sites,
  failed descent and failed roofing are remembered with a :reason and block
  retries even with carried blocks. Descent without vertical progress gives
  up after three attempts. On flat ground the start cell has no solid side neighbour to roof against, so the pit is
  3 deep and roofed in the ground layer one below the start cell (a 2-deep pit when a side of the start cell is solid);
  with no solid side at either height it does not dig (dig_in_failed warn, :no-roof-support site). Dig mode keeps its :roof and :target-y, but if the
  body's x or z no longer matches that column it chooses again from the
  current cell. Returns :continue until roofed.
  When it ends, however it ends, it writes a :shelter entry {:pos :roof :state
  :built} (cap 10, kept one in-game day) with :pos the current feet cell;
  :roof is the cell it actually placed above the body, absent when none was.
  In walls mode the entry also has :door, the feet-height and head-height
  cells of one side it placed itself; the pit has no :door. The entry is
  history for now: nothing reads it. A body that cannot place or dig gives up
  after three failures with a dig_in_failed warn. The mode is the first of walls (when enough blocks are carried)
  and dig whose cells are all permitted by the zone rules; when every way is in another's zone or claim it takes the
  first anyway, as a last resort, with one dig-in.trespass-last-resort warn (a missing zone list changes nothing).
  A cell the place primitive reports occupied counts as sealed only when
  the block fills it (leaves); a block a mob walks through (torch, sapling,
  cobweb) is dug out once and placed again, and a cell that is occupied
  again, or whose dig fails, is given up on, so the job always ends.
  Memory: writes :shelter and :dig-in-futile; reads :dig-in-futile.")

(def building-blocks
  ["dirt" "cobblestone" "cobbled_deepslate" "stone" "andesite" "diorite" "granite" "netherrack"
   "oak_planks" "spruce_planks" "birch_planks" "jungle_planks" "acacia_planks" "dark_oak_planks"
   "mangrove_planks" "cherry_planks"])

(def args
  {:roof-height {:doc "a solid block within this many blocks above counts as a roof" :default sh/default-roof-height}
   :blocks {:doc "names of the blocks it may place" :default building-blocks}
   :max-places {:doc "placements per round" :default 4}})

(def shelter-policy {:cap 10 :ttl sh/ms-per-day})

(def futile-policy {:cap 5 :ttl 600000})

(def futile-radius 8)

(def sides [[1 0] [-1 0] [0 1] [0 -1]])

(def hazards #{"lava" "water"})

(defn carried
  "The carried [{:name :count}] whose name is in blocks, in the order of blocks."
  [c blocks]
  (let [have (into {} (map (juxt :name :count)) (u/inventory (:primitives c)))]
    (vec (for [b blocks :let [n (get have b 0)] :when (pos? n)] {:name b :count n}))))

(defn pick [c blocks] (:name (first (carried c blocks))))

(defn remember-failed-site! [c reason]
  (ctx/remember! c :dig-in-futile
                 {:pos (or (:roof (ctx/mem c)) (sh/feet (:primitives c))) :reason reason}
                 futile-policy))

(defn fail-site! [c reason text]
  (let [result (u/fail! c :dig_in_failed text)]
    (when (= :done result) (remember-failed-site! c reason))
    result))

(defn lateral-fluid [p {:keys [x y z]}]
  (some (fn [[dx dz]]
          (let [name (u/block-name p {:x (+ x dx) :y y :z (+ z dz)})]
            (when (hazards name) name)))
        sides))

(def mob-proof-shapes
  "Blocks that stop a mob although their collision shape does not fill the cell: fences (1.5 high), walls, panes,
  iron bars, gates, doors and trapdoors."
  #"(_fence|_fence_gate|_wall|_pane|_door|_trapdoor)$|^iron_bars$")

(defn sealed?
  "Whether a cell already stops a mob: its block fills the cell (blockAt's :fullCube: stone, leaves, glass) or is a
  fence, wall, pane, bar, gate or door. Signs, rails, plates, buttons, levers, carpets, torches, plants, slabs and
  stairs let a mob walk or step through (or leave a gap it fits through), so they are not sealed; nor is an
  unloaded cell."
  [p cell]
  (let [b (.blockAt p (clj->js cell))]
    (boolean (and b (or (.-fullCube b) (re-find mob-proof-shapes (.-name b)))))))

(defn open-cells
  "The cells to fill around the feet cell, in placement order: sides at feet
  height, sides at head height, a support beside the roof cell (a block needs a
  solid face neighbour to be placed against, and the roof cell has none until
  the support exists), then the roof cell above the head; only those not sealed?."
  [p {:keys [x y z]}]
  (filterv #(not (sealed? p %))
           (concat (for [dy [0 1] [dx dz] sides] {:x (+ x dx) :y (+ y dy) :z (+ z dz)})
                   [{:x (inc x) :y (+ y 2) :z z} {:x x :y (+ y 2) :z z}])))

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
  (boolean (some-> (.blockAt (:primitives c) (clj->js cell)) .-fullCube)))

(defn ^:async place-all!
  "Place item-picked blocks at cells in order. Resolves to :ok, or the first
  status that is not placed or occupied (no-item when none is carried). A cell
  reported occupied by a block that fills it (its :fullCube) is sealed already
  and remembered in :occupied. One a mob walks through (a torch, a sapling, a
  cobweb) is dug out once (a last resort, via tidy) and placed again; if that
  second try is occupied too, or the dig fails, the cell is remembered in
  :occupied so it is never tried a third time."
  [c blocks cells]
  (loop [cells cells]
    (let [item (pick c blocks)
          cell (first cells)]
      (cond
        (empty? cells) :ok
        (nil? item) "no-item"
        :else (let [r (await (tidy/place! c cell item true))
                    status (.-status r)]
                (cond
                  (= "placed" status)
                  (do (ctx/update-mem! c update :placed (fnil conj #{}) cell)
                      (recur (rest cells)))

                  (not= "occupied" status) status

                  (or (full-cube? c cell) (contains? (:cleared (ctx/mem c) #{}) cell))
                  (do (ctx/update-mem! c update :occupied (fnil conj #{}) cell)
                      (recur (rest cells)))

                  :else
                  (let [d (await (tidy/dig! c cell true))]
                    (ctx/update-mem! c update :cleared (fnil conj #{}) cell)
                    (when (not= "dug" (.-status d))
                      (ctx/update-mem! c update :occupied (fnil conj #{}) cell))
                    (recur (if (= "dug" (.-status d)) cells (rest cells))))))))))

(defn ^:async walls-round [c]
  (let [{:keys [blocks max-places roof-height]} (:args c)
        p (:primitives c)
        occupied (:occupied (ctx/mem c) #{})
        cells (remove occupied (open-cells p (sh/feet p)))
        status (await (place-all! c blocks (take max-places cells)))]
    (cond
      (empty? cells) :done
      (not= :ok status) (fail-site! c :walls-failed (str "cannot place a block: " status))
      (sh/roofed? p roof-height) :done
      :else :continue)))

(defn ^:async collect-drops!
  "Pick up the placeable blocks a dig dropped."
  [c blocks drops]
  (loop [ds (filter #(some #{(.-name %)} blocks) (array-seq drops))]
    (when (seq ds)
      (await (ctx/act c :collect #js {:id (.-id (first ds))}))
      (recur (rest ds)))))

(defn ^:async descend-round
  "One step down toward the pit: dig the block below the feet, collect what
  it dropped, and step into the hole. Gives up (dig_in_failed warn, done)
  rather than dig when the block below is a hazard or the cell under it is not
  solid (a thin floor over water, lava or air)."
  [c]
  (let [{:keys [blocks]} (:args c)
        p (:primitives c)
        {:keys [x y z]} (sh/feet p)
        below {:x x :y (dec y) :z z}
        name (u/block-name p below)
        under (u/block-name p {:x x :y (- y 2) :z z})
        fluid (lateral-fluid p below)]
    (cond
      fluid (do (remember-failed-site! c :fluid-adjacent)
                (ctx/emit! c :dig_in_failed :warn {:text (str fluid " beside the descent cell; not opening the pit")})
                :done)
      (hazards name) (do (ctx/emit! c :dig_in_failed :warn {:text (str name " below the body; not digging down")})
                         :done)
      (and (sh/solid-at? p below) (not (sh/solid? under)))
      (do (ctx/emit! c :dig_in_failed :warn {:text (str (or under "an unloaded cell") " under the floor; not digging through it")})
          :done)
      (not (sh/solid-at? p below))
      (let [before (:y (sh/feet p))
            r (await (ctx/act c :moveTo (clj->js {:pos below :range 0.5})))]
        (if (< (:y (sh/feet p)) before)
          (do (ctx/update-mem! c dissoc :failures) :continue)
          (fail-site! c :descent-stalled (str "cannot descend into the pit: " (.-status r)))))
      :else (let [r (await (tidy/dig! c below true))]
              (if (= "dug" (.-status r))
                (let [placeable (some #(some #{(.-name %)} blocks) (array-seq (.-drops r)))]
                  (await (collect-drops! c blocks (.-drops r)))
                  (if (or placeable (some? (pick c blocks)))
                    :continue
                    (do (ctx/remember! c :dig-in-futile {:pos (:roof (ctx/mem c))} futile-policy)
                        (ctx/emit! c :dig_in_failed :warn {:text "nothing to roof the pit with"})
                        :done)))
                (u/fail! c :dig_in_failed (str "cannot dig down: " (.-status r))))))))

(defn ^:async roof-round
  "In the pit: place one block at the cell the body started in."
  [c]
  (let [{:keys [blocks]} (:args c)
        item (pick c blocks)
        roof (:roof (ctx/mem c))]
    (if (nil? item)
      :done
      (let [r (await (tidy/place! c roof item true))]
        (if (#{"placed" "occupied"} (.-status r))
          (do (when (= "placed" (.-status r)) (ctx/update-mem! c update :placed (fnil conj #{}) roof))
              :done)
          (fail-site! c :roof-failed (str "cannot roof the pit: " (.-status r))))))))

(defn supported?
  "Whether a block placed in cell has a solid side neighbour to be placed against."
  [p {:keys [x y z]}]
  (boolean (some (fn [[dx dz]] (sh/solid-at? p {:x (+ x dx) :y y :z (+ z dz)})) sides)))

(defn dig-plan
  "{:roof :depth} for a pit dug from start: the roof goes at start when a side of it is solid (the pit is 2 deep), else
  one lower, in the ground layer, when a side of that is solid (3 deep: flat ground has nothing beside the start cell
  to place the roof against). nil when neither can be roofed."
  [p start]
  (let [below (update start :y dec)]
    (cond
      (supported? p start) {:roof start :depth 2}
      (supported? p below) {:roof below :depth 3})))

(defn mode-choice
  "[mode refusal] for the shelter from start: the first of :walls (only when walls-ok?) and :dig whose cells are all
  permitted, else the first of them with its refusal (nil when permitted). dig-plan is the pit's {:roof :depth}, nil
  when it cannot be roofed (the start cell's rules are then checked)."
  [c start walls-cells walls-ok? dig-plan]
  (let [in (access/rules-input c)
        {:keys [roof depth]} (or dig-plan {:roof start :depth 2})
        walls-v (some #(access/trespass-refusal in :place %) walls-cells)
        dig-v (or (some #(access/trespass-refusal in :dig %) (map #(update start :y - %) (range 1 (inc depth))))
                  (access/trespass-refusal (assoc in :feet nil) :place roof))
        options (cond-> [] walls-ok? (conj [:walls walls-v]) :always (conj [:dig dig-v]))]
    (or (first (filter (comp nil? second) options)) (first options))))

(defn choose-mode
  "Record in job memory how this shelter is built. Walls mode stores only
  :mode :walls (the cells are recomputed from the feet every round). Dig mode
  stores :roof, the starting cell, and :target-y. Chosen once, again only when
  the body leaves a dig-mode column. See mode-choice for the zone rule."
  [c]
  (let [p (:primitives c)
        {:keys [mode roof]} (ctx/mem c)
        {:keys [x z] :as start} (sh/feet p)
        moved (and (= :dig mode) (not (and (= x (:x roof)) (= z (:z roof)))))]
    (when moved (ctx/update-mem! c dissoc :mode :roof :target-y))
    (when (or moved (not mode))
      (let [cells (open-cells p start)
            have (reduce + (map :count (carried c (:blocks (:args c)))))
            plan (dig-plan p start)
            [chosen refusal] (mode-choice c start cells (>= have (count cells)) plan)]
        (access/trespass! c "dig-in" refusal)
        (cond
          (= :walls chosen) (ctx/update-mem! c assoc :mode :walls)
          (nil? plan) (ctx/update-mem! c assoc :mode :no-roof-support)
          :else (ctx/update-mem! c assoc :mode :dig :roof (:roof plan) :start start
                                 :target-y (- (:y start) (:depth plan))))))))

(defn futile-nearby?
  "Whether a recent failed site blocks digging here. Material-only failures
  can be retried after collecting blocks; unsafe or inaccessible sites cannot."
  [c]
  (let [here (u/self-pos c)
        have-blocks (seq (carried c (:blocks (:args c))))]
    (boolean (some #(and (or (:reason (:data %)) (not have-blocks))
                         (<= (u/dist here (:pos (:data %))) futile-radius))
                   (ctx/entries c :dig-in-futile)))))

(defn check [c]
  (and (sh/night? (:primitives c))
       (not (sh/roofed? (:primitives c) (:roof-height (:args c))))
       (not (futile-nearby? c))))

(defn no-roof-round
  "No cell of the pit could be roofed (nothing solid beside the start cell or the ground cell under it): do not dig,
  since the body would be left in an open pit."
  [c]
  (remember-failed-site! c :no-roof-support)
  (ctx/emit! c :dig_in_failed :warn {:text "nothing solid beside the roof cell to place against; not digging a pit"})
  :done)

(defn ^:async step [c]
  (choose-mode c)
  (let [{:keys [mode target-y]} (ctx/mem c)]
    (cond
      (= :walls mode) (await (walls-round c))
      (= :no-roof-support mode) (no-roof-round c)
      (> (:y (sh/feet (:primitives c))) target-y) (await (descend-round c))
      :else (await (roof-round c)))))

(defn ^:async round [c]
  (let [r (await (step c))]
    (when (= :done r)
      (let [p (:primitives c)
            feet (sh/feet p)
            placed (:placed (ctx/mem c) #{})
            roof (update feet :y + 2)
            roof (if (= :walls (:mode (ctx/mem c))) roof (:roof (ctx/mem c)))
            door (when (= :walls (:mode (ctx/mem c))) (door placed feet))]
        (ctx/remember! c :shelter (cond-> {:pos feet :state :built}
                                    (contains? placed roof) (assoc :roof roof)
                                    door (assoc :door door))
                       shelter-policy)))
    r))
