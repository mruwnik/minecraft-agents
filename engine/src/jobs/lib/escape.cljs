(ns jobs.lib.escape
  "Reads for a body that cannot walk to its goal from where it stands, so jobs.movement.go-to can pick how to make a
  way (its escalation; jobs.lib.escape.way prices and chooses one):
  - pit-depth: how many levels up from the feet are walled on at least three sides.
  - rim-pillar: how high to pillar beside a wall to step onto its top (a pit too wide for pit-depth).
  - door: the cells of a thin wall straight ahead with a floor beyond it, or a landing at most drop-limit below (jobs.access.clear-path digs them).
  - stair-heading: a heading with a solid block in front to stair up on (jobs.access.stair).
  - stair-cuts: the cells a stair up cuts.
  - door-beside?: whether a door or gate borders the cells the body can walk to (a way out go-to handles).
  Cells are [x y z]. block-at maps a cell to its block name (nil when not loaded)."
  (:require [jobs.lib.choose :as choose]
            [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.blocks :as b]
            [jobs.lib.cost :as cost]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.look :as look]
            [jobs.lib.reach :as reach]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.util :as u]
            [jobs.lib.walk.world :as wworld]))

(a/defargs settings
  {::max-depth {:default 8 :doc "How deep an escape digs or climbs at most." :spec (a/int-in 1 nil)}})

(defn max-depth [] (settings/get settings ::max-depth))

(def wall-search-radius
  "How far (blocks along x and along z) wall-spot looks."
  24)

(def cardinals [[1 0] [-1 0] [0 1] [0 -1]])

(def heading-names {[1 0] :east [-1 0] :west [0 1] :south [0 -1] :north})

(def heading-dirs (into {} (map (fn [[k v]] [v k])) heading-names))

(def protected
  "Blocks a way out never digs: doors, gates, trapdoors, beds, containers, signs, and what cannot be broken."
  #"_door$|_gate$|_trapdoor$|_bed$|chest$|barrel$|shulker_box$|furnace$|^smoker$|^hopper$|^dispenser$|^dropper$|^brewing_stand$|^lectern$|_sign$|^spawner$|^bedrock$|^barrier$|portal|^command_block$|^structure_block$|^jigsaw$")

(defn block-at-of
  "block-at over what the body at primitives p senses: a cell it has not sensed is rock (access/hidden-guess, so a dig
  goes ahead and looks), nil when not loaded."
  [p]
  (access/sensed-at p access/hidden-guess))

(defn unseen-of
  "(unseen? cell): the body at primitives p has not sensed cell (loaded, never seen)."
  [p]
  (fn [cell] (dig-look/unknown? p cell)))

(defn dig-share-fn
  "(fn [cell]) what share of a rock dig an unsensed cell is expected to cost: the measured chance it is solid
  (look/unseen-solid-at-fn), 1 for a sensed cell."
  [p]
  (let [unseen? (unseen-of p)
        solid-at (look/unseen-solid-at-fn p)]
    (fn [cell] (if (unseen? cell) (solid-at cell) 1))))

(defn add [[x y z] [dx dy dz]] [(+ x dx) (+ y dy) (+ z dz)])

(defn up [cell n] (add cell [0 n 0]))

(defn unseen-pillar-blocks
  "How many of the height blocks of a pillar up from feet have their cell or one of the two over it unseen."
  [p feet height]
  (let [unseen? (unseen-of p)]
    (count (filter #(some unseen? (map (fn [dy] (up feet (+ % dy))) [1 2])) (range height)))))

(defn ahead [cell [dx dz] k] (add cell [(* k dx) 0 (* k dz)]))

(defn solid?
  "A loaded block that is neither passable (block-sets/passable?) nor a fluid."
  [n]
  (boolean (and n (not (bs/passable? n)) (not (bs/fluids n)))))

(defn open-sides-fn
  "(fn [cell]) how many of the four sides of a body's feet and head cells at cell are expected open: a side is held by a
  solid block at feet or head, a seen cell solid or not, an unseen one at the measured chance it is solid
  (look/unseen-solid-at-fn)."
  [p]
  (let [block-at (block-at-of p)
        unseen? (unseen-of p)
        solid-at (look/unseen-solid-at-fn p)
        held (fn [cell] (cond (unseen? cell) (solid-at cell) (solid? (block-at cell)) 1 :else 0))]
    (fn [cell] (transduce (map (fn [[dx dz]] (* (- 1 (held (add cell [dx 0 dz]))) (- 1 (held (add (up cell 1) [dx 0 dz])))))) + 0 cardinals))))

(defn pillar-open-sides
  "[n ...]: open-sides-fn at each of the height blocks of a pillar up from feet, the cells before each block."
  [p feet height]
  (let [open-sides (open-sides-fn p)]
    (mapv #(open-sides (up feet %)) (range height))))

(defn pillar-knock
  "cost/pillar-knock-of a pillar of height blocks up from feet: each block's open sides of four share its chance."
  [p feet height]
  (cost/pillar-knock-of p (map #(/ % 4) (pillar-open-sides p feet height))))

(defn protected? [n] (boolean (and n (re-find protected n))))

(def layer-top "Cells below this y are in the overworld's bedrock layer (the nether's bedrock is not classed: no job goes there)." -59)

(def layer-bottom "The lowest y of the overworld." -64)

(def layer-classes "One class per y of the bedrock layer: vanilla makes a cell rock never dug with chance 1 at the bottom, falling 0.2 a level." (mapv #(keyword (str "layer" %)) (range layer-bottom layer-top)))

(def guess-classes "The classes of a guessed rock cell (guess-class)." (conj layer-classes :placed :apart))

(defn guess-class
  "The class of an unseen cell at height y that a route guesses as rock, by the names of its seen solid face neighbours: in
  the bedrock layer (below layer-top) the class of its y (layer-classes: the rate falls with y whatever is beside it); above
  it :placed with a protected neighbour that is not bedrock (a chest, a door: a build, not a rock room), else :apart. The
  measured rate of rock never dug differs between them (world-test.guess-rock)."
  [y names]
  (if (< y layer-top)
    (nth layer-classes (- (max y layer-bottom) layer-bottom))
    (if (some #(and (protected? %) (not= "bedrock" %)) names) :placed :apart)))

(defn heading
  "Unit step [dx dz] along the larger horizontal axis from cell a toward cell b, or nil when b is straight above or
  below."
  [[ax _ az] [bx _ bz]]
  (let [dx (- bx ax) dz (- bz az)]
    (cond
      (and (zero? dx) (zero? dz)) nil
      (>= (js/Math.abs dx) (js/Math.abs dz)) [(js/Math.sign dx) 0]
      :else [0 (js/Math.sign dz)])))

(defn walled-at?
  "At least three of the four sides of cell are solid. With unseen? ((unseen? cell)), a level with a side cell the body
  has not sensed is not walled: what it has not seen is not known to be rock."
  ([block-at cell] (walled-at? block-at (constantly false) cell))
  ([block-at unseen? cell]
   (let [sides (map #(ahead cell % 1) cardinals)]
     (and (not-any? unseen? sides)
          (>= (count (filter #(solid? (block-at %)) sides)) 3)))))

(def look-levels
  "Levels from the feet up whose cells beside the body go-to's escalation looks at before it chooses; an unseen cell
  there is rock behind rock."
  4)

(defn pit-depth
  "Levels up from the feet, 0, 1, ..., walled on at least three sides, at most max-depth. Above look-levels a side
  cell the body has not sensed (unseen?) is not known to be rock: the pit reads as ending there."
  ([block-at feet] (pit-depth block-at (constantly false) feet))
  ([block-at unseen? feet]
   (count (take-while #(walled-at? block-at (if (< % look-levels) (constantly false) unseen?) (up feet %))
                      (range (max-depth))))))

(defn rim-height
  "How many levels h (2 to max-depth) the side of feet along d ([dx dz]) is solid from the feet up, with a floor on top
  and room for the body above it; nil when it is not such a wall. A cell over the top the body has not sensed
  (unseen?) counts as room: the wall hides it from below, and the pillar shows it before the step."
  [block-at unseen? feet d]
  (let [side (ahead feet d 1)
        h (count (take-while #(let [cell (up side %)] (and (not (unseen? cell)) (solid? (block-at cell))))
                             (range (inc (max-depth)))))
        room? #(let [cell (up side %)] (or (unseen? cell) (bs/passable? (block-at cell))))]
    (when (and (<= 2 h (max-depth)) (rules/solid-floor? block-at (up side (dec h))) (room? h) (room? (inc h)))
      h)))

(defn rim-pillar
  "The pillar beside a wall that leaves the body a step below the wall's top: h - 1 for a wall h high (rim-height),
  with the feet column clear up to the head of a body on the rim. The wall along dir (toward the goal) when it is
  one, else the lowest; nil when none."
  [block-at unseen? feet dir]
  (let [n-of (fn [d] (when-let [h (rim-height block-at unseen? feet d)]
                       (when (every? #(bs/passable? (block-at (up feet %))) (range 2 (+ h 2)))
                         (dec h))))]
    (or (some-> dir n-of) (some->> cardinals (keep n-of) seq (apply min)))))

(defn stair-cuts
  "The cells a stair up along dir ([dx dz]) cuts from feet in n steps, step by step (plan.shape/step-cells)."
  [feet [dx dz] n]
  (vec (for [i (range n)
             :let [f (add feet [(* i dx) i (* i dz)])
                   nxt (add f [dx 1 dz])]
             cell [(up f 2) (up nxt 1) nxt]]
         cell)))

(def usable-door #"^(?!iron_door$)[a-z_]+_door$|_fence_gate$")

(defn enclosed?
  "reach-room/enclosed?, with every shut door, gate and trapdoor a wall when doors is :never (the walk will not open them).
  unseen: how a cell the body has not seen reads (reach/world-kinds), the worse case for the caller's question."
  [p doors unseen]
  (if (not= :never doors)
    (reach-room/enclosed? p unseen)
    (reach-room/shut-in? p (reach/world-kinds p reach/kind-of unseen))))

(defn walk-enclosed?
  "enclosed? for a go-to with args (its :doors and :unknown; spec its mask spec, nil: a fresh one), over the world its
  plans read (wworld/unseen-kind): the verdict must agree with the way the walk could not find."
  ([c] (walk-enclosed? (:primitives c) (:args c) (:unknown c)))
  ([p args spec]
   (enclosed? p (some-> (:doors args) keyword) (wworld/unseen-kind p (when-not (false? (:unknown args)) (or spec #js {}))))))

(defn walk-spec
  "A fresh mask spec for a walk of job c's child go-to toward goal ([x y z], or nil), seeded as go-to seeds its own (goal, the
  stairs and tunnels the body cut), so the rock guess spares what its plans spare. The cells the walk looked at and could not
  see (its blocked set) are not known to c."
  [c goal]
  (wworld/with-seeds (wworld/unknown-spec c) goal (into (ledger/stair-stands c) (ledger/tunnel-stands c))))

(defn scan-doors
  "Whether a door or gate the body can open borders a cell it can walk to from feet, with room behind it for the body
  (want-room? true) or without (false: a trunk over the cell behind a gate)."
  [p [fx fy fz] want-room?]
  (let [kind-at (reach/seen-lookup p reach-room/body-kind-of)
        block-at (block-at-of p)
        room-at? (fn [x y z] (and (reach/passable? kind-at x y z) (reach/passable? kind-at x (inc y) z)))
        door? (fn [[x y z]] (some (fn [[dx dz]]
                                    (and (some #(some->> (block-at [(+ x dx) (+ y %) (+ z dz)]) (re-find usable-door)) [0 1])
                                         (let [ax (+ x (* 2 dx)) az (+ z (* 2 dz))]
                                           (= want-room? (boolean (or (room-at? ax y az) (room-at? ax (inc y) az)))))))
                                  cardinals))]
    (loop [queue #queue [[fx fy fz]] seen #{[fx fy fz]}]
      (if-let [cell (peek queue)]
        (if (door? cell)
          true
          (let [nbrs (->> (apply reach/forward kind-at cell) (map vec) (remove seen))]
            (if (>= (count seen) reach-room/room-cells)
              false
              (recur (into (pop queue) nbrs) (into seen nbrs)))))
        false))))

(defn open-door-beside?
  "door-beside? without the :doors check: a door with room behind it (scan-doors)."
  [p feet]
  (scan-doors p feet true))

(defn blocked-door-beside?
  "Whether a door the body can open borders its room with no room behind it (a trunk over the cell behind a gate): its
  way out is to clear what stands there."
  [p feet]
  (scan-doors p feet false))

(defn door-beside?
  "Whether a door or gate the body can open is beside, at feet or head height, a cell the body can walk to from feet
  (jobs.lib.reach/forward, at most reach-room/room-cells cells): the body's enclosure has a way out go-to opens or a
  player would, never one to dig round. Iron doors are walls, and so is every door when doors is :never."
  ([p feet] (door-beside? p feet :shut))
  ([p [fx fy fz] doors]
   (and (not= :never doors) (open-door-beside? p [fx fy fz]))))


(defn walled-side? [block-at cell] (boolean (some #(solid? (block-at (ahead cell % 1))) cardinals)))

(defn drop-limit
  "The longest drop in blocks the body at primitives p survives within the walk's settings (cost/drop-limit; default the body's), the limit of a door's landing."
  ([p] (drop-limit p (wworld/walk-settings {})))
  ([p settings] (cost/drop-limit (cost/equipment-of (.-equipment (.self p))) (wworld/self-body p) settings)))

(defn landing
  "The first cell below row (a cell with room for the body and no floor) within max-drop blocks with room and a floor, or
  water over a floor or more water, as {:through cell :fall blocks :on block}, :on the block name landed on (\"water\", or the floor's: cost/landing-factor) and
  :floor-unseen true when unseen? says the floor under it was never sensed (block-at guessed it rock). nil when the
  column under row is not open down to one."
  [block-at unseen? row max-drop]
  (loop [d 1]
    (let [cell (up row (- d))
          below (up cell -1)
          water? (= "water" (block-at cell))]
      (cond
        (> d max-drop) nil
        (and water? (bs/passable-or-water? (block-at (up cell 1))) (or (= "water" (block-at below)) (rules/solid-floor? block-at below)))
        (cond-> {:through cell :fall d :on "water"} (unseen? below) (assoc :floor-unseen true))
        (not-every? bs/passable? (map block-at [cell (up cell 1)])) nil
        (rules/solid-floor? block-at below) (cond-> {:through cell :fall d :on (block-at below)} (unseen? below) (assoc :floor-unseen true))
        :else (recur (inc d))))))

(defn door-from
  "door from row k on (rows before it already open): the wall's solid cells from row k and the first row beyond with
  room for the body and a floor, or, for a row beyond seen open with no floor, the landing below it within
  max-drop (landing: {:fall blocks} too). unseen? (cell -> true for a cell the body has not sensed): a row past
  the first whose cells are unseen or seen open, at least one unseen (a body seeing over the wall sees only some), is
  taken for the far side, {:unseen true}: the dig shows it, and the digger goes on with door-from there."
  [block-at unseen? feet dir k max-drop]
  (loop [k k cells []]
    (let [row (ahead feet dir k)
          pair [(up row 1) row]
          names (map block-at pair)
          floor? (rules/solid-floor? block-at (up row -1))]
      (cond
        (some nil? names) nil
        (and (not floor?) (> k 1) (every? bs/passable? names)) (some->> (landing block-at unseen? row max-drop) (merge {:cells cells}) (#(cond-> % (:floor-unseen %) (assoc :unseen true))))
        (not floor?) nil
        (and (> k 1) (some unseen? pair) (every? #(or (unseen? %) (bs/passable? (block-at %))) pair))
        {:cells cells :through row :unseen true}
        (every? bs/passable? names) (when (> k 1) {:cells cells :through row})
        (some protected? names) nil
        (some bs/fluids names) nil
        :else (recur (inc k) (into cells (filter #(solid? (block-at %)) pair)))))))

(defn door
  "A door through a wall straight ahead of feet along dir: {:cells [...] :through cell}, the wall's solid cells at feet
  and head height (head first, row by row) and the first cell beyond with room for the body and a floor. nil when
  the cell in front is open, a row has no floor, or a cell is protected. How thick a wall is worth digging is the
  caller's price (jobs.lib.cost/dig-cells-s), not a count. With unseen?, a far side the body cannot see is guessed
  (door-from), and a drop beyond it is at most max-drop (drop-limit)."
  [block-at unseen? feet dir max-drop]
  (door-from block-at unseen? feet dir 1 max-drop))

(defn door-key
  "The skip key of the door from feet along heading (a keyword): a door that failed rules out only itself."
  [feet heading]
  [:clear-path feet heading])

(defn skipped-door-cells
  "The cells whose door skip (a set of skip keys) rules out."
  [skip]
  (keep (fn [k] (when (and (vector? k) (= :clear-path (first k))) (second k))) skip))

(defn diggable?
  "Whether an escalation may cut cell: it is open (air or a plant), or not protected and may-dig? allows it (go-to: no
  other owner's zone, claim or plan footprint; outside those every block may be broken, a build too, priced like any dig)."
  [block-at may-dig? cell]
  (let [n (block-at cell)]
    (or (bs/passable? n)
        (and n (not (protected? n)) (boolean (may-dig? cell))))))

(defn stair-heading
  "The first heading of dirs (each [dx dz]) with a solid block in front at feet height to stand on, no fence or wall
  (reach/tall-block: no step climbs onto one) (bridge?: or none, the stair lays that floor) and every cell of the n steps' cuts diggable? (may-dig?), as a keyword
  (:east ...), or nil."
  ([block-at feet dirs] (stair-heading block-at feet dirs 1 (constantly true)))
  ([block-at feet dirs n may-dig?] (stair-heading block-at feet dirs n may-dig? false))
  ([block-at feet dirs n may-dig? bridge?]
   (some (fn [d]
           (let [front (ahead feet d 1)]
             (when (and (or bridge? (solid? (block-at front))) (not (protected? (block-at front)))
                        (not (some->> (block-at front) (re-find reach/tall-block)))
                        (every? #(diggable? block-at may-dig? %) (stair-cuts feet d (max 1 n))))
               (heading-names d))))
         dirs)))

(defn line-cells
  "The cells a block apart on the straight line from cell a to cell b, both left out."
  [a b]
  (let [n (js/Math.ceil (apply js/Math.hypot (map - b a)))]
    (for [i (range 1 n)] (mapv #(js/Math.round (+ %1 (* (- %2 %1) (/ i n)))) a b))))

(defn level-steps
  "(steps cell): the standable cells (seen, jobs.lib.reach/standable-cell?) beside cell, those a step up or down too
  with climb?, within wall-search-radius of feet along x and z."
  [p [hx _ hz] climb?]
  (let [kind-at (reach/seen-lookup p)
        near? (fn [[x _ z]] (and (<= (js/Math.abs (- x hx)) wall-search-radius)
                                 (<= (js/Math.abs (- z hz)) wall-search-radius)))]
    (fn [cell]
      (->> cardinals
           (mapcat (fn [d] (map #(up (ahead cell d 1) %) (if climb? [0 -1 1] [0]))))
           (filter #(and (near? %) (reach/standable-cell? p (zipmap [:x :y :z] %) kind-at)))))))

(defn pillar-item
  "{:item :count}: the carried climb block (choose/fillers-for, so reserves and an :avoid apply) with the largest count, or nil."
  [c]
  (when-let [l (seq (:list (choose/fillers-for c :climb 1)))]
    (let [{:keys [item count]} (apply max-key :count l)] {:item item :count count})))

(defn carried-pillar-item
  "pillar-item over a bare primitives p: the choose/fillers :climb blocks carried, no reserves."
  [p]
  (let [have (->> (u/inventory p)
                  (filter #(some #{(:name %)} (choose/fillers :climb)))
                  (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {}))]
    (when (seq have)
      (let [[item n] (apply max-key val have)] {:item item :count n}))))

(defn gap-floor
  "The floor cell of the 1-cell gap a refused jump crosses from stand cell takeoff to land on stand cell landing, or nil:
  takeoff and landing lie 2 apart on a cardinal line and the gap between is the planner's word, so a dark gap is no
  blocker."
  [landing takeoff]
  (let [[dx _ dz] (map - takeoff landing)]
    (when (and (= 2 (+ (Math/abs dx) (Math/abs dz))) (zero? (* dx dz)))
      (up (add landing [(/ dx 2) 0 (/ dz 2)]) -1))))

(defn door-back
  "For a body in the free part of a shut door's cell feet: the cell on the free side, to step back to and look round
  from, unless it is seen and not standable (an unseen cell is walked to and looked at) or already walked to (visited).
  A door cell the body has not sensed is no known door (nil): the body's own cell is felt, so this is not met live."
  ([p cell] (door-back p cell #{}))
  ([p [x y z] visited]
  (let [kind-at (reach/seen-lookup p)]
    (when-let [[dx dz] (reach-room/panel-step p kind-at {:x x :y y :z z})]
      (let [back [(- x dx) y (- z dz)]]
        (when (let [unseen? (unseen-of p)
                    ok? (fn [cell good?] (or (unseen? cell) (good? kind-at (cell 0) (cell 1) (cell 2))))]
                (and (not (visited back)) (ok? back reach/passable?) (ok? (up back 1) reach/passable?)
                     (ok? (up back -1) #(keyword-identical? :solid (%1 %2 %3 %4)))))
          back))))))

(defn note-hole!
  "Write cell, where block was dug, to the tidy ledger (jobs.lib.tidy, body memory) as a :dig entry with tag merged,
  the :zone, :claim or :plan it trespassed on, and :any-of, the items that put it back (the block or what it drops);
  only when another's zone, claim or plan covers the cell or tag has :mend (else the body's own diggings stay), it is air now
  (or dug? true: a child reported digging it, maybe since out of sight) and no tagged
  entry is there yet (a plain one the dig itself noted is replaced). The :note arg of jobs.access.stair
  (go-to's escalation), so a cut or cancel leaves no hole unknown."
  ([c tag cell block] (note-hole! c tag cell block false))
  ([c tag cell block dug?]
  (let [p (:primitives c)
        cell (vec cell)
        noted (first (filter #(= cell (:cell %)) (tidy/entries c)))
        v (when-not (:escalation noted) (tidy/refusal c :dig (zipmap [:x :y :z] cell)))]
    (when (and (or v (and (:mend tag) (not (:escalation noted)))) (or dug? (bs/air (u/seen-name p (zipmap [:x :y :z] cell)))))
      (when noted (tidy/forget-cell! c cell))
      (tidy/record! c (merge {:cell cell :action :dig :was block :any-of (tidy/put-back-items block (b/drops-of p block))}
                             (select-keys v [:zone :claim :plan]) tag)
                    "air")))))
