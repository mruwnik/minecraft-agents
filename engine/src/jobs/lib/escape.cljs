(ns jobs.lib.escape
  "Reads for a body that cannot walk to its goal from where it stands, so jobs.movement.go-to can pick how to make a
  way (its escalation):
  - pit-depth: how many levels up from the feet are walled on at least three sides.
  - rim-pillar: how high to pillar beside a wall to step onto its top (a pit too wide for pit-depth).
  - door: the cells of a thin wall straight ahead with a floor beyond it (jobs.access.clear-path digs them).
  - stair-heading: a heading with a solid block in front to stair up on (jobs.access.stair).
  - wall-spot: the cheapest cell beside a wall (priced-spot), for a body in a hollow with no wall next to it.
  - stair-cuts: the cells a stair up cuts.
  - door-beside?: whether a door or gate borders the cells the body can walk to (a way out go-to handles).
  - choose: the escalation for a body here and a goal.
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
            [jobs.lib.reach :as reach]
            [jobs.lib.reach.room :as reach-room]
            
            [jobs.lib.tidy :as tidy]
            [jobs.lib.util :as u]
            [jobs.lib.walk.world :as wworld]))

(a/defargs settings
  {::max-depth {:default 8 :doc "How deep an escape digs or climbs at most." :spec (a/int-in 1 nil)}
   ::max-door {:default 3 :doc "The thickest wall (blocks) a :clear-path step digs through." :spec (a/int-in 1 nil)}})

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

(defn add [[x y z] [dx dy dz]] [(+ x dx) (+ y dy) (+ z dz)])

(defn up [cell n] (add cell [0 n 0]))

(defn ahead [cell [dx dz] k] (add cell [(* k dx) 0 (* k dz)]))

(defn solid?
  "A loaded block that is neither passable (block-sets/passable?) nor a fluid."
  [n]
  (boolean (and n (not (bs/passable? n)) (not (bs/fluids n)))))

(defn protected? [n] (boolean (and n (re-find protected n))))

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
  (wworld/with-seeds (wworld/unknown-spec) goal (into (ledger/stair-stands c) (ledger/tunnel-stands c))))

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

(defn door-from
  "door from row k on (rows before it already open): the wall's solid cells from row k and the first row beyond with
  room for the body and a floor. unseen? (cell -> true for a cell the body has not sensed): a row past the first whose
  cells are unseen or seen open, at least one unseen (a body seeing over the wall sees only some), is taken for the far
  side, {:unseen true}: the dig shows it, and the digger goes on with door-from there."
  [block-at unseen? feet dir max-thick k]
  (loop [k k cells []]
    (let [row (ahead feet dir k)
          pair [(up row 1) row]
          names (map block-at pair)]
      (cond
        (not (rules/solid-floor? block-at (up row -1))) nil
        (some nil? names) nil
        (and (> k 1) (some unseen? pair) (every? #(or (unseen? %) (bs/passable? (block-at %))) pair))
        {:cells cells :through row :unseen true}
        (every? bs/passable? names) (when (> k 1) {:cells cells :through row})
        (> k max-thick) nil
        (some protected? names) nil
        (some bs/fluids names) nil
        :else (recur (inc k) (into cells (filter #(solid? (block-at %)) pair)))))))

(defn door
  "A door through a wall straight ahead of feet along dir: {:cells [...] :through cell}, the wall's solid cells at feet
  and head height (head first, row by row) and the first cell beyond with room for the body and a floor. nil when
  the cell in front is open, the wall is thicker than max-thick, a row has no floor, or a cell is protected. With
  unseen?, a far side the body cannot see is guessed (door-from)."
  ([block-at feet dir max-thick] (door block-at (constantly false) feet dir max-thick))
  ([block-at unseen? feet dir max-thick] (door-from block-at unseen? feet dir max-thick 1)))

(defn log? [n] (boolean (and n (re-find #"_log$" n))))

(def tree-gap
  "How far up a trunk is followed past cells already cut out of it."
  12)

(defn tree-log?
  "Whether the log at cell belongs to a tree: its column of logs stands on natural ground and leaves grow at its top.
  A log wall or pillar of a build has no leaves, so it stays a build; a trunk part cut away (air between its logs) is
  still a trunk."
  [block-at cell]
  (let [base (last (take-while #(log? (block-at %)) (iterate #(up % -1) cell)))
        reach (take-while #(or (log? (block-at %)) (= "air" (block-at %))) (take tree-gap (iterate #(up % 1) cell)))
        top (last (filter #(log? (block-at %)) reach))]
    (boolean (and (rules/natural? (block-at (up base -1)))
                  (some (fn [d] (some->> (block-at (add top d)) (re-find #"_leaves$")))
                        (for [dx [-1 0 1] dy (range 4) dz [-1 0 1]] [dx dy dz]))))))

(defn diggable?
  "Whether an escalation may cut cell: it is open (air or a plant), or natural terrain (a tree's log too) or a block
  the body placed itself (own?: (own? cell block)) that may-dig? allows."
  ([block-at may-dig? cell] (diggable? block-at may-dig? (constantly false) cell))
  ([block-at may-dig? own? cell]
   (let [n (block-at cell)]
     (or (bs/passable? n)
         (and (or (rules/natural? n) (and (log? n) (tree-log? block-at cell)) (boolean (and n (own? cell n))))
              (boolean (may-dig? cell)))))))

(defn stair-heading
  "The first heading of dirs (each [dx dz]) with a solid block in front at feet height to stand on (bridge?: or none,
  the stair lays that floor) and every cell of the n steps' cuts diggable? (natural terrain, may-dig?), as a keyword
  (:east ...), or nil."
  ([block-at feet dirs] (stair-heading block-at feet dirs 1 (constantly true)))
  ([block-at feet dirs n may-dig?] (stair-heading block-at feet dirs n may-dig? (constantly false)))
  ([block-at feet dirs n may-dig? own?] (stair-heading block-at feet dirs n may-dig? own? false))
  ([block-at feet dirs n may-dig? own? bridge?]
   (some (fn [d]
           (let [front (ahead feet d 1)]
             (when (and (or bridge? (solid? (block-at front))) (not (protected? (block-at front)))
                        (every? #(diggable? block-at may-dig? own? %) (stair-cuts feet d (max 1 n))))
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

(defn priced-spot
  "The cheapest cell (jobs.lib.cost seconds: the walk there, plus the walk on to goal along the straight line with its
  unseen cells at the unseen price, wworld/unseen-share, rest-s) other than feet and the cells in visited (already walked to)
  for which (take? cell g rest-s), g its walk's seconds, over the cells steps (level-steps) reaches from feet: tried
  cheapest first, at most cap tried (nil: no cap). nil when none."
  [p feet goal steps take? visited cap]
  (let [kind-at (reach/seen-lookup p reach/kind-of :ground)
        walk-s (fn [a b] (:cost (cost/walk-cost {:distance (apply js/Math.hypot (map - b a))})))
        share (wworld/unseen-share p)
        rest-s (memoize (fn [cell] (+ (walk-s cell goal) (cost/unseen-route-s share kind-at (map #(zipmap [:x :y :z] %) (line-cells cell goal))))))]
    (loop [open (sorted-set [(rest-s feet) 0 0 feet]) best {feet 0} n 1 tried 0]
      (when-let [[_ _ g cell :as top] (first open)]
        (let [open (disj open top)
              try? (and (= g (best cell)) (not= cell feet) (not (visited cell)))]
          (cond
            (< (best cell) g) (recur open best n tried)
            (and try? (take? cell g rest-s)) cell
            (and cap (>= tried cap)) nil
            :else (let [nbrs (keep #(let [g2 (+ g (walk-s cell %))] (when (< g2 (get best % js/Infinity)) [% g2])) (steps cell))]
                    (recur (into open (map-indexed (fn [i [c g2]] [(+ g2 (rest-s c)) (+ n i) g2 c]) nbrs))
                           (into best nbrs) (+ n (count nbrs)) (cond-> tried try? inc)))))))))

(defn wall-spot
  "The cheapest cell (priced-spot through standable cells) with a solid side: one whose side toward goal is solid
  when there is one, else one with any. nil when none. Cells in visited (already walked to) do not count."
  ([p feet goal] (wall-spot p feet goal #{}))
  ([p feet goal visited]
   (let [block-at (block-at-of p)
         steps (level-steps p feet false)
         toward? (fn [cell _ _] (when-let [d (heading cell goal)] (solid? (block-at (ahead cell d 1)))))]
     (or (priced-spot p feet goal steps toward? visited nil)
         (priced-spot p feet goal steps (fn [cell _ _] (walled-side? block-at cell)) visited nil)))))

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

(defn max-door [] (settings/get settings ::max-door))

(defn door-spot
  "The cheapest cell (priced-spot through standable cells) from which a wall straight toward goal is a door (door)
  whose every cell is diggable? and whose far side is cheaper to goal from there than from feet, the walk there
  counted: where to walk when the wall at feet
  cannot be dug (an iron door or another's build in line). nil when none. Cells in visited (already walked to) do not
  count."
  [p feet goal diggable visited]
  (let [block-at (block-at-of p)
        unseen? (unseen-of p)
        take? (fn [cell g rest-s] (when-let [d (heading cell goal)]
                                    (when-let [gap (door block-at unseen? cell d (max-door))]
                                      (and (every? diggable (:cells gap)) (< (+ g (rest-s (:through gap))) (rest-s feet))))))]
    (priced-spot p feet goal (level-steps p feet false) take? visited nil)))

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

(declare choose)

(def way-spot-cap
  "Standable cells way-spot tries choose from at most."
  12)

(defn way-end
  "Where way e (escape/choose's) made from cell leaves the body: a pillar's top, a stair's last step, a clear-path's far
  side; nil for any other step."
  [p cell goal {:keys [step height heading steps]}]
  (case step
    :pillar (up cell height)
    :stair (let [[dx dz] (heading-dirs heading)] (add cell [(* steps dx) steps (* steps dz)]))
    :clear-path (:through (door (block-at-of p) (unseen-of p) cell (heading cell goal) (max-door)))
    nil))

(defn way-spot
  "The cheapest cell (priced-spot through standable cells a step up or down included, at most way-spot-cap tried) from
  which choose makes a way (a pillar, stair or clear-path) toward goal: where to walk when no way can be made from feet
  itself, e.g. a body inside its hut beside the doorway it cannot cut through the wall to. A clear-path, stair or pillar must
  leave the walk there and on from its end (way-end) cheaper than the walk from feet (a stair or pillar up toward a higher goal); one out of a pit or room counts as it is. nil when none."
  [p feet goal may-dig? opts visited]
  (let [opts (update opts :skip (fnil conj #{}) :approach)
        take? (fn [cell g rest-s]
                (let [e (choose p cell goal may-dig? opts)
                      end (way-end p cell goal e)]
                  (case (:step e)
                    :clear-path (boolean (and end (< (+ g (rest-s end)) (rest-s feet))))
                    (:stair :pillar) (or (<= (goal 1) (cell 1)) (< (+ g (rest-s end)) (rest-s feet)))
                    false)))]
    (priced-spot p feet goal (level-steps p feet true) take? visited way-spot-cap)))

(defn choose
  "How a body at feet (a cell) that cannot walk to goal (a cell) can make a way, cheapest first. It only ever digs
  natural terrain (natural?) and blocks it placed itself (opts :own?, (own? cell block)) in cells (may-dig? cell)
  allows (go-to: no other owner's zone or claim); opts :pillar-item ({:item :count} or nil, escape/pillar-item) replaces the climb block read from the inventory. opts :skip is a set of steps that already failed (a stair heading
  that failed: [:stair :east]): the next one is chosen; opts :approached, the cells an approach already walked to; opts :stands, the feet cells of the body's own stairs (a ledge stair's first floor never lands on one: the stair job refuses :undercuts-way).
  - {:step :pillar :height n :item}: in a pit d deep with room above the head and at least n pillar blocks carried: n
    the rim-pillar (d - 1, then a step onto the rim), d when no side has a rim to step onto. First only when the
    column over the pit is seen to be clear to the top; with an unseen or solid cell in it, after the dig ways.
  - {:step :stair :heading kw :steps n}: a block in front to stair up on, n how far the goal is above or the pit's
    depth, whichever is more, at most max-depth (the pit's depth when the longer cut is not all diggable?), every cell
    of its cut diggable?.
  - {:step :clear-path :heading kw}: a wall at most max-door thick straight toward the goal, with floor beyond,
    every cell diggable?. Chosen before a stair when the goal is no higher than the feet: a sealed tunnel reads as a
    pit, and its stair would climb away from the goal.
  - a stair with no pit and no goal above (a sealed tunnel, a goal through thick rock): the deepest diggable one, up
    to max-depth.
  - {:step :pillar :height n :item}, the last way before :none: beside a wall (a pit too wide to be walled on three
    sides) with no walk or dig way, n the rim-pillar.
  - {:step :approach :pos cell}: no side of the body's cell is solid: walk to wall-spot first; a wall in line it cannot dig: to door-spot; in the free part of a
    shut door's cell with no door-spot in sight, step back off the panel (door-back).
  - on a ledge (no side solid, no wall-spot) under a higher goal with a pillar block carried: a stair whose first
    floor the stair job lays.
  - {:step :none :why kw}: none of these. :why :no-headroom: a pit the carried blocks would pillar out of but for
    the block over the head; :no-dig: nothing natural and allowed to dig; :no-wall: no wall to walk to."
  ([p feet goal] (choose p feet goal (constantly true)))
  ([p feet goal may-dig?] (choose p feet goal may-dig? nil))
  ([p feet goal may-dig? {:keys [own? skip approached stands] :or {own? (constantly false) skip #{} approached #{} stands #{}} :as opts}]
   (let [approached (or approached #{})
         block-at (block-at-of p)
         dir (heading feet goal)
         depth (pit-depth block-at (unseen-of p) feet)
         {:keys [item count]} (if (contains? opts :pillar-item) (:pillar-item opts) (carried-pillar-item p))
         rim-n (when (and item (not (skip :pillar))) (rim-pillar block-at (unseen-of p) feet dir))
         height (when (pos? depth) (min depth (or rim-n depth)))
         pillar-blocks? (and (not (skip :pillar)) height item (>= count height))
         headroom? (bs/passable? (block-at (up feet 2)))
         open-above? (and pillar-blocks? (every? #(bs/passable? (block-at (up feet %))) (range 2 (+ height 2))))
         rise (min (max-depth) (max depth (- (second goal) (second feet))))
         stair-dirs (remove #(skip [:stair (heading-names %)]) (distinct (cond->> cardinals dir (cons dir))))
         [stair rise] (if (or (skip :stair) (not (pos? rise)))
                        [nil rise]
                        (or (some (fn [n] (some-> (stair-heading block-at feet stair-dirs n may-dig? own?) (vector n)))
                                  (distinct (filter pos? [rise depth])))
                            [nil rise]))
         climb (delay (when-not (or (skip :stair) (pos? rise))
                        (some (fn [n] (some-> (stair-heading block-at feet stair-dirs n may-dig? own?) (vector n)))
                              (range (max-depth) 0 -1))))
         ledge (delay (when (and item (not (skip :stair)) (pos? rise) (not (walled-side? block-at feet)))
                        (stair-heading block-at feet (remove #(contains? stands (ahead feet % 1)) stair-dirs) rise may-dig? own? true)))
         gap (when (and dir (not (skip :clear-path))) (door block-at (unseen-of p) feet dir (max-door)))
         spot (delay (when (and dir (not (skip :approach)) (not (skip :clear-path)) (walled-side? block-at feet))
                       (door-spot p feet goal #(diggable? block-at may-dig? own? %) approached)))
         wall (delay (wall-spot p feet goal approached))
         back (delay (when-not (or (skip :approach) (skip :clear-path)) (door-back p feet approached)))
         door? (and gap (every? #(diggable? block-at may-dig? own? %) (:cells gap)))
         rim (when (and rim-n (>= count rim-n)) rim-n)]
     (cond
       (and open-above? headroom?)
       {:step :pillar :height height :item item}

       (and door? (<= (second goal) (second feet)))
       {:step :clear-path :heading (heading-names dir)}

       stair
       {:step :stair :heading stair :steps rise}

       door?
       {:step :clear-path :heading (heading-names dir)}

       @spot
       {:step :approach :pos @spot}

       @back
       {:step :approach :pos @back}

       (and (not (skip :approach)) (not (and (skip :clear-path) (skip :stair))) (not (walled-side? block-at feet)) @wall)
       {:step :approach :pos @wall}

       @ledge
       {:step :stair :heading @ledge :steps rise}

       (and (not (skip :approach)) (not (walled-side? block-at feet)))
       {:step :none :why :no-wall}

       @climb
       (let [[heading n] @climb] {:step :stair :heading heading :steps n})

       (and pillar-blocks? headroom?)
       {:step :pillar :height height :item item}

       rim
       {:step :pillar :height rim :item item}

       pillar-blocks? {:step :none :why :no-headroom}

       :else {:step :none :why :no-dig}))))

(defn note-hole!
  "Write cell, where block was dug, to the tidy ledger (jobs.lib.tidy, body memory) as a :dig entry with tag merged,
  the :zone, :claim or :plan it trespassed on, and :any-of, the items that put it back (the block or what it drops);
  only when another's zone, claim or plan covers the cell (the body's own diggings stay), it is air now and no tagged
  entry is there yet (a plain one the dig itself noted is replaced). The :note arg of jobs.access.stair
  (go-to's escalation), so a cut or cancel leaves no hole unknown."
  [c tag cell block]
  (let [p (:primitives c)
        cell (vec cell)
        noted (first (filter #(= cell (:cell %)) (tidy/entries c)))
        v (when-not (:escalation noted) (tidy/refusal c :dig (zipmap [:x :y :z] cell)))]
    (when (and v (bs/air (u/seen-name p (zipmap [:x :y :z] cell))))
      (when noted (tidy/forget-cell! c cell))
      (tidy/record! c (merge {:cell cell :action :dig :was block :any-of (vec (distinct (cons block (b/drops-of p block))))}
                             (select-keys v [:zone :claim :plan]) tag)
                    "air"))))
