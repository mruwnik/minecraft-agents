(ns jobs.lib.reach.room
  "Rooms and enclosure for the danger checks: enclosed? (the body shut in), room (the closed room around a cell, every
  door shut, kept per body), the floods under them and the unseen-edge reads that say 'look first'.
  See jobs.lib.reach for the block reads and walk searches."
  (:require [clojure.string :as str]
            [jobs.lib.cost.look :as glance]
            [jobs.lib.memory-age :as memory-age]
            [engine.perception.rays :as rays]
            [jobs.lib.reach :as reach :refer [openable? open-prop? tall-block kind-of keyer passable? seen-standable? standing-cell
                                    forward backward search-steps world-kinds]]
            [jobs.lib.util :as u]))

(def room-cells
  "Standable cells a flood reaches before the body counts as having room. Large enough that a 20x20 hollow still
  floods as closed."
  1024)

(defn body-kind-of
  "kind-of for the body's own walk: a shut wooden door, gate or trapdoor is :open; iron ones are not."
  [b]
  (let [name (some-> b .-name)]
    (if (and name (openable? name) (not (str/starts-with? name "iron_")))
      :open
      (kind-of b))))

(defn flood-visit
  "Breadth-first over (next-cells x y z) from start [x y z], expanding at most budget cells: {:cells #js [[x y z]..] in
  visiting order, :closed? whether every reachable cell was expanded within budget}, fewer where reach/*search* runs out
  (its cells charged). An abort? (cell #js [x y z]) that is true for a cell about to be expanded ends the flood there,
  not closed."
  ([next-cells start budget] (flood-visit next-cells start budget nil))
  ([next-cells [sx sy sz] budget abort?]
  (let [key (keyer sx sz)
        seen (js/Set.)
        queue #js [#js [sx sy sz]]
        cap (reach/metered-cap budget :read-s)
        end (fn [closed? cut?] (reach/charge! (min (.-size seen) cap) :read-s cut?) {:cells queue :closed? closed?})]
    (.add seen (key sx sy sz))
    (loop [head 0]
      (cond
        (== head (.-length queue)) (end true false)
        (>= (.-size seen) cap) (end false (< cap budget))
        (and abort? (abort? (aget queue head))) (end false false)
        :else (let [c (aget queue head)
                    nexts (next-cells (aget c 0) (aget c 1) (aget c 2))]
                (dotimes [i (.-length nexts)]
                  (let [n (aget nexts i)
                        k (key (aget n 0) (aget n 1) (aget n 2))]
                    (when-not (.has seen k)
                      (.add seen k)
                      (.push queue n))))
                (recur (inc head))))))))

(defn flood-cells
  "flood-visit's cells when every reachable cell was expanded within budget (none stopping it, abort?), else nil."
  ([next-cells start budget] (flood-cells next-cells start budget nil))
  ([next-cells start budget abort?]
   (let [{:keys [cells closed?]} (flood-visit next-cells start budget abort?)]
     (when closed? cells))))

(defn flood
  "flood-cells' answer as :closed (every reachable cell expanded within budget) or :room."
  [next-cells start budget]
  (if (flood-cells next-cells start budget) :closed :room))

(def door-panel-step
  "The step [dx dz] from a door cell onto the side its closed panel fills: opposite the door's facing."
  {"north" [0 1] "south" [0 -1] "east" [-1 0] "west" [1 0]})

(defn panel-step
  "The step [dx dz] from the cell {:x :y :z} across a shut door's panel, when kind-at counts the cell solid and the block
  there is a shut door, else nil. A body standing in the free part of that cell cannot walk through the panel."
  [p kind-at {:keys [x y z]}]
  (when (keyword-identical? :solid (kind-at x y z))
    (let [b (u/seen-block p {:x x :y y :z z})
          name (some-> b .-name)]
      (when (and name (str/ends-with? name "_door") (not (open-prop? b)))
        (door-panel-step (some-> b .-properties .-facing))))))

(def shut-room-cells
  "Cells a body must still reach after a cell is filled for it not to count as shut in by that fill: half of
  room-cells, so a fill that trims a region of about room-cells (a plot-sized platform) by one cell does not shut it in."
  (quot room-cells 2))

(defn shut-in?
  "Whether the body is shut in: it can walk to fewer than budget (default room-cells) cells over kind-at. A body in
  the free part of a shut door's cell cannot step across the panel."
  ([p kind-at] (shut-in? p kind-at room-cells))
  ([p kind-at budget]
  (let [{:keys [x y z] :as start} (standing-cell p kind-at)
        panel (panel-step p kind-at start)
        next-cells (fn [cx cy cz]
                     (let [ns (forward kind-at cx cy cz)]
                       (if (and panel (== cx x) (== cy y) (== cz z))
                         (.filter ns (fn [n] (not (and (== (- (aget n 0) x) (first panel)) (== (- (aget n 2) z) (second panel))))))
                         ns)))]
    (= :closed (flood next-cells [x y z] budget)))))

(defn walks-beyond?
  "Whether the body can walk (over kind-at) to a cell more than dist blocks from where it stands, sideways: out of its hole
  or room into open ground, however large the room."
  [p kind-at dist]
  (let [{:keys [x y z]} (standing-cell p kind-at)
        d2 (* dist dist)]
    (not (:closed? (flood-visit #(forward kind-at %1 %2 %3) [x y z] ##Inf
                                (fn [c] (> (+ (js/Math.pow (- (aget c 0) x) 2) (js/Math.pow (- (aget c 2) z) 2)) d2)))))))

(defn enclosed-over
  "Whether the body is shut in: it can walk to fewer than budget cells over base (a kind-at) with the cells of solid (a set
  of [x y z]) treated as solid blocks (what if they were filled). False with no body position."
  [p solid base budget]
  (if (some-> (.self p) .-pos)
    (shut-in? p (if (empty? solid)
                  base
                  (fn [x y z] (if (contains? solid [x y z]) :solid (base x y z))))
              budget)
    false))

(declare kept)

(defn kept-at-pos
  "(f) kept as kept does for the key ck and the body's position, when unseen (a reach/world-kinds policy) is a keyword:
  a policy fn is made anew each call, so its answer is not kept."
  [p ck unseen f]
  (if-let [pos (and (keyword? unseen) (some-> (.self p) .-pos))]
    (kept p (str ck "|" unseen "|" (.-x pos) "," (.-y pos) "," (.-z pos)) (fn [_] (f)))
    (f)))

(defn enclosed?
  "Whether the body is shut in: it can walk to fewer than room-cells cells (one step up, up to three down, water swum,
  wooden doors opened). A pit or sealed room is; open ground or a hut with a door is not. A body in the free part of a
  shut door's cell cannot step across the panel. False with no body position. Kept (kept-at-pos): a go-to's checks ask it
  several times a round.
  unseen: how a cell the body has not seen reads (reach/world-kinds). solid: a set of cells [x y z] treated as solid
  blocks (what if they were filled)."
  ([p unseen] (enclosed? p unseen #{}))
  ([p unseen solid] (enclosed? p unseen solid room-cells))
  ([p unseen solid budget]
   (kept-at-pos p (str "enclosed|" (pr-str (sort solid)) "|" budget) unseen
                #(enclosed-over p solid (world-kinds p body-kind-of unseen) budget))))

(defn walled-in?
  "Whether the body is shut in by walls: enclosed? (unseen as there), and no cell it can reach has a neighbour open to
  stand in that it cannot step to (an edge over a drop is no wall: a platform is shut in by its drops, not by walls)."
  [p unseen]
  (let [kind-at (world-kinds p body-kind-of unseen)
        {:keys [x y z]} (standing-cell p kind-at)
        {:keys [cells closed?]} (flood-visit (fn [cx cy cz] (reach/forward kind-at cx cy cz)) [x y z] room-cells)
        drop-edge? (fn [c]
                     (let [cx (aget c 0) cy (aget c 1) cz (aget c 2)]
                       (some (fn [d] (let [nx (+ cx (aget d 0)) nz (+ cz (aget d 1))]
                                       (and (passable? kind-at nx cy nz) (passable? kind-at nx (inc cy) nz)
                                            (nil? (reach/step-to kind-at cx cy cz nx nz)))))
                             reach/dirs)))]
    (boolean (and (some-> (.self p) .-pos) closed? (not-any? drop-edge? cells)))))

(defn dry-shut-in?
  "Whether the body is shut in when water is a wall: a dry pocket whose only way on is a swim. unseen as enclosed?."
  [p unseen]
  (let [base (world-kinds p body-kind-of unseen)]
    (shut-in? p (fn [x y z] (let [k (base x y z)] (if (keyword-identical? :water k) :solid k))))))

(defn fill-kinds
  "[filled open]: the kind-at fns of the two floods of a fill check. unseen :fill: a cell the body has not seen is open
  ground in both, so a flood that reaches the edge of what it has seen is not shut in (a small sight alone must not read as
  a wall); only a ring of seen walls is. Else unseen as world-kinds reads it, for both."
  [p unseen]
  (let [base (world-kinds p body-kind-of (if (keyword-identical? :fill unseen) :ground unseen))] [base base]))

(defn closes-in?
  "Whether the body has room to walk now but would be left fewer than shut-room-cells cells with cell [x y z] solid.
  unseen as fill-kinds. The filled flood goes first, as it is mostly open."
  [p unseen cell]
  (let [[filled open] (fill-kinds p unseen)]
    (boolean (and (enclosed-over p #{(vec cell)} filled shut-room-cells) (not (enclosed-over p #{} open room-cells))))))

(defn cut-off
  "The feet cells [x y z], nearest walk first, the body walks to now but would not with cell [x y z] solid: the far side of
  a fill that closes it in (closes-in?). unseen as fill-kinds."
  [p unseen cell]
  (let [[filled open] (fill-kinds p unseen)
        with (fn [x y z] (if (= [x y z] (vec cell)) :solid (filled x y z)))
        {:keys [x y z]} (standing-cell p open)
        visit (fn [kind-at budget] (map vec (:cells (flood-visit (fn [x y z] (forward kind-at x y z)) [x y z] budget))))
        shut (set (visit with shut-room-cells))]
    (remove shut (visit open room-cells))))

(defn walls-in?
  "Whether a walker on the floor under pos [x y z] (within four cells) can walk to fewer than 64 cells with cell solid,
  and to more without, shut in or not (a small room only: the floods stay cheap). unseen as fill-kinds."
  [p unseen [x y z] cell]
  (let [[filled-base base] (fill-kinds p unseen)
        floor-y (first (filter #(and (passable? base x % z) (passable? base x (inc %) z) (keyword-identical? :solid (base x (dec %) z)))
                               (range (dec y) (- y 5) -1)))
        filled (fn [x y z] (if (= [x y z] (vec cell)) :solid (filled-base x y z)))
        visit (fn [kind-at] (flood-visit (fn [x y z] (forward kind-at x y z)) [x floor-y z] 64))]
    (boolean (and floor-y (not= [x floor-y z] (vec cell))
                  (let [with (visit filled)]
                    (and (:closed? with) (< (.-length (:cells with)) (.-length (:cells (visit base))))))))))

(defn seen-free? [p kind-at x y z]
  (and (some? (u/seen-block p {:x x :y y :z z})) (passable? kind-at x y z)))

(defn memo-kind-at
  "kind-at answering each cell once: a flood asks about a cell ~20 times, and these wrappers read the body's senses."
  [kind-at]
  (let [cache (js/Map.)
        key (volatile! nil)]
    (fn [x y z]
      (let [key-of (or @key (vreset! key (keyer x z)))
            k (key-of x y z)
            hit (.get cache k)]
        (if (some? hit)
          hit
          (let [v (kind-at x y z)]
            (.set cache k v)
            v))))))

(defn room-kind-at
  "kind-at for the room flood: a floor cell the body has not seen under seen-free feet and head cells is a floor (a
  level look does not show the floor beside the feet)."
  [p kind-at]
  (memo-kind-at
   (fn [x y z]
     (if (and (nil? (u/seen-block p {:x x :y y :z z}))
              (seen-free? p kind-at x (inc y) z) (seen-free? p kind-at x (+ y 2) z))
       :solid
       (kind-at x y z)))))

(defn unseen-edge?
  "Whether a walker at cell #js [x y z] may lead somewhere the body has not seen: a side feet cell, or the head cell over
  a side block it could step onto, is unseen. Unseen is unknown, not wall. A side block with a seen solid two over it
  cannot be stepped onto, so what is unseen over it leads nowhere. unseen? (x y z): a caller's own read of it."
  ([p kind-at c] (unseen-edge? p kind-at c (fn [x y z] (nil? (u/seen-block p {:x x :y y :z z})))))
  ([_p kind-at c unseen?]
  (let [x (aget c 0) y (aget c 1) z (aget c 2)]
    (some (fn [[dx dz]]
            (let [nx (+ x dx) nz (+ z dz)]
              (or (unseen? nx y nz)
                  (and (not (passable? kind-at nx y nz)) (passable? kind-at x (+ y 2) z) (unseen? nx (inc y) nz)
                       (or (unseen? nx (+ y 2) nz) (passable? kind-at nx (+ y 2) nz))))))
          [[1 0] [-1 0] [0 1] [0 -1]]))))

(defn step-cells
  "The feet and head cells [x y z] a walker at x y z passes stepping to nx ny nz: up, the cell over its head then the new
  feet and head; else the new head down to the new feet."
  [x y z nx ny nz]
  (if (> ny y)
    [[x (+ y 2) z] [nx ny nz] [nx (inc ny) nz]]
    (map (fn [cy] [nx cy nz]) (range (inc y) (dec ny) -1))))

(defn unseen-fn
  "(x y z) -> whether the body has not seen the cell, each cell read once: a flood asks again at every neighbour."
  [p]
  (memo-kind-at (fn [x y z] (nil? (u/seen-block p {:x x :y y :z z})))))

(defn pocket-steps
  "next-cells for the room flood: forward's steps, but onto an unseen feet cell only from a walker whose own feet cell is
  unseen, or from the start when `entered?` (stepped into through a seen door). From a seen open cell an unseen one is the
  edge of what the body knows, not more room: grazing sight down a tunnel misses a wall cell, and the rock behind it is no
  part of the tunnel. A refused step's unseen cells (step-cells) go to (edge! [x y z])."
  [unseen? kind-at [sx sy sz] entered? edge!]
  (fn [x y z]
    (let [pocket? (or (unseen? x y z) (and entered? (== x sx) (== y sy) (== z sz)))]
      (.filter (forward kind-at x y z)
               (fn [n]
                 (let [nx (aget n 0) ny (aget n 1) nz (aget n 2)]
                   (or pocket? (not (unseen? nx ny nz))
                       (do (doseq [c (step-cells x y z nx ny nz) :when (apply unseen? c)] (edge! c))
                           false))))))))

(defn room-flood
  "{:cells [...] :unseen? bool :edge #{[x y z]}}: the cells a walker at cell [x y z] reaches (room-kind-at, pocket-steps)
  fewer than budget (default room-cells), the unseen cells it stopped at (:edge), and whether there are any or any cell
  is an unseen-edge?; nil when the flood is open, or abort? (a cell #js [x y z] -> bool) stopped it. `kind-at`: room-kind-at of
  a shut-kind-at. entered?: the start was stepped into through a seen door (pocket-steps)."
  ([p kind-at x y z] (room-flood p kind-at x y z room-cells))
  ([p kind-at x y z budget] (room-flood p kind-at x y z budget false))
  ([p kind-at x y z budget entered?] (room-flood p kind-at x y z budget entered? nil))
  ([p kind-at x y z budget entered? abort?]
   (let [edge (volatile! (transient #{}))
         unseen? (unseen-fn p)]
     (when-let [cells (flood-cells (pocket-steps unseen? kind-at [x y z] entered? #(vswap! edge conj! %)) [x y z] budget abort?)]
       (let [edge (persistent! @edge)]
         {:cells cells :edge edge :unseen? (boolean (or (seq edge) (some #(unseen-edge? p kind-at % unseen?) cells)))})))))

(defn room-of
  "{:cells #{[x y z]} :doors [...]} (room*'s answer) for the cells #js [[x y z]..] of a closed flood."
  [p cells]
  (let [cells (set (map vec cells))
        doors (for [[cx cy cz] cells
                    [dx dz] [[1 0] [-1 0] [0 1] [0 -1]]
                    :let [cell {:x (+ cx dx) :y cy :z (+ cz dz)}
                          name (some-> (u/seen-block p cell) .-name)]
                    :when (and name (openable? name) (not (str/ends-with? name "_trapdoor")))]
                {:cell cell :inside {:x cx :y cy :z cz}})]
    {:cells cells :doors (vec (vals (into {} (map (juxt :cell identity)) doors)))}))

(defn closed-room
  "room-of the cells of a room-flood answer, nil when it is open or any cell of it is an unseen-edge?."
  [p {:keys [cells unseen?]}]
  (when (and cells (not unseen?))
    (room-of p cells)))

(defn room-from
  "The closed room a walker at cell [x y z] reaches (room*'s answer), nil when it is no closed room or any cell of it is
  an unseen-edge?."
  ([p kind-at x y z] (room-from p kind-at x y z room-cells))
  ([p kind-at x y z budget] (room-from p kind-at x y z budget false))
  ([p kind-at x y z budget entered?] (closed-room p (room-flood p kind-at x y z budget entered?))))

(defn shut-kind
  "kind-of block b with a door, gate or trapdoor shut: :solid, a gate :tall (a walker cannot step onto it)."
  [b]
  (cond (not (and b (openable? (.-name b)))) (kind-of b)
        (re-find tall-block (.-name b)) :tall
        :else :solid))

(defn shut-kind-at
  "kind-of over the blocks the body has seen, every door, gate and trapdoor shut (shut-kind)."
  [p]
  (memo-kind-at (fn [x y z] (shut-kind (u/seen-block p {:x x :y y :z z})))))

(defn door-kind-at
  "kind-of over the blocks the body has seen, each door, gate and trapdoor as it stands."
  [p]
  (memo-kind-at (fn [x y z] (kind-of (u/seen-block p {:x x :y y :z z})))))

(declare room-with)

(defn room*
  "The closed room around cell pos {:x :y :z}: the cells a walker there reaches with every door, gate and trapdoor shut,
  fewer than room-cells (flood over the blocks the body has seen, room-kind-at; no closed room where it meets an unseen
  or unloaded cell, unseen-edge?). {:cells #{[x y
  z]} feet cells, :doors [{:cell {:x :y :z} :inside {:x :y :z}}] each door or gate beside a room cell at its feet, with
  that room cell}; nil when it is no closed room. A door or gate cell is the room's boundary whether open or shut, so
  a pos in one is judged by the first closed room beside it. `kind-at`: room-kind-at of shut-kind-at p, or one kept for a while."
  ([p pos] (room* p pos (room-kind-at p (shut-kind-at p))))
  ([p pos kind-at] (room* p pos kind-at room-cells))
  ([p pos kind-at budget] (room-with p pos (fn [x y z entered?] (room-flood p kind-at x y z budget entered?)))))

(defn room-with
  "room*'s answer over (flood x y z entered?), a room-flood from that cell."
  [p {:keys [x y z]} flood]
  (let [in-door? (some-> (u/seen-block p {:x x :y y :z z}) .-name openable?)]
    (if in-door?
      (some (fn [[dx dz]] (closed-room p (flood (+ x dx) y (+ z dz) true))) [[1 0] [-1 0] [0 1] [0 -1]])
      (closed-room p (flood x y z false)))))

(defn enterable?
  "Whether a walker on seen ground outside the room (room*'s answer) steps into one of its cells: a pit under a roof is open
  to a mob on its rim though the body in it cannot climb out."
  ([p room] (enterable? p room (room-kind-at p (shut-kind-at p))))
  ([p {:keys [cells]} kind-at]
    (boolean (some (fn [[x y z]]
                     (some #(and (not (cells (vec %))) (seen-standable? p {:x (aget % 0) :y (aget % 1) :z (aget % 2)}))
                           (backward kind-at x y z)))
                   cells))))

(defn spider-gap?
  "Whether a spider (1.4 wide, 0.9 high) gets into the room (room*'s answer) through a wall opening a walker cannot use:
  two seen air cells side by side beside a room cell, at its feet or head height, walled in on every other side (a gap
  two wide and one high), with open air beyond it."
  [p {:keys [cells]}]
  (let [kind-at (shut-kind-at p)
        solid? (fn [x y z] (keyword-identical? :solid (kind-at x y z)))
        air? (fn [x y z] (and (some? (u/seen-block p {:x x :y y :z z})) (passable? kind-at x y z)
                              (not-any? #(cells [x (- y %) z]) (range 4))))
        gap? (fn [x y z dx dz]
               (let [px (js/Math.abs dz) pz (js/Math.abs dx)
                     slot (fn [k] [(+ x (* k px)) y (+ z (* k pz))])
                     bounded? (fn [[sx sy sz]] (and (solid? sx (dec sy) sz) (solid? sx (inc sy) sz)))]
                 (and (apply air? (slot 0)) (apply air? (slot 1))
                      (bounded? (slot 0)) (bounded? (slot 1))
                      (apply solid? (slot -1)) (apply solid? (slot 2))
                      (air? (+ x dx) y (+ z dz)))))]
    (boolean (some (fn [[x y z]]
                     (some (fn [[dx dz]] (some #(gap? (+ x dx) (+ y %) (+ z dz) dx dz) [0 1]))
                           [[1 0] [-1 0] [0 1] [0 -1]]))
                   cells))))

(defn unseen-room
  "The closed room (room*'s answer, doors included) around the cell pos {:x :y :z} that closes over what the body has seen
  but has a cell it has not seen beside it (room* nil for that reason, not because the room is open): it is unknown, look
  first. nil when the room is open or fully seen."
  [p {:keys [x y z]}]
  (let [kind-at (room-kind-at p (shut-kind-at p))
        in-door? (some-> (u/seen-block p {:x x :y y :z z}) .-name openable?)
        starts (if in-door? (map (fn [[dx dz]] [(+ x dx) y (+ z dz)]) [[1 0] [-1 0] [0 1] [0 -1]]) [[x y z]])]
    (some (fn [[sx sy sz]] (let [f (room-flood p kind-at sx sy sz room-cells in-door?)] (when (:unseen? f) (room-of p (:cells f))))) starts)))

(defn unseen-room?
  "Whether the cell pos holds an unseen-room."
  [p pos]
  (some? (unseen-room p pos)))

(declare kept-flood kept-flood?)

(defn boundary-kept?
  "Whether boundary's floods around pos are all kept: judging its look costs no flood."
  [p {:keys [x y z]}]
  (let [in-door? (some-> (u/seen-block p {:x x :y y :z z}) .-name openable?)
        starts (if in-door? (map (fn [[dx dz]] [(+ x dx) y (+ z dz)]) [[1 0] [-1 0] [0 1] [0 -1]]) [[x y z]])]
    (every? (fn [[sx sy sz]] (kept-flood? p sx sy sz in-door?)) starts)))

(defn boundary
  "{:seen n :unseen [[x y z]]} of the room around pos {:x :y :z} (as unseen-room?): how many feet cells beside its room
  cells the body has seen (its walls and doors, as the flood closed over them) and those it has not seen (with the flood's
  :edge cells but a head cell over another, which an up or down step leaves off the same-level sides), what a look into the room has to show."
  [p {:keys [x y z]}]
  (let [in-door? (some-> (u/seen-block p {:x x :y y :z z}) .-name openable?)
        starts (if in-door? (map (fn [[dx dz]] [(+ x dx) y (+ z dz)]) [[1 0] [-1 0] [0 1] [0 -1]]) [[x y z]])
        floods (keep (fn [[sx sy sz]] (kept-flood p sx sy sz in-door?)) starts)
        cells (set (map vec (mapcat :cells floods)))
        edge (into #{} (mapcat :edge) floods)
        sides (->> cells
                   (mapcat (fn [[cx cy cz]] (map (fn [[dx dz]] [(+ cx dx) cy (+ cz dz)]) [[1 0] [-1 0] [0 1] [0 -1]])))
                   (remove cells)
                   distinct)
        {unseen true seen false} (group-by (fn [[cx cy cz]] (nil? (u/seen-block p {:x cx :y cy :z cz}))) sides)]
    {:seen (count seen) :unseen (vec (distinct (concat unseen (remove (fn [[ex ey ez]] (contains? edge [ex (dec ey) ez])) edge))))}))

(defn closed-chance
  "The chance the room round pos {:x :y :z} is closed while it is an unseen-room (an opening can only be in a side not
  seen): cost/closed-chance of boundary's seen and unseen sides; nil for any other room."
  [p pos]
  (when (unseen-room? p pos)
    (let [{:keys [seen unseen]} (boundary p pos)] (glance/closed-chance seen (count unseen)))))

(defn open-sides
  "{:open n :closed n} of the feet cells [[x y z]] (boundary's :unseen, seen by now) by their columns (feet and head): open
  when both are seen and a walker could pass, closed when a seen one blocks; a column with an unseen cell and no seen
  blocker is in neither count. A door, gate or trapdoor counts as it stands: open lets a mob pass, shut does not."
  [p cells]
  (let [kind-at (door-kind-at p)
        verdict (fn [[x y z]]
                  (let [col (map (fn [dy] [(some? (u/seen-block p {:x x :y (+ y dy) :z z})) (passable? kind-at x (+ y dy) z)]) [0 1])]
                    (cond (some (fn [[seen ok]] (and seen (not ok))) col) :closed
                          (every? first col) :open)))
        counts (frequencies (keep verdict cells))]
    {:open (get counts :open 0) :closed (get counts :closed 0)}))

(defn unseen-sides
  "The feet cells [x y z] beside the room cells of the room around pos {:x :y :z} (as unseen-room?) that the body has not
  seen (boundary's :unseen)."
  [p pos]
  (:unseen (boundary p pos)))

(defn unroofed-cell
  "{:cell :closed?}: the first cell a walker flood from pos {:x :y :z} (doors shut, within room-cells) reaches with no
  block seen within room-roof cells over it ({:x :y :z}, nil when every cell is roofed), and whether the flood closed.
  shut, a set of [x y z] cells, counts as solid. first?: the flood stops at that cell (the same cell; :closed? then false)."
  ([p pos] (unroofed-cell p pos nil))
  ([p pos shut] (unroofed-cell p pos shut false))
  ([p {:keys [x y z]} shut first?]
   (let [kind-at (fn [cx cy cz]
                   (if (contains? shut [cx cy cz]) :solid (shut-kind (u/seen-block p {:x cx :y cy :z cz}))))
         roofed? (fn [cx cy cz] (some #(let [b (u/seen-block p {:x cx :y (+ cy %) :z cz})] (and b (not= :open (kind-of b))))
                                      (range 2 7)))
         {:keys [cells closed?]} (flood-visit (fn [cx cy cz] (forward kind-at cx cy cz)) [x y z] room-cells
                                              (when first? (fn [c] (not (roofed? (aget c 0) (aget c 1) (aget c 2))))))
         c (.find cells (fn [c] (not (roofed? (aget c 0) (aget c 1) (aget c 2)))))]
     {:cell (when c {:x (aget c 0) :y (aget c 1) :z (aget c 2)}) :closed? closed?})))

(defn escape-cell
  "Where a walker at pos {:x :y :z} gets out of a room with a hole (room* nil, doors shut): unroofed-cell's cell when the
  flood is open; nil when it is closed or every cell is roofed."
  [p pos]
  (let [{:keys [cell closed?]} (unroofed-cell p pos)]
    (when-not closed? cell)))

(defn steady-key
  "What a room answer depends on: the raw world's epoch, the dimension and the memory's epoch (a block it remembers changed,
  or a remembered cell's change chance moved a level, memory-age/epoch); nil offline."
  [{:keys [raw st]}]
  (when-let [^js eye (.eye ^js raw)]
    (str (rays/epoch-of raw) "|" (.-dimension eye) "|" (pr-str (memory-age/epoch st)))))

(defn kept-with
  "(f kind-at put!) for the key `ck`, kept in the body's perception store while the world and its memory stay the same (the same shared kind-at serves every cell); put! (key value) keeps a further answer for another key of the
  same steady key (a no-op unkept). Not kept without a perception, nor when a flood or search of f was cut (reach/cuts)."
  [p ck f]
  (let [per (aget p "perception") k (some-> per steady-key)]
    (if-not k
      (f (room-kind-at p (shut-kind-at p)) (fn [_ _]))
      (let [^js st (:st per)]
        (when-not (= k (.-roomKey st))
          (set! (.-roomKey st) k)
          (set! (.-roomMemo st) (js/Map.))
          (set! (.-roomKind st) (room-kind-at p (shut-kind-at p))))
        (let [^js m (.-roomMemo st)
              n (reach/cuts)
              whole? #(= n (reach/cuts))]
          (if (.has m ck)
            (.get m ck)
            (let [v (f (.-roomKind st) (fn [k v] (when (whole?) (.set m k v))))] (when (whole?) (.set m ck v)) v)))))))

(defn kept?
  "Whether an answer for the key `ck` is kept (kept-with) under the world and memory as they are now."
  [p ck]
  (let [per (aget p "perception") k (some-> per steady-key)]
    (boolean (and k (= k (.-roomKey ^js (:st per))) (.has ^js (.-roomMemo ^js (:st per)) ck)))))

(defn kept
  "(f kind-at) for the key `ck`, kept as kept-with does."
  [p ck f]
  (kept-with p ck (fn [kind-at _] (f kind-at))))

(defn flood-key [x y z entered?] (str "flood|" x "|" y "|" z "|" (boolean entered?)))

(defn kept-flood?
  "Whether kept-flood's answer for the cell is kept."
  [p x y z entered?]
  (kept? p (flood-key x y z entered?)))

(defn kept-flood
  "room-flood from cell x y z within room-cells, kept as kept does: a check asks the same room's flood several times."
  [p x y z entered?]
  (kept p (flood-key x y z entered?) #(room-flood p % x y z room-cells entered?)))

(defn room
  "room* kept per body (kept); not kept without a perception."
  [p {:keys [x y z] :as pos}]
  (kept p (str x "|" y "|" z) (fn [_] (room-with p pos (partial kept-flood p)))))

(defn room-near
  "room's answer at pos when every cell of the room lies within r blocks across of pos, else nil: its flood stops at the
  first cell farther, so it reads no more than the cells within r. Kept as room is."
  [p {:keys [x y z] :as pos} r]
  (kept p (str "near|" x "|" y "|" z "|" r)
        (fn [kind-at]
          (let [far? (fn [c] (> (js/Math.hypot (- (aget c 0) x) (- (aget c 2) z)) r))]
            (room-with p pos (fn [sx sy sz entered?] (room-flood p kind-at sx sy sz room-cells entered? far?)))))))

(defn look-key [{:keys [x y z]}] (str "look|" x "|" y "|" z))

(defn look
  "{:room (room*'s answer) :unseen (a delay of unseen-room's)} of the cell pos from one flood a start, kept per body as
  room is: a judge that asks both floods once."
  [p {:keys [x y z] :as pos}]
  (kept p (look-key pos)
        (fn [kind-at]
          (let [in-door? (some-> (u/seen-block p pos) .-name openable?)
                starts (if in-door? (mapv (fn [[dx dz]] [(+ x dx) y (+ z dz)]) [[1 0] [-1 0] [0 1] [0 -1]]) [[x y z]])
                floods (mapv (fn [[sx sy sz]] (delay (room-flood p kind-at sx sy sz room-cells in-door?))) starts)]
            {:room (some (fn [f] (let [{:keys [cells unseen?]} @f] (when (and cells (not unseen?)) (room-of p cells)))) floods)
             :unseen (delay (some (fn [f] (when (:unseen? @f) (room-of p (:cells @f)))) floods))}))))

(def wall-cells
  "Cells the flood of walled-region may hold: a closed cave bigger than room-cells is still judged."
  2048)

(defn walker-sources
  "The cells of the flood `cells` (#js [[x y z]..] from start [x y z]) a walker steps from to the start and so on, within the
  flood: each reaches everything the start does. step (default backward): forward for a flood of backward steps, the
  cells the start walks to, which everything that reaches the start reaches."
  ([kind-at cells start] (walker-sources kind-at cells start backward))
  ([kind-at cells [sx sy sz] step]
  (let [key (keyer sx sz)
        in-flood (js/Set.)
        _ (.forEach cells (fn [c] (.add in-flood (key (aget c 0) (aget c 1) (aget c 2)))))
        seen (js/Set.)
        queue #js [#js [sx sy sz]]]
    (.add seen (key sx sy sz))
    (loop [head 0]
      (when (< head (.-length queue))
        (let [c (aget queue head)
              ns (step kind-at (aget c 0) (aget c 1) (aget c 2))]
          (dotimes [i (.-length ns)]
            (let [n (aget ns i)
                  k (key (aget n 0) (aget n 1) (aget n 2))]
              (when (and (.has in-flood k) (not (.has seen k)))
                (.add seen k)
                (.push queue n))))
          (recur (inc head)))))
    queue)))

(defn walled-region
  "{:room :checks} for the walkers' flood from cell pos {:x :y :z} (doors shut, within budget, at most wall-cells): :room as room*'s answer
  (nil when the flood is open or reaches an unseen edge, which ends it), :checks a delay of whether no walker outside can step in
  (enterable?) and no spider squeezes through (spider-gap?). Kept per body (kept), and for every cell of the flood
  that walks back to pos (walker-sources: it reaches the same cells), so a cave is flooded once, not once per cell."
  [p {:keys [x y z] :as pos} budget]
  (kept-with
   p (str "wall|" budget "|" x "|" y "|" z)
   (fn [kind-at put!]
     (let [door? (some-> (u/seen-block p pos) .-name openable?)
           unseen (volatile! false)
           unseen? (unseen-fn p)
           {:keys [cells closed?]} (flood-visit (partial forward kind-at) [x y z] budget
                                                (fn [c] (when (unseen-edge? p kind-at c unseen?) (vreset! unseen true) true)))
           room (if door?
                  (room* p pos kind-at budget)
                  (when (and closed? (not @unseen)) (room-of p cells)))
           region {:room room
                   :checks (delay (and room (not (enterable? p room kind-at)) (not (spider-gap? p room))))}]
       (when-not door?
         (.forEach (walker-sources kind-at cells [x y z])
                   (fn [c] (put! (str "wall|" budget "|" (aget c 0) "|" (aget c 1) "|" (aget c 2)) region))))
       region))))

(defn reaches?
  "Whether a walker from cell pos {:x :y :z} reaches, over the blocks the body has seen (doors shut, at most budget cells), a cell
  that (far? [x y z]) holds for; the flood ends at the first one. Kept per body (kept)."
  [p {:keys [x y z]} budget far?]
  (kept p (str "reaches|" budget "|" x "|" y "|" z)
        (fn [kind-at]
          (let [hit (volatile! false)]
            (flood-visit (partial forward kind-at) [x y z] budget
                         (fn [c] (when (far? c) (vreset! hit true) true)))
            @hit))))

(defn walk-steps
  "The steps of a walk (search-steps) from cell from to a cell next to cell to ({:x :y :z} each) over the blocks the
  body has seen, wooden doors opened (body-kind-of); nil when none is found. expanded! as search-steps'."
  ([p from to] (walk-steps p from to nil))
  ([p from to expanded!]
   (let [kind-at (fn [x y z] (body-kind-of (u/seen-block p {:x x :y y :z z})))
         r (search-steps (partial forward kind-at) (mapv from [:x :y :z]) (mapv to [:x :y :z]) nil expanded!)]
     (when (number? r) r))))

(defn closed-room?
  "Whether a walker in cell pos {:x :y :z} could reach fewer than room-cells cells with every door, gate and trapdoor
  shut (room): a hut a chased body can shut itself in."
  [p pos]
  (some? (room p pos)))
