(ns jobs.lib.reach.room
  "Rooms and enclosure for the danger checks: enclosed? (the body shut in), room (the closed room around a cell, every
  door shut, kept per body), the floods under them and the unseen-edge reads that say 'look first'.
  See jobs.lib.reach for the block reads and walk searches."
  (:require [clojure.string :as str]
            [engine.perception.rays :as rays]
            [jobs.lib.reach :refer [openable? open-prop? kind-of keyer lookup passable? seen-standable? standing-cell
                                    forward backward search-steps]]
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
  visiting order, :closed? whether every reachable cell was expanded within budget}."
  [next-cells [sx sy sz] budget]
  (let [key (keyer sx sz)
        seen (js/Set.)
        queue #js [#js [sx sy sz]]]
    (.add seen (key sx sy sz))
    (loop [head 0]
      (cond
        (== head (.-length queue)) {:cells queue :closed? true}
        (>= (.-size seen) budget) {:cells queue :closed? false}
        :else (let [c (aget queue head)
                    nexts (next-cells (aget c 0) (aget c 1) (aget c 2))]
                (dotimes [i (.-length nexts)]
                  (let [n (aget nexts i)
                        k (key (aget n 0) (aget n 1) (aget n 2))]
                    (when-not (.has seen k)
                      (.add seen k)
                      (.push queue n))))
                (recur (inc head)))))))

(defn flood-cells
  "flood-visit's cells when every reachable cell was expanded within budget, else nil."
  [next-cells start budget]
  (let [{:keys [cells closed?]} (flood-visit next-cells start budget)]
    (when closed? cells)))

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

(defn enclosed?
  "Whether the body is shut in: it can walk to fewer than room-cells cells (one step up, up to three down, water swum,
  wooden doors opened). A pit or sealed room is; open ground or a hut with a door is not. A body in the free part of a
  shut door's cell cannot step across the panel. False with no body position.
  solid: a set of cells [x y z] treated as solid blocks (what if they were filled)."
  ([p] (enclosed? p #{}))
  ([p solid] (enclosed? p solid (lookup p body-kind-of)))
  ([p solid base] (enclosed? p solid base room-cells))
  ([p solid base budget]
   (if (some-> (.self p) .-pos)
     (shut-in? p (if (empty? solid)
                   base
                   (fn [x y z] (if (contains? solid [x y z]) :solid (base x y z))))
               budget)
     false)))

(defn closes-in?
  "Whether the body has room to walk now but would be left fewer than shut-room-cells cells with cell [x y z] solid.
  Both floods read the cells through one lookup; the filled one goes first, as it is mostly open."
  [p cell]
  (let [base (lookup p body-kind-of)]
    (boolean (and (enclosed? p #{(vec cell)} base shut-room-cells) (not (enclosed? p #{} base))))))

(defn walls-in?
  "Whether a walker on the floor under pos [x y z] (within four cells) can walk to fewer than 64 cells with cell solid,
  and to more without, shut in or not (a small room only: the floods stay cheap)."
  [p [x y z] cell]
  (let [base (lookup p body-kind-of)
        floor-y (first (filter #(and (passable? base x % z) (passable? base x (inc %) z) (keyword-identical? :solid (base x (dec %) z)))
                               (range (dec y) (- y 5) -1)))
        filled (fn [x y z] (if (= [x y z] (vec cell)) :solid (base x y z)))
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
  a side block it could step onto, is unseen. Unseen is unknown, not wall."
  [p kind-at c]
  (let [x (aget c 0) y (aget c 1) z (aget c 2)
        unseen? (fn [x y z] (nil? (u/seen-block p {:x x :y y :z z})))]
    (some (fn [[dx dz]]
            (let [nx (+ x dx) nz (+ z dz)]
              (or (unseen? nx y nz)
                  (and (not (passable? kind-at nx y nz)) (passable? kind-at x (+ y 2) z) (unseen? nx (inc y) nz)))))
          [[1 0] [-1 0] [0 1] [0 -1]])))

(defn room-flood
  "{:cells [...] :unseen? bool}: the cells a walker at cell [x y z] reaches (room-kind-at) fewer than room-cells, and
  whether any is an unseen-edge?; nil when the flood is open."
  [p kind-at x y z]
  (let [kind-at (room-kind-at p kind-at)]
    (when-let [cells (flood-cells (partial forward kind-at) [x y z] room-cells)]
      {:cells cells :unseen? (boolean (some #(unseen-edge? p kind-at %) cells))})))

(defn room-from
  "The closed room a walker at cell [x y z] reaches (room*'s answer), nil when it is no closed room or any cell of it is
  an unseen-edge?."
  [p kind-at x y z]
  (let [{:keys [cells unseen?]} (room-flood p kind-at x y z)]
    (when (and cells (not unseen?))
      (let [cells (set (map vec cells))
            doors (for [[cx cy cz] cells
                        [dx dz] [[1 0] [-1 0] [0 1] [0 -1]]
                        :let [cell {:x (+ cx dx) :y cy :z (+ cz dz)}
                              name (some-> (u/seen-block p cell) .-name)]
                        :when (and name (openable? name) (not (str/ends-with? name "_trapdoor")))]
                    {:cell cell :inside {:x cx :y cy :z cz}})]
        {:cells cells :doors (vec (vals (into {} (map (juxt :cell identity)) doors)))}))))

(defn shut-kind-at
  "kind-of over the blocks the body has seen, every door, gate and trapdoor shut."
  [p]
  (memo-kind-at
   (fn [x y z]
     (let [b (u/seen-block p {:x x :y y :z z})]
       (if (and b (openable? (.-name b))) :solid (kind-of b))))))

(defn room*
  "The closed room around cell pos {:x :y :z}: the cells a walker there reaches with every door, gate and trapdoor shut,
  fewer than room-cells (flood over the blocks the body has seen, room-kind-at; no closed room where it meets an unseen
  or unloaded cell, unseen-edge?). {:cells #{[x y
  z]} feet cells, :doors [{:cell {:x :y :z} :inside {:x :y :z}}] each door or gate beside a room cell at its feet, with
  that room cell}; nil when it is no closed room. A door or gate cell is the room's boundary whether open or shut, so
  a pos in one is judged by the first closed room beside it."
  [p {:keys [x y z]}]
  (let [kind-at (shut-kind-at p)
        in-door? (some-> (u/seen-block p {:x x :y y :z z}) .-name openable?)]
    (if in-door?
      (some (fn [[dx dz]] (room-from p kind-at (+ x dx) y (+ z dz))) [[1 0] [-1 0] [0 1] [0 -1]])
      (room-from p kind-at x y z))))

(defn enterable?
  "Whether a walker on seen ground outside the room (room*'s answer) steps into one of its cells: a pit under a roof is open
  to a mob on its rim though the body in it cannot climb out."
  [p {:keys [cells]}]
  (let [kind-at (room-kind-at p (shut-kind-at p))]
    (boolean (some (fn [[x y z]]
                     (some #(and (not (cells (vec %))) (seen-standable? p {:x (aget % 0) :y (aget % 1) :z (aget % 2)}))
                           (backward kind-at x y z)))
                   cells))))

(defn unseen-room?
  "Whether the cell pos {:x :y :z} holds a room that closes over what the body has seen but has a cell it has not seen
  beside it (room* nil for that reason, not because the room is open): it is unknown, look first."
  [p {:keys [x y z]}]
  (let [kind-at (shut-kind-at p)
        in-door? (some-> (u/seen-block p {:x x :y y :z z}) .-name openable?)
        starts (if in-door? (map (fn [[dx dz]] [(+ x dx) y (+ z dz)]) [[1 0] [-1 0] [0 1] [0 -1]]) [[x y z]])]
    (boolean (some (fn [[sx sy sz]] (:unseen? (room-flood p kind-at sx sy sz))) starts))))

(defn unseen-sides
  "The feet cells [x y z] beside the room cells of the room around pos {:x :y :z} (as unseen-room?) that the body has not
  seen: what a look into the room has to show."
  [p {:keys [x y z]}]
  (let [kind-at (shut-kind-at p)
        in-door? (some-> (u/seen-block p {:x x :y y :z z}) .-name openable?)
        starts (if in-door? (map (fn [[dx dz]] [(+ x dx) y (+ z dz)]) [[1 0] [-1 0] [0 1] [0 -1]]) [[x y z]])]
    (->> starts
         (mapcat (fn [[sx sy sz]]
                   (for [c (:cells (room-flood p kind-at sx sy sz))
                         [dx dz] [[1 0] [-1 0] [0 1] [0 -1]]
                         :let [cell [(+ (aget c 0) dx) (aget c 1) (+ (aget c 2) dz)]]
                         :when (nil? (u/seen-block p {:x (cell 0) :y (cell 1) :z (cell 2)}))]
                     cell)))
         distinct
         vec)))

(defn unroofed-cell
  "{:cell :closed?}: the first cell a walker flood from pos {:x :y :z} (doors shut, within room-cells) reaches with no
  block seen within room-roof cells over it ({:x :y :z}, nil when every cell is roofed), and whether the flood closed.
  shut, a set of [x y z] cells, counts as solid."
  ([p pos] (unroofed-cell p pos nil))
  ([p {:keys [x y z]} shut]
   (let [kind-at (fn [cx cy cz]
                   (let [b (u/seen-block p {:x cx :y cy :z cz})]
                     (cond
                       (contains? shut [cx cy cz]) :solid
                       (and b (openable? (.-name b))) :solid
                       :else (kind-of b))))
         roofed? (fn [cx cy cz] (some #(let [b (u/seen-block p {:x cx :y (+ cy %) :z cz})] (and b (not= :open (kind-of b))))
                                      (range 2 7)))
         {:keys [cells closed?]} (flood-visit (fn [cx cy cz] (forward kind-at cx cy cz)) [x y z] room-cells)
         c (.find cells (fn [c] (not (roofed? (aget c 0) (aget c 1) (aget c 2)))))]
     {:cell (when c {:x (aget c 0) :y (aget c 1) :z (aget c 2)}) :closed? closed?})))

(defn escape-cell
  "Where a walker at pos {:x :y :z} gets out of a room with a hole (room* nil, doors shut): unroofed-cell's cell when the
  flood is open; nil when it is closed or every cell is roofed."
  [p pos]
  (let [{:keys [cell closed?]} (unroofed-cell p pos)]
    (when-not closed? cell)))

(def room-memo-ms
  "How long a room answer is reused while the body and the world stand still: the night trigger asks every 250 ms and a flood reads up to room-cells cells."
  2000)

(defn steady-key
  "What a room answer depends on: the raw world's epoch, the eye and the clock in room-memo-ms steps; nil offline."
  [{:keys [raw opts]}]
  (when-let [^js eye (.eye ^js raw)]
    (str (js/Math.floor (/ ((:now opts)) room-memo-ms)) "|" (rays/epoch-of raw) "|" (.-dimension eye) "|"
         (.-x eye) "|" (.-y eye) "|" (.-z eye) "|" (.-yaw eye) "|" (.-pitch eye))))

(defn room
  "room* kept per body (in its perception store) for room-memo-ms while its eye and the world stay the same; not kept without a perception."
  [p pos]
  (let [per (aget p "perception") k (some-> per steady-key)]
    (if-not k
      (room* p pos)
      (let [^js st (:st per)]
        (when-not (= k (.-roomKey st))
          (set! (.-roomKey st) k)
          (set! (.-roomMemo st) (js/Map.)))
        (let [^js m (.-roomMemo st) ck (str (:x pos) "|" (:y pos) "|" (:z pos))]
          (if (.has m ck)
            (.get m ck)
            (let [v (room* p pos)] (.set m ck v) v)))))))

(defn walk-steps
  "The steps of a walk (search-steps) from cell from to a cell next to cell to ({:x :y :z} each) over the blocks the
  body has seen, wooden doors opened (body-kind-of); nil when none is found."
  [p from to]
  (let [kind-at (fn [x y z] (body-kind-of (u/seen-block p {:x x :y y :z z})))
        r (search-steps (partial forward kind-at) (mapv from [:x :y :z]) (mapv to [:x :y :z]))]
    (when (number? r) r)))

(defn closed-room?
  "Whether a walker in cell pos {:x :y :z} could reach fewer than room-cells cells with every door, gate and trapdoor
  shut (room): a hut a chased body can shut itself in."
  [p pos]
  (some? (room p pos)))
