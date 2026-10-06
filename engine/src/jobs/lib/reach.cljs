(ns jobs.lib.reach
  "Whether a hostile mob is a real danger to the body.
  A melee mob is one when it has a walkable way to the body: a bounded search
  as a zombie walks (one step up, up to three down, open doors only, water
  swum). A ranged mob is one when it has a line of fire: a ray from its eye to
  the body's eye or centre that no arrow-stopping block crosses.
  A mob walled off, across a pit, or with the body sealed in, is not.
  Candidates are the mobs the body knows of (known-hostiles).
  The mobs of one query share block reads (lookup) and walk proofs (proofs)."
  (:require [clojure.string :as str]
            [jobs.lib.combat :as combat]
            [jobs.lib.shelter :as sh]
            [jobs.lib.util :as u]))

(def node-budget
  "Cells one search expands before it gives up and answers 'unknown'."
  1500)

(def max-drop 3)

(defn openable? [name]
  (or (str/ends-with? name "_door") (str/ends-with? name "_fence_gate") (str/ends-with? name "_trapdoor")))

(defn open-prop? [b]
  (let [v (some-> b .-properties .-open)]
    (or (true? v) (= "true" v))))

(def hazard-blocks
  "Blocks a walker treats as walls and never stands on."
  #{"lava" "fire" "soul_fire" "magma_block" "cactus" "sweet_berry_bush"})

(def tall-block
  "Blocks with a 1.5-block collision box: nothing walking jumps onto them (fences, walls, shut fence gates)."
  #"_(fence|fence_gate|wall)$")

(defn kind-of
  "What a block is to a walker: :open, :water, :solid, or :tall (solid and too high to step onto: fence, wall, shut
  fence gate). An unloaded cell (nil) is open."
  [b]
  (let [name (some-> b .-name)]
    (cond
      (nil? name) :open
      (= "water" name) :water
      (hazard-blocks name) :solid
      (openable? name) (if (open-prop? b) :open (if (re-find tall-block name) :tall :solid))
      (re-find tall-block name) :tall
      (str/ends-with? name "_leaves") :solid
      (sh/solid? name) :solid
      :else :open)))

(def key-span
  "Blocks either side of a key's origin (x and z) that cell keys tell apart."
  524288)

(defn keyer
  "A function (key x y z) -> a number, one per cell within key-span of ox oz. Searches key their seen sets and the
  block cache on it (a number hashes faster than a vector)."
  [ox oz]
  (fn [x y z] (+ (* (+ (- x ox) key-span) 4294967296) (* (+ (- z oz) key-span) 4096) (+ y 2048))))

(defn state-lookup
  "A kind-at fn over a raw world reader (engine/js/raw-world.mjs). Reads the cell's state id and maps it with
  block-kind, once per state id. An unloaded cell (id < 0) maps as nil."
  [^js raw block-kind]
  (let [by-id (js/Map.)
        unloaded (block-kind nil)]
    (fn [x y z]
      (let [id (.stateAt raw x y z)]
        (if (< id 0)
          unloaded
          (let [hit (.get by-id id)]
            (if (some? hit)
              hit
              (let [v (block-kind (.stateInfo raw id))]
                (.set by-id id v)
                v))))))))

(defn lookup
  "A function (kind-at x y z) giving block-kind (default kind-of) of the block there.
  Reads state ids from p.rawWorld when there is one (state-lookup), else blockAt once per cell.
  One lookup serves one query."
  ([p] (lookup p kind-of))
  ([p block-kind]
   (if-let [raw (.-rawWorld p)]
     (state-lookup raw block-kind)
     (let [cache (js/Map.)
           key (volatile! nil)]
       (fn [x y z]
         (let [key-of (or @key (vreset! key (keyer x z)))
               k (key-of x y z)
               hit (.get cache k)]
           (if (some? hit)
             hit
             (let [v (block-kind (.blockAt p #js {:x x :y y :z z}))]
               (.set cache k v)
               v))))))))

(defn passable? [kind-at x y z]
  (let [k (kind-at x y z)]
    (not (or (keyword-identical? :solid k) (keyword-identical? :tall k)))))

(defn standable?
  "Feet and head cells free and something to stand on (a floor, or water)."
  [kind-at x y z]
  (and (passable? kind-at x y z) (passable? kind-at x (inc y) z)
       (or (keyword-identical? :water (kind-at x y z))
           (not (keyword-identical? :open (kind-at x (dec y) z))))))

;; Follows the planner's floor rule: a torch, sign or carpet is no floor.
(defn standable-cell?
  "Whether a body can stand with its feet in cell pos {:x :y :z}: feet and head free, and a solid floor below (not a
  torch, plant, rail, lava, fire, cactus or water)."
  [p {:keys [x y z]}]
  (let [kind-at (lookup p)
        below (.blockAt p #js {:x x :y (dec y) :z z})]
    (and (passable? kind-at x y z) (passable? kind-at x (inc y) z)
         (keyword-identical? :solid (kind-at x (dec y) z))
         (not (hazard-blocks (some-> below .-name))))))

(def hitbox-half 0.3)

(defn overlap [a0 a1 c] (max 0 (- (min a1 (inc c)) (max a0 c))))

(defn standing-cell
  "The feet cell {:x :y :z} the body is supported by, as the planner starts a walk (planner-tuned start-query): the cell
  under its centre when that stands (standable-cell?), else, for a body on a block's edge, the first other cell its
  0.6-wide hitbox overlaps (most overlap, then lower x, then lower z) that stands at the same feet height. Else the
  centre cell. kind-at: a lookup to read the blocks through (one query's reads shared), hazards then count as floor."
  ([p] (standing-cell p nil))
  ([p kind-at]
  (let [{:keys [x z] :as pos} (u/self-pos {:primitives p})
        centre (sh/cell pos)
        stands? (if kind-at
                  (fn [{:keys [x y z]}] (and (passable? kind-at x y z) (passable? kind-at x (inc y) z)
                                             (keyword-identical? :solid (kind-at x (dec y) z))))
                  #(standable-cell? p %))]
    (if (stands? centre)
      centre
      (or (->> (for [cx (range (js/Math.floor (- x hitbox-half)) (inc (js/Math.floor (+ x hitbox-half))))
                     cz (range (js/Math.floor (- z hitbox-half)) (inc (js/Math.floor (+ z hitbox-half))))
                     :let [area (* (overlap (- x hitbox-half) (+ x hitbox-half) cx) (overlap (- z hitbox-half) (+ z hitbox-half) cz))]
                     :when (and (pos? area) (not (and (== cx (:x centre)) (== cz (:z centre)))))]
                 [area cx cz])
               (sort-by (fn [[area cx cz]] [(- area) cx cz]))
               (map (fn [[_ cx cz]] {:x cx :y (:y centre) :z cz}))
               (filter stands?)
               first)
          centre)))))

(def dirs #js [#js [1 0] #js [-1 0] #js [0 1] #js [0 -1]])

(defn step-to
  "The cell #js [x y z] a walker at x y z reaches by stepping to the neighbour column nx nz (up one, level, or down up
  to max-drop), or nil."
  [kind-at x y z nx nz]
  (cond
    (and (not (passable? kind-at nx y nz)) (not (keyword-identical? :tall (kind-at nx y nz)))
         (passable? kind-at x (+ y 2) z) (standable? kind-at nx (inc y) nz))
    #js [nx (inc y) nz]
    (and (passable? kind-at nx y nz) (passable? kind-at nx (inc y) nz))
    (loop [k 0]
      (cond
        (> k max-drop) nil
        (standable? kind-at nx (- y k) nz) #js [nx (- y k) nz]
        (passable? kind-at nx (- y k) nz) (recur (inc k))
        :else nil))
    :else nil))

(defn forward
  "The cells #js [x y z] a walker standing at x y z steps to in one move."
  [kind-at x y z]
  (let [out #js []]
    (dotimes [i 4]
      (let [d (aget dirs i)]
        (when-let [c (step-to kind-at x y z (+ x (aget d 0)) (+ z (aget d 1)))]
          (.push out c))))
    out))

(defn backward
  "The standable cells #js [x y z] from which a walker steps to x y z (forward's edges reversed)."
  [kind-at x y z]
  (let [out #js []]
    (dotimes [i 4]
      (let [d (aget dirs i)
            nx (+ x (aget d 0))
            nz (+ z (aget d 1))]
        (loop [ny (dec y)]
          (when (< ny (+ y max-drop 2))
            (when (and (standable? kind-at nx ny nz)
                       (some-> (step-to kind-at nx ny nz x z) (aget 1) (== y)))
              (.push out #js [nx ny nz]))
            (recur (inc ny))))))
    out))

(defn heuristic [x y z tx ty tz]
  (+ (js/Math.abs (- x tx)) (js/Math.abs (- z tz)) (* 0.5 (js/Math.abs (- y ty)))))

(defn near-cell?
  "Whether x y z is within one block horizontally (diagonal included) and one vertically of tx ty tz."
  [x y z tx ty tz]
  (and (<= (+ (* (- x tx) (- x tx)) (* (- z tz) (- z tz))) 2) (<= (js/Math.abs (- y ty)) 1)))

(defn before?
  "Heap order of open entries #js [f g x y z]: lower f, then lower g."
  [a b]
  (let [fa (aget a 0) fb (aget b 0)]
    (or (< fa fb) (and (== fa fb) (< (aget a 1) (aget b 1))))))

(defn heap-swap! [h i j]
  (let [t (aget h i)]
    (aset h i (aget h j))
    (aset h j t)))

(defn heap-push! [h e]
  (.push h e)
  (loop [i (dec (.-length h))]
    (when (pos? i)
      (let [up (bit-shift-right (dec i) 1)]
        (when (before? (aget h i) (aget h up))
          (heap-swap! h i up)
          (recur up))))))

(defn heap-pop! [h]
  (let [top (aget h 0)
        last-e (.pop h)
        n (.-length h)]
    (when (pos? n)
      (aset h 0 last-e)
      (loop [i 0]
        (let [l (inc (* 2 i))
              r (inc l)
              m (if (and (< l n) (before? (aget h l) (aget h i))) l i)
              m (if (and (< r n) (before? (aget h r) (aget h m))) r m)]
          (when (not= m i)
            (heap-swap! h i m)
            (recur m)))))
    top))

(defn search
  "Best-first search from start [x y z] over (next-cells x y z) to a cell near target [x y z]: :found, :closed (every
  reachable cell expanded, no way) or :budget (node-budget cells expanded)."
  [next-cells [sx sy sz] [tx ty tz]]
  (let [key (keyer sx sz)
        seen (js/Set.)
        open #js []]
    (.add seen (key sx sy sz))
    (heap-push! open #js [(heuristic sx sy sz tx ty tz) 0 sx sy sz])
    (loop [n 0]
      (cond
        (zero? (.-length open)) :closed
        (>= n node-budget) :budget
        :else
        (let [top (heap-pop! open)
              g (aget top 1) x (aget top 2) y (aget top 3) z (aget top 4)]
          (if (near-cell? x y z tx ty tz)
            :found
            (let [nexts (next-cells x y z)
                  g' (inc g)]
              (dotimes [i (.-length nexts)]
                (let [c (aget nexts i)
                      cx (aget c 0) cy (aget c 1) cz (aget c 2)
                      k (key cx cy cz)]
                  (when-not (.has seen k)
                    (.add seen k)
                    (heap-push! open #js [(+ g' (heuristic cx cy cz tx ty tz)) g' cx cy cz]))))
              (recur (inc n)))))))))

(defn cell-of [pos] (let [{:keys [x y z]} (sh/cell pos)] [x y z]))

(defn way?
  "walkable-way? over kind-at (a lookup), from the mob's cell [x y z] to the body's."
  [kind-at mob body]
  (case (search (partial forward kind-at) mob body)
    :found true
    :closed false
    (not= :closed (search (partial backward kind-at) body mob))))

;; ---- proofs shared by the mobs of one danger query (one body cell)

(defn proofs
  "Facts the walk searches of one danger query have proved, shared with the next mob's search. Keyed by one key fn.
  :dead  cells with no way to the body (a closed search saw them all).
  :alive cells with a way to the body (on a found way).
  :reach near-closure's result, set once a search ran out of budget: the set of cells with a way, or :budget."
  [[bx _ bz]]
  #js {:key (keyer bx bz) :dead (js/Set.) :alive (js/Set.) :reach nil})

(defn forward-proving
  "search from the mob's cell over forward moves, using proofs: a dead cell is not expanded, an alive one counts as
  reaching the body. A closed search marks every cell it saw dead, a found one marks its way alive."
  [kind-at ^js pr [sx sy sz] [tx ty tz]]
  (let [key (.-key pr) dead (.-dead pr) alive (.-alive pr)
        parent (js/Map.)
        open #js []
        k0 (key sx sy sz)]
    (.set parent k0 -1)
    (heap-push! open #js [(heuristic sx sy sz tx ty tz) 0 sx sy sz k0])
    (loop [n 0]
      (cond
        (zero? (.-length open)) (do (.forEach parent (fn [_ k] (.add dead k))) :closed)
        (>= n node-budget) :budget
        :else
        (let [top (heap-pop! open)
              g (aget top 1) x (aget top 2) y (aget top 3) z (aget top 4) k (aget top 5)]
          (cond
            (or (near-cell? x y z tx ty tz) (.has alive k))
            (do (loop [c k] (when-not (== c -1) (.add alive c) (recur (.get parent c)))) :found)
            (.has dead k) (recur (inc n))
            :else
            (let [nexts (forward kind-at x y z)
                  g' (inc g)]
              (dotimes [i (.-length nexts)]
                (let [c (aget nexts i)
                      cx (aget c 0) cy (aget c 1) cz (aget c 2)
                      ck (key cx cy cz)]
                  (when-not (.has parent ck)
                    (.set parent ck k)
                    (heap-push! open #js [(+ g' (heuristic cx cy cz tx ty tz)) g' cx cy cz ck]))))
              (recur (inc n)))))))))

(def near-budget
  "Cells near-closure expands before it gives up."
  node-budget)

(def near-offsets
  "Offsets of the cells near-cell? counts as near: one either way on each axis."
  (vec (for [dx [-1 0 1] dy [-1 0 1] dz [-1 0 1]] [dx dy dz])))

(defn near-closure
  "Breadth-first back from the cells near the body over backward moves. Returns the set of keys of every cell with a
  way to the body, or :budget when more than near-budget cells were expanded. Every cell met is added to pr's alive."
  [kind-at ^js pr [bx by bz]]
  (let [key (.-key pr)
        ^js alive (.-alive pr)
        seen (js/Set.)
        queue #js []
        meet! (fn [x y z c]
                (let [k (key x y z)]
                  (when-not (.has seen k)
                    (.add seen k)
                    (.add alive k)
                    (.push queue c))))]
    (doseq [[dx dy dz] near-offsets]
      (let [x (+ bx dx) y (+ by dy) z (+ bz dz)]
        (meet! x y z #js [x y z])))
    (loop [head 0]
      (cond
        (== head (.-length queue)) seen
        (>= head near-budget) :budget
        :else (let [c (aget queue head)
                    nexts (backward kind-at (aget c 0) (aget c 1) (aget c 2))]
                (dotimes [i (.-length nexts)]
                  (let [n (aget nexts i)]
                    (meet! (aget n 0) (aget n 1) (aget n 2) n)))
                (recur (inc head)))))))

(defn proved-way?
  "way? using the proofs pr of the query's earlier mobs.
  Once near-closure has closed, the answer is a set lookup.
  Otherwise a forward search skips dead cells and stops at alive ones.
  If that runs out of budget, near-closure runs once for the query.
  If near-closure also runs out, a search back from the body decides, and unknown counts as a way.
  After one such unknown, every later mob counts as having a way."
  [kind-at ^js pr mob body]
  (let [[mx my mz] mob
        key (.-key pr)
        ^js alive (.-alive pr)
        mk (key mx my mz)
        ^js reach (.-reach pr)]
    (cond
      (instance? js/Set reach) (.has reach mk)
      (keyword-identical? reach :open) (not= :closed (forward-proving kind-at pr mob body))
      :else
      (case (forward-proving kind-at pr mob body)
        :found true
        :closed false
        (let [r (or reach (set! (.-reach pr) (near-closure kind-at pr body)))]
          (if (instance? js/Set r)
            (.has r mk)
            (let [back (search (fn [x y z]
                                 (let [ns (backward kind-at x y z)]
                                   (dotimes [i (.-length ns)]
                                     (let [n (aget ns i)] (.add alive (key (aget n 0) (aget n 1) (aget n 2)))))
                                   ns))
                               body mob)]
              (when (keyword-identical? back :budget) (set! (.-reach pr) :open))
              (not= :closed back))))))))

(defn walkable-way?
  "Whether a walker at the mob's cell can reach the body's. Searches from the mob; on budget, from the body (which
  settles a sealed body or a small island). Unknown counts as a way.
  solid: a set of cells treated as solid blocks. open: a set treated as air."
  ([p mob-pos body-pos] (walkable-way? p mob-pos body-pos #{}))
  ([p mob-pos body-pos solid] (walkable-way? p mob-pos body-pos solid #{}))
  ([p mob-pos body-pos solid open]
   (let [base (lookup p)
         kind-at (if (and (empty? solid) (empty? open))
                   base
                   (fn [x y z]
                     (let [c [x y z]]
                       (cond (contains? solid c) :solid
                             (contains? open c) :open
                             :else (base x y z)))))]
     (way? kind-at (cell-of mob-pos) (cell-of body-pos)))))

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

(defn flood
  "Breadth-first over (next-cells x y z) from start [x y z]: :closed when every reachable cell is expanded within budget
  cells, else :room."
  [next-cells [sx sy sz] budget]
  (let [key (keyer sx sz)
        seen (js/Set.)
        queue #js [#js [sx sy sz]]]
    (.add seen (key sx sy sz))
    (loop [head 0]
      (cond
        (== head (.-length queue)) :closed
        (>= (.-size seen) budget) :room
        :else (let [c (aget queue head)
                    nexts (next-cells (aget c 0) (aget c 1) (aget c 2))]
                (dotimes [i (.-length nexts)]
                  (let [n (aget nexts i)
                        k (key (aget n 0) (aget n 1) (aget n 2))]
                    (when-not (.has seen k)
                      (.add seen k)
                      (.push queue n))))
                (recur (inc head)))))))

(def door-panel-step
  "The step [dx dz] from a door cell onto the side its closed panel fills: opposite the door's facing."
  {"north" [0 1] "south" [0 -1] "east" [-1 0] "west" [1 0]})

(defn panel-step
  "The step [dx dz] from the cell {:x :y :z} across a shut door's panel, when kind-at counts the cell solid and the block
  there is a shut door, else nil. A body standing in the free part of that cell cannot walk through the panel."
  [p kind-at {:keys [x y z]}]
  (when (keyword-identical? :solid (kind-at x y z))
    (let [b (.blockAt p #js {:x x :y y :z z})
          name (some-> b .-name)]
      (when (and name (str/ends-with? name "_door") (not (open-prop? b)))
        (door-panel-step (some-> b .-properties .-facing))))))

(defn shut-in?
  "Whether the body is shut in: it can walk to fewer than room-cells cells over kind-at. A body in the free part of a
  shut door's cell cannot step across the panel."
  [p kind-at]
  (let [{:keys [x y z] :as start} (standing-cell p kind-at)
        panel (panel-step p kind-at start)
        next-cells (fn [cx cy cz]
                     (let [ns (forward kind-at cx cy cz)]
                       (if (and panel (== cx x) (== cy y) (== cz z))
                         (.filter ns (fn [n] (not (and (== (- (aget n 0) x) (first panel)) (== (- (aget n 2) z) (second panel))))))
                         ns)))]
    (= :closed (flood next-cells [x y z] room-cells))))

(defn enclosed?
  "Whether the body is shut in: it can walk to fewer than room-cells cells (one step up, up to three down, water swum,
  wooden doors opened). A pit or sealed room is; open ground or a hut with a door is not. A body in the free part of a
  shut door's cell cannot step across the panel. False with no body position.
  solid: a set of cells [x y z] treated as solid blocks (what if they were filled)."
  ([p] (enclosed? p #{}))
  ([p solid]
   (if (some-> (.self p) .-pos)
     (let [base (lookup p body-kind-of)
           kind-at (if (empty? solid)
                     base
                     (fn [x y z] (if (contains? solid [x y z]) :solid (base x y z))))]
       (shut-in? p kind-at))
     false)))

(def arrow-passes
  "Blocks, besides shelter's non-solid and walk-through ones, that an arrow flies through: plants, torches, ladders,
  fences and gates."
  #"_(sapling|flower|tulip|roots|torch|fence|fence_gate|bush)$|^(poppy|dandelion|blue_orchid|allium|azure_bluet|oxeye_daisy|cornflower|lily_of_the_valley|wither_rose|sunflower|lilac|rose_bush|peony|torchflower|pitcher_plant|brown_mushroom|red_mushroom|sugar_cane|kelp|kelp_plant|lily_pad|ladder|nether_sprout|wheat|carrots|potatoes|beetroots|bubble_column|pink_petals|wildflowers|leaf_litter|short_dry_grass|tall_dry_grass|hanging_roots|glow_lichen|moss_carpet|redstone_torch|soul_torch|light|structure_void)$")

(defn arrow-kind-of
  "What a block b is to an arrow: :open or :solid. Unloaded (nil) is open. Doors, gates and trapdoors follow their
  open property. Shelter's non-solid and walk-through blocks and arrow-passes are open. Anything else stops it."
  [b]
  (let [name (some-> b .-name)]
    (cond
      (nil? name) :open
      (openable? name) (if (open-prop? b) :open :solid)
      (sh/non-solid name) :open
      (re-find sh/walk-through name) :open
      (re-find arrow-passes name) :open
      :else :solid)))

(def eye-height 1.62)

(defn ray-clear?
  "Whether the segment from a to b ([x y z] numbers) crosses no :solid cell of kind-at (start and end cells do not
  count). Same cell walk as engine/js/sight.mjs lineClear."
  [kind-at [ax ay az] [bx by bz]]
  (let [dx (- bx ax) dy (- by ay) dz (- bz az)
        end-x (js/Math.floor bx) end-y (js/Math.floor by) end-z (js/Math.floor bz)
        sx (js/Math.sign dx) sy (js/Math.sign dy) sz (js/Math.sign dz)
        tdx (if (zero? dx) js/Infinity (js/Math.abs (/ 1 dx)))
        tdy (if (zero? dy) js/Infinity (js/Math.abs (/ 1 dy)))
        tdz (if (zero? dz) js/Infinity (js/Math.abs (/ 1 dz)))
        cx0 (js/Math.floor ax) cy0 (js/Math.floor ay) cz0 (js/Math.floor az)
        tmax (fn [d cur a td] (if (zero? d) js/Infinity (* (if (pos? d) (- (inc cur) a) (- a cur)) td)))]
    (loop [cx cx0 cy cy0 cz cz0
           tx (tmax dx cx0 ax tdx) ty (tmax dy cy0 ay tdy) tz (tmax dz cz0 az tdz)
           n (+ (js/Math.abs (- end-x cx0)) (js/Math.abs (- end-y cy0)) (js/Math.abs (- end-z cz0)))]
      (if (zero? n)
        true
        (let [axis (cond (and (<= tx ty) (<= tx tz)) :x (<= ty tz) :y :else :z)
              t (case axis :x tx :y ty :z tz)]
          (if (> t 1)
            true
            (let [cx' (if (= axis :x) (+ cx sx) cx)
                  cy' (if (= axis :y) (+ cy sy) cy)
                  cz' (if (= axis :z) (+ cz sz) cz)]
              (cond
                (and (== cx' end-x) (== cy' end-y) (== cz' end-z)) true
                (keyword-identical? :solid (kind-at cx' cy' cz')) false
                :else (recur cx' cy' cz'
                             (if (= axis :x) (+ tx tdx) tx) (if (= axis :y) (+ ty tdy) ty) (if (= axis :z) (+ tz tdz) tz)
                             (dec n))))))))))

(defn line-of-fire?
  "Whether a ranged mob at mob-pos ({:x :y :z}, feet) has a clear arrow line to the body at body-pos (feet): a ray
  from the mob's eye to the body's eye or centre."
  [arrow-at mob-pos body-pos]
  (let [from [(:x mob-pos) (+ (:y mob-pos) eye-height) (:z mob-pos)]
        bx (:x body-pos) bz (:z body-pos)]
    (boolean (some #(ray-clear? arrow-at from [bx (+ (:y body-pos) %) bz]) [eye-height 0.9]))))

(defn seen-mob?
  "Whether the body has seen or heard hostile e: the seen or heard flag of a known-hostiles entry, else (raw entity)
  its visible field."
  [e]
  (let [s (.-seen e)]
    (if (some? s) (or (true? s) (true? (.-heard e))) (true? (.-visible e)))))

(defn known-hostiles
  "The hostiles the body knows of within radius (ranged ones within :ranged-radius), nearest first. Uses the
  perception's mob memory (p.knownMobs) when p has one, else combat/hostiles."
  [p radius {:keys [ranged-radius]}]
  (if-let [known (.-knownMobs p)]
    (let [rr (or ranged-radius radius)]
      (filterv #(and (= "hostile" (.-kind %)) (<= (.-distance %) (if (combat/ranged? %) rr radius)))
               (array-seq (.call known p))))
    (combat/hostiles p radius {:ranged-radius ranged-radius})))

(defn danger-in?
  "danger? reading blocks through kind-at (shared by the mobs of one query) and, with pr, sharing walk proofs."
  ([p kind-at e opts] (danger-in? p kind-at nil e opts))
  ([p kind-at pr e {:keys [sight?] :or {sight? true}}]
   (if (combat/ranged? e)
     (line-of-fire? (lookup p arrow-kind-of) (u/pos-of (.-pos e)) (u/pos-of (.-pos (.self p))))
     (and (or (seen-mob? e) (not sight?))
          (let [mob (cell-of (u/pos-of (.-pos e)))
                body (cell-of (u/pos-of (.-pos (.self p))))]
            (if pr (proved-way? kind-at pr mob body) (way? kind-at mob body)))))))

(defn danger?
  "Whether hostile e (JS entity) is a real danger to the body.
  A ranged mob needs a line of fire. A melee mob needs a walkable way to the body and, unless :sight? is false, to
  have been seen (seen-mob?)."
  ([p e] (danger? p e {}))
  ([p e opts] (danger-in? p (lookup p) e opts)))

(defn query-proofs [p] (proofs (cell-of (u/pos-of (.-pos (.self p))))))

(defn dangers
  "The hostiles of known-hostiles (same opts) that are real dangers, nearest first."
  ([p radius opts] (dangers p radius opts {}))
  ([p radius opts danger-opts]
   (let [kind-at (lookup p)
         pr (query-proofs p)]
     (filterv #(danger-in? p kind-at pr % danger-opts) (known-hostiles p radius opts)))))

(defn nearest-danger
  "The nearest of known-hostiles (same opts) that is a real danger, or nil. danger-opts as danger?, plus :skip, a set of
  mob ids to leave out. Stops at the first danger."
  [p radius opts {:keys [skip] :as danger-opts}]
  (let [skip (set skip)
        kind-at (lookup p)
        pr (delay (query-proofs p))
        danger-opts (dissoc danger-opts :skip)]
    (some #(when (and (not (contains? skip (.-id %))) (danger-in? p kind-at @pr % danger-opts)) %)
          (known-hostiles p radius opts))))
