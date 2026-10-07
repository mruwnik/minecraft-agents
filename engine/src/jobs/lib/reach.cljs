(ns jobs.lib.reach
  "Block reads and walk searches for the danger checks: kind-of and lookup (what a block is to a walker or an arrow),
  the mob's bounded search (search, way?: one step up, up to three down, open doors only, water swum),
  enclosed? (the body shut in) and ray-clear?/line-of-fire? (a ray from a ranged mob's eye to the body's eye or centre
  that no arrow-stopping block crosses). jobs.lib.reach.proofs shares walk proofs between the mobs of one query;
  jobs.lib.danger decides which known mobs are real dangers."
  (:require [engine.game :as game]
            [clojure.string :as str]
            [engine.entity-observations :as obs]
            [engine.sight :as sight]
            [jobs.lib.combat :as combat]
            [jobs.lib.solid :as solid]
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
  #{"lava" "fire" "soul_fire" "magma_block" "cactus" "sweet_berry_bush" "campfire" "soul_campfire"})

(def tall-block
  "Blocks with a 1.5-block collision box: nothing walking jumps onto them (fences, walls, shut fence gates)."
  #"_(fence|fence_gate|wall)$")

(def arrow-passes
  "Blocks, besides solid's non-solid and walk-through ones, that an arrow flies through and a walker walks through
  (fences and gates are :tall to a walker first): plants, torches, ladders."
  #"_(sapling|flower|tulip|torch|fence|fence_gate|bush)$|^(poppy|dandelion|blue_orchid|allium|azure_bluet|oxeye_daisy|cornflower|lily_of_the_valley|wither_rose|sunflower|lilac|rose_bush|peony|torchflower|pitcher_plant|brown_mushroom|red_mushroom|sugar_cane|kelp|kelp_plant|lily_pad|ladder|nether_sprout|wheat|carrots|potatoes|beetroots|bubble_column|pink_petals|wildflowers|leaf_litter|short_dry_grass|tall_dry_grass|warped_roots|crimson_roots|hanging_roots|glow_lichen|moss_carpet|redstone_torch|soul_torch|light|structure_void)$")

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
      (re-find arrow-passes name) :open
      (solid/solid? name) :solid
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
             (let [v (block-kind (u/block-at p {:x x :y y :z z}))]
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
  ([p pos] (standable-cell? p pos (lookup p)))
  ([p {:keys [x y z]} kind-at]
   (let [below (u/block-at p {:x x :y (dec y) :z z})]
     (and (passable? kind-at x y z) (passable? kind-at x (inc y) z)
          (keyword-identical? :solid (kind-at x (dec y) z))
          (not (hazard-blocks (some-> below .-name)))))))


(defn overlap [a0 a1 c] (max 0 (- (min a1 (inc c)) (max a0 c))))

(defn standing-cell
  "The feet cell {:x :y :z} the body is supported by, as the planner starts a walk (planner-tuned start-query): the cell
  under its centre when that stands (standable-cell?), else, for a body on a block's edge, the first other cell its
  0.6-wide hitbox overlaps (most overlap, then lower x, then lower z) that stands at the same feet height. Else the
  centre cell. kind-at: a lookup to read the blocks through (one query's reads shared), hazards then count as floor."
  ([p] (standing-cell p nil))
  ([p kind-at]
  (let [{:keys [x z] :as pos} (u/self-pos {:primitives p})
        centre (solid/cell pos)
        stands? (if kind-at
                  (fn [{:keys [x y z]}] (and (passable? kind-at x y z) (passable? kind-at x (inc y) z)
                                             (keyword-identical? :solid (kind-at x (dec y) z))))
                  #(standable-cell? p %))]
    (if (stands? centre)
      centre
      (or (->> (for [cx (range (js/Math.floor (- x game/hitbox-half)) (inc (js/Math.floor (+ x game/hitbox-half))))
                     cz (range (js/Math.floor (- z game/hitbox-half)) (inc (js/Math.floor (+ z game/hitbox-half))))
                     :let [area (* (overlap (- x game/hitbox-half) (+ x game/hitbox-half) cx) (overlap (- z game/hitbox-half) (+ z game/hitbox-half) cz))]
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

(defn cell-of [pos] (let [{:keys [x y z]} (solid/cell pos)] [x y z]))

(defn way?
  "walkable-way? over kind-at (a lookup), from the mob's cell [x y z] to the body's."
  [kind-at mob body]
  (case (search (partial forward kind-at) mob body)
    :found true
    :closed false
    (not= :closed (search (partial backward kind-at) body mob))))

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
    (let [b (u/block-at p {:x x :y y :z z})
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

(defn arrow-kind-of
  "What a block b is to an arrow: :open or :solid. Unloaded (nil) is open. Doors, gates and trapdoors follow their
  open property. solid's non-solid and walk-through blocks and arrow-passes are open. Anything else stops it."
  [b]
  (let [name (some-> b .-name)]
    (cond
      (nil? name) :open
      (openable? name) (if (open-prop? b) :open :solid)
      (solid/non-solid name) :open
      (re-find solid/walk-through name) :open
      (re-find arrow-passes name) :open
      :else :solid)))


(defn ray-clear?
  "Whether the segment from a to b ([x y z] numbers) crosses no :solid cell of kind-at (start and end cells do not
  count). The cell walk is engine.sight/line-clear."
  [kind-at [ax ay az] [bx by bz]]
  (sight/line-clear ax ay az bx by bz (fn [x y z] (keyword-identical? :solid (kind-at x y z)))))

(defn line-of-fire?
  "Whether a ranged mob at mob-pos ({:x :y :z}, feet) has a clear arrow line to the body at body-pos (feet): a ray
  from the mob's eye to the body's eye or centre."
  [arrow-at mob-pos body-pos]
  (let [from [(:x mob-pos) (+ (:y mob-pos) game/eye-height) (:z mob-pos)]
        bx (:x body-pos) bz (:z body-pos)]
    (boolean (some #(ray-clear? arrow-at from [bx (+ (:y body-pos) %) bz]) [game/eye-height 0.9]))))
