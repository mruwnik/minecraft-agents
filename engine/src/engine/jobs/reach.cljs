(ns engine.jobs.reach
  "Whether a hostile mob is a real danger to the body: a melee mob that has a
  walkable way to the body (a bounded search over the blocks as a zombie
  walks: one step up, up to three down, doors only when open, water swum),
  and a ranged mob (skeleton and the like) that has a line of fire (a ray from
  its eye to the body's eye or centre that no arrow-stopping block crosses:
  glass, leaves, solids and shut trapdoors stop it; air, grass, flowers and
  torches do not). A mob walled off, across a pit it cannot climb out of, or with the
  body sealed in, is not."
  (:require [clojure.string :as str]
            [engine.jobs.combat :as combat]
            [engine.jobs.shelter :as sh]
            [engine.jobs.util :as u]))

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

(defn kind-of
  "What a block block is to a walker: :open, :water or :solid. An unloaded cell (nil) is open."
  [b]
  (let [name (some-> b .-name)]
    (cond
      (nil? name) :open
      (= "water" name) :water
      (hazard-blocks name) :solid
      (openable? name) (if (open-prop? b) :open :solid)
      (str/ends-with? name "_leaves") :solid
      (sh/solid? name) :solid
      :else :open)))

(def key-span
  "Blocks either side of a key's origin (x and z) that cell keys tell apart; a search budget never strays that far."
  524288)

(defn keyer
  "A function (key x y z) -> a number, one per cell within key-span blocks (x and z) of ox oz: what the searches' seen
  sets and the block cache are keyed on (a number hashes far faster than a vector)."
  [ox oz]
  (fn [x y z] (+ (* (+ (- x ox) key-span) 4294967296) (* (+ (- z oz) key-span) 4096) (+ y 2048))))

(defn lookup
  "A function (kind-at x y z) over primitives p: block-kind (default kind-of) of the block there, each cell read once for
  the lookup's life. One lookup is one query (a search, or the searches of every mob of one danger query)."
  ([p] (lookup p kind-of))
  ([p block-kind]
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
             v)))))))

(defn passable? [kind-at x y z] (not (keyword-identical? :solid (kind-at x y z))))

(defn standable?
  "Feet and head cells free and something to stand on (a floor, or water)."
  [kind-at x y z]
  (and (passable? kind-at x y z) (passable? kind-at x (inc y) z)
       (or (keyword-identical? :water (kind-at x y z))
           (not (keyword-identical? :open (kind-at x (dec y) z))))))

;; the one job-side rule for a cell a body can stand in (restore-broken's clear-cell asks it); it follows the planner's
;; floor: a torch, a sign or a carpet is no floor, so go-to refuses such a goal as :goal-not-standable
(defn standable-cell?
  "Whether a body can stand with its feet in cell pos {:x :y :z} of primitives p: feet and head cells free and a floor
  below that is a solid block (a torch, plant, rail, lava, fire, cactus or water is none)."
  [p {:keys [x y z]}]
  (let [kind-at (lookup p)
        below (.blockAt p #js {:x x :y (dec y) :z z})]
    (and (passable? kind-at x y z) (passable? kind-at x (inc y) z)
         (keyword-identical? :solid (kind-at x (dec y) z))
         (not (hazard-blocks (some-> below .-name))))))

(def dirs #js [#js [1 0] #js [-1 0] #js [0 1] #js [0 -1]])

(defn step-to
  "The cell #js [x y z] a walker standing at x y z reaches stepping to column nx nz next to it (one up, level, or a drop
  of up to max-drop), or nil."
  [kind-at x y z nx nz]
  (cond
    (and (not (passable? kind-at nx y nz)) (passable? kind-at x (+ y 2) z) (standable? kind-at nx (inc y) nz))
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
  "The standable cells #js [x y z] from which a walker steps to x y z (forward's edges reversed: the one step a cell
  next to it takes toward its column lands on it)."
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

(defn walkable-way?
  "Whether a walker at the mob's cell can reach the body's. A search from the
  mob; if it runs out of budget, one from the body (which settles a body
  sealed in or on a small island); unknown counts as a way. With a set of
  cells solid, those cells count as solid blocks (what the way would be were
  they filled); with a set of cells open, those count as air (what the way would be were they dug)."
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
  "Standable cells a flood from the body reaches before the body counts as having room (not enclosed). Large enough
  that a wide, shallow hollow (20x20, 2 deep: about 400 cells) still floods as closed."
  1024)

(defn body-kind-of
  "kind-of for the body's own walk: a shut wooden door, gate or trapdoor is :open (a hand opens it); iron ones are not."
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

(defn enclosed?
  "Whether the body of primitives p is shut in: the cells it can walk to as a player does (one step up, up to three down,
  water swum, wooden doors, gates and trapdoors opened) number fewer than room-cells (1024). A pit or a sealed room is; open
  ground, or a hut with a door, is not. False with no body position."
  [p]
  (if (some-> (.self p) .-pos)
    (let [kind-at (lookup p body-kind-of)
          {:keys [x y z]} (sh/feet p)]
      (= :closed (flood (partial forward kind-at) [x y z] room-cells)))
    false))

(def arrow-passes
  "Blocks (beyond shelter's non-solid and walk-through ones) an arrow flies through: plants and flowers, torches,
  ladders, fences and gates (thin posts), snow layers."
  #"_(sapling|flower|tulip|roots|torch|fence|fence_gate|bush)$|^(poppy|dandelion|blue_orchid|allium|azure_bluet|oxeye_daisy|cornflower|lily_of_the_valley|wither_rose|sunflower|lilac|rose_bush|peony|torchflower|pitcher_plant|brown_mushroom|red_mushroom|sugar_cane|kelp|kelp_plant|lily_pad|ladder|nether_sprout|wheat|carrots|potatoes|beetroots|bubble_column|pink_petals|wildflowers|leaf_litter|short_dry_grass|tall_dry_grass|hanging_roots|glow_lichen|moss_carpet|redstone_torch|soul_torch|light|structure_void)$")

(defn arrow-kind-of
  "What a block b is to an arrow: :open or :solid. An unloaded cell (nil) is open; a shut or open door, gate or
  trapdoor follows its open property; shelter's non-solid blocks, walk-through ones and arrow-passes fly through;
  anything else (glass, leaves, every solid) stops it."
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
  "Whether the segment from a to b ([x y z] numbers) crosses no :solid cell of kind-at (the start and end cells do not
  count): the cell walk of engine/js/sight.mjs lineClear."
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
  "Whether a ranged mob standing at mob-pos ({:x :y :z}, its feet) has a clear arrow line to the body at body-pos
  (feet): a ray from the mob's eye to the body's eye or to its centre that no arrow-stopping block crosses. A few dozen
  block reads."
  [arrow-at mob-pos body-pos]
  (let [from [(:x mob-pos) (+ (:y mob-pos) eye-height) (:z mob-pos)]
        bx (:x body-pos) bz (:z body-pos)]
    (boolean (some #(ray-clear? arrow-at from [bx (+ (:y body-pos) %) bz]) [eye-height 0.9]))))

(defn danger-in?
  "danger? with the blocks read through kind-at (a lookup the caller shares between the mobs of one query)."
  [p kind-at e {:keys [sight?] :or {sight? true}}]
  (let [seen? (true? (.-visible e))]
    (if (combat/ranged? e)
      (line-of-fire? (lookup p arrow-kind-of) (u/pos-of (.-pos e)) (u/pos-of (.-pos (.self p))))
      (and (or seen? (not sight?))
           (way? kind-at (cell-of (u/pos-of (.-pos e))) (cell-of (u/pos-of (.-pos (.self p)))))))))

(defn danger?
  "Whether hostile e (JS entity) is a real danger to the body of primitives p.
  A ranged mob needs a line of fire (line-of-fire?); a melee mob needs a walkable
  way to the body and, unless :sight? is false, to be in sight."
  ([p e] (danger? p e {}))
  ([p e opts] (danger-in? p (lookup p) e opts)))

(defn dangers
  "The hostiles of combat/hostiles (same opts) that are real dangers, nearest first. The mobs' searches share one read
  of each block."
  ([p radius opts] (dangers p radius opts {}))
  ([p radius opts danger-opts]
   (let [kind-at (lookup p)]
     (filterv #(danger-in? p kind-at % danger-opts) (combat/hostiles p radius (dissoc opts :sight))))))

(defn nearest-danger
  "The nearest of combat/hostiles (same opts) that is a real danger, nil when none; danger-opts as danger?, plus :skip, a
  set of ids left out. It stops at the first danger found, so the walk search runs for no mob farther off; the mobs'
  searches share one read of each block."
  [p radius opts {:keys [skip] :as danger-opts}]
  (let [skip (set skip)
        kind-at (lookup p)
        danger-opts (dissoc danger-opts :skip)]
    (some #(when (and (not (contains? skip (.-id %))) (danger-in? p kind-at % danger-opts)) %)
          (combat/hostiles p radius (dissoc opts :sight)))))
