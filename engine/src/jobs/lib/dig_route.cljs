(ns jobs.lib.dig-route
  "The fewest cells to dig for a walkable way from one feet cell to another through stone: a pure search over what the
  caller says each cell is (block-at cell -> name, or nil when never seen)."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.armour :as armour]
            [jobs.lib.cost.ways :as cw]
            [jobs.lib.per-body :as per-body]
            [jobs.lib.reach :as reach]
            [jobs.lib.walk.world :as wworld]))

(a/defargs settings
  {::max-nodes {:default 500 :doc "The most cells a route search expands before it gives up (:budget)." :spec (a/int-in 1 nil)}})

(def falling #{"sand" "red_sand" "gravel"})

(defn open?
  "Whether a walker passes the block as it is: air, a plant, a torch, a gate (opened). A fence or wall stops it, though an arrow flies over."
  [n]
  (boolean (and n (or (bs/air n) (and (re-find reach/arrow-passes n) (not (re-find #"_(fence|wall)$" n)))))))

(defn cell-cost
  "Digs it takes to open a cell: 0 when open, 1 for a block (an unseen cell is dug to see), nil when it cannot be
  passed (a fluid, a hazard, bedrock, a falling block)."
  [n]
  (cond
    (open? n) 0
    (nil? n) 1
    (or (bs/fluids n) (bs/harms-in-cell n) (rules/unbreakable n) (falling n)) nil
    :else 1))

(defn floor-ok?
  "Whether a step may end on the block: a seen solid one, not a fence, wall or gate (1.5 high: nothing steps up onto it)."
  [n]
  (boolean (and n (not (open? n)) (not (re-find reach/tall-block n)) (not (bs/fluids n)) (not (bs/harms-in-cell n)))))

(defn step-cells
  "The cells that must be open to walk from feet cell m to the adjacent feet cell n (a fall included), or to jump-place
  up to the cell over m (a pillar's block)."
  [[mx my mz :as m] [nx ny nz :as n]]
  (cond
    (and (== mx nx) (== mz nz)) [[mx (+ my 2) mz]]
    (> ny my) [[(m 0) (+ my 2) (m 2)] n [nx (inc ny) nz]]
    (< ny my) (mapv (fn [y] [nx y nz]) (range (inc my) (dec ny) -1))
    :else [n [nx (inc ny) nz]]))

(defn- before? [^js a ^js b]
  (or (< (aget a 0) (aget b 0)) (and (== (aget a 0) (aget b 0)) (< (aget a 1) (aget b 1)))))

(defn- swap-at! [^js h i j]
  (let [t (aget h i)] (aset h i (aget h j)) (aset h j t)))

(defn- heap-push!
  "Push entry #js [price tick cell-key] onto binary heap h, cheapest (then earliest) on top."
  [^js h e]
  (.push h e)
  (loop [i (dec (.-length h))]
    (let [up (quot (dec i) 2)]
      (when (and (pos? i) (before? (aget h i) (aget h up)))
        (swap-at! h i up)
        (recur up)))))

(defn- heap-pop! [^js h]
  (let [top (aget h 0) last (.pop h) n (.-length h)]
    (when (pos? n)
      (aset h 0 last)
      (loop [i 0]
        (let [l (inc (* 2 i)) r (inc l)
              m (if (and (< l n) (before? (aget h l) (aget h i))) l i)
              m (if (and (< r n) (before? (aget h r) (aget h m))) r m)]
          (when-not (== m i)
            (swap-at! h i m)
            (recur m)))))
    top))

(def dirs #js [1 0 -1 0 0 1 0 -1])

(defn search
  "route's search begun: a fn of n that runs it on for at most n more cells expanded, [answer expanded]: answer route's
  once the search has ended (the same each run after), nil while it goes on; expanded the cells this run expanded.
  Plain numbers throughout (a cell is a key, its reads are cached by key): a climb expands about 10k cells."
  [block-at from goal {:keys [dig-s step-s stair-s place-s pillar-s fall-s max-places unseen? unseen-step-s ways max-dig-s margin] :or {dig-s (constantly 1) step-s 0 ways #{}} :as opts}]
  (let [[fx _ fz] from
        [gx gy gz] goal
        ;; one number a cell, for js Maps and Sets (a vector key is compared by identity there)
        key (fn [x y z] (+ (* (+ (- x fx) 1048576) 2147483648) (* (+ (- z fz) 1048576) 1024) y 512))
        cell-of (fn [k] (let [y (- (mod k 1024) 512)
                              r (/ (- k y 512) 1024)
                              z (+ (- (mod r 2097152) 1048576) fz)
                              x (+ (- (js/Math.floor (/ r 2097152)) 1048576) fx)]
                          [x y z]))
        ;; what a name is to the search, once a name (open? and floor-ok? each run regexes), and so once a cell
        kinds (js/Map.)
        kind-of (fn [n] (or (.get kinds n)
                            (let [k #js {:name n :cost (cell-cost n) :open (open? n) :floor (floor-ok? n)
                                         :fluid (boolean (bs/fluids n)) :falling (boolean (falling n))}]
                              (.set kinds n k) k)))
        read (js/Map.)
        at (fn [x y z] (let [i (key x y z)] (or (.get read i) (let [k (kind-of (block-at [x y z]))] (.set read i k) k))))
        priced (js/Map.)
        dig-at (fn [n x y z] (let [i (key x y z)] (if (.has priced i) (.get priced i) (let [s (dig-s n [x y z])] (.set priced i s) s))))
        unseen-cell? (let [m (js/Map.)] (fn [x y z] (let [i (key x y z) u (.get m i)] (if (some? u) u (let [u (boolean (unseen? [x y z]))] (.set m i u) u)))))
        ;; a step whose feet, head or floor is unseen, as cost/unseen-route-s prices one
        unseen-at (fn [x y z] (if (and unseen? (or (unseen-cell? x y z) (unseen-cell? x (inc y) z) (unseen-cell? x (dec y) z))) unseen-step-s 0))
        way-keys (let [s (js/Set.)] (doseq [[x y z] ways] (.add s (key x y z))) s)
        max-digs (:max-digs opts)
        ;; without a body's own, the drops a body with no hp to spend takes
        max-fall (or (:max-fall opts) (:max-drop (armour/fall-profile {})))
        fall-prices (let [a #js []] (dotimes [dy (inc max-fall)] (.push a (if (and fall-s (pos? dy)) (fall-s dy) 0))) a)
        ;; a fall that costs hp is taken only into a hole already open: dug, the stair down beside it costs no hp
        free-fall (count (take-while zero? fall-prices))
        [lx ly lz] (when margin (mapv #(- (min %1 %2) margin) from goal))
        [hx hy hz] (when margin (mapv #(+ (max %1 %2) margin) from goal))
        inside? (fn [x y z] (or (nil? margin) (and (<= lx x hx) (<= ly y hy) (<= lz z hz))))
        heuristic (fn [x y z] (* step-s (max (+ (js/Math.abs (- x gx)) (js/Math.abs (- z gz))) (- gy y))))
        solid-at? (fn [x y z] (let [^js k (at x y z)] (and (some? (.-name k)) (not (.-open k)))))
        ;; a block is placed only against a face (a solid neighbour), never on a step of the body's own stair (it would shut
        ;; it): :seen, :unseen when every face is a guess (it may be air), nil
        face (fn [x y z] (when (solid-at? x y z) (if (and unseen? (unseen-cell? x y z)) :unseen :seen)))
        placeable (fn [x y z] (when-not (.has way-keys (key x (dec y) z))
                                (let [a (face x (dec y) z) b (face (inc x) y z) c (face (dec x) y z) d (face x y (inc z)) e (face x y (dec z))]
                                  (cond (or (keyword-identical? :seen a) (keyword-identical? :seen b) (keyword-identical? :seen c)
                                            (keyword-identical? :seen d) (keyword-identical? :seen e)) :seen
                                        (or a b c d e) :unseen))))
        ;; the cost of opening cell x y z for a step, accumulated in acc #js [seconds digs dig-seconds dug-keys]; false when it may not be
        open-cell! (fn [^js acc x y z]
                     (let [^js k (at x y z)
                           d (.-cost k)]
                       (cond
                         (nil? d) false
                         (zero? d) true
                         (.-falling ^js (at x (inc y) z)) false
                         (.has way-keys (key x y z)) false
                         :else (let [s (dig-at (.-name k) x y z)]
                                 (if (nil? s)
                                   false
                                   (do (aset acc 0 (+ (aget acc 0) s)) (aset acc 1 (+ (aget acc 1) d)) (aset acc 2 (+ (aget acc 2) s)) (.push ^js (aget acc 3) (key x y z)) true))))))
        ;; key -> #js [seconds digs dig-seconds prev-key move blocks-placed dug] of the cheapest way found to the cell, dug the set
        ;; of the keys of every cell its steps dug
        best (js/Map.) done (js/Set.) queue #js []
        tick (volatile! 1)
        answer (volatile! nil)
        relax! (fn [ck ^js from-node ^js acc nx ny nz move]
                 (let [nk (key nx ny nz)
                       k (+ (aget from-node 1) (aget acc 1)) ds (+ (aget from-node 2) (aget acc 2))
                       placed (cond-> (aget from-node 5) (not (keyword-identical? :walk move)) inc)]
                   (when (and (or (nil? max-digs) (<= k max-digs)) (or (nil? max-dig-s) (<= ds max-dig-s))
                              (or (nil? max-places) (<= placed max-places)))
                     (let [nd (+ (aget from-node 0) (aget acc 0) (unseen-at nx ny nz))
                           ^js old (.get best nk)]
                       (when (or (nil? old) (< nd (aget old 0)))
                         (.set best nk #js [nd k ds ck move placed (let [dug (aget from-node 6) ks (aget acc 3)] (if (zero? (alength ks)) dug (into dug (array-seq ks))))])
                         (heap-push! queue #js [(+ nd (heuristic nx ny nz)) @tick nk])
                         (vswap! tick inc))))))
        ;; whether a step of the way to the cell of key k opened cell x y z: a floor the route itself digs away is none (a
        ;; stair that turns back over its own cut)
        dug-behind? (fn [k x y z] (contains? (aget (.get best k) 6) (key x y z)))
        expand! (fn [ck]
                  (let [[mx my mz] (cell-of ck)
                        node (.get best ck)
                        ;; a step to n opening the cell over m's head when head?, and n's column from top down to bot
                        ;; a step's unseen cells are priced once (relax!'s unseen-at): the cell over m's head and a guessed face
                        ;; (unseen-face?) only when n's own cells are seen
                        try! (fn [nx ny nz move s0 head? top bot unseen-face?]
                               (when (and (inside? nx ny nz) (not (.has done (key nx ny nz))))
                                 (let [acc #js [(+ s0 (if (and unseen? (zero? (unseen-at nx ny nz))
                                                               (or unseen-face? (and head? (unseen-cell? mx (+ my 2) mz))))
                                                        unseen-step-s 0))
                                                0 0 #js []]]
                                   (when (and (or (not head?) (open-cell! acc mx (+ my 2) mz))
                                              (loop [y top] (or (< y bot) (and (open-cell! acc nx y nz) (recur (dec y))))))
                                     ;; a step up that cuts or lays its floor is a stair's step: no less than :stair-s
                                     (when (and stair-s (> ny my) (not= :pillar move) (or (= :laid move) (pos? (aget acc 1))))
                                       (aset acc 0 (cw/stair-leg-cost (aget acc 0) step-s stair-s)))
                                     (relax! ck node acc nx ny nz move)))))]
                    (when-let [pf (and pillar-s (placeable mx my mz))]
                      (try! mx (inc my) mz :pillar pillar-s true 0 1 (= :unseen pf)))
                    (loop [i 0]
                      (when (< i 8)
                        (let [nx (+ mx (aget dirs i)) nz (+ mz (aget dirs (inc i)))
                              ny (inc my) ^js f (at nx my nz)
                              dug? (and (.-floor f) (dug-behind? ck nx my nz))]
                          ;; up: the head cell over m, n and its head
                          (cond
                            (and (.-floor f) (not dug?)) (try! nx ny nz :walk step-s true (inc ny) ny false)
                            (and place-s (or dug? (and (.-open f) (not (.-fluid f)))))
                            (when-let [pf (placeable nx my nz)]
                              (try! nx ny nz :laid (+ step-s place-s) true (inc ny) ny (= :unseen pf))))
                          ;; down by up to max-fall, through the cells it falls by, and level: n and its head
                          (loop [dy 0 clear? (and (.-open ^js (at nx (inc my) nz)) (.-open ^js (at nx my nz)))]
                            (when (<= dy max-fall)
                              (let [ny (- my dy)
                                    below-open? (.-open ^js (at nx (dec ny) nz))]
                                (when (and (.-floor ^js (at nx (dec ny) nz)) (not (dug-behind? ck nx (dec ny) nz)))
                                  (try! nx ny nz :walk (+ step-s (aget fall-prices dy)) false (if (zero? dy) (inc ny) (inc my)) ny false))
                                (when (or (< (inc dy) free-fall) (and clear? below-open?))
                                  (recur (inc dy) (and clear? below-open?))))))
                          (recur (+ i 2)))))))
        start-key (key fx (from 1) fz)]
    (.set best start-key #js [0 0 0 nil nil 0 #{}])
    (heap-push! queue #js [(heuristic fx (from 1) fz) 0 start-key])
    (fn [n]
      (let [start (.-size done)
            limit (+ start n)
            goal-key (key gx gy gz)
            a (or @answer
                  (loop []
                    (let [top (heap-pop! queue)
                          ck (some-> top (aget 2))]
                      (cond
                        (nil? top) {:problem :no-route}
                        (.has done ck) (recur)
                        (== ck goal-key)
                        (let [path (mapv cell-of (reverse (take-while some? (iterate #(aget (.get best %) 3) ck))))
                              steps (mapv (fn [m n]
                                            (let [move (aget (.get best (apply key n)) 4)]
                                              (cond-> {:to n :move move
                                                       :dig (vec (filter #(pos? (.-cost ^js (apply at %))) (sort-by #(- (% 1)) (step-cells m n))))}
                                                (= :laid move) (assoc :place (update n 1 dec))
                                                (= :pillar move) (assoc :place m))))
                                          path (rest path))]
                          {:dig (vec (distinct (mapcat :dig steps))) :steps steps :seconds (aget (.get best ck) 0)})
                        ;; the popped cell goes back: the next run expands it first
                        (>= (.-size done) limit) (do (heap-push! queue top) nil)
                        :else (do (.add done ck) (expand! ck) (recur))))))]
        (vreset! answer a)
        [a (- (.-size done) start)]))))

(defn body-prices
  "route's prices for the body of primitives p, as go-to prices its walks: a step's walk-cost, a fall's hp (fall-damage at
  hp-cost, at most the falls it survives), a block's dig-cost (nil: never dug; a nameless cell
  has none), a stair's step at least its stair-cost, and a step whose cell is (unseen? cell) at the unseen walk's price (wworld/unseen-share)."
  [p unseen?]
  (let [self (.self p)
        equipment (cost/equipment-of (.-equipment self))
        dig-cost (memoize cost/dig-cost)]
    {:max-fall (:max-drop (cost/fall-profile {:equipment equipment :damage-budget (cost/survivable-budget (wworld/self-body p) {})}))
     :fall-s (fn [blocks] (cost/hp-cost (.-health self) (cost/fall-damage equipment blocks)))
     :dig-s (fn [name _] (dig-cost name))
     :step-s (:cost (cost/walk-cost {:distance 1}))
     :stair-s (cw/stair-cost 1)
     :unseen? unseen?
     :unseen-step-s (cost/unseen-route-s (wworld/unseen-share p) (constantly :ground) [{:x 0 :y 0 :z 0}])}))

(defn route
  "The cells to dig (head cell first within a step, in walking order) of the cheapest route from feet cell from to feet
  cell goal, falls of at most :max-fall blocks (default: those a body with no hp to spend takes, cost.armour/fall-profile),
  each priced :fall-s (blocks -> seconds) on top of its step; each step's floor must be a seen solid block, unless the step is up and
  :place-s prices laying one (against a face, the stair job's floor). A* with the walked steps' price as the lower bound.
  Priced :step-s a step, (:dig-s name cell) a cell dug (default 0 and 1: the fewest digs), a step up that digs or
  lays its floor at least :stair-s (a stair's step) and :place-s a floor laid,
  :pillar-s a jump-placed block (a step straight up; none without it) and :unseen-step-s a step whose feet, head or
  floor is (:unseen? cell), or on a step up the cell over the head it leaves, or every face its placed block rests on; a
  block :dig-s prices nil is not dug, nor a cell under sand or gravel (it would fall on the body walking in), nor a
  floor of :ways (the floors of the body's own stairs), on which nothing is placed either. Bounds, each only when
  given: :max-dig-s seconds of digging, :max-digs cells, :max-places blocks laid or jump-placed (the blocks carried),
  :margin cells round the two ends.
  {:dig [cell ..] :steps [{:to feet :move :walk|:laid|:pillar :dig [cell ..] :place cell} ..] :seconds the route's price}, {:problem :no-route},
  or {:problem :budget} when :max-nodes cells were expanded first (search goes on past it)."
  [block-at from goal opts]
  (or (first ((search block-at from goal opts) (get opts :max-nodes (settings/get settings ::max-nodes)))) {:problem :budget}))

(defonce ^{:doc "{key search-fn} of the searches route-resumed left unfinished, the oldest dropped past kept-max."} kept (js/Map.))

(per-body/store! ::kept #(.clear kept))

(def kept-max 16)

(defn route-resumed
  "route, but a search that spends :max-nodes is kept under key and gone on by the next call with the same key:
  {:problem :searching} till it ends. A key names the caller and the search ([job-id :kind from goal]); a search never
  asked again falls out of kept."
  [key block-at from goal opts]
  (let [key (pr-str key)
        run (or (.get kept key) (search block-at from goal opts))
        [a] (run (get opts :max-nodes (settings/get settings ::max-nodes)))]
    (.delete kept key)
    (when-not a
      (.set kept key run)
      (when (> (.-size kept) kept-max) (.delete kept (.-value (.next (.keys kept))))))
    (or a {:problem :searching})))
