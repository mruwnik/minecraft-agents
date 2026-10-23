(ns jobs.lib.dig-route
  "The fewest cells to dig for a walkable way from one feet cell to another through stone: a pure search over what the
  caller says each cell is (block-at cell -> name, or nil when never seen)."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.reach :as reach]))

(a/defargs settings
  {::max-digs {:default 16 :doc "The most cells a route digs." :spec (a/int-in 0 nil)}
   ::margin {:default 3 :doc "How far (blocks) round the two ends a route may stray." :spec (a/int-in 0 nil)}
   ::max-nodes {:default 500 :doc "The most cells a route search expands before it gives up (:budget)." :spec (a/int-in 1 nil)}
   ::max-fall {:default 3 :doc "The longest fall (blocks) a route's step may drop." :spec (a/int-in 0 nil)}})

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

(defn neighbours [[x y z] max-fall]
  (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]] dy (cons 1 (map - (range max-fall -1 -1)))] [(+ x dx) (+ y dy) (+ z dz)]))

(defn step-cells
  "The cells that must be open to walk from feet cell m to the adjacent feet cell n (a fall included)."
  [[_ my _ :as m] [nx ny nz :as n]]
  (cond
    (> ny my) [[(m 0) (+ my 2) (m 2)] n [nx (inc ny) nz]]
    (< ny my) (mapv (fn [y] [nx y nz]) (range (inc my) (dec ny) -1))
    :else [n [nx (inc ny) nz]]))

(defn- before? [^js a ^js b]
  (or (< (aget a 0) (aget b 0)) (and (== (aget a 0) (aget b 0)) (< (aget a 1) (aget b 1)))))

(defn- swap-at! [^js h i j]
  (let [t (aget h i)] (aset h i (aget h j)) (aset h j t)))

(defn- heap-push!
  "Push entry #js [price tick cell] onto binary heap h, cheapest (then earliest) on top."
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
          (when (not= m i)
            (swap-at! h i m)
            (recur m)))))
    top))

(defn search
  "route's search begun: a fn of n that runs it on for at most n more cells expanded, [answer expanded]: answer route's
  once the search has ended (the same each run after), nil while it goes on; expanded the cells this run expanded."
  [block-at from goal {:keys [dig-s step-s] :or {dig-s (constantly 1) step-s 0} :as opts}]
  (let [[fx _ fz] from
        ;; one number a cell, for js Maps and Sets (a vector key is compared by identity there)
        key (fn [[x y z]] (+ (* (+ (- x fx) 1048576) 2147483648) (* (+ (- z fz) 1048576) 1024) y 512))
        read (js/Map.)
        block-at (fn [c] (let [i (key c)] (if (.has read i) (.get read i) (let [n (block-at c)] (.set read i n) n))))
        priced (js/Map.)
        dig-s (fn [n c] (let [i (key c)] (if (.has priced i) (.get priced i) (let [s (dig-s n c)] (.set priced i s) s))))
        by-name (fn [f] (let [m (js/Map.)] (fn [n] (if (.has m n) (.get m n) (let [v (f n)] (.set m n v) v)))))
        ;; per name, not per cell: open? and floor-ok? each run regexes
        cell-cost (by-name cell-cost)
        floor-ok? (by-name floor-ok?)
        max-digs (get opts :max-digs (settings/get settings ::max-digs))
        margin (get opts :margin (settings/get settings ::margin))
        max-fall (get opts :max-fall (settings/get settings ::max-fall))
        lo (mapv #(- (min %1 %2) margin) from goal)
        hi (mapv #(+ (max %1 %2) margin) from goal)
        heuristic (fn [[x _ z]] (* step-s (+ (js/Math.abs (- x (goal 0))) (js/Math.abs (- z (goal 2))))))
        inside? (fn [[x y z]] (and (<= (lo 0) x (hi 0)) (<= (lo 1) y (hi 1)) (<= (lo 2) z (hi 2))))
        step (fn [m n]
               (when (floor-ok? (block-at [(n 0) (dec (n 1)) (n 2)]))
                 (reduce (fn [[s k] cell]
                           (let [name (block-at cell)
                                 d (when-not (and (pos? (or (cell-cost name) 1)) (falling (block-at (update cell 1 inc)))) (cell-cost name))
                                 price (when d (if (pos? d) (dig-s name cell) 0))]
                             (if price [(+ s price) (+ k d)] (reduced nil))))
                         [step-s 0] (step-cells m n))))
        dist (js/Map.) digs (js/Map.) prev (js/Map.) done (js/Set.) queue #js []
        tick (volatile! 1)
        answer (volatile! nil)]
    (.set dist (key from) 0) (.set digs (key from) 0)
    (heap-push! queue #js [(heuristic from) 0 from])
    (fn [n]
      (let [start (.-size done)
            limit (+ start n)
            a (or @answer
                  (loop []
                    (let [top (heap-pop! queue)
                          cur (some-> top (aget 2))
                          ck (some-> cur key)]
                      (cond
                        (nil? top) {:problem :no-route}
                        (.has done ck) (recur)
                        (= cur goal)
                        (let [path (reverse (take-while some? (iterate #(.get prev (key %)) goal)))]
                          {:dig (vec (distinct (for [[m n] (map vector path (rest path))
                                                     c (sort-by #(- (% 1)) (step-cells m n))
                                                     :when (pos? (cell-cost (block-at c)))]
                                                 c)))})
                        ;; the popped cell goes back: the next run expands it first
                        (>= (.-size done) limit) (do (heap-push! queue top) nil)
                        :else
                        (let [d (.get dist ck) k0 (.get digs ck)]
                          (.add done ck)
                          (doseq [n (neighbours cur max-fall)]
                            (let [nk (key n)
                                  [s k] (when (and (inside? n) (not (.has done nk))) (step cur n))
                                  nd (when (and s (<= (+ k0 k) max-digs)) (+ d s))]
                              (when (and nd (< nd (if (.has dist nk) (.get dist nk) js/Infinity)))
                                (.set dist nk nd) (.set digs nk (+ k0 k)) (.set prev nk cur)
                                (heap-push! queue #js [(+ nd (heuristic n)) @tick n])
                                (vswap! tick inc))))
                          (recur))))))]
        (vreset! answer a)
        [a (- (.-size done) start)]))))

(defn route
  "The cells to dig (head cell first within a step, in walking order) of the cheapest route from feet cell from to feet
  cell goal, at most :max-digs of them, within :margin cells of both ends, falls of at most :max-fall; each step's floor must be a seen solid block.
  A* with the walked steps' price as the lower bound. Priced :step-s a step and (:dig-s name cell) a cell dug (default 0 and 1: the fewest digs); a block :dig-s prices nil is
  not dug; nor is a cell under sand or gravel (it would fall on the body walking in). {:dig [cell ..]}, {:problem :no-route},
  or {:problem :budget} when :max-nodes cells were expanded first (search goes on past it)."
  [block-at from goal opts]
  (or (first ((search block-at from goal opts) (get opts :max-nodes (settings/get settings ::max-nodes)))) {:problem :budget}))

(defonce ^{:doc "{key search-fn} of the searches route-resumed left unfinished, the oldest dropped past kept-max."} kept (js/Map.))

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
