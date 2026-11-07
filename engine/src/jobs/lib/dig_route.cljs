(ns jobs.lib.dig-route
  "The cheapest walkable way, cells dug included, from one feet cell to another through stone: a pure search over what the
  caller says each cell is (block-at cell -> name, or nil when never seen)."
  (:require ["v8" :as v8]
            [engine.args :as a]
            [engine.path.blocks :as blocks]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.armour :as armour]
            [jobs.lib.cost.ways :as cw]
            [jobs.lib.lava-risk :as lava-risk]
            [jobs.lib.per-body :as per-body]
            [jobs.lib.reach :as reach]
            [jobs.lib.seen-sections :as seen-sections]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.lib.walk.world :as wworld]))

(a/defargs settings
  {::max-nodes {:default 500 :doc "The most cells a route search expands before it gives up (:budget)." :spec (a/int-in 1 nil)}
   ::heap-full {:default 0.75 :spec (a/num-in 0 1)
                :doc "The share of the heap's limit used past which a kept search grows no more: past it V8 collects ever more often and the body runs out of memory (a 1.4 GB heap did, 2026-10-30)."}})

(def falling #{"sand" "red_sand" "gravel"})

(defn open?
  "Whether a walker passes the block as it is: air, a plant, a torch, a gate (opened). A fence or wall stops it, though an arrow flies over."
  [n]
  (boolean (and n (or (bs/air n) (and (re-find reach/arrow-passes n) (not (re-find #"_(fence|wall)$" n)))))))

(def open-ids
  "1 by state id for a block open? passes."
  (delay (let [^js t (blocks/default-state-table)
               a (js/Uint8Array. (.-length (.-kind t)))]
           (.forEach (.-nameIds t) (fn [^js r n] (when (open? n) (.fill a 1 (aget r 0) (inc (aget r 1))))))
           a)))

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

(defn- tie
  "A bound rounded to 1e-10 s: ways of one price summed in another order differ in the last bits, and a bound a bit lower
  would be expanded first, so every way tied on the bound was (a plateau of them)."
  [s]
  (/ (js/Math.round (* s 1e10)) 1e10))

(def back-pace "Steps of the search back from the goal (its lower bound) to each cell the search proper expands: one is cheaper." 4)

(defn flood-dig
  "A dig at height y of a block name, which lets in water to level l (nil: none), on a way under flood (body-prices
  :flood) whose water stands at level (nil: none let in yet), the head under it for under seconds: [seconds over its
  :dig-s, level after, under after], nil when it changes nothing. A dig at or under the level is made inside the flood;
  the first water let in pays its opening, once; a higher pool's raises the level."
  [flood level under y name l]
  (let [level' (if (and l (or (nil? level) (< level l))) l level)]
    (cond
      (and level (<= y level)) (let [[w u] ((:dig flood) under name)] [w level' u])
      level [0 level' under]
      l (let [[w u] @(:open flood)] [w l u]))))

(defn flood-walk
  "A step to a feet cell at height y of a way whose flood stands at level, the head under it for under seconds: [seconds
  over a walked step, under after]: swum with the head at or under the level, else walked, the body breathing."
  [flood level under y]
  (if (<= (inc y) level) ((:step flood) under) [0 0]))

(defn flood-s
  "Seconds over their :dig-s and walked steps a way's moves cost under flood (flood-dig, flood-walk), moves [[:dig cell
  name] | [:step feet-cell] ..] in order."
  [flood moves]
  (first (reduce (fn [[s level under :as acc] [kind [_ y _ :as cell] name]]
                   (if (= :dig kind)
                     (if-let [[w level under] (flood-dig flood level under y name ((:level flood) cell))] [(+ s w) level under] acc)
                     (if level (let [[w under] (flood-walk flood level under y)] [(+ s w) level under]) acc)))
                 [0 nil 0] moves)))

(defn cells-s
  "Seconds each of a dug route's cells costs, in order: its :dig-s (name-at cell; 0 when never dug) and, under prices' :flood,
  the water its dig lets in and the dig inside it (flood-dig); the one place a dig leg is booked or compared at the price the
  search paid."
  [prices name-at cells]
  (let [{:keys [dig-s flood]} prices]
    (second (reduce (fn [[[level under] out] [_ y _ :as cell]]
                      (let [name (name-at cell)
                            [w level' under'] (or (when flood (flood-dig flood level under y name ((:level flood) cell))) [0 level under])]
                        [[level' under'] (conj out (+ (or (dig-s name cell) 0) w))]))
                    [[nil 0] []] cells))))

(defn search
  "route's search begun: a fn of n (and a :max-s that lowers the search's, as the caller's bound falls) that runs it on for at most n more cells expanded, [answer expanded stale? read]: answer route's
  once the search has ended (the same each run after), nil while it goes on; expanded and read the cells this run expanded and first read; stale? (fn []) whether a cell read reads otherwise now.
  Plain numbers throughout (a cell is a key, its reads are cached by key): a climb expands about 10k cells."
  [block-at from goal {:keys [dig-s step-s stair-s place-s pillar-s fall-s max-places unseen? unseen-step-s ways max-dig-s max-s margin near sees back-h? full? prior-h? seen-steps seen-cells open-ys unseen-dig-s unseen-digs guess floor-absent-s flood lava-s searched-s read-s only] r :range :or {dig-s (constantly 1) step-s 0 ways #{} unseen-dig-s 0 guess (constantly access/hidden-guess) searched-s 0} :as opts}]
  (let [[fx _ fz] from
        prior? (boolean (and prior-h? unseen? unseen-step-s seen-steps seen-cells))
        ;; under prior-h the search reads the memory its bound was made from: a cell seen since (outside every box
        ;; seen-cells measured) is read as never seen, else the bound could price it dearer than the search does
        unboxed? (fn [x y z] (and prior? (pos? (seen-cells x y z))))
        [gx gy gz] goal
        r (when (and r (pos? r)) r)
        [ax ay az] (or near goal)
        fy (from 1)
        ;; one number a cell, for js Maps and Sets (a vector key is compared by identity there): within 512 of the start a
        ;; small integer (V8 hashes it without a boxed double), else 2^30 on from a wider packing
        key (fn [x y z] (let [dx (- x fx) dy (- y fy) dz (- z fz)]
                          (if (and (< -513 dx 512) (< -513 dy 512) (< -513 dz 512))
                            (bit-or (bit-shift-left (+ dx 512) 20) (bit-shift-left (+ dz 512) 10) (+ dy 512))
                            (+ 1073741824 (* (+ (* (+ dx 131072) 262144) (+ dz 131072)) 65536) y 32768))))
        ;; a way that let water in is a label of its own, a negative id (label-cell: its cell's key): a dearer dry way to a cell
        ;; is not lost to a flooded one, nor a flooded one to another unless that is no worse (label!); nor, once a floor is
        ;; contested or with path state (path-state?), a dry way to another dry one
        label-cell (js/Map.)
        cell-of (fn [k] (let [k (if (neg? k) (.get label-cell k) k)]
                          (if (< k 1073741824)
                            [(+ (- (bit-shift-right k 20) 512) fx) (+ (- (bit-and k 1023) 512) fy) (+ (- (bit-and (bit-shift-right k 10) 1023) 512) fz)]
                            (let [k (- k 1073741824)
                                  y (- (mod k 65536) 32768)
                                  r (/ (- k y 32768) 65536)]
                              [(+ (- (js/Math.floor (/ r 262144)) 131072) fx) y (+ (- (mod r 262144) 131072) fz)]))))
        ;; what a name is to the search, once a name (open? and floor-ok? each run regexes), and so once a cell
        kinds (js/Map.)
        kind-of (fn [n] (or (.get kinds n)
                            (let [k #js {:name n :cost (cell-cost n) :open (open? n) :floor (floor-ok? n)
                                         :fluid (boolean (bs/fluids n)) :falling (boolean (falling n))}]
                              (.set kinds n k) k)))
        ;; with :open-ys, a step up from feet height h is cut-free only when cells at h+1 and h+2 may read open (its feet and
        ;; both heads); every other rise is a stair's step or a pillar block, rise-s over a step. The body's own cells are open
        rise-s (if (and open-ys stair-s)
                 (max 0 (- (min (max step-s stair-s) (if pillar-s (if (fn? pillar-s) (pillar-s 1 0 0 0) pillar-s) ##Inf)) step-s))
                 0)
        ^js open-set (when (pos? rise-s) (doto (js/Set. open-ys) (.add fy) (.add (inc fy))))
        ^js free (when open-set
                   (let [a #js []]
                     (.forEach open-set (fn [y] (when (.has open-set (inc y)) (.push a (dec y)))))
                     (.sort (js/Int32Array.from a))))
        ;; the rises from feet height lo to hi that must cut (or pillar)
        cut-rises (if free
                    (let [below (fn [v] (loop [l 0 h (.-length free)]
                                          (if (< l h) (let [m (bit-shift-right (+ l h) 1)] (if (< (aget free m) v) (recur (inc m) h) (recur l m))) l)))]
                      (fn [lo hi] (if (< lo hi) (- hi lo (- (below hi) (below lo))) 0)))
                    (constantly 0))
        ;; the rise bound reads open-ys as the search begins: a cell first read open at another height (seen meanwhile) is
        ;; read as never seen, as one outside the boxes is under prior-h
        fresh-y? (fn [y n] (and open-set (not (.has open-set y)) (.-open ^js (kind-of n))))
        read-name (fn [x y z] (if (unboxed? x y z) (guess [x y z]) (let [n (block-at [x y z])] (if (fresh-y? y n) (guess [x y z]) n))))
        fresh (js/Set.)
        read (js/Map.)
        at (fn [x y z] (let [i (key x y z)]
                         (or (.get read i)
                             (let [n (if (unboxed? x y z)
                                       (guess [x y z])
                                       (let [n (block-at [x y z])] (if (fresh-y? y n) (do (.add fresh i) (guess [x y z])) n)))
                                   k (kind-of n)]
                               (.set read i k) k))))
        priced (js/Map.)
        ;; the flood's level of a cell dug, asked only for a plug (:plugs) or when the search reads a fluid beside or over it
        ;; (a sensed read is dear)
        levels (js/Map.)
        plug-keys (let [s (js/Set.)] (doseq [[x y z] (:plugs flood)] (.add s (key x y z))) s)
        fluid? (fn [x y z] (.-fluid ^js (at x y z)))
        level-at (fn [x y z] (let [i (key x y z)]
                               (if (.has levels i)
                                 (.get levels i)
                                 (let [l (when (or (.has plug-keys i) (fluid? x (inc y) z) (fluid? (inc x) y z) (fluid? (dec x) y z) (fluid? x y (inc z)) (fluid? x y (dec z)))
                                           ((:level flood) [x y z]))]
                                   (.set levels i l) l))))
        dig-at (fn [n x y z] (let [i (key x y z) s (.get priced i)] (if (undefined? s) (let [s (dig-s n [x y z])] (.set priced i s) s) s)))
        fresh? (fn [x y z] (and open-set (not (.has open-set y)) (do (at x y z) (.has fresh (key x y z)))))
        unseen-cell? (let [m (js/Map.)] (fn [x y z] (let [i (key x y z) u (.get m i)] (if (some? u) u (let [u (or (unboxed? x y z) (fresh? x y z) (boolean (unseen? [x y z])))] (.set m i u) u)))))
        ;; a step whose feet, head or floor is unseen, as cost/unseen-route-s prices one
        unseen-at (fn [x y z] (if (and unseen? (or (unseen-cell? x y z) (unseen-cell? x (inc y) z) (unseen-cell? x (dec y) z))) unseen-step-s 0))
        way-keys (let [s (js/Set.)] (doseq [[x y z] ways] (.add s (key x y z))) s)
        only-keys (when only (let [s (js/Set.)] (doseq [[x y z] only] (.add s (key x y z))) s))
        ;; without a body's own, the drops a body with no hp to spend takes
        max-fall (or (:max-fall opts) (:max-drop (armour/fall-profile {})))
        fall-prices (let [a #js []] (dotimes [dy (inc max-fall)] (.push a (if (and fall-s (pos? dy)) (fall-s dy) 0))) a)
        ;; a fall that costs hp is taken only into a hole already open: dug, the stair down beside it costs no hp
        free-fall (count (take-while zero? fall-prices))
        [lx ly lz] (when margin (mapv #(- (min %1 %2) margin) from goal))
        [hx hy hz] (when margin (mapv #(+ (max %1 %2) margin) from goal))
        inside? (fn [x y z] (or (nil? margin) (and (<= lx x hx) (<= ly y hy) (<= lz z hz))))
        ;; a cell the search (or the back search) would have entered but for the box: its :no-route is then the box's
        boxed (volatile! false)
        back-boxed (volatile! false)
        to-goal (fn [x y z] (+ (* step-s (max (+ (js/Math.abs (- x gx)) (js/Math.abs (- z gz))) (- gy y))) (* rise-s (cut-rises y gy))))
        ;; a cell within range of near is at most 2 range across and below it, and no more than range below it
        step-h (if r
                 (fn [x y z] (min (to-goal x y z)
                                  (+ (* step-s (max 0 (- (max (+ (js/Math.abs (- x ax)) (js/Math.abs (- z az))) (- ay y)) (* 2 r))))
                                     (* rise-s (cut-rises y (- ay (js/Math.floor r)))))))
                 to-goal)
        ;; with back-h?, the back search's bound too (back-lb): a dear wall round the goal is priced before the rock round it is searched
        back-lb (volatile! (constantly 0))
        ;; with prior-h?, the unseen a way must cross priced on: each step into a cell whose step is unseen pays unseen-step-s
        ;; (seen-steps: the fewest steps from a cell to one whose step is seen) and each cell it opens whose block is unseen
        ;; at least unseen-dig-s (seen-cells: the fewest cells, max(across, up or down), to one whose block is seen). The steps
        ;; before the first seen one and those after the last are apart, so their two counts add; a way that never meets the
        ;; seen takes them all. A step opens a cell per block it rises or falls and two more when it moves across (a pillar
        ;; none), so a way down d opens at least d and two a fall. Each within its share of a step's price of a neighbour's:
        ;; a consistent lower bound in which seen ground or a seen cave past the unseen is priced as seen
        [px py pz] (if r [ax ay az] goal)
        pm (if r (* 2 r) 0)
        mf (max 1 max-fall)
        ;; a fall that opens cells is dug, so one costing hp never does (free-fall): the falls the unseen counts are shorter
        dug-mf (max 1 (min mf (dec free-fall)))
        across-to (fn [x z] (+ (js/Math.abs (- x px)) (js/Math.abs (- z pz))))
        steps-to (fn [x y z] (max 0 (- (js/Math.max (across-to x z) (- py y) (js/Math.ceil (/ (- y py) mf))) pm)))
        digs-of (fn [a dn up] (+ dn up (* 2 (js/Math.max a (js/Math.ceil (/ dn dug-mf)) (if pillar-s 0 up)))))
        digs-to (fn [x y z] (digs-of (max 0 (- (across-to x z) pm)) (max 0 (- y py pm)) (max 0 (- py y pm))))
        least-step (if pillar-s (min step-s pillar-s) step-s)
        end-steps (when prior? (max 0 (- (seen-steps px py pz) pm)))
        ;; the fewest unseen cells opened after the last seen one: the step that opened it ends at most a fall and a head
        ;; under it (e - dug-mf - 1 over the end), across e - 1 or e under it; with that step's own unseen cells under the seen
        ;; one, last-digs (which the first steps' count cannot add to: they may be that one step's)
        [end-digs last-digs] (when prior?
                               (let [e (seen-cells px py pz) t (max 0 (- e pm)) under (if pillar-s t (* 3 t))]
                                 [(min (digs-of (max 0 (- e 1 pm)) 0 0) (digs-of 0 (max 0 (- e dug-mf 1 pm)) 0) under)
                                  (min (max 0 (dec (* 2 t))) (+ t (* 2 (js/Math.ceil (/ (max 0 (- t dug-mf 1)) dug-mf)))) under)]))
        ;; the first steps open a cell seen-cells less one away, a pillar's first two over the feet
        lead (if pillar-s 2 1)
        ;; the first steps of a way from a cell seen-cells s off open cells (each its feet, a pillar's its head) at least s - j
        ;; - lead + 1 off at the j-th, at most y + j + lead - 1 high: with :unseen-digs each such dig at least its tier's
        ;; seconds (low where that height is low-y or under), a non-decreasing row by distance; (lead-digs k m y) the m first
        ;; of the k of a cell at height y. A step from the cell opens the first, so the sum falls by no more than its dig
        lead-digs (when-let [{:keys [low-y low any]} unseen-digs]
                    (let [prefix (fn [ws] (let [a (js/Float64Array. (inc (count ws)))] (dotimes [i (count ws)] (aset a (inc i) (+ (aget a i) (nth ws i)))) a))
                          upto (fn [^js a l] (let [n (dec (alength a))] (cond (<= l 0) 0 (<= l n) (aget a l) :else (+ (aget a n) (* (- l n) (- (aget a n) (aget a (dec n))))))))
                          lows (prefix low) anys (prefix any)
                          span (fn [a lo hi] (if (< hi lo) 0 (- (upto a hi) (upto a (dec lo)))))]
                      (fn [k m y] (let [a (max 0 (min m (- low-y y lead -1)))]
                                    (+ (span lows (inc (- k a)) k) (span anys (inc (- k m)) (- k a)))))))
        ;; a stair's step or pillar block over a step on top: the unseen's tolls and digs are paid besides it
        prior-h (fn [x y z] (let [n (steps-to x y z)]
                              (+ (* least-step n)
                                 (* rise-s (cut-rises y (if r (- ay (js/Math.floor r)) gy)))
                                 (* unseen-step-s (min n (+ (max 0 (dec (seen-steps x y z))) end-steps)))
                                 ;; from a seen cell every way meets the seen: the count of a way that never does is no bound there
                                 (let [sc (seen-cells x y z) k (max 0 (- sc lead))]
                                   (if lead-digs
                                     (let [meets (max (+ (lead-digs k k y) (* unseen-dig-s end-digs)) (* unseen-dig-s last-digs))]
                                       (if (zero? sc)
                                         meets
                                         (let [d (digs-to x y z) m (min k d)] (min (+ (lead-digs k m y) (* unseen-dig-s (- d m))) meets))))
                                     (* unseen-dig-s (let [b (max (+ k end-digs) last-digs)] (if (zero? sc) b (min (digs-to x y z) b)))))))))
        heuristic (let [h (if back-h? (fn [x y z] (max (step-h x y z) (@back-lb x y z))) step-h)]
                    (if prior? (fn [x y z] (max (h x y z) (prior-h x y z))) h))
        end? (fn [ck] (let [[x y z] (cell-of ck)]
                        (and (<= (+ (* (- x ax) (- x ax)) (* (- y ay) (- y ay)) (* (- z az) (- z az))) (* r r))
                             (or (nil? sees) (sees x y z)))))
        solid-at? (fn [x y z] (let [^js k (at x y z)] (and (some? (.-name k)) (not (.-open k)))))
        ;; a block is placed only against a face (a solid neighbour), never on a step of the body's own stair (it would shut
        ;; it): :seen, :unseen when every face is a guess (it may be air), nil
        face (fn [x y z] (when (solid-at? x y z) (if (and unseen? (unseen-cell? x y z)) :unseen :seen)))
        placeable (fn [x y z] (when-not (.has way-keys (key x (dec y) z))
                                (let [a (face x (dec y) z) b (face (inc x) y z) c (face (dec x) y z) d (face x y (inc z)) e (face x y (dec z))]
                                  (cond (or (keyword-identical? :seen a) (keyword-identical? :seen b) (keyword-identical? :seen c)
                                            (keyword-identical? :seen d) (keyword-identical? :seen e)) :seen
                                        (or a b c d e) :unseen))))
        ;; what a move costs before the cells it opens, falls and unseen steps: the forward search adds those, the back search
        ;; (a lower bound) leaves out falls and the refusals of open-cell! (a falling block over a cell, the body's own ways)
        base-s (fn [move] (case move :walk step-s :laid (+ step-s place-s) :pillar (if (fn? pillar-s) (pillar-s 1 0 0 0) pillar-s)))
        ;; a pillar block's seconds as the n-th of a run of them from cell x y z
        pillar-at (fn [^js node x y z] (if (fn? pillar-s) (pillar-s (inc (if (keyword-identical? :pillar (aget node 4)) (aget node 7) 0)) x y z) pillar-s))
        ;; seconds to open cell x y z (0 when open), nil when no step opens it
        open-s (fn [x y z] (let [^js k (at x y z) d (.-cost k)] (cond (nil? d) nil (zero? d) 0 :else (dig-at (.-name k) x y z))))
        ;; lava above or beside cell x y z: the dig seals each such face as it opens the cell (priced by :lava-s; with none, the cell is not dug)
        lava? (fn [x y z] (if (= "lava" (.-name ^js (at x y z))) 1 0))
        lava-faces (fn [x y z] (+ (lava? x (inc y) z) (lava? (inc x) y z) (lava? (dec x) y z) (lava? x y (inc z)) (lava? x y (dec z))))
        ;; what opening cell x y z is, once a cell: false when it may not be, true when open, else #js [seconds (lava's seal
        ;; in) flood-level name key]
        opened (js/Map.)
        open-of (fn [x y z]
                  (let [i (key x y z) v (.get opened i)]
                    (if (undefined? v)
                      (let [s (open-s x y z)
                            v (cond
                                (nil? s) false
                                (zero? (.-cost ^js (at x y z))) true
                                (.-falling ^js (at x (inc y) z)) false
                                (.has way-keys i) false
                                :else (let [lf (lava-faces x y z)]
                                        (if (and (nil? lava-s) (pos? lf))
                                          false
                                          #js [(+ s (if (pos? lf) (lava-s lf) 0)) (when flood (level-at x y z)) (.-name ^js (at x y z)) i])))]
                        (.set opened i v)
                        v)
                      v)))
        ;; the cost of opening cell x y z for a step, accumulated in acc #js [seconds digs dig-seconds dug-keys flood-level
        ;; under-water-s] (flood-dig); false when it may not be
        open-cell! (fn [^js acc x y z]
                     (let [v (open-of x y z)]
                       (if (boolean? v)
                         v
                         (let [l (aget v 1)
                               s (if-let [[w level under] (when (or l (some? (aget acc 4))) (flood-dig flood (aget acc 4) (aget acc 5) y (aget v 2) l))]
                                   (do (aset acc 4 level) (aset acc 5 under) (+ (aget v 0) w))
                                   (aget v 0))]
                           (aset acc 0 (+ (aget acc 0) s)) (aset acc 1 (+ (aget acc 1) 1)) (aset acc 2 (+ (aget acc 2) s)) (.push ^js (aget acc 3) (aget v 3)) true))))
        ;; key (a label: its id) -> #js [seconds digs dig-seconds prev-key move blocks-placed dug pillar-run flood-level under-water-s sig] of the
        ;; cheapest way found to the cell (the flood its digs let in: flood-dig, flood-walk), dug its
        ;; steps' dug cells as a chain #js [keys parent lx hx ly hy lz hz] (nil: none), each link the box of its own and every
        ;; earlier link's cells: links share their tails, where a set a node grew to kilobytes of heap
        dug-link (fn [^js ks ^js parent]
                   (let [l (if parent
                             #js [ks parent (aget parent 2) (aget parent 3) (aget parent 4) (aget parent 5) (aget parent 6) (aget parent 7)]
                             #js [ks nil ##Inf ##-Inf ##Inf ##-Inf ##Inf ##-Inf])]
                     (dotimes [i (alength ks)]
                       (let [[x y z] (cell-of (aget ks i))]
                         (aset l 2 (min x (aget l 2))) (aset l 3 (max x (aget l 3)))
                         (aset l 4 (min y (aget l 4))) (aset l 5 (max y (aget l 5)))
                         (aset l 6 (min z (aget l 6))) (aset l 7 (max z (aget l 7)))))
                     l))
        best (js/Map.) done (js/Set.) queue #js [] expanded (volatile! 0)
        tick (volatile! 1)
        answer (volatile! nil)
        goal-key (key gx gy gz)
        end-key? (fn [k] (or (== (if (neg? k) (.get label-cell k) k) goal-key) (and r (end? k))))
        ;; #js [seconds key] of the cheapest end cell any step reached so far: a route in hand before the search proves it cheapest
        incumbent #js [##Inf nil]
        ;; the wet labels of a cell's key: none another is no worse than (label!)
        wet-at (js/Map.)
        wet-n (volatile! 0)
        ;; labels another came to dominate: never expanded
        dropped (js/Set.)
        ;; the floors refused where the way to a cell dug them (dug-behind?): only those an answer was found to stand on
        ;; (checked). Any other dug floor is stood on as the block it was, so a step costs the same whatever way led to it;
        ;; a way is labelled by the contested cells it dug (sig: their keys, sorted, joined; "" for none)
        contested (js/Set.)
        sig-of (fn [sig ^js ks]
                 (if (and (pos? (.-size contested)) (.some ks #(.has contested %)))
                   (let [o (js/Set. (if (= "" sig) #js [] (.map (.split sig ",") js/Number)))]
                     (.forEach ks #(when (.has contested %) (.add o %)))
                     (.join (.sort (js/Array.from o) -) ","))
                   sig))
        sub-sig? (fn [a b] (or (= "" a) (let [o (js/Set. (.split b ","))] (.every (.split a ",") #(.has o %)))))
        ;; the path state a way's next steps depend on beyond its cell and dug floors: blocks placed (max-places), dig seconds
        ;; (max-dig-s) and the pillar run (a fn pillar-s: a later block of a run costs no less). A way with no more of each
        ;; may take every step another may, at no higher price
        path-state? (or (some? max-places) (some? max-dig-s) (fn? pillar-s))
        no-more? (fn [placed ds run placed' ds' run']
                   (and (or (nil? max-places) (<= placed placed')) (or (nil? max-dig-s) (<= ds ds')) (or (not (fn? pillar-s)) (<= run run'))))
        dry-at (js/Map.)
        ;; a new label of cell key i in at (wet-at or dry-at) for a way of s seconds, its water at level (nil: dry), the head
        ;; under it for under seconds, the contested cells sig dug (fewer: no worse) and its path state; nil when one the cell
        ;; holds is no worse on all, or, dry, one expanded of the same sig is no worse on all but seconds (a cell expanded is
        ;; not reopened). The labels the new one is no worse than are dropped
        label! (fn [^js at i s level under sig placed ds run]
                 (let [^js ids (or (.get at i) #js [])
                       wet? (some? level)
                       over? (fn [id] (let [^js a (.get best id)]
                                        (and (or (< (aget a 0) s) (and (== (aget a 0) s) (or wet? (= sig (aget a 10)))) (and (not wet?) (.has done id) (= sig (aget a 10))))
                                             (or (not wet?) (and (<= (aget a 8) level) (<= (aget a 9) under)))
                                             (sub-sig? (aget a 10) sig) (no-more? (aget a 5) (aget a 2) (aget a 7) placed ds run))))
                       under? (fn [id] (let [^js a (.get best id)]
                                         (and (or (< s (aget a 0)) (and (== s (aget a 0)) (or wet? (= sig (aget a 10)))))
                                              (or (not wet?) (and (<= level (aget a 8)) (<= under (aget a 9))))
                                              (sub-sig? sig (aget a 10)) (no-more? placed ds run (aget a 5) (aget a 2) (aget a 7)))))]
                   (when-not (.some ids over?)
                     (let [id (vswap! wet-n dec)
                           kept (.filter ids #(or (not (under? %)) (do (.add dropped %) false)))]
                       (.push kept id) (.set at i kept) (.set label-cell id i)
                       id))))
        relax! (fn [ck ^js from-node ^js acc nx ny nz move stair? face-s]
                 (let [level (aget acc 4)
                       i (key nx ny nz)
                       wet-s (if (some? level) (let [[w u] (flood-walk flood level (aget acc 5) ny)] (aset acc 5 u) w) 0)
                       k (+ (aget from-node 1) (aget acc 1)) ds (+ (aget from-node 2) (aget acc 2))
                       placed (cond-> (aget from-node 5) (not (keyword-identical? :walk move)) inc)]
                   (when (and (or (nil? max-dig-s) (<= ds max-dig-s))
                              (or (nil? max-places) (<= placed max-places)))
                     (let [unseen-s (let [u (unseen-at nx ny nz)] (if (zero? u) face-s u))
                           ;; a walk onto a floor not seen pays the chance it is none as seconds
                           absent-s (if (and floor-absent-s unseen? (keyword-identical? :walk move) (unseen-cell? nx (dec ny) nz)) (floor-absent-s [nx (dec ny) nz] (- ny (nth (cell-of ck) 1))) 0)
                           nd (+ (aget from-node 0) (cw/step-price (aget acc 0) step-s stair-s stair? unseen-s) absent-s wet-s)
                           sig (sig-of (aget from-node 10) (aget acc 3))
                           run (if (keyword-identical? :pillar move) (inc (if (keyword-identical? :pillar (aget from-node 4)) (aget from-node 7) 0)) 0)]
                       (when-let [nk (cond
                                       (some? level) (label! wet-at i nd level (aget acc 5) sig placed ds run)
                                       (or path-state? (pos? (.-size contested))) (label! dry-at i nd nil 0 sig placed ds run)
                                       :else (let [^js old (.get best i)] (when (and (not (.has done i)) (or (nil? old) (< nd (aget old 0)))) i)))]
                         (.set best nk #js [nd k ds ck move placed (let [dug (aget from-node 6) ks (aget acc 3)] (if (zero? (alength ks)) dug (dug-link ks dug))) run (aget acc 4) (aget acc 5) sig])
                         (when (and (< nd (aget incumbent 0)) (end-key? nk)) (aset incumbent 0 nd) (aset incumbent 1 nk))
                         ;; among equal bounds the farthest along first: under prior-h a straight way's cells all bound alike
                         (heap-push! queue #js [(tie (+ nd (heuristic nx ny nz))) (if prior? (- nd) @tick) nk])
                         (vswap! tick inc))))))
        ;; whether a step of the way to the cell of key k opened cell x y z: a floor the route itself digs away is none (a
        ;; stair that turns back over its own cut)
        ;; a link's box holds every cell before it: a cell outside it is dug by none of them
        dug-behind? (fn [k x y z]
                      (let [c (key x y z)]
                        (loop [^js l (aget (.get best k) 6)]
                          (cond
                            (or (nil? l) (< x (aget l 2)) (> x (aget l 3)) (< y (aget l 4)) (> y (aget l 5)) (< z (aget l 6)) (> z (aget l 7))) false
                            (.includes (aget l 0) c) true
                            :else (recur (aget l 1))))))
        refused? (fn [k x y z] (and (.has contested (key x y z)) (dug-behind? k x y z)))
        expand! (fn [ck]
                  (let [[mx my mz] (cell-of ck)
                        node (.get best ck)
                        ;; a step to n opening the cell over m's head when head?, and n's column from top down to bot
                        ;; a step's unseen cells are priced once (relax!'s unseen-at): the cell over m's head and a guessed face
                        ;; (unseen-face?) only when n's own cells are seen
                        try! (fn [nx ny nz move s0 head? top bot unseen-face?]
                               (let [acc #js [s0 0 0 #js [] (aget node 8) (aget node 9)]
                                     face-s (if (and unseen? (or unseen-face? (and head? (unseen-cell? mx (+ my 2) mz)))) unseen-step-s 0)]
                                 (when (and (or (nil? only-keys) (.has only-keys (key nx ny nz)))
                                            (or (not head?) (open-cell! acc mx (+ my 2) mz))
                                            (loop [y top] (or (< y bot) (and (open-cell! acc nx y nz) (recur (dec y)))))
                                            (or (inside? nx ny nz) (do (vreset! boxed true) false)))
                                   ;; a step up that cuts or lays its floor is a stair's step: no less than :stair-s
                                   (relax! ck node acc nx ny nz move
                                           (boolean (and stair-s (> ny my) (not= :pillar move) (or (= :laid move) (pos? (aget acc 1)))))
                                           face-s))))]
                    (when-let [pf (and pillar-s (placeable mx my mz))]
                      (try! mx (inc my) mz :pillar (pillar-at node mx my mz) true 0 1 (= :unseen pf)))
                    (loop [i 0]
                      (when (< i 8)
                        (let [nx (+ mx (aget dirs i)) nz (+ mz (aget dirs (inc i)))
                              ny (inc my) ^js f (at nx my nz)
                              dug? (and (.-floor f) (refused? ck nx my nz))]
                          ;; up: the head cell over m, n and its head
                          (cond
                            (and (.-floor f) (not dug?)) (try! nx ny nz :walk (base-s :walk) true (inc ny) ny false)
                            (and place-s (or dug? (and (.-open f) (not (.-fluid f)))))
                            (when-let [pf (placeable nx my nz)]
                              (try! nx ny nz :laid (base-s :laid) true (inc ny) ny (= :unseen pf))))
                          ;; down by up to max-fall, through the cells it falls by, and level: n and its head
                          (loop [dy 0 clear? (and (.-open ^js (at nx (inc my) nz)) (.-open ^js (at nx my nz)))]
                            (when (<= dy max-fall)
                              (let [ny (- my dy)
                                    below-open? (.-open ^js (at nx (dec ny) nz))]
                                (when (and (.-floor ^js (at nx (dec ny) nz)) (not (refused? ck nx (dec ny) nz)))
                                  (try! nx ny nz :walk (+ (base-s :walk) (aget fall-prices dy)) false (if (zero? dy) (inc ny) (inc my)) ny false))
                                (when (or (< (inc dy) free-fall) (and clear? below-open?))
                                  (recur (inc dy) (and clear? below-open?))))))
                          (recur (+ i 2)))))))
        start-key (key fx (from 1) fz)
        route-to (fn [ck]
                   (let [ks (vec (reverse (take-while some? (iterate #(aget (.get best %) 3) ck))))
                         path (mapv cell-of ks)
                         steps (mapv (fn [m n nk]
                                       (let [move (aget (.get best nk) 4)]
                                         (cond-> {:to n :move move
                                                  :dig (vec (filter #(pos? (.-cost ^js (apply at %))) (sort-by #(- (% 1)) (step-cells m n))))}
                                           (= :laid move) (assoc :place (update n 1 dec))
                                           (= :pillar move) (assoc :place m))))
                                     path (rest path) (rest ks))]
                     {:dig (vec (distinct (mapcat :dig steps))) :steps steps :seconds (aget (.get best ck) 0)}))
        ;; with read-s, the search's CPU is a cost of the route it finds: it goes on only while what is left to find may
        ;; still save more than the search has spent (a ski rental's rule: at worst twice the best), against the route in
        ;; hand (its cheapest end reached) or max-s; :searched-s, what earlier searches of the same question spent, counts too.
        ;; Counted by the cells read, not expanded: the back search, the dig prices and the neighbours read are most of the
        ;; work and come per cell read, while a cell expanded reads 3 to 9 of them as the scene goes
        spent-s (fn [] (+ searched-s (if read-s (* read-s (.-size read)) 0)))
        max-s (volatile! (or max-s ##Inf))
        priced (fn [r] (cond-> r read-s (assoc :searched-s (spent-s))))
        bound-s (fn [] (min @max-s (aget incumbent 0)))
        start-node #js [0 0 0 nil nil 0 nil 0 nil 0 ""]
        ;; the floors of the way to label ck its own steps dug
        stood-on-dug (fn [ck] (loop [k ck out #js []]
                                (let [^js n (.get best k) pk (aget n 3)]
                                  (if (nil? pk)
                                    out
                                    (let [[x y z] (cell-of k)]
                                      (when (and (keyword-identical? :walk (aget n 4)) (dug-behind? pk x (dec y) z)) (.push out (key x (dec y) z)))
                                      (recur pk out))))))
        ;; the route to label ck, or nil when it stands on a floor it dug: that floor is contested and the search begins
        ;; again (the back search, a bound whatever is contested, goes on). Refusing only some floors prices no way dearer
        ;; than refusing all, so an answer standing on no floor it dug is the cheapest of every way that stands on none
        checked (fn [ck]
                  (let [bad (stood-on-dug ck)]
                    (if (zero? (alength bad))
                      (priced (route-to ck))
                      (do (.forEach bad #(.add contested %))
                          (.clear best) (.clear done) (.clear wet-at) (.clear dry-at) (.clear dropped) (.clear label-cell)
                          (set! (.-length queue) 0) (vreset! tick 1) (aset incumbent 0 ##Inf) (aset incumbent 1 nil)
                          (.set best start-key start-node)
                          (heap-push! queue #js [(heuristic fx fy fz) 0 start-key])
                          nil))))
        ended (fn [] (if (<= (aget incumbent 0) @max-s) (checked (aget incumbent 1)) {:problem :dearer}))
        ;; a lower bound of every route's price, searched back from the goal toward the start: a body's two cells per node,
        ;; each step priced base-s of its cheapest move and the cells it opens (across: the two cells entered; up across: the cell
        ;; over the head too, a stair's step when it cuts; down: the cell entered; a pillar: the cell over the head), up and a
        ;; pillar the unseen toll of the cell entered too, falls and refusals left out, a step across or up only from a cell over a floor. Its frontier exhausted is no route; its cheapest entry past max-s, every route dearer.
        own-cell? (fn [x y z] (or (== (key x y z) start-key) (== (key x (dec y) z) start-key)))
        back-s (fn [x y z] (if (own-cell? x y z) 0 (open-s x y z)))
        ;; as the search proper reads a cell to open: dug however little its dig costs
        back-cut? (fn [x y z] (not (or (own-cell? x y z) (zero? (.-cost ^js (at x y z))))))
        stands? (fn [x y z] (and (some? (back-s x y z)) (some? (back-s x (inc y) z)) (or (inside? x y z) (do (vreset! back-boxed true) false))))
        ;; under prior-h also the unseen cells a way from the start opens, each at least unseen-dig-s: a step into a cell whose
        ;; seen-cells passes the start's by 2 or more opens one (its feet; a pillar its head)
        to-start (let [walk (fn [x y z] (+ (* step-s (max (+ (js/Math.abs (- x fx)) (js/Math.abs (- z fz))) (- y fy))) (* rise-s (cut-rises fy y))))
                       s0 (when prior? (inc (seen-cells fx fy fz)))]
                   (if prior?
                     (fn [x y z] (max (walk x y z) (+ (* step-s (+ (js/Math.abs (- x fx)) (js/Math.abs (- z fz)))) (* rise-s (cut-rises fy y)) (* unseen-dig-s (max 0 (- (seen-cells x y z) s0))))))
                     walk))
        back-best (js/Map.) back-done (js/Set.) back-queue #js []
        back-tick (volatile! 0)
        ;; the end cells of a range search, nearest the start first: one enters the back search only once its bound is the
        ;; cheapest there (a farther one cannot change what is popped before), so the block reads of the many stay unread
        seeds (volatile! ())
        back-over (volatile! false)
        back-relax! (fn [g x y z]
                      (let [k (key x y z) old (.get back-best k)]
                        (when (and (not (.has back-done k)) (or (nil? old) (< g old)))
                          (.set back-best k g)
                          (heap-push! back-queue #js [(+ g (to-start x y z)) (vswap! back-tick inc) k]))))
        back-into! (fn [g x y z s]
                     (when (and (some? s) (stands? x y z)) (back-relax! (+ g s) x y z)))
        ;; a step across or up leaves a node, which stands on a floor (or on a block laid, when blocks may be): a fall's
        ;; cells over air are entered only from above or from such a node beside the fall's top
        floored? (fn [x y z] (or place-s pillar-s (== (key x y z) start-key) (.-floor ^js (at x (dec y) z))))
        seed-due! (fn []
                    (loop []
                      (when-let [[x y z :as cell] (first @seeds)]
                        (when (or (zero? (.-length back-queue)) (<= (to-start x y z) (aget back-queue 0 0)))
                          (vswap! seeds rest)
                          (when (and (stands? x y z) (or (= cell goal) (end? (key x y z)))) (back-relax! 0 x y z))
                          (recur)))))
        back-step! (fn []
                     (seed-due!)
                     (let [top (heap-pop! back-queue)
                           k (some-> top (aget 2))]
                       (cond
                         (nil? top) (cond-> {:problem :no-route} @back-boxed (assoc :boxed true :searched-s (spent-s)))
                         (> (+ (aget top 0) (spent-s)) (bound-s)) (ended)
                         (.has back-done k) nil
                         (== k start-key) (do (vreset! back-over true) nil)
                         :else
                         (let [[x y z] (cell-of k)
                               g (.get back-best k)
                               feet (back-s x y z) head (back-s x (inc y) z)
                               toll (unseen-at x y z)]
                           (.add back-done k)
                           ;; level into the cell (or into the top of a column fallen down, a cell at a time below)
                           (loop [i 0]
                             (when (< i 8)
                               (let [mx (- x (aget dirs i)) mz (- z (aget dirs (inc i)))]
                                 (when (floored? mx y mz) (back-into! g mx y mz (+ (base-s :walk) feet head)))
                                 (recur (+ i 2)))))
                           ;; up into the cell: the cell over the head it leaves too, a stair's step when it cuts
                           (loop [i 0]
                             (when (< i 8)
                               (let [mx (- x (aget dirs i)) mz (- z (aget dirs (inc i)))]
                                 (when-let [over (when (floored? mx (dec y) mz) (back-s mx (inc y) mz))]
                                   (let [cuts (+ over feet head)
                                         stair? (and stair-s (or (pos? cuts) (back-cut? mx (inc y) mz) (back-cut? x y z) (back-cut? x (inc y) z)))]
                                     (back-into! g mx (dec y) mz (+ (if stair? (cw/stair-leg-cost (+ step-s cuts) step-s stair-s) (+ step-s cuts)) toll))))
                                 (recur (+ i 2)))))
                           ;; down the column a cell at a time, as a fall opens it and the straight dig digs it
                           (back-into! g x (inc y) z feet)
                           (when pillar-s (back-into! g x (dec y) z (+ (base-s :pillar) head toll)))
                           nil))))]
    ;; a cell the back search expanded is priced exactly, any other at least the back queue's least bound less its way to the start
    (vreset! back-lb (fn [x y z] (let [k (key x y z)]
                                   (if (.has back-done k)
                                     (.get back-best k)
                                     (if-let [^js t (aget back-queue 0)] (max 0 (- (aget t 0) (to-start x y z))) 0)))))
    (.set best start-key start-node)
    (heap-push! queue #js [(heuristic fx fy fz) 0 start-key])
    (vreset! seeds (sort-by (fn [[x y z]] (to-start x y z))
                            (cons goal (when r
                                         (let [w (js/Math.ceil r) ds (range (- w) (inc w))]
                                           (for [dx ds dy ds dz ds]
                                             [(+ ax dx) (+ ay dy) (+ az dz)]))))))
    (fn run
      ([n] (run n nil))
      ([n max-s']
       (when max-s' (vswap! max-s min max-s'))
       (let [start @expanded
             reads (.-size read)
             limit (+ start n)
             ;; whether a cell read in an earlier slice reads otherwise now
             stale? (fn [] (let [out (volatile! false)]
                             (.forEach read (fn [k i] (let [[x y z] (cell-of i)]
                                                        (when (not= (.-name k) (read-name x y z))
                                                          (vreset! out true)))))
                             @out))
             a (or @answer
                   ;; a physical limit: the heap holds no more
                   (when (and full? (full?)) (let [e (ended)] (if (:dig e) e {:problem :too-big})))
                   (loop []
                     (or
                      (loop [i 0] (when (and (< i back-pace) (not @back-over)) (or (back-step!) (recur (inc i)))))
                      (let [top (heap-pop! queue)
                           ck (some-> top (aget 2))]
                       (cond
                         (nil? top) (cond-> {:problem :no-route} @boxed (assoc :boxed true :searched-s (spent-s)))
                         ;; the heuristic is a lower bound: every route left costs more
                         (> (+ (aget top 0) (spent-s)) (bound-s)) (or (ended) (recur))
                         (or (.has done ck) (.has dropped ck)) (recur)
                         (end-key? ck) (or (checked ck) (recur))
                         ;; the popped cell goes back: the next run expands it first
                         (>= @expanded limit) (do (heap-push! queue top) nil)
                         :else (do (.add done ck) (vswap! expanded inc) (expand! ck) (recur)))))))]
         (vreset! answer a)
         [a (- @expanded start) stale? (- (.-size read) reads)])))))

(defn body-prices
  "route's prices for the body of primitives p, as go-to prices its walks: a step's walk-cost, a fall's hp (fall-damage at
  hp-cost, at most the falls it survives), a block's dig at the quickest carried tool or the bare hand (tools/clear-ms, or opts
  :clear-ms (fn [name] ms); nil: never dug (cost/dig-cost nil, or no tool breaks it); a nameless cell has none) with what the dig destroys (cost/destroyed-fn over opts :sensed, (fn [cell]) a sensed block, default the
  body's; a cell (opts :own-s cell) prices over 0, the body's own build, at that instead), as :flood (route's) the level
  of seen water that flows into a cell dug (rules/fluid-neighbours, beside or above; for a cell in opts :plugs also a
  neighbour not seen, as water), the water let in (cost/flood-dig-s,
  flood-under-s) and a dig or step on inside it (cost/wet-dig-s, wet-step-s), a stair's step at least its
  stair-cost, a step whose cell is (unseen? cell) at the unseen walk's price (wworld/unseen-share), :seen-steps (falls
  of at most the longest that costs no hp) and :seen-cells by the sections the body remembers (seen-sections/steps-fn), :open-ys
  the heights at which it remembers a cell open (seen-sections/open-heights)."
  ([p unseen?] (body-prices p unseen? {}))
  ([p unseen? {:keys [sensed own-s max-damage plugs clear-ms]}]
   (let [self (.self p)
         ;; the body as the prices are made: a search kept over later ticks prices its whole way at one health
         health (.-health self)
         equipment (cost/equipment-of (.-equipment self))
         max-fall (cost/drop-limit equipment (wworld/self-body p) (wworld/walk-settings {:args (when max-damage {:max-damage max-damage})}))
         fall-s (fn [blocks] (cost/hp-cost health (cost/fall-damage equipment blocks)))
         clear-ms (or clear-ms #(tools/clear-ms p %))
         dig-cost (memoize (fn [name] (when (cost/dig-cost name) (let [s (/ (clear-ms name) 1000)] (when (js/isFinite s) s)))))
         sensed (or sensed (fn [[x y z]] (u/sensed p {:x x :y y :z z})))
         destroyed (cost/destroyed-fn sensed)
         cut-s (fn [name cell] (let [own (if own-s (own-s cell) 0)] (if (pos? own) own (destroyed name cell false))))
         seen-name (fn [cell] (when-let [b (sensed cell)] (when-not (true? (.-unknown b)) (.-name b))))
         wet (delay (merge (cost/air-profile (.-equipment self) (some-> (.-effects self) array-seq)) (cost/swim-profile (.-equipment self))
                           {:dig-pace (cost/wet-dig-pace (.-equipment self))}))
         level (memoize (fn [[_ y _ :as cell]] (some->> (rules/fluid-neighbours (if (contains? plugs cell) #(or (seen-name %) "water") seen-name) cell) (keep (fn [[n [_ ny _]]] (when (and (not= "lava" n) (>= ny y)) ny))) seq (apply max))))]
     {:max-fall max-fall
      ;; a fall past the longest that costs no hp is taken only down cells seen open, so from a cell 1 off them: before
      ;; the way meets the memory its falls are dug, no longer
      :seen-steps (seen-sections/steps-fn p (max 1 (count (take-while #(zero? (fall-s %)) (range 1 (inc max-fall))))))
      :seen-cells (seen-sections/steps-fn p 1)
      :open-ys (seen-sections/open-heights p @open-ids)
      :fall-s fall-s
      :dig-s (fn [name cell] (when-let [d (dig-cost name)] (+ d (cut-s name cell))))
      :flood {:level level
              :plugs plugs
              :open (delay [(cost/flood-dig-s @wet health (cost/walk-hp-seconds)) (cost/flood-under-s @wet)])
              :dig (fn [under-s name] (let [d (or (dig-cost name) 0)]
                                        [(cost/wet-dig-s @wet health (cost/walk-hp-seconds) under-s d) (+ under-s (* (:dig-pace @wet) d))]))
              :step (fn [under-s] [(cost/wet-step-s @wet health (cost/walk-hp-seconds) under-s) (+ under-s (cost/wet-move-s @wet :walk))])}
      :step-s (cost/walk-step-s)
      :stair-s (cw/stair-cost 1)
      :unseen? unseen?
      :unseen-step-s (cost/unseen-step-s (wworld/unseen-share p))
      :lava-s (fn [faces] (+ (* faces (cw/fill-cost 1)) (* (lava-risk/hp-from [[1 0 0]] [0 0 0]) (cost/walk-hp-seconds))))})))

(defn by-share
  "prices with each cell's dig priced at (share cell) of its price (escape/dig-share-fn: an unseen cell is dug only at the chance it is solid)."
  [prices share]
  (update prices :dig-s (fn [dig-s] (fn [name cell] (when-let [s (dig-s name cell)] (let [k (share cell)] (if (zero? k) 0 (* s k))))))))

(defn less-yield
  "prices with each seen block's dig priced less what its drop is worth to the caller (yield-s name cell -> seconds), never
  below 0: a cut through what the caller digs for pays for itself."
  [prices yield-s]
  (update prices :dig-s (fn [dig-s] (fn [name cell] (when-let [s (dig-s name cell)] (max 0 (- s (if name (yield-s name cell) 0))))))))

(defn route
  "The cells to dig (head cell first within a step, in walking order) of the cheapest route from feet cell from to feet
  cell goal, falls of at most :max-fall blocks (default: those a body with no hp to spend takes, cost.armour/fall-profile),
  each priced :fall-s (blocks -> seconds) on top of its step; each step's floor must be a seen solid block the route has not dug, unless the step is up and
  :place-s prices laying one (against a face, the stair job's floor). A* with the walked steps' price as the lower bound.
  Priced :step-s a step, (:dig-s name cell) a cell dug (default 0 and 1: the fewest digs; with :flood, body-prices', the water
  a way's digs let in and its digs and steps inside it on top: flood-dig, flood-walk), a step up that digs or
  lays its floor at least :stair-s (a stair's step) and :place-s a floor laid,
  :pillar-s a jump-placed block (a step straight up; none without it; or a fn [n x y z] of the seconds of the n-th block of a run of them, from cell x y z, n 1 the least) and :unseen-step-s a step whose feet, head or
  floor is (:unseen? cell), or on a step up the cell over the head it leaves, or every face its placed block rests on, and
  :lava-s (fn [faces]) the seconds a cut with that many lava faces over or beside it costs, sealed as it is cut (the blocks and the risk the flow reaches the body) and
  :floor-absent-s (fn [floor-cell rise]) the seconds a walk rising rise blocks onto a floor that is (:unseen? cell) adds, the chance it is none; a
  block :dig-s prices nil is not dug, nor a cell under sand or gravel (it would fall on the body walking in) or, without :lava-s, with lava over or beside it (it would flow in), nor a
  floor of :ways (the floors of the body's own stairs), on which nothing is placed either. Bounds, each only when
  given: :max-dig-s seconds of digging, :max-s seconds of the whole route, :max-places blocks laid or jump-placed (the blocks carried),
  :margin cells round the two ends, :only feet cells (none else is stepped onto); :range r (> 0) also ends it on any feet cell within r of :near (default goal) that :sees (fn [x y z], when given) passes;
  :read-s seconds of CPU a cell read (priced-search), :searched-s seconds earlier searches of the question spent (counted as this one's), :back-h? the back search's bound in the heuristic, :prior-h? with
  :seen-steps and :seen-cells (fns [x y z], each within 1 of a neighbour's: the fewest steps from the cell to one whose step
  is seen, falls among cells never seen no longer than the longest :fall-s prices 0, the fewest cells across, up or down to one whose block is seen) the unseen steps and digs a way must take priced
  in the bound (:unseen-dig-s the least a dig of an unseen cell costs, default 0; :unseen-digs {:low-y :low [s ..] :any [s ..]}
  the least by its seen-cells distance, 1 the first, the last for any past, where it lies at low-y or under and at any
  height, each row non-decreasing and low no less than any, for the cells a way opens before it meets the seen; a cell seen since the search began read
  as never seen, by :guess, a fn [cell] of block-at's name for a cell never seen, default access/hidden-guess), :open-ys (an array of the heights at which block-at may read a cell open, default every height: a rise
  through cells at other heights is priced in both bounds as a stair's step or a pillar block, and a cell read open at one is read as never seen, by :guess), :full? (fn []) true when the heap holds no more.
  {:dig [cell ..] :steps [{:to feet :move :walk|:laid|:pillar :dig [cell ..] :place cell} ..] :seconds the route's price :searched-s, with :read-s, the search's CPU seconds, :searched-s given included}, {:problem :no-route} (:boxed true when the
  :margin box kept it from a cell, with :searched-s the CPU seconds the search spent, :searched-s given included),
  {:problem :dearer} when every route costs more than :max-s, {:problem :too-big} when :full? with no route in hand, or {:problem :budget} when :max-nodes cells were expanded first (search goes on past it)."
  [block-at from goal opts]
  (or (first ((search block-at from goal opts) (get opts :max-nodes (settings/get settings ::max-nodes)))) {:problem :budget}))

(defn route-within
  "search's [answer expanded] stepping only onto the feet cells of route r's steps: r priced at opts' prices (the body's
  health now), or a way through its cells no dearer. Run to its end: it expands no more than r's cells."
  [block-at from goal r opts]
  (take 2 ((search block-at from goal (assoc opts :only (map :to (:steps r)))) ##Inf)))

(defonce ^{:doc "{caller [key search-fn]} of the searches route-resumed left unfinished, one a caller, the oldest dropped past kept-max."} kept (js/Map.))

(per-body/store! ::kept #(.clear kept))

(def kept-max 16)

(defn heap-full?
  "Whether the heap is past ::heap-full of its limit."
  []
  (let [h (v8/getHeapStatistics)]
    (> (.-used_heap_size h) (* (.-heap_size_limit h) (settings/get settings ::heap-full)))))

(defn priced-search
  "search with its CPU priced (cost/search-read-s), the back search's bound in its heuristic (:back-h?), the unseen steps a
  way must take priced in it (:prior-h?, with :seen-steps and :seen-cells), grown only while the heap has room (heap-full?). A fn of n as search's."
  [block-at from goal opts]
  (search block-at from goal (assoc opts :read-s (cost/search-read-s) :back-h? true :prior-h? true :full? heap-full?)))

(defn route-resumed
  "route, but a search that spends its nodes (:max-nodes) is kept under key and
  gone on by the next call with the same key: {:problem :searching} till it ends, priced as priced-search ({:problem
  :too-big} once the heap is full with no route in hand). A key names the caller and the search ([job-id :kind from goal]):
  the caller's search under another key is dropped (its question changed), and one never asked again falls out of kept.
  An answer of a search gone on over several calls is kept only if every cell the search read still reads as it did (a
  cave, a lava pool or a wall seen meanwhile): else the search begins again."
  [key block-at from goal opts]
  (let [caller (pr-str (vec (take 2 key)))
        skey (pr-str key)
        [k run] (.get kept caller)
        resumed (when (= k skey) run)
        run (or resumed (priced-search block-at from goal opts))
        nodes (get opts :max-nodes (settings/get settings ::max-nodes))
        [a _ stale?] (run nodes)]
    (.delete kept caller)
    (if (and resumed a (not= :too-big (:problem a)) (stale?))
      (route-resumed key block-at from goal opts)
      (do (when-not a
            (.set kept caller [skey run])
            (when (> (.-size kept) kept-max) (.delete kept (.-value (.next (.keys kept))))))
          (or a {:problem :searching})))))
