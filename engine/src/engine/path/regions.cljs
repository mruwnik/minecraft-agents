(ns engine.path.regions
  "The region map: which parts of the loaded world the planner's own moves join, kept per body. A query tells in
  about a millisecond whether a goal is reachable, provably unreachable, or unknown (the way may run through land
  that is not loaded). A reachable answer carries a coarse route and a cost estimate.

  How it is built:
  - Nodes are the planner's nodes: a feet cell, or one free-space region of a tight cell. A cell the body fits only
    once a door, gate or trapdoor is opened is one node.
  - Each 16x16x16 section gets its node moves from the planner itself (planner-tuned/capture-search, captureAt: the
    same expandAt the search runs, from a node with no history).
  - Nodes joined by moves that stay inside the section form its regions (strongly connected components).
  - Each region keeps its edges out: to other regions of the section, and to node refs in other sections. Those are
    resolved when asked, building the other section if it is loaded and not yet built.
  So a region path always holds a real planner path. It does not prove the path for every planner variant: the
  capture is a superset of the planner's moves, since history and the walker's limits only remove moves, and the
  goal-dependent rules read every cell as in the goal.

  Encoding, typed arrays per section: node keys (li*16+region, sorted, Uint16) and each node's region (Uint16). Per
  region: a representative cell, flags (open = within a gap jump of an unloaded column; water), CSR edge lists (Float64
  targets: -1-region for a region of the same section, a node ref for another section; Float32 costs), and the
  regions with a move into it from the same section. `into` maps a target section to the sections with edges into
  it; the backward closure reads it instead of keeping reverse edges.

  Keys: section key ((sx+2^14)*2^15 + sz+2^14)*64 + sy (sy counted from the world's minY). Node ref =
  section*65536 + li*16 + region. Region id = section*65536 + region index. This covers worlds of +-262000 blocks.

  Updates:
  - A block change drops every section whose moves may read that block: 5 columns round, 4 below, 5 above. With
    water at or below the change (a drop into water), the sections up to 69 above go too. Dropped sections are queued
    to be built again.
  - A column loaded or unloaded drops the sections of its own and the 8 columns round (open flags and edges across
    the border).
  - The background build (build-step!, attach!) builds the queue in slices.

  Queries: route answers {:status :reachable :regions :waypoints :cost :estimate}, {:status :unreachable :why} or
  {:status :unknown :why}."
  (:require [engine.path.planner-tuned :as planner]))

(set! *warn-on-infer* true)

(def ^:const SEC-HALF 16384)
(def ^:const SEC-SPAN 32768)
(def ^:const LEVELS 64)
(def ^:const WALK-S 0.23164234422052352)
(def ^:const SQRT2 1.4142135623730951)
(def ^:const GAP-REACH 5) ; a region within this many blocks of an unloaded column may have a way into it
(def ^:const WATER 2) ; blocks.mjs kind of water
(def ^:const WATER-DROP 64) ; the planner's costs.maxWaterDrop: a drop into water of up to this many blocks
(def ^:const SEARCH-REACH 1024) ; a capture Search is made afresh for a section further than this from its centre
(def ^:const SEARCH-USES 256) ; ... or after this many sections (its caches grow)

(def defaults
  "closure-budget: regions the backward closure looks at before giving up its proof; max-builds: sections one query
  may build (beyond: :unknown :building); slice-ms / every-ms: the background build's slices."
  {:closure-budget 256 :max-builds js/Infinity :slice-ms 8 :every-ms 25})

;; ---- keys ----

(defn sec-key [sx sy sz] (+ (* (+ (* (+ sx SEC-HALF) SEC-SPAN) (+ sz SEC-HALF)) LEVELS) sy))
(defn key-sy [k] (js-mod k LEVELS))
(defn key-sz [k] (- (js-mod (js/Math.floor (/ k LEVELS)) SEC-SPAN) SEC-HALF))
(defn key-sx [k] (- (js/Math.floor (/ k (* LEVELS SEC-SPAN))) SEC-HALF))
(defn ref-sec [ref] (js/Math.floor (/ ref 65536)))
(defn region-sec [g] (js/Math.floor (/ g 65536)))
(defn region-index [g] (js-mod g 65536))

(defn octile [ax az bx bz]
  (let [a (js/Math.abs (- ax bx)) b (js/Math.abs (- az bz))]
    (+ (js/Math.max a b) (* (- SQRT2 1) (js/Math.min a b)))))

;; ---- a section ----

(deftype Section [key n ^js keys ^js comp m ^js rep ^js flags ^js eoff ^js etarget ^js ecost ^js ioff ^js iin ^js targets])

(def FLAG-OPEN 1)
(def FLAG-WATER 2)

(defn- lookup
  "the node index of key k in the sorted keys, -1 when absent"
  [^js keys n k]
  (loop [lo 0 hi (dec n)]
    (if (> lo hi)
      -1
      (let [mid (unsigned-bit-shift-right (+ lo hi) 1)
            v (aget keys mid)]
        (cond (== v k) mid
              (< v k) (recur (inc mid) hi)
              :else (recur lo (dec mid)))))))

(defn- node-of
  "the node of local key k (li*16+region) in the section: that region, else the cell's first node (a cell that is one
  node for all regions), else -1"
  [^Section sec k]
  (let [i (lookup (.-keys sec) (.-n sec) k)]
    (if (>= i 0) i (lookup (.-keys sec) (.-n sec) (* 16 (bit-shift-right k 4))))))

(defn section-bytes [^Section sec]
  (reduce + 0 (map (fn [^js a] (if (some? a) (.-byteLength a) 0))
                   [(.-keys sec) (.-comp sec) (.-rep sec) (.-flags sec) (.-eoff sec) (.-etarget sec) (.-ecost sec)
                    (.-ioff sec) (.-iin sec)])))

;; ---- the map ----

(deftype RegionMap [^js snapshot ^js options min-y levels
                    ^js sections ; key -> Section
                    ^js into ; target section key -> Set of source section keys
                    ^js queue ^js urgent ; section keys to build (Sets, in order)
                    ^:mutable ^js search ^:mutable search-x ^:mutable search-z ^:mutable search-uses
                    ^js stats
                    ;; one query's build allowance and whether it ran out
                    ^:mutable builds-left ^:mutable ^boolean starved])

(defn create
  "A region map over world {snapshot table space} (a pathWorld: the snapshot answers stateAt, hasColumn, sectionHas and
  must stay the body's view of the world: block changes reach the map through invalidate!)."
  [^js world]
  (let [^js snapshot (.-snapshot world)]
    (->RegionMap snapshot #js {:table (.-table world) :space (.-space world)} (.-minY snapshot)
                 (bit-shift-right (.-height snapshot) 4)
                 (js/Map.) (js/Map.) (js/Set.) (js/Set.) nil 0 0 0
                 #js {:built 0 :buildMs 0 :nodes 0 :regions 0 :dropped 0}
                 js/Infinity false)))

(defn- loaded? [^RegionMap rm sx sz] (true? (.hasColumn ^js (.-snapshot rm) sx sz)))

(defn- search-for!
  "the capture Search to build at x, z with: the one in use unless far, worn or stale (nil after a change)"
  ^js [^RegionMap rm x z]
  (when (or (nil? (.-search rm)) (> (js/Math.abs (- x (.-search-x rm))) SEARCH-REACH)
            (> (js/Math.abs (- z (.-search-z rm))) SEARCH-REACH) (>= (.-search-uses rm) SEARCH-USES))
    (set! (.-search rm) (planner/capture-search (.-snapshot rm) #js {:x x :y (+ (.-min-y rm) 64) :z z} (.-options rm)))
    (set! (.-search-x rm) x)
    (set! (.-search-z rm) z)
    (set! (.-search-uses rm) 0))
  (set! (.-search-uses rm) (inc (.-search-uses rm)))
  (.-search rm))

(defn- forget-targets! [^RegionMap rm key ^Section sec]
  (doseq [t (array-seq (.-targets sec))]
    (when-let [^js s (.get (.-into rm) t)]
      (.delete s key))))

(def EMPTY "the one Section of every section without a node" (Section. -1 0 (js/Uint16Array. 0) (js/Uint16Array. 0) 0 nil nil (js/Uint32Array. 1) nil nil (js/Uint32Array. 1) nil #js []))

(defn- store-empty! [^RegionMap rm key t0]
  (when-let [old (.get (.-sections rm) key)] (forget-targets! rm key old))
  (.set (.-sections rm) key EMPTY)
  (let [^js st (.-stats rm)]
    (set! (.-built st) (inc (.-built st)))
    (set! (.-buildMs st) (+ (.-buildMs st) (- (js/performance.now) t0))))
  EMPTY)

(defn- tarjan
  "strongly connected components of nodes 0..n-1 over the CSR adjacency off/adj: Int32Array comp, and the count"
  [n ^js off ^js adj]
  (let [comp (.fill (js/Int32Array. n) -1)
        num (.fill (js/Int32Array. n) -1)
        low (js/Int32Array. n)
        on (js/Uint8Array. n)
        stack (js/Int32Array. n)
        work (js/Int32Array. n)
        pos (js/Int32Array. n)
        counter (volatile! 0)
        comps (volatile! 0)
        sp (volatile! 0)]
    (dotimes [root n]
      (when (== -1 (aget num root))
        (aset num root @counter) (aset low root @counter) (vswap! counter inc)
        (aset stack @sp root) (vswap! sp inc) (aset on root 1)
        (aset work 0 root) (aset pos 0 (aget off root))
        (loop [wp 1]
          (when (pos? wp)
            (let [v (aget work (dec wp))
                  p (aget pos (dec wp))]
              (if (< p (aget off (inc v)))
                (let [w (aget adj p)]
                  (aset pos (dec wp) (inc p))
                  (cond
                    (== -1 (aget num w))
                    (do (aset num w @counter) (aset low w @counter) (vswap! counter inc)
                        (aset stack @sp w) (vswap! sp inc) (aset on w 1)
                        (aset work wp w) (aset pos wp (aget off w))
                        (recur (inc wp)))
                    (== 1 (aget on w))
                    (do (aset low v (js/Math.min (aget low v) (aget num w))) (recur wp))
                    :else (recur wp)))
                (do
                  (when (> wp 1)
                    (let [u (aget work (- wp 2))] (aset low u (js/Math.min (aget low u) (aget low v)))))
                  (when (== (aget low v) (aget num v))
                    (loop []
                      (vswap! sp dec)
                      (let [w (aget stack @sp)]
                        (aset on w 0)
                        (aset comp w @comps)
                        (when-not (== w v) (recur))))
                    (vswap! comps inc))
                  (recur (dec wp)))))))))
    #js [comp @comps]))

(defn- csr
  "CSR of the pairs (src, val) for src in 0..n-1: #js [off vals], vals in the order given"
  [n ^js srcs ^js vals ctor]
  (let [off (js/Uint32Array. (inc n))
        out (js/Reflect.construct ctor #js [(.-length srcs)])
        fill (js/Uint32Array. n)]
    (dotimes [k (.-length srcs)] (aset off (inc (aget srcs k)) (inc (aget off (inc (aget srcs k))))))
    (dotimes [i n] (aset off (inc i) (+ (aget off (inc i)) (aget off i))))
    (dotimes [k (.-length srcs)]
      (let [s (aget srcs k)]
        (aset out (+ (aget off s) (aget fill s)) (aget vals k))
        (aset fill s (inc (aget fill s)))))
    #js [off out]))

(defn- build-section
  "the Section at key, made from the planner's moves (see the ns doc); nil when its column is not loaded or the key is
  outside the world's height"
  ^Section [^RegionMap rm key]
  (let [sx (key-sx key) sy (key-sy key) sz (key-sz key)]
    (when (and (< sy (.-levels rm)) (loaded? rm sx sz))
      (let [t0 (js/performance.now)
            x0 (* sx 16) y0 (+ (.-min-y rm) (* sy 16)) z0 (* sz 16)
            ^js s (search-for! rm (+ x0 8) (+ z0 8))
            nk #js [] nx #js [] ny #js [] nz #js [] nh #js [] nr #js []]
        ;; the nodes, in key order
        (dotimes [ly 16]
          (dotimes [lz 16]
            (dotimes [lx 16]
              (let [x (+ x0 lx) y (+ y0 ly) z (+ z0 lz)
                    h0 (.nodeH s x y z)
                    h (if (>= h0 0) h0 (.floodHeight s x y z))
                    li (bit-or (bit-shift-left ly 8) (bit-shift-left lz 4) lx)]
                (when (>= h 0)
                  (if (neg? h0)
                    (do (.push nk (* li 16)) (.push nx x) (.push ny y) (.push nz z) (.push nh h) (.push nr -1))
                    (dotimes [r (.floodRegions s x y z h)]
                      (.push nk (+ (* li 16) r)) (.push nx x) (.push ny y) (.push nz z) (.push nh h) (.push nr r))))))))
        (if (zero? (.-length nk))
          (store-empty! rm key t0)
        (let [n (.-length nk)
              keys (js/Uint16Array. nk)
              probe (Section. key n keys nil 0 nil nil nil nil nil nil nil nil)
              ;; the moves: per node, local targets (node index) and cross targets (node ref) with costs
              lsrc #js [] ldst #js [] lcost #js []
              xsrc #js [] xref #js [] xcost #js []
              out #js []]
          (dotimes [i n]
            (set! (.-length out) 0)
            (.captureAt s (aget nx i) (aget ny i) (aget nz i) (aget nh i) (aget nr i) out)
            (loop [k 0]
              (when (< k (.-length out))
                (let [tx (aget out k) ty (aget out (+ k 1)) tz (aget out (+ k 2)) tr (aget out (+ k 3)) cost (aget out (+ k 4))
                      tsy (bit-shift-right (- ty (.-min-y rm)) 4)
                      tkey (sec-key (bit-shift-right tx 4) tsy (bit-shift-right tz 4))
                      lk (+ (* 16 (bit-or (bit-shift-left (bit-and (- ty (.-min-y rm)) 15) 8) (bit-shift-left (bit-and tz 15) 4) (bit-and tx 15))) tr)]
                  (if (== tkey key)
                    (let [j (node-of probe lk)]
                      (when (>= j 0) (.push lsrc i) (.push ldst j) (.push lcost cost)))
                    (when (and (>= tsy 0) (< tsy (.-levels rm)))
                      (.push xsrc i) (.push xref (+ (* tkey 65536) lk)) (.push xcost cost)))
                  (recur (+ k 5))))))
          (let [^js adj (csr n lsrc ldst js/Int32Array)
                ^js res (tarjan n (aget adj 0) (aget adj 1))
                ^js ncomp (aget res 0)
                m (aget res 1)
                comp (js/Uint16Array. ncomp)
                ;; representative: the node nearest its region's centroid
                cx (js/Float64Array. m) cy (js/Float64Array. m) cz (js/Float64Array. m) size (js/Float64Array. m)
                rep (js/Uint16Array. m) best (.fill (js/Float64Array. m) js/Infinity)
                flags (js/Uint8Array. m)
                open-col (.fill (js/Int8Array. 256) -1)]
            (dotimes [i n]
              (let [c (aget ncomp i)]
                (aset size c (inc (aget size c)))
                (aset cx c (+ (aget cx c) (aget nx i))) (aset cy c (+ (aget cy c) (aget ny i))) (aset cz c (+ (aget cz c) (aget nz i)))))
            (dotimes [i n]
              (let [c (aget ncomp i)
                    x (aget nx i) y (aget ny i) z (aget nz i)
                    d (+ (js/Math.pow (- x (/ (aget cx c) (aget size c))) 2) (js/Math.pow (- y (/ (aget cy c) (aget size c))) 2)
                         (js/Math.pow (- z (/ (aget cz c) (aget size c))) 2))
                    col (bit-or (bit-shift-left (bit-and z 15) 4) (bit-and x 15))]
                (when (< d (aget best c)) (aset best c d) (aset rep c (bit-shift-right (aget keys i) 4)))
                (when (== -1 (aget open-col col))
                  (aset open-col col
                        (if (some (fn [[dx dz]] (not (loaded? rm (bit-shift-right (+ x dx) 4) (bit-shift-right (+ z dz) 4))))
                                  (for [dx [(- GAP-REACH) 0 GAP-REACH] dz [(- GAP-REACH) 0 GAP-REACH]] [dx dz]))
                          1 0)))
                (when (== 1 (aget open-col col)) (aset flags c (bit-or (aget flags c) FLAG-OPEN)))
                (when (.isWater s x y z) (aset flags c (bit-or (aget flags c) FLAG-WATER)))))
            ;; region edges: the cheapest move per (region, target region) and per (region, target ref)
            (let [per (js/Array. m)
                  targets (js/Set.)]
              (dotimes [c m] (aset per c (js/Map.)))
              (dotimes [k (.-length lsrc)]
                (let [a (aget ncomp (aget lsrc k)) b (aget ncomp (aget ldst k))]
                  (when-not (== a b)
                    (let [^js mp (aget per a) t (- -1 b) prev (.get mp t)]
                      (when (or (undefined? prev) (< (aget lcost k) prev)) (.set mp t (aget lcost k)))))))
              (dotimes [k (.-length xsrc)]
                (let [^js mp (aget per (aget ncomp (aget xsrc k))) t (aget xref k) prev (.get mp t)]
                  (.add targets (ref-sec t))
                  (when (or (undefined? prev) (< (aget xcost k) prev)) (.set mp t (aget xcost k)))))
              (let [eoff (js/Uint32Array. (inc m))
                    total (reduce + 0 (map (fn [^js mp] (.-size mp)) (array-seq per)))
                    etarget (js/Float64Array. total)
                    ecost (js/Float32Array. total)
                    isrc #js [] ival #js []]
                (loop [c 0 at 0]
                  (when (< c m)
                    (aset eoff c at)
                    (let [^js mp (aget per c)
                          at2 (reduce (fn [at [t cost]]
                                        (aset etarget at t) (aset ecost at cost)
                                        (when (neg? t) (.push isrc (- -1 t)) (.push ival c))
                                        (inc at))
                                      at (es6-iterator-seq (.entries mp)))]
                      (recur (inc c) at2))))
                (aset eoff m total)
                (dotimes [i n] (aset comp i (aget ncomp i)))
                (let [^js inv (csr m isrc ival js/Uint16Array)
                      tk (js/Array.from targets)
                      sec (Section. key n keys comp m rep flags eoff etarget ecost (aget inv 0) (aget inv 1) tk)]
                  (when-let [old (.get (.-sections rm) key)] (forget-targets! rm key old))
                  (.set (.-sections rm) key sec)
                  (doseq [t (array-seq tk)]
                    (let [^js s2 (or (.get (.-into rm) t) (let [s2 (js/Set.)] (.set (.-into rm) t s2) s2))]
                      (.add s2 key)))
                  (let [^js st (.-stats rm)]
                    (set! (.-built st) (inc (.-built st)))
                    (set! (.-nodes st) (+ (.-nodes st) n))
                    (set! (.-regions st) (+ (.-regions st) m))
                    (set! (.-buildMs st) (+ (.-buildMs st) (- (js/performance.now) t0))))
                  sec))))))))))

(defn section
  "the built Section at key, building it when its column is loaded and a query's allowance (or none) permits; nil when
  it cannot be had"
  ^Section [^RegionMap rm key]
  (or (.get (.-sections rm) key)
      (cond
        (<= (.-builds-left rm) 0) (do (set! (.-starved rm) true) nil)
        :else (do (set! (.-builds-left rm) (dec (.-builds-left rm)))
                  (.delete (.-queue rm) key)
                  (.delete (.-urgent rm) key)
                  (build-section rm key)))))

(defn- cell-key [^RegionMap rm x y z]
  (let [ry (- y (.-min-y rm))]
    (when (and (>= ry 0) (< (bit-shift-right ry 4) (.-levels rm)))
      #js [(sec-key (bit-shift-right x 4) (bit-shift-right ry 4) (bit-shift-right z 4))
           (bit-or (bit-shift-left (bit-and ry 15) 8) (bit-shift-left (bit-and z 15) 4) (bit-and x 15))])))

(defn region-of-ref
  "the region id of node ref, -1 when its section holds no such node, -2 when the section cannot be had"
  [^RegionMap rm ref]
  (let [k (ref-sec ref)
        sec (section rm k)]
    (if (nil? sec)
      -2
      (let [i (node-of sec (js-mod ref 65536))]
        (if (neg? i) -1 (+ (* k 65536) (aget (.-comp sec) i)))))))

(defn regions-at
  "the region ids of the nodes of cell x y z (every region of a tight cell): a vector; nil when the section cannot be had"
  [^RegionMap rm x y z]
  (when-let [^js ck (cell-key rm x y z)]
    (when-let [sec (section rm (aget ck 0))]
      (let [li (aget ck 1)
            first-i (lookup (.-keys sec) (.-n sec) (* li 16))]
        (if (neg? first-i)
          []
          (loop [i first-i acc []]
            (if (and (< i (.-n sec)) (== li (bit-shift-right (aget (.-keys sec) i) 4)))
              (recur (inc i) (conj acc (+ (* (aget ck 0) 65536) (aget (.-comp sec) i))))
              acc)))))))

(defn region-cell
  "{:x :y :z} of region g's representative cell"
  [^RegionMap rm g]
  (let [k (region-sec g)
        ^Section sec (.get (.-sections rm) k)
        li (aget (.-rep sec) (region-index g))]
    {:x (+ (* 16 (key-sx k)) (bit-and li 15))
     :y (+ (.-min-y rm) (* 16 (key-sy k)) (bit-shift-right li 8))
     :z (+ (* 16 (key-sz k)) (bit-and (bit-shift-right li 4) 15))}))

(defn- rep-at!
  "region g's representative cell into out at offset o (x y z): region-cell without the map"
  [^RegionMap rm g ^js out o]
  (let [k (region-sec g)
        ^Section sec (.get (.-sections rm) k)
        li (aget (.-rep sec) (region-index g))]
    (aset out o (+ (* 16 (key-sx k)) (bit-and li 15)))
    (aset out (+ o 1) (+ (.-min-y rm) (* 16 (key-sy k)) (bit-shift-right li 8)))
    (aset out (+ o 2) (+ (* 16 (key-sz k)) (bit-and (bit-shift-right li 4) 15)))))

(def ^:private scratch (js/Float64Array. 6))

(defn- flag? [^RegionMap rm g bit]
  (let [^Section sec (.get (.-sections rm) (region-sec g))]
    (not (zero? (bit-and (aget (.-flags sec) (region-index g)) bit)))))

(defn edges-of
  "[[region cost] ...] of region g's moves out (cross ones whose section can be had); sets starved when some cannot"
  [^RegionMap rm g]
  (let [k (region-sec g)
        ^Section sec (.get (.-sections rm) k)
        c (region-index g)]
    (loop [e (aget (.-eoff sec) c) acc (transient [])]
      (if (< e (aget (.-eoff sec) (inc c)))
        (let [t (aget (.-etarget sec) e)
              cost (aget (.-ecost sec) e)]
          (if (neg? t)
            (recur (inc e) (conj! acc [(+ (* k 65536) (- -1 t)) cost]))
            (let [r (region-of-ref rm t)]
              (recur (inc e) (if (>= r 0) (conj! acc [r cost]) acc)))))
        (persistent! acc)))))

(defn preds-of
  "the set of regions with a move into region g: the sections round g's (a section further up over water) are built
  first so each one that can hold a move into it has registered (see ns doc); nil when one of them could not be had
  because the query's allowance ran out"
  [^RegionMap rm g]
  (let [k (region-sec g)
        sx (key-sx k) sy (key-sy k) sz (key-sz k)
        up (if (flag? rm g FLAG-WATER) 5 1)]
    (doseq [dx [-1 0 1] dz [-1 0 1] dy (range -1 (inc up))]
      (let [ny (+ sy dy)]
        (when (and (>= ny 0) (< ny (.-levels rm)))
          (section rm (sec-key (+ sx dx) ny (+ sz dz))))))
    (when-not (.-starved rm)
      (let [^Section sec (.get (.-sections rm) k)
            c (region-index g)
            acc (volatile! (transient #{}))]
        (loop [e (aget (.-ioff sec) c)]
          (when (< e (aget (.-ioff sec) (inc c)))
            (vswap! acc conj! (+ (* k 65536) (aget (.-iin sec) e)))
            (recur (inc e))))
        (doseq [src (some-> ^js (.get (.-into rm) k) js/Array.from array-seq)]
          (when-let [^Section ss (.get (.-sections rm) src)]
            (dotimes [sc (.-m ss)]
              (loop [e (aget (.-eoff ss) sc)]
                (when (< e (aget (.-eoff ss) (inc sc)))
                  (let [t (aget (.-etarget ss) e)]
                    (when (and (>= t 0) (== (ref-sec t) k)
                               (let [i (node-of sec (js-mod t 65536))] (and (>= i 0) (== c (aget (.-comp sec) i)))))
                      (vswap! acc conj! (+ (* src 65536) sc))))
                  (recur (inc e)))))))
        (persistent! @acc)))))

;; ---- goals ----

(defn- goal-cells
  "calls f with x y z of each cell of goal {:kind :x :y :z :range} that a planner goal test takes; for an xz disc, the
  cells of its columns in each section of the world's height"
  [^RegionMap rm {:keys [kind x y z range] :or {range 0}} f]
  (let [r (js/Math.ceil range) r2 (* range range)]
    (if (= kind "near")
      (doseq [dx (clojure.core/range (- r) (inc r)) dy (clojure.core/range (- r) (inc r)) dz (clojure.core/range (- r) (inc r))
              :when (<= (+ (* dx dx) (* dy dy) (* dz dz)) r2)]
        (f (+ x dx) (+ y dy) (+ z dz)))
      (doseq [dx (clojure.core/range (- r) (inc r)) dz (clojure.core/range (- r) (inc r))
              :when (<= (+ (* dx dx) (* dz dz)) r2)
              yy (clojure.core/range (.-min-y rm) (+ (.-min-y rm) (* 16 (.-levels rm))))]
        (f (+ x dx) yy (+ z dz))))))

(defn goal-regions
  "{:regions #{ids} :per-goal [#{ids} ...] :unknown bool}: the regions holding a node in any goal's area (and in each
  goal's); unknown when some goal cell lies in a section that cannot be had (an unloaded column, or the allowance ran out)"
  [^RegionMap rm goals]
  (let [unknown (volatile! false)
        per-goal (mapv (fn [g]
                         (let [found (volatile! (transient #{}))]
                           (goal-cells rm g (fn [x y z]
                                              (when-let [^js ck (cell-key rm x y z)]
                                                (if (nil? (section rm (aget ck 0)))
                                                  (vreset! unknown true)
                                                  (doseq [r (regions-at rm x y z)] (vswap! found conj! r))))))
                           (persistent! @found)))
                       goals)]
    {:regions (reduce into #{} per-goal) :per-goal per-goal :unknown @unknown}))

;; ---- queries ----

(defn closure
  "Backward closure from the goal regions over preds-of, up to budget regions: :closed (nothing outside it has a move
  in, and it holds no start: a proof), :met (a start is in it), :leak (a region by unloaded land), :budget or :starved."
  [^RegionMap rm goals starts budget]
  (let [seen (js/Set. (to-array goals))
        queue (into [] goals)]
    (loop [head 0 queue queue]
      (if (>= head (count queue))
        :closed
        (let [g (nth queue head)]
          (cond
            (contains? starts g) :met
            (flag? rm g FLAG-OPEN) :leak
            :else
            (let [ps (preds-of rm g)]
              (if (nil? ps)
                :starved
                (let [fresh (vec (remove #(.has seen %) ps))]
                  (doseq [p fresh] (.add seen p))
                  (if (> (.-size seen) budget)
                    :budget
                    (recur (inc head) (into queue fresh))))))))))))

(defn- rep-cost [^RegionMap rm a b c]
  (rep-at! rm a scratch 0)
  (rep-at! rm b scratch 3)
  (js/Math.max c (+ (* WALK-S (octile (aget scratch 0) (aget scratch 2) (aget scratch 3) (aget scratch 5)))
                    (* 0.1 (js/Math.max 0 (- (aget scratch 4) (aget scratch 1)))))))

(defn- heap-push! [^js heap f g]
  (.push heap #js [f g])
  (loop [i (dec (.-length heap))]
    (when (pos? i)
      (let [p (bit-shift-right (dec i) 1)]
        (when (< (aget (aget heap i) 0) (aget (aget heap p) 0))
          (let [t (aget heap i)] (aset heap i (aget heap p)) (aset heap p t))
          (recur p))))))

(defn- heap-pop! [^js heap]
  (let [top (aget heap 0)
        last (.pop heap)]
    (when (pos? (.-length heap))
      (aset heap 0 last)
      (loop [i 0]
        (let [l (inc (* 2 i)) r (+ 2 (* 2 i))
              m (if (and (< l (.-length heap)) (< (aget (aget heap l) 0) (aget (aget heap i) 0))) l i)
              m (if (and (< r (.-length heap)) (< (aget (aget heap r) 0) (aget (aget heap m) 0))) r m)]
          (when-not (== m i)
            (let [t (aget heap i)] (aset heap i (aget heap m)) (aset heap m t))
            (recur m)))))
    top))

(defn- search-regions
  "A* over regions from starts to any of goals (sets of ids), edge costs rep-cost, heuristic h: {:hit :dist :parent
  :open :settled} (hit -1 when none)"
  [^RegionMap rm starts goals h]
  (let [dist (js/Map.) parent (js/Map.) done (js/Set.) heap #js []]
    (doseq [g starts] (.set dist g 0) (.set parent g -1) (heap-push! heap (h g) g))
    (loop [open false settled 0]
      (if (zero? (.-length heap))
        {:hit -1 :dist dist :parent parent :open open :settled settled}
        (let [g (aget (heap-pop! heap) 1)]
          (if (.has done g)
            (recur open settled)
            (let [_ (.add done g)
                  open (or open (flag? rm g FLAG-OPEN))]
              (if (contains? goals g)
                {:hit g :dist dist :parent parent :open open :settled (inc settled)}
                (let [d (.get dist g)]
                  (doseq [[t c] (edges-of rm g)]
                    (let [nd (+ d (rep-cost rm g t c))
                          old (.get dist t)]
                      (when (or (undefined? old) (< nd old))
                        (.set dist t nd) (.set parent t g) (heap-push! heap (+ nd (h t)) t))))
                  (recur open (inc settled)))))))))))

(defn- estimate
  "seconds along from, the waypoints and the goal: octile walking plus 0.35 s a block climbed"
  [from waypoints goal]
  (let [pts (concat [from] waypoints [(update goal :y #(or % (:y from)))])]
    (reduce + 0 (map (fn [a b] (+ (* WALK-S (octile (:x a) (:z a) (:x b) (:z b))) (* 0.35 (js/Math.max 0 (- (:y b) (:y a))))))
                     pts (rest pts)))))

(defn route
  "Is any of goals ({:kind \"near\"|\"xz\" :x :y :z :range}, the planner's goals) reachable from cell from {:x :y :z}?
  {:status :reachable :regions [ids] :waypoints [{:x :y :z}] (the regions' cells between) :cost (region graph seconds)
   :estimate (seconds) :goal (index of the goal of the end region)}, {:status :unreachable :why
  :goal-not-standable|:goal-enclosed|:exhausted} (a proof for every planner variant), or {:status :unknown :why
  :start-unloaded|:start-not-standable|:goal-unloaded|:unloaded|:building}. opts over defaults (:closure-budget,
  :max-builds)."
  ([rm from goals] (route rm from goals {}))
  ([^RegionMap rm from goals opts]
   (let [{:keys [closure-budget max-builds]} (merge defaults opts)
         goals (vec goals)]
     (set! (.-builds-left rm) max-builds)
     (set! (.-starved rm) false)
     (let [starts (regions-at rm (:x from) (:y from) (:z from))
           answer
           (cond
             (nil? starts) {:status :unknown :why (if (.-starved rm) :building :start-unloaded)}
             (empty? starts) {:status :unknown :why :start-not-standable}
             :else
             (let [{gs :regions per-goal :per-goal unknown :unknown} (goal-regions rm goals)
                   start-set (set starts)]
               (cond
                 (.-starved rm) {:status :unknown :why :building}
                 (and (empty? gs) unknown) {:status :unknown :why :goal-unloaded}
                 (empty? gs) {:status :unreachable :why :goal-not-standable}
                 :else
                 (let [cl (if (and (pos? closure-budget) (not unknown)) (closure rm gs start-set closure-budget) :skipped)]
                   (if (= cl :closed)
                     {:status :unreachable :why :goal-enclosed}
                     (let [_ (set! (.-starved rm) false)
                           gxs (to-array (map :x goals)) gzs (to-array (map :z goals))
                           grs (to-array (map #(+ 16 (or (:range %) 0)) goals))
                           h (fn [g]
                               (rep-at! rm g scratch 0)
                               (loop [i 0 best js/Infinity]
                                 (if (< i (alength gxs))
                                   (recur (inc i) (js/Math.min best (js/Math.max 0 (- (octile (aget scratch 0) (aget scratch 2) (aget gxs i) (aget gzs i))
                                                                                       (aget grs i)))))
                                   (* WALK-S best))))
                           {:keys [hit dist parent open]} (search-regions rm starts gs h)]
                       (cond
                         (>= hit 0)
                         (let [path (loop [g hit acc ()] (if (== g -1) (vec acc) (recur (.get parent g) (conj acc g))))
                               waypoints (mapv #(region-cell rm %) (butlast (rest path)))
                               end (region-cell rm hit)
                               gi (first (keep-indexed (fn [i gset] (when (contains? gset hit) i)) per-goal))]
                           {:status :reachable :regions path :waypoints waypoints :cost (.get dist hit)
                            :estimate (estimate from waypoints (nth goals (or gi 0))) :goal (or gi 0) :end end})
                         (.-starved rm) {:status :unknown :why :building}
                         (or open unknown) {:status :unknown :why :unloaded}
                         :else {:status :unreachable :why :exhausted})))))))]
     (set! (.-builds-left rm) js/Infinity)
     (set! (.-starved rm) false)
     (assoc answer :sections (.-size (.-sections rm)))))))

;; ---- updates ----

(defn- drop-section! [^RegionMap rm key]
  (when-let [sec (.get (.-sections rm) key)]
    (forget-targets! rm key sec)
    (.delete (.-sections rm) key)
    (set! (.-dropped ^js (.-stats rm)) (inc (.-dropped ^js (.-stats rm)))))
  (if (loaded? rm (key-sx key) (key-sz key))
    (do (.delete (.-queue rm) key) (.add (.-urgent rm) key))
    (do (.delete (.-queue rm) key) (.delete (.-urgent rm) key))))

(defn- water-id? [^RegionMap rm id]
  (and (some? id) (not (== id 0xFFFF)) (== WATER (aget (.-kind ^js (.-table ^js (.-options rm))) id))))

(defn- water-below?
  "water in the 3x3 columns round x z from y down a drop into water"
  [^RegionMap rm x y z]
  (let [^js snap (.-snapshot rm)]
    (some (fn [[dx dz]]
            (loop [yy y]
              (cond (< yy (- y WATER-DROP 1)) false
                    (water-id? rm (.stateAt snap (+ x dx) yy (+ z dz))) true
                    :else (recur (dec yy)))))
          (for [dx [-1 0 1] dz [-1 0 1]] [dx dz]))))

(defn invalidate!
  "Block x y z changed (old-id: its state before, when known): drop and queue the sections whose moves may read it (ns
  doc). Built sections only matter: the rest are built from the world as it is."
  ([rm x y z] (invalidate! rm x y z nil))
  ([^RegionMap rm x y z old-id]
   (set! (.-search rm) nil)
   (let [water (or (water-id? rm old-id) (water-below? rm x y z))
         dys (if water [-4 0 5 16 32 48 64 69] [-4 0 5])]
     (doseq [dx [(- GAP-REACH) 0 GAP-REACH] dz [(- GAP-REACH) 0 GAP-REACH] dy dys]
       (when-let [^js ck (cell-key rm (+ x dx) (+ y dy) (+ z dz))]
         (drop-section! rm (aget ck 0)))))))

(defn column-changed!
  "Column cx cz was loaded or unloaded: drop the sections of it and of the 8 round it, and queue the loaded ones."
  [^RegionMap rm cx cz]
  (set! (.-search rm) nil)
  (doseq [dx [-1 0 1] dz [-1 0 1] sy (range (.-levels rm))]
    (let [k (sec-key (+ cx dx) sy (+ cz dz))]
      (when (or (.has (.-sections rm) k) (and (zero? dx) (zero? dz)))
        (drop-section! rm k)))))

(defn queue-column!
  "queue every section of column cx cz to be built (the background build)"
  [^RegionMap rm cx cz]
  (dotimes [sy (.-levels rm)]
    (let [k (sec-key cx sy cz)]
      (when-not (.has (.-sections rm) k) (.add (.-queue rm) k)))))

(defn pending [^RegionMap rm] (+ (.-size (.-urgent rm)) (.-size (.-queue rm))))

(defn build-step!
  "Build queued sections (the ones a change dropped first) for about ms milliseconds; the number built."
  [^RegionMap rm ms]
  (let [t0 (js/performance.now)]
    (loop [built 0]
      (let [^js from (if (pos? (.-size (.-urgent rm))) (.-urgent rm) (.-queue rm))]
        (if (or (zero? (.-size from)) (> (- (js/performance.now) t0) ms))
          (do (when (zero? (pending rm)) (set! (.-search rm) nil)) ; its caches are dead weight between builds
              built)
          (let [k (.-value (.next (.values from)))]
            (.delete from k)
            (when-not (.has (.-sections rm) k) (build-section rm k))
            (recur (inc built))))))))

(defn memory
  "{:sections :nodes :regions :bytes}: what the map holds now (bytes: its typed arrays)"
  [^RegionMap rm]
  (reduce (fn [acc ^Section sec]
            (-> acc (update :sections inc) (update :nodes + (.-n sec)) (update :regions + (.-m sec))
                (update :bytes + (section-bytes sec))))
          {:sections 0 :nodes 0 :regions 0 :bytes 0}
          (es6-iterator-seq (.values (.-sections rm)))))

;; ---- per body ----

(defn attach!
  "A region map kept up to date over source (engine/js/path/region-source.mjs over a bot: {snapshot table space columns()
  onChange(fn)}, onChange calling fn with #js {:type \"block\" :x :y :z :old} or #js {:type \"load\"|\"unload\" :cx :cz}),
  built in the background in slices of :slice-ms every :every-ms. {:map rm :stop fn}."
  ([source] (attach! source {}))
  ([^js source opts]
   (let [{:keys [slice-ms every-ms]} (merge defaults opts)
         rm (create source)
         timer (volatile! nil)
         stopped (volatile! false)
         tick (fn tick []
                (vreset! timer nil)
                (when-not @stopped
                  (build-step! rm slice-ms)
                  (when (pos? (pending rm)) (vreset! timer (js/setTimeout tick every-ms)))))
         wake (fn [] (when (and (nil? @timer) (not @stopped) (pos? (pending rm)))
                       (vreset! timer (js/setTimeout tick every-ms))))
         unsubscribe (.onChange source
                                (fn [^js e]
                                  (case (.-type e)
                                    "block" (invalidate! rm (.-x e) (.-y e) (.-z e) (.-old e))
                                    "load" (do (column-changed! rm (.-cx e) (.-cz e)) (queue-column! rm (.-cx e) (.-cz e)))
                                    "unload" (column-changed! rm (.-cx e) (.-cz e))
                                    nil)
                                  (wake)))]
     (doseq [^js c (array-seq (.columns source))] (queue-column! rm (aget c 0) (aget c 1)))
     (wake)
     {:map rm
      :stop (fn [] (vreset! stopped true)
              (when @timer (js/clearTimeout @timer))
              (when (fn? unsubscribe) (unsubscribe)))})))
