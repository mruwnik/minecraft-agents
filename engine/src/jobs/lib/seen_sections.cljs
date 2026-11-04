(ns jobs.lib.seen-sections
  "The body's memory a section (16 cells a side) at a time: the fewest steps from a cell to any cell it remembers, the
  openings it saw since a mark, and the frontier it could walk to nearest a goal."
  (:require [engine.perception.store :as store]
            [jobs.lib.walk.rock :as rock]))

(defn- st-of [p] (let [^js st (some-> (aget p "perception") :st)] (when (some-> st .-stores) st)))

(defn- sections [^js st] (store/store-of st (.-dim st)))

;; js/Math.max and min here and in steps-fn: cljs max of three or more compiles to a variadic call, which made a section's
;; boxes take a ms to order
(defn- gap
  "Cells between a and the 16 from lo, 0 inside."
  [a lo]
  (js/Math.max 0 (- lo a) (- a (+ lo 15))))

(defn- steps
  "The fewest steps (one cell across, one up or max-fall down each) between cells dx dy dz apart, either way."
  [dx dy dz mf]
  (js/Math.max (+ dx dz) (js/Math.ceil (/ dy mf))))

(defonce ^{:doc "section -> #js [wrote x0 y0 z0 x1 y1 z1]: the box of its remembered cells as of its last write."} extents (js/WeakMap.))

(defn- extent
  "The box [x0 y0 z0 x1 y1 z1] of the cells section s remembers, nil with none: scanned once a write."
  [^js s]
  (let [^js e (.get extents s)]
    (if (and e (identical? (aget e 0) (.-wrote s)))
      (when (<= (aget e 1) (aget e 4)) e)
      (let [^js ids (.-ids s)
            e #js [(.-wrote s) 16 16 16 -1 -1 -1]]
        (dotimes [i 4096]
          (when-not (zero? (aget ids i))
            (let [x (bit-and i 15) y (bit-shift-right i 8) z (bit-and (bit-shift-right i 4) 15)]
              (aset e 1 (js/Math.min (aget e 1) x)) (aset e 2 (js/Math.min (aget e 2) y)) (aset e 3 (js/Math.min (aget e 3) z))
              (aset e 4 (js/Math.max (aget e 4) x)) (aset e 5 (js/Math.max (aget e 5) y)) (aset e 6 (js/Math.max (aget e 6) z)))))
        (let [bx (* 16 (.-cx s)) by (* 16 (.-sy s)) bz (* 16 (.-cz s))]
          (doseq [[i o] [[1 bx] [2 by] [3 bz] [4 bx] [5 by] [6 bz]]] (aset e i (+ o (aget e i)))))
        (.set extents s e)
        (when (<= (aget e 1) (aget e 4)) e)))))

(defn steps-fn
  "A fn [x y z] of the fewest steps (each at most one cell across, one up or max-fall down) between the cell and any cell
  the body remembers, by the box of each section's remembered cells (##Inf with none): within one of a neighbour cell's,
  so a search may price on it. nil without a memory. The sections are the memory's when first asked."
  [p max-fall]
  (when-let [st (st-of p)]
    (let [mf (max 1 max-fall)
          ;; x0 y0 z0 x1 y1 z1 a box, in one array of plain numbers
          boxes (delay (let [a #js []] (.forEach (sections st) (fn [^js s] (when-let [e (extent s)] (.push a (aget e 1) (aget e 2) (aget e 3) (aget e 4) (aget e 5) (aget e 6))))) (js/Float64Array.from a)))
          span (fn [a lo hi] (js/Math.max 0 (- lo a) (- a hi)))
          ;; by a cell's section #js [order union values]: order the boxes that may be nearest a cell of it, nearest first
          ;; (those no farther from the section than the box all its cells are nearest to at worst; an entry is steps x n +
          ;; the box's index, sorted as plain numbers); union their joint box, no farther than any of them (under a wide
          ;; surface the many boxes alike far end the scan at the first); values each cell's answer once found (NaN not yet)
          by-section (js/Map.)
          section-of (fn [k cx sy cz]
                       (let [^js bs @boxes
                             n (/ (.-length bs) 6)
                             ox (* 16 cx) oy (* 16 sy) oz (* 16 cz)
                             near (fn [o lo hi] (js/Math.max 0 (- lo (+ o 15)) (- o hi)))
                             far (fn [o lo hi] (js/Math.max 0 (- lo o) (- (+ o 15) hi)))
                             lo (js/Float64Array. n)
                             worst (loop [i 0 u ##Inf]
                                     (if (>= i n)
                                       u
                                       (let [b (* 6 i) x0 (aget bs b) y0 (aget bs (+ b 1)) z0 (aget bs (+ b 2)) x1 (aget bs (+ b 3)) y1 (aget bs (+ b 4)) z1 (aget bs (+ b 5))]
                                         (aset lo i (steps (near ox x0 x1) (near oy y0 y1) (near oz z0 z1) mf))
                                         (recur (inc i) (js/Math.min u (steps (far ox x0 x1) (far oy y0 y1) (far oz z0 z1) mf))))))
                             o #js []
                             union #js [##Inf ##Inf ##Inf ##-Inf ##-Inf ##-Inf]]
                         (dotimes [i n]
                           (when (<= (aget lo i) worst)
                             (.push o (+ (* n (aget lo i)) i))
                             (dotimes [j 3]
                               (aset union j (js/Math.min (aget union j) (aget bs (+ (* 6 i) j))))
                               (aset union (+ j 3) (js/Math.max (aget union (+ j 3)) (aget bs (+ (* 6 i) j 3)))))))
                         (let [e #js [(.sort (js/Float64Array.from o)) union (.fill (js/Float32Array. 4096) js/NaN)]]
                           (.set by-section k e)
                           e)))]
      (fn [x y z]
        (let [cx (bit-shift-right x 4) sy (bit-shift-right y 4) cz (bit-shift-right z 4)
              k (store/section-key cx sy cz)
              ^js e (or (.get by-section k) (section-of k cx sy cz))
              ^js values (aget e 2)
              c (store/cell-index x y z)
              v (aget values c)]
          (if-not (js/isNaN v)
            v
            (let [^js bs @boxes
                  n (/ (.-length bs) 6)
                  ^js order (aget e 0)
                  ^js u (aget e 1)
                  least (if (zero? (.-length order)) ##Inf
                            (steps (span x (aget u 0) (aget u 3)) (span y (aget u 1) (aget u 4)) (span z (aget u 2) (aget u 5)) mf))
                  v (loop [i 0 best ##Inf]
                      (if (or (>= i (.-length order)) (<= best least) (>= (js/Math.floor (/ (aget order i) n)) best))
                        best
                        (let [b (* 6 (mod (aget order i) n))]
                          (recur (inc i) (js/Math.min best (steps (span x (aget bs b) (aget bs (+ b 3))) (span y (aget bs (+ b 1)) (aget bs (+ b 4)))
                                                          (span z (aget bs (+ b 2)) (aget bs (+ b 5))) mf))))))]
              (aset values c v)
              v)))))))

(defn- section-keys
  "The keys of the sections a box [lo hi] of cells touches."
  [[lx ly lz] [hx hy hz]]
  (for [cx (range (bit-shift-right lx 4) (inc (bit-shift-right hx 4)))
        sy (range (bit-shift-right ly 4) (inc (bit-shift-right hy 4)))
        cz (range (bit-shift-right lz 4) (inc (bit-shift-right hz 4)))]
    (store/section-key cx sy cz)))

(defn mark
  "What the body remembers in the box of cells lo hi now, for opened: the memory's write count and a copy of each section's
  cells. nil without a memory."
  [p lo hi]
  (when-let [st (st-of p)]
    (let [secs (sections st)
          ids (js/Map.)]
      (doseq [k (section-keys lo hi)]
        (when-let [^js s (.get secs k)] (.set ids k (.slice (.-ids s)))))
      #js {:writes (.-writes st) :ids ids})))

(defn- each-cell!
  "(f x y z i) over the 4096 cells of section s."
  [^js s f]
  (let [bx (* 16 (.-cx s)) by (* 16 (.-sy s)) bz (* 16 (.-cz s))]
    (dotimes [i 4096] (f (+ bx (bit-and i 15)) (+ by (bit-shift-right i 8)) (+ bz (bit-and (bit-shift-right i 4) 15)) i))))

(defn- open-fn []
  (let [^js open (rock/open-ids)] (fn [v] (and (pos? v) (== 1 (aget open (dec v)))))))

(defn opened
  "The cells of the box lo hi the body remembers open now but did not at mark (unseen then, or seen solid): only the
  sections written since are read."
  [p ^js mark lo hi]
  (let [st (st-of p)]
    (if (or (nil? mark) (nil? st) (== (.-writes st) (.-writes mark)))
      []
      (let [secs (sections st)
            open? (open-fn)
            [lx ly lz] lo [hx hy hz] hi
            out #js []]
        (doseq [k (section-keys lo hi)
                :let [^js s (.get secs k)]
                :when (and s (> (.-wrote s) (.-writes mark)))]
          (let [^js now (.-ids s) ^js was (.get (.-ids mark) k)]
            (each-cell! s (fn [x y z i]
                            (when (and (open? (aget now i)) (not (and was (open? (aget was i))))
                                       (<= lx x hx) (<= ly y hy) (<= lz z hz))
                              (.push out [x y z]))))))
        (vec out)))))

(defn walk-steps
  "Steps of a straight walk between two cells: the larger of the sideways and the vertical run; what a walk costs at least, times a step's seconds."
  [[ax ay az] [bx by bz]]
  (max (+ (js/Math.abs (- ax bx)) (js/Math.abs (- az bz))) (js/Math.abs (- ay by))))
