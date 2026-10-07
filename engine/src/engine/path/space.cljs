(ns engine.path.space
  "Free space for the body centre inside a cell: which of the 17x17 sub-positions (1/16 apart) let the 0.62-wide body
  stand clear of every collision box nearby. Boxes are world-coordinate, 6 floats each: x0 y0 z0 x1 y1 z1. `space` is the
  JS object the planner takes as options.space (boxesNear, freeMask, labelRegions)."
  (:require [engine.path.offsets :as offsets]
            [engine.path.planner.base :as base]))

(def ^:const UNLOADED base/UNLOADED)
(def ^:const HALF-WIDTH 0.31)
(def ^:const GRID 17)
;; the server refuses a move that leaves the body's box exactly touching a face, so touching counts as overlap
(def ^:const EPS 1e-4)
(def ^:private SCRATCH (js/Float32Array. (* 6 512)))

(defn- push!
  "Append a box to the scratch holder h (#js {:out :n}) when it overlaps [lo, hi) vertically; the buffer doubles when full."
  [^js h x0 y0 z0 x1 y1 z1 lo hi]
  (when-not (or (<= y1 lo) (>= y0 hi))
    (let [n (.-n h)]
      (when (> (+ n 6) (.-length ^js (.-out h)))
        (let [bigger (js/Float32Array. (* 2 (.-length ^js (.-out h))))]
          (.set bigger ^js (.-out h))
          (set! (.-out h) bigger)))
      (let [out ^js (.-out h)]
        (aset out n x0)
        (aset out (+ n 1) y0)
        (aset out (+ n 2) z0)
        (aset out (+ n 3) x1)
        (aset out (+ n 4) y1)
        (aset out (+ n 5) z1))
      (set! (.-n h) (+ n 6)))))

(defn boxes-near
  "Collision boxes of the 3x3 cells around (x, z), cell rows y-1 .. y+2, that overlap [lo, hi) vertically, as a Float32Array.
  Unloaded cells are a full box (unknown is solid). Bamboo / dripstone get their per-position offset."
  [snapshot table x y z lo hi]
  (let [box-start ^js (.-boxStart ^js table)
        box-count ^js (.-boxCount ^js table)
        boxes ^js (.-boxes ^js table)
        offset-max ^js (.-offsetMax ^js table)
        block-offset (.-blockOffset ^js (offsets/offsets))
        h #js {:out SCRATCH :n 0}]
    (loop [cy (dec y) cz (dec z) cx (dec x)]
      (cond
        (> cy (+ y 2)) nil
        (> cz (inc z)) (recur (inc cy) (dec z) (dec x))
        (> cx (inc x)) (recur cy (inc cz) (dec x))
        :else
        (let [id (.stateAt ^js snapshot cx cy cz)]
          (if (== id UNLOADED)
            (push! h cx cy cz (inc cx) (inc cy) (inc cz) lo hi)
            (let [cnt (aget box-count id)]
              (when-not (zero? cnt)
                (let [mx (aget offset-max id)
                      off (when (> mx 0) (block-offset cx cz mx))
                      dx (if off (.-dx ^js off) 0)
                      dz (if off (.-dz ^js off) 0)]
                  (loop [b 0 at (* 6 (aget box-start id))]
                    (when (< b cnt)
                      (push! h (+ cx (aget boxes at) dx) (+ cy (aget boxes (+ at 1))) (+ cz (aget boxes (+ at 2)) dz)
                             (+ cx (aget boxes (+ at 3)) dx) (+ cy (aget boxes (+ at 4))) (+ cz (aget boxes (+ at 5)) dz) lo hi)
                      (recur (inc b) (+ at 6))))))))
          (recur cy cz (inc cx)))))
    (.slice ^js (.-out h) 0 (.-n h))))

(defn body-hits?
  "does the body centred at (px, pz) touch or overlap any of the boxes? (vertical overlap was filtered by boxes-near)"
  [boxes px pz]
  (let [boxes ^js boxes
        len (.-length boxes)]
    (loop [i 0]
      (cond
        (>= i len) false
        (and (> (+ px HALF-WIDTH) (- (aget boxes i) EPS)) (< (- px HALF-WIDTH) (+ (aget boxes (+ i 3)) EPS))
             (> (+ pz HALF-WIDTH) (- (aget boxes (+ i 2)) EPS)) (< (- pz HALF-WIDTH) (+ (aget boxes (+ i 5)) EPS))) true
        :else (recur (+ i 6))))))

(defn free-mask
  "1 where the body centred at (x + i/16, z + j/16) hits no box; index j * 17 + i"
  [boxes x z]
  (let [mask (js/Uint8Array. (* GRID GRID))]
    (dotimes [j GRID]
      (dotimes [i GRID]
        (aset mask (+ (* j GRID) i) (if (body-hits? boxes (+ x (/ i 16)) (+ z (/ j 16))) 0 1))))
    mask))

(defn- visit!
  "Push the free, unlabelled position nk on the stack (labelling it); the new stack top."
  [top mask labels stack label nk]
  (if (and (not (zero? (aget ^js mask nk))) (== (aget ^js labels nk) -1))
    (do (aset ^js labels nk label) (aset ^js stack top nk) (inc top))
    top))

(defn label-regions
  "4-connected components of the free positions: #js {labels regs}: labels (region index per position, -1 where blocked) and
  regs, one #js {size px pz} per region: the point nearest the cell centre in 1/16 (ties: lowest j, then i)"
  [mask]
  (let [mask ^js mask
        len (.-length mask)
        labels (.fill (js/Int8Array. len) -1)
        stack (js/Int32Array. len)
        regs #js []]
    (dotimes [start len]
      (when (and (not (zero? (aget mask start))) (== (aget labels start) -1))
        (let [label (.-length regs)]
          (aset stack 0 start)
          (aset labels start label)
          (loop [top 1 size 0 best start best-d js/Infinity]
            (if (zero? top)
              (.push regs #js {:size size :px (rem best GRID) :pz (quot best GRID)})
              (let [top (dec top)
                    k (aget stack top)
                    i (rem k GRID)
                    j (quot k GRID)
                    d (+ (* (- i 8) (- i 8)) (* (- j 8) (- j 8)))
                    bj (quot best GRID)
                    better? (or (< d best-d) (and (== d best-d) (or (< j bj) (and (== j bj) (< i (rem best GRID))))))
                    top (cond-> top
                          (> i 0) (visit! mask labels stack label (dec k))
                          (< i (dec GRID)) (visit! mask labels stack label (inc k))
                          (> j 0) (visit! mask labels stack label (- k GRID))
                          (< j (dec GRID)) (visit! mask labels stack label (+ k GRID)))]
                (recur top (inc size) (if better? k best) (if better? d best-d))))))))
    #js {:labels labels :regs regs}))

(defn regions
  "label-regions' regs: one #js {size px pz} per 4-connected component of the mask"
  [mask]
  (.-regs ^js (label-regions mask)))

(defn segment-free?
  "can the body centre move in a straight line from -> to ({x, z} world) at height [lo, hi)? Sampled every 1/16 block."
  [snapshot table from to lo hi]
  (let [y (js/Math.floor lo)
        cache (js/Map.)
        near (fn [cx cz]
               (let [k (+ (* cx 65536) cz)] ;; cells within +-32k of origin; a collision only costs a wrong cache hit far away
                 (or (.get cache k)
                     (let [boxes (boxes-near snapshot table cx y cz lo hi)] (.set cache k boxes) boxes))))
        fx (.-x ^js from) fz (.-z ^js from) tx (.-x ^js to) tz (.-z ^js to)
        n (max 1 (js/Math.ceil (* (js/Math.hypot (- tx fx) (- tz fz)) 16)))]
    (loop [s 0]
      (if (> s n)
        true
        (let [px (+ fx (/ (* (- tx fx) s) n))
              pz (+ fz (/ (* (- tz fz) s) n))]
          (if (body-hits? (near (js/Math.floor px) (js/Math.floor pz)) px pz)
            false
            (recur (inc s))))))))

(def space
  "The module the planner takes as options.space."
  #js {:boxesNear boxes-near :freeMask free-mask :labelRegions label-regions :regions regions :segmentFree segment-free?
       :bodyHits body-hits? :HALF_WIDTH HALF-WIDTH :GRID GRID})
