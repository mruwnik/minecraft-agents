(ns engine.path.planner.tight
  "Search methods: tight cells, where the body fits in only part of a cell: free-space masks, regions and the moves
   between them."
  (:require [engine.path.planner.base :refer [BENT-COST BODY-BLOCKS CENTRE DROP-INSET GRID MOVE-DROP NO-MASKS REGIONS TABLE TIGHT-S UNLOADED]]
            [engine.path.planner.search :refer [Search]]))

(set! *warn-on-infer* true)

(extend-type Search
  Object

  ;; ---- tight cells: where in the cell the body fits ----
  (partialInColumn [s x y z]
    (loop [cy (dec y)]
      (if (> cy (+ y 2))
        0
        (let [id (.stateAt s x cy z)]
          (if (and (not (== id UNLOADED)) (not (zero? (aget (.-tbl-partial s) id))))
            1
            (recur (inc cy)))))))

  ;; partial collision in rows y-1..y+2 of the column, cached (the opening pass sees other blocks: its own cache entries)
  (columnPartial [s x y z]
    (let [key (* (inc (.keyOf s x y z 0)) (if ^boolean (.-open-mode s) -1 1))
          slot (bit-and (.hashOf s x y z 0) (dec TABLE))]
      (if (== (aget (.-column-keys s) slot) key)
        (aget (.-column-flags s) slot)
        (let [flag (.partialInColumn s x y z)]
          (aset (.-column-keys s) slot key)
          (aset (.-column-flags s) slot flag)
          flag))))

  (partialAround [s x y z]
    (loop [cz (dec z)
           cx (dec x)]
      (cond
        (> cz (inc z)) 0
        (> cx (inc x)) (recur (inc cz) (dec x))
        (== (.columnPartial s cx y cz) 1) 1
        :else (recur cz (inc cx)))))

  ;; no section the block of cells x +-r, z +-r, rows y-below..y+above touches holds a partial block or a climbable (or a
  ;; magma column, or something openable): the usual case, answered without reading cells
  (sectionsClear [s x y z r below above]
    (let [sy1 (bit-shift-right (- (+ y above) (.-min-y s)) 4)
          sz0 (bit-shift-right (- z r) 4)
          sz1 (bit-shift-right (+ z r) 4)
          sx0 (bit-shift-right (- x r) 4)
          sx1 (bit-shift-right (+ x r) 4)]
      (loop [sy (bit-shift-right (- (- y below) (.-min-y s)) 4)
             sz sz0
             sx sx0]
        (cond
          (> sy sy1) true
          (> sz sz1) (recur (inc sy) sz0 sx0)
          (> sx sx1) (recur sy (inc sz) sx0)
          ;; (a ^boolean hint on a call into JS would keep its name out of the inferred externs)
          (true? (.sectionHas ^js (.-snapshot s) (.-tbl-special s) sx sy sz)) false
          :else (recur sy sz (inc sx))))))

  ;; a partial block within one cell of the body, rows y-1..y+2: the cell is one node per free region
  (isTight [s x y z]
    (if ^boolean (.sectionsClear s x y z 1 1 2)
      false
      (let [key (* (inc (.keyOf s x y z 0)) (if ^boolean (.-open-mode s) -1 1))
            slot (bit-and (.hashOf s x y z 0) (dec TABLE))]
        (if (== (aget (.-cell-keys s) slot) key)
          (== (aget (.-cell-flags s) slot) 1)
          (let [flag (.partialAround s x y z)]
            (aset (.-cell-keys s) slot key)
            (aset (.-cell-flags s) slot flag)
            (== flag 1))))))

  (tightAt [s x y z]
    (and (not ^boolean (.-quiet s)) ^boolean (.isTight s x y z)))

  (makeShape [s x y z lo16 key]
    (let [t (js/performance.now)
          lo (/ lo16 16)
          mask (.freeMask ^js (.-space s) (.boxesNear ^js (.-space s) ^js (.-view s) ^js (.-table s) x y z lo (+ lo BODY-BLOCKS)) x z)
          ^js labelled (.labelRegions ^js (.-space s) mask)
          labels (.-labels labelled)
          ^js regs (.-regs labelled)
          shape #js {:mask mask :labels labels :regs regs :centre (aget labels (+ (* 8 GRID) 8))}]
      (.set ^js (.-mask-cache s) key shape)
      (set! (.-mask-ms s) (+ (.-mask-ms s) (- (js/performance.now) t)))
      (set! (.-masks s) (inc (.-masks s)))
      (when ^boolean (.isTight s x y z)
        (set! (.-tight-masks s) (inc (.-tight-masks s)))
        (set! (.-regions-seen s) (+ (.-regions-seen s) (.-length regs)))
        (.add ^js (.-tight-seen s) (.keyOf s x y z 0)))
      shape))

  ;; free-position mask of the cell for a body standing at lo16 (absolute 1/16), its labelled regions, and the region of
  ;; the cell centre. One per (cell, height) per search.
  (shapeOf [s x y z lo16]
    (let [key (* (+ (* (.keyOf s x y z 0) 128) (+ (- lo16 (* y 16)) 32)) (if ^boolean (.-open-mode s) -1 1))
          cached (.get ^js (.-mask-cache s) key)]
      (if (undefined? cached)
        (.makeShape s x y z lo16 key)
        cached)))

  (nearestRegion [s labels p snap]
    (let [pi (js-mod p GRID)
          pj (/ (- p pi) GRID)
          i0 (js/Math.max 0 (- pi snap))
          i1 (js/Math.min (dec GRID) (+ pi snap))
          j1 (js/Math.min (dec GRID) (+ pj snap))]
      (loop [j (js/Math.max 0 (- pj snap))
             i i0
             best -1
             best-dist js/Infinity]
        (cond
          (> j j1) best
          (> i i1) (recur (inc j) i0 best best-dist)
          :else
          (let [label (aget labels (+ (* j GRID) i))
                d (+ (* (- i pi) (- i pi)) (* (- j pj) (- j pj)))]
            (if (and (>= label 0) (< d best-dist))
              (recur j (inc i) label d)
              (recur j (inc i) best best-dist)))))))

  ;; The region of a boundary point in a cell's own mask. Where the two cells' heights differ the point can be blocked only
  ;; by the step itself (the body crosses at the higher level, then settles), so a blocked point takes the region of the
  ;; nearest free position within snap/16: a whole cell (GRID) for a body leaving a climbable by a rise, or falling onto one.
  (regionNear [s ^js shape p snap]
    (let [labels (.-labels shape)
          label (aget labels p)]
      (if (>= label 0)
        label
        (.nearestRegion s labels p snap))))

  ;; index in the 17x17 mask of boundary point t (0..16 along the shared edge) for the cell moved from (A) and to (B),
  ;; for the cardinal c: 0 east, 1 west, 2 south (+z), 3 north
  (indexA [s c t]
    (cond
      (== c 0) (+ (* t GRID) 16)
      (== c 1) (* t GRID)
      (== c 2) (+ (* 16 GRID) t)
      :else t))

  (indexB [s c t]
    (cond
      (== c 0) (* t GRID)
      (== c 1) (+ (* t GRID) 16)
      (== c 2) t
      :else (+ (* 16 GRID) t)))

  ;; index in B's mask of boundary point t moved d/16 into B, away from the shared edge
  (insetB [s c t d]
    (cond
      (== c 0) (+ (* t GRID) d)
      (== c 1) (+ (* t GRID) (- 16 d))
      (== c 2) (+ (* d GRID) t)
      :else (+ (* (- 16 d) GRID) t)))

  ;; What a node knows beyond its cell, in one Uint32 (0 for an ordinary cell reached from an ordinary one): bits 0-3
  ;; region, bit 4 set for a tight cell with its representative point px (5-9) and pz (10-14) in 1/16, bit 15 set with the
  ;; crossing point the move in came by, relative to the cell in 1/16: x (16-20), z (21-25).
  (packShape [s region ^boolean tight px pz ^boolean crossed cross-x cross-z]
    (bit-or region
            (if tight (bit-or 16 (bit-shift-left px 5) (bit-shift-left pz 10)) 0)
            (if crossed (bit-or (bit-shift-left 1 15) (bit-shift-left cross-x 16) (bit-shift-left cross-z 21)) 0)))

  ;; does a mask of `masks` leave boundary point p blocked?
  (fallBlocked [s ^js masks p]
    (loop [k 0]
      (if (< k (.-length masks))
        (if (zero? (aget (aget masks k) p))
          true
          (recur (inc k)))
        false)))

  ;; a drop falls straight down the neighbour column: every tight cell between must be free at the crossing point too
  (fallMasks [s x2 y y2 z2]
    (let [out #js []]
      (loop [k (inc y2)]
        (when (< k y)
          (when ^boolean (.isTight s x2 k z2)
            (.push out (.-mask ^js (.shapeOf s x2 k z2 (* k 16)))))
          (recur (inc k))))
      out))

  ;; The walker goes in a straight line from a cell's representative point to the crossing point of a move, and from
  ;; a crossing point to the representative point it leads to. So each such line must be free. Example: a fence post
  ;; beside a gap leaves its cell a U-shaped region. A crossing on the strip without the representative point lies
  ;; behind the post, and the body would walk into it and stick. A ring of free space round a bamboo stalk is the same.
  ;; Is every mask point the segment (ai, aj) - (bi, bj) passes (one per 1/16 along its longer axis, rounded) free?
  ;; If an end is itself blocked (a boundary point snapped to the nearest region, at a step or a climbable), the leg
  ;; is not judged.
  (lineFree [s ^js mask ai aj bi bj]
    (let [di (- bi ai)
          dj (- bj aj)
          n (js/Math.max (js/Math.abs di) (js/Math.abs dj))]
      (if (or (zero? (aget mask (+ (* aj GRID) ai))) (zero? (aget mask (+ (* bj GRID) bi))))
        true
        (loop [k 1]
          (cond
            (>= k n) true
            (not (zero? (aget mask (+ (* (js/Math.round (+ aj (/ (* dj k) n))) GRID) (js/Math.round (+ ai (/ (* di k) n)))))))
            (recur (inc k))
            :else false)))))

  ;; pick[rb]: for each region of B a boundary point free for both cells leads to from region `label` of A, the point
  ;; nearest the line between the two representative points; -1 where none does. A drop (inset DROP-INSET, else 0)
  ;; falls from the point `inset` into B, where the body has cleared the ledge it walked off: it must pass there at the
  ;; joint height, and the fall and the landing are judged there. The legs either side of the crossing must be straight
  ;; and free in their cell (lineFree) where one is: a bent leg (counted in pick's bits above 4) is taken only for want of a
  ;; straight one, and the path then carries the bends (bendsIn).
  (crossings [s c label ^js rep-a ^boolean tight-b ^js own-a ^js own-b ^js joint-a ^js joint-b snap-a snap-b ^js falls inset]
    (let [mask-a (.-mask joint-a)
          mask-b (.-mask joint-b)]
      (.fill ^js (.-pick s) -1)
      (loop [t 0]
        (when (<= t 16)
          (let [pa (.indexA s c t)
                pb (.indexB s c t)
                pf (if (zero? inset) pb (.insetB s c t inset))]
            (when (and (not (zero? (aget mask-a pa))) (not (zero? (aget mask-b pb))) (not (zero? (aget mask-b pf)))
                       (== (.regionNear s own-a pa snap-a) label)
                       (not ^boolean (.fallBlocked s falls pf)))
              (let [lb (.regionNear s own-b pf snap-b)
                    rb (if tight-b lb (if (== lb (.-centre own-b)) 0 -1))]
                (when (and (>= rb 0) (< rb REGIONS))
                  (let [^js rep-b (if tight-b (aget (.-regs own-b) rb) CENTRE)
                        along (if (< c 2) (/ (+ (.-pz rep-a) (.-pz rep-b)) 2) (/ (+ (.-px rep-a) (.-px rep-b)) 2))
                        picked (aget ^js (.-pick s) rb)
                        bent (+ (if ^boolean (.lineFree s (.-mask own-a) (.-px rep-a) (.-pz rep-a) (js-mod pa GRID) (js/Math.floor (/ pa GRID))) 0 1)
                                (if ^boolean (.lineFree s (.-mask own-b) (js-mod pf GRID) (js/Math.floor (/ pf GRID)) (.-px rep-b) (.-pz rep-b)) 0 1))]
                    (when (or (== picked -1)
                              (< (+ (js/Math.abs (- t along)) (* BENT-COST bent))
                                 (+ (js/Math.abs (- (bit-and picked 31) along)) (* BENT-COST (bit-shift-right picked 5)))))
                      (aset ^js (.-pick s) rb (bit-or t (bit-shift-left bent 5)))))))))
          (recur (inc t))))))

  ;; one edge per picked region of B
  (tightEdges [s i c x2 y2 z2 h1 move sec drisk slow-to ^boolean tight-b ^js own-b]
    (loop [rb 0]
      (when (< rb REGIONS)
        (let [picked (aget ^js (.-pick s) rb)
              t (bit-and picked 31)]
          (when (>= picked 0)
            (let [^js rep (if tight-b (aget (.-regs own-b) rb) CENTRE)
                  ;; the crossing, relative to B: on its west edge (0) for an eastward move, its east edge (16) for a
                  ;; westward one...
                  cross-x (cond (== c 0) 0 (== c 1) 16 :else t)
                  cross-z (cond (== c 2) 0 (== c 3) 16 :else t)]
              (.edge s x2 y2 z2 h1 move i sec drisk slow-to 0
                     (.packShape s rb tight-b (.-px rep) (.-pz rep) true cross-x cross-z)))))
        (recur (inc rb)))))

  ;; the cardinal move c from cell A (region `region`, or every region when -1) to cell B, either of them tight: one edge
  ;; per region of B that a boundary point free for both leads to. Costs are those of the plain move, plus TIGHT_S into a
  ;; tight cell.
  (tightMove [s i x y z h region c x2 y2 z2 h1 move dsec drisk slow-to snap-a snap-b]
    (let [lo-a (+ (* y 16) h)
          lo-b (+ (* y2 16) h1)
          joint (js/Math.max lo-a lo-b) ; the body straddles the boundary at the higher of the two heights
          tight-a ^boolean (.isTight s x y z)
          tight-b ^boolean (.isTight s x2 y2 z2)
          ^js own-a (.shapeOf s x y z lo-a)
          ^js own-b (.shapeOf s x2 y2 z2 lo-b)
          ^js joint-a (if (== lo-a joint) own-a (.shapeOf s x (js/Math.max y y2) z joint))
          ^js joint-b (if (== lo-b joint) own-b (.shapeOf s x2 (js/Math.max y y2) z2 joint))
          ^js falls (if (== move MOVE-DROP) (.fallMasks s x2 y y2 z2) NO-MASKS)
          ^js regs-a (.-regs own-a)
          last-region (cond (>= region 0) region tight-a (dec (.-length regs-a)) :else 0)
          sec (+ dsec (if tight-b TIGHT-S 0))]
      (loop [ra (if (neg? region) 0 region)]
        (when (and (<= ra last-region) (< ra REGIONS))
          (let [label (if tight-a ra (.-centre own-a))]
            (when (>= label 0)
              (.crossings s c label (if tight-a (aget regs-a ra) CENTRE) tight-b own-a own-b joint-a joint-b snap-a snap-b falls
                          (if (== move MOVE-DROP) DROP-INSET 0))
              (.tightEdges s i c x2 y2 z2 h1 move sec drisk slow-to tight-b own-b)))
          (recur (inc ra))))))

  ;; the edges of a vertical move from region ra of A: one per region of B a position fitting at both ends and every cell
  ;; between joins, the position nearest the middle of the two regions' points being the crossing
  (verticalPick [s ra label-a tight-a tight-b ^js own-a ^js own-b ^js between]
    (let [^js rep-a (if ^boolean tight-a (aget (.-regs own-a) ra) CENTRE)
          mask-a (.-mask own-a)
          labels-a (.-labels own-a)
          mask-b (.-mask own-b)
          labels-b (.-labels own-b)
          centre-b (.-centre own-b)]
      (.fill ^js (.-best-d s) js/Infinity)
      (loop [p 0]
        (when (< p (* GRID GRID))
          (when (and (not (zero? (aget mask-a p))) (== (aget labels-a p) label-a) (not (zero? (aget mask-b p)))
                     (not ^boolean (.fallBlocked s between p)))
            (let [lb (aget labels-b p)
                  rb (if ^boolean tight-b lb (if (== lb centre-b) 0 -1))]
              (when (and (>= rb 0) (< rb REGIONS))
                (let [^js rep-b (if ^boolean tight-b (aget (.-regs own-b) rb) CENTRE)
                      pi (js-mod p GRID)
                      pj (/ (- p pi) GRID)
                      dx (- pi (/ (+ (.-px rep-a) (.-px rep-b)) 2))
                      dz (- pj (/ (+ (.-pz rep-a) (.-pz rep-b)) 2))
                      d (+ (* dx dx) (* dz dz))]
                  (when (< d (aget ^js (.-best-d s) rb))
                    (aset ^js (.-best-d s) rb d)
                    (aset ^js (.-best-at s) rb p))))))
          (recur (inc p))))))

  (verticalEdges [s i x y2 z h2 move dsec drisk slow-to tight-b ^js own-b]
    (loop [rb 0]
      (when (< rb REGIONS)
        (when-not (== (aget ^js (.-best-d s) rb) js/Infinity)
          (let [^js rep (if ^boolean tight-b (aget (.-regs own-b) rb) CENTRE)
                p (aget ^js (.-best-at s) rb)
                pi (js-mod p GRID)]
            (.edge s x y2 z h2 move i dsec drisk slow-to 0
                   (.packShape s rb tight-b (.-px rep) (.-pz rep) true pi (/ (- p pi) GRID)))))
        (recur (inc rb)))))

  ;; the cells between two of one column, as masks for a body standing at each cell's floor
  (betweenMasks [s x y y2 z]
    (let [out #js []]
      (loop [k (inc (js/Math.min y y2))]
        (when (< k (js/Math.max y y2))
          (.push out (.-mask ^js (.shapeOf s x k z (* k 16))))
          (recur (inc k))))
      out))

  ;; A vertical move in one column from cell y (stand height h, region `region` or every one when -1) to cell y2: climbing, a
  ;; jump into a ladder, a fall. The body stays at one position (x + i/16, z + j/16) all the way, so it must fit there at the
  ;; start, the end and every cell between.
  (verticalMove [s i x y z h region y2 h2 move dsec drisk slow-to]
    (let [tight-a ^boolean (.isTight s x y z)
          tight-b ^boolean (.isTight s x y2 z)]
      (if (and (not tight-a) (not tight-b))
        (.edge s x y2 z h2 move i dsec drisk slow-to 0 0)
        (let [^js own-a (.shapeOf s x y z (+ (* y 16) h))
              ^js own-b (.shapeOf s x y2 z (+ (* y2 16) h2))
              between (.betweenMasks s x y y2 z)
              last-region (cond (>= region 0) region tight-a (dec (.-length (.-regs own-a))) :else 0)]
          (loop [ra (if (neg? region) 0 region)]
            (when (and (<= ra last-region) (< ra REGIONS))
              (let [label-a (if tight-a ra (.-centre own-a))]
                (when (>= label-a 0)
                  (.verticalPick s ra label-a tight-a tight-b own-a own-b between)
                  (.verticalEdges s i x y2 z h2 move dsec drisk slow-to tight-b own-b)))
              (recur (inc ra)))))))))
