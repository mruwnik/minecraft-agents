(ns engine.path.planner.current
  "Flowing water's push on a body, as prismarine-physics (the body's client physics) has it: each water cell the body's
  box touches pulls it along that cell's flow (getFlow), the sum normalised, at 0.014 blocks a tick, while the body's own
  input in water adds liquidAcceleration 0.02 times the 0.98 forward input. The planner prices moves through a current
  with it (crossing-factor); the walker steers against it (stroke).")

(def PUSH
  "A current's pull as a share of the body's own stroke in water: 0.014 / (0.02 x 0.98). The same share when swimming:
  both are scaled by the one inertia."
  (/ 0.014 (* 0.02 0.98)))

(def ^:private DX #js [0 -1 0 1])
(def ^:private DZ #js [1 0 -1 0])

(defn- water-ids [^js table] (.get (.-nameIds table) "water"))

(defn depth
  "prismarine-physics' rendered depth of state id: -1 not water; 0 a source, falling water or a waterlogged block;
  else the water's level (1-7, lower water the higher). An id past the table (unloaded) is not water."
  [^js table id]
  (if (not (pos? (aget (.-wet table) id)))
    -1
    (let [^js r (water-ids table)
          level (- id (aget r 0))]
      (if (and (<= 0 level) (<= id (aget r 1)) (< level 8)) level 0))))

(defn- solid? [^js table id] (pos? (aget (.-top table) id)))

(defn flow
  "The unit flow #js [fx fy fz] of the water cell at x y z (prismarine-physics getFlow over state-at, a fn of x y z to a
  state id), nil where the cell is not water or has no flow. Falling water beside a block pulls mostly down."
  [^js table state-at x y z]
  (let [id (state-at x y z)
        cur (depth table id)]
    (if (neg? cur)
      nil
      (let [^js r (water-ids table)
            ^boolean falling (and (<= (+ (aget r 0) 8) id) (<= id (aget r 1)))
            v #js [0 0 0]]
        (dotimes [k 4]
          (let [dx (aget DX k) dz (aget DZ k)
                adj (state-at (+ x dx) y (+ z dz))
                ad (depth table adj)
                below (if (and (neg? ad) ^boolean (solid? table adj)) (depth table (state-at (+ x dx) (dec y) (+ z dz))) -1)
                f (cond (>= ad 0) (- ad cur) (>= below 0) (- below (- cur 8)) :else 0)]
            (aset v 0 (+ (aget v 0) (* dx f)))
            (aset v 2 (+ (aget v 2) (* dz f)))))
        (when falling
          (dotimes [k 4]
            (let [dx (aget DX k) dz (aget DZ k)]
              (when (or ^boolean (solid? table (state-at (+ x dx) y (+ z dz))) ^boolean (solid? table (state-at (+ x dx) (inc y) (+ z dz))))
                (let [n (js/Math.hypot (aget v 0) (aget v 1) (aget v 2))]
                  (when (pos? n) (dotimes [j 3] (aset v j (/ (aget v j) n))))
                  (aset v 1 (- (aget v 1) 6)))))))
        (let [n (js/Math.hypot (aget v 0) (aget v 1) (aget v 2))]
          (if (zero? n)
            nil
            (do (dotimes [j 3] (aset v j (/ (aget v j) n))) v)))))))

(defn crossing-factor
  "Time a move along the unit horizontal (ux uz) takes through a pull of (px pz) (stroke shares, PUSH times the flow),
  over the time it takes in still water: the stroke cancels the pull across the way and the rest moves the body along
  it. Infinity when the body cannot hold its way or make headway."
  [px pz ux uz]
  (let [along (+ (* px ux) (* pz uz))
        across (- (* px uz) (* pz ux))
        hold (- 1 (* across across))
        k (if (pos? hold) (+ along (js/Math.sqrt hold)) 0)]
    (if (pos? k) (/ 1 k) js/Infinity)))

(defn stroke
  "The unit heading #js [ux uz] the body strokes along to move along the unit horizontal (dx dz) through a pull of
  (px pz): the across part cancelled, the rest along the way; straight against the across pull when it is the stronger."
  [px pz dx dz]
  (let [across (- (* px dz) (* pz dx))
        nx dz nz (- dx)]
    (if (>= (js/Math.abs across) 1)
      (let [s (js/Math.sign across)] #js [(- (* s nx)) (- (* s nz))])
      (let [along (js/Math.sqrt (- 1 (* across across)))]
        #js [(- (* along dx) (* across nx)) (- (* along dz) (* across nz))]))))
