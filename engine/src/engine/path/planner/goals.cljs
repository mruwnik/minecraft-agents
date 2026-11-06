(ns engine.path.planner.goals
  "A goal set (query.goals) indexed by place, so the search's per-node goal test and least-distance questions do not scan
   every goal. The least distances come from a grid of x-z buckets visited nearest first, each skipped when a bound on
   its goals cannot beat the least found; the goal test looks up the columns within reach of the cell. Both give exactly
   what a scan of every goal gives."
  (:require [engine.path.planner.base :refer [SQRT2]]))

(set! *warn-on-infer* true)

(defn octile [a b]
  (let [a (js/Math.abs a)
        b (js/Math.abs b)]
    (+ (js/Math.max a b) (* (- SQRT2 1) (js/Math.min a b)))))

(defn- tilt [h dy] (+ (js/Math.max h dy) (* (- SQRT2 1) (js/Math.min h dy))))

(defn- column-key [x z] (+ (* (+ x 1048576) 2097152) (+ z 1048576)))

;; xs ys zs r2 slack near: each goal's cell, squared range, octile slack, 1 for a sphere (near) or 0 for an x-z disc.
;; The grid: buckets of b by b columns from x0 z0, nbx by nbz; the goals of bucket k are order[starts[k] .. starts[k+1]),
;; bslack[k] the most slack among them, max-slack the most of all. Reach groups: goals of one whole range r (columns within
;; r of the goal) as #js [r map-or-nil indices], the map {column-key [goal indices ascending]} when looking up the
;; (2r+1)^2 columns round a cell costs less than scanning the group.
(deftype GoalIndex [n ^js xs ^js ys ^js zs ^js r2 ^js slack ^js near
                    x0 z0 b nbx nbz ^js starts ^js order ^js bslack max-slack ^js groups])

(defn- reach-groups [n ^js r2]
  (let [by-r (js/Map.)]
    (dotimes [i n]
      (let [r (js/Math.ceil (js/Math.sqrt (aget r2 i)))]
        (when-not ^boolean (.has by-r r) (.set by-r r #js []))
        (.push ^js (.get by-r r) i)))
    (.map (js/Array.from (.entries by-r))
          (fn [^js e]
            (let [r (aget e 0)
                  ^js is (aget e 1)]
              #js [r (when (< (* (inc (* 2 r)) (inc (* 2 r))) (.-length is)) is) is])))))

(defn index
  "The index of goal arrays as the Search keeps them (n goals)."
  ^GoalIndex [n ^js xs ^js ys ^js zs ^js r2 ^js slack ^js near]
  (let [x0 (apply min (array-seq xs)) x1 (apply max (array-seq xs))
        z0 (apply min (array-seq zs)) z1 (apply max (array-seq zs))
        ;; about sqrt(n) buckets of about sqrt(n) goals: a far query checks every bucket's bound, a near one scans a few
        b (js/Math.max 1 (js/Math.ceil (js/Math.sqrt (/ (* (inc (- x1 x0)) (inc (- z1 z0))) (js/Math.sqrt n)))))
        nbx (inc (quot (- x1 x0) b))
        nbz (inc (quot (- z1 z0) b))
        nb (* nbx nbz)
        bucket (fn [i] (+ (* (quot (- (aget xs i) x0) b) nbz) (quot (- (aget zs i) z0) b)))
        starts (js/Int32Array. (inc nb))
        order (js/Int32Array. n)
        bslack (js/Float64Array. nb)
        fill (js/Int32Array. nb)]
    (dotimes [i n] (let [k (bucket i)] (aset starts (inc k) (inc (aget starts (inc k))))))
    (dotimes [k nb] (aset starts (inc k) (+ (aget starts (inc k)) (aget starts k))))
    (dotimes [i n]
      (let [k (bucket i)]
        (aset order (+ (aget starts k) (aget fill k)) i)
        (aset fill k (inc (aget fill k)))
        (aset bslack k (js/Math.max (aget bslack k) (aget slack i)))))
    (let [^js groups (reach-groups n r2)]
      (doseq [^js g groups]
        (when (some? (aget g 1))
          (let [^js is (aget g 1)
                m (js/Map.)]
            (doseq [i (array-seq is)]
              (let [k (column-key (aget xs i) (aget zs i))]
                (when-not ^boolean (.has m k) (.set m k #js []))
                (.push ^js (.get m k) i)))
            (aset g 1 m))))
      (GoalIndex. n xs ys zs r2 slack near x0 z0 b nbx nbz starts order bslack (apply max 0 (array-seq bslack)) groups))))

(defn- holds? [^GoalIndex g i x y z]
  (let [dx (- x (aget (.-xs g) i))
        dy (if (== 1 (aget (.-near g) i)) (- y (aget (.-ys g) i)) 0)
        dz (- z (aget (.-zs g) i))]
    (<= (+ (* dx dx) (* dy dy) (* dz dz)) (aget (.-r2 g) i))))

(defn- first-holding
  "The first goal of the ascending list below best whose area holds the cell, else best."
  [^GoalIndex g ^js is x y z best]
  (loop [j 0]
    (if (>= j (.-length is))
      best
      (let [i (aget is j)]
        (cond
          (>= i best) best
          ^boolean (holds? g i x y z) i
          :else (recur (inc j)))))))

(defn goal-at
  "The least index of a goal whose area holds the cell, -1 for none."
  [^GoalIndex g x y z]
  (let [^js groups (.-groups g)
        n (.-n g)
        best (loop [gi 0 best n]
               (if (>= gi (.-length groups))
                 best
                 (let [^js grp (aget groups gi)
                       r (aget grp 0)
                       ^js m (aget grp 1)
                       span (inc (* 2 r))]
                   (recur (inc gi)
                          (if (nil? m)
                            (first-holding g (aget grp 2) x y z best)
                            (loop [c 0 best best]
                              (if (>= c (* span span))
                                best
                                (let [^js col (.get m (column-key (+ x (- (quot c span) r)) (+ z (- (rem c span) r))))]
                                  (recur (inc c) (if (nil? col) best (first-holding g col x y z best)))))))))))]
    (if (== best n) -1 best)))

(defn- goal-value
  "mode 0: the goal's heuristic distance (octile less its slack, at least 0); 1: octile; 2: progress (a sphere goal's
  height gap past its range tilted in)."
  [^GoalIndex g mode i x y z]
  (let [h (octile (- x (aget (.-xs g) i)) (- z (aget (.-zs g) i)))]
    (cond
      (== mode 0) (js/Math.max 0 (- h (aget (.-slack g) i)))
      (or (== mode 1) (zero? (aget (.-near g) i))) h
      :else (tilt h (js/Math.max 0 (- (js/Math.abs (- y (aget (.-ys g) i))) (js/Math.sqrt (aget (.-r2 g) i))))))))

(defn- box-gap
  "how far v lies outside [lo, hi]"
  [v lo hi]
  (js/Math.max 0 (- lo v) (- v hi)))

(defn- scan-bucket
  "The least of best and the goal-values of bucket bx bz's goals; the bucket is skipped when its box, less its most slack
  (mode 0), is no nearer than best."
  [^GoalIndex g mode bx bz x y z best]
  (let [b (.-b g)
        k (+ (* bx (.-nbz g)) bz)
        lo-x (+ (.-x0 g) (* bx b))
        lo-z (+ (.-z0 g) (* bz b))
        bound (- (octile (box-gap x lo-x (+ lo-x b -1)) (box-gap z lo-z (+ lo-z b -1)))
                 (if (== mode 0) (aget (.-bslack g) k) 0))]
    (if (>= bound best)
      best
      (let [^js order (.-order g)
            end (aget (.-starts g) (inc k))]
        (loop [j (aget (.-starts g) k) best best]
          (if (< j end)
            (recur (inc j) (js/Math.min best (goal-value g mode (aget order j) x y z)))
            best))))))

(defn least
  "The least goal-value of mode over the goals, as a scan of every goal gives it. Every value is at least the octile
  distance to its goal less (mode 0) its slack, so a ring of buckets k from the cell's (clamped to the grid) is at least
  (k-1) buckets away, and no nearer than the grid's box."
  [^GoalIndex g mode x y z]
  (let [b (.-b g) x0 (.-x0 g) z0 (.-z0 g) nbx (.-nbx g) nbz (.-nbz g)
        slack (if (== mode 0) (.-max-slack g) 0)
        qx (js/Math.min (dec nbx) (js/Math.max 0 (js/Math.floor (/ (- x x0) b))))
        qz (js/Math.min (dec nbz) (js/Math.max 0 (js/Math.floor (/ (- z z0) b))))
        out (js/Math.max (box-gap x x0 (+ x0 (* nbx b) -1)) (box-gap z z0 (+ z0 (* nbz b) -1)))
        last-k (js/Math.max qx (- nbx 1 qx) qz (- nbz 1 qz))]
    (loop [k 0 best js/Infinity]
      (if (or (> k last-k) (<= best 0) (>= (- (js/Math.max out (* (dec k) b)) slack) best))
        best
        (recur (inc k)
               (loop [bz (js/Math.max 0 (- qz k)) best best]
                 (if (> bz (js/Math.min (dec nbz) (+ qz k)))
                   best
                   (recur (inc bz)
                          (if (== k (js/Math.abs (- bz qz)))
                            (loop [bx (js/Math.max 0 (- qx k)) best best]
                              (if (> bx (js/Math.min (dec nbx) (+ qx k)))
                                best
                                (recur (inc bx) (scan-bucket g mode bx bz x y z best))))
                            (let [best (if (>= (- qx k) 0) (scan-bucket g mode (- qx k) bz x y z best) best)]
                              (if (< (+ qx k) nbx) (scan-bucket g mode (+ qx k) bz x y z best) best)))))))))))
