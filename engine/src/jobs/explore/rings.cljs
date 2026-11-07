(ns jobs.explore.rings
  "A search's lattice of leg points, nearest a place first, made lazily ring by ring round it: a caller that stops at
  its first fit never builds the whole disc a large worth pays for.")

(defn spiral-key
  "[ring index] of the lattice offset [a b] in the order search/spiral-points walks its rings."
  [[a b]]
  (let [r (max (js/Math.abs a) (js/Math.abs b))]
    [r (cond (and (= b (- r)) (< a r)) (+ a r)
             (and (= a r) (< b r)) (+ (* 3 r) b)
             (and (= b r) (> a (- r))) (- (* 5 r) a)
             :else (- (* 7 r) b))]))

(defn side-span
  "[lo hi] along a lattice line, relative to the body, of the wedge side [axis sign] (search/side-of of a column
  offset): the line has the fixed offset v on fixed (:x or :z)."
  [[axis sign] fixed v]
  (if (= axis fixed)
    (let [d (* sign v)] (when (>= d 0) [(- d) d]))
    (if (pos? sign) [(js/Math.abs v) ##Inf] [##-Inf (- (js/Math.abs v))])))

(defn nearest-points
  "search/spiral-points' [x z] points (spacing apart round the origin [ox oz], the origin left out, within
  max-distance), nearest the cell [x z] first, ties in spiral order; lazy. With side only those in that wedge from
  [x z] (side-span; may hold a few beside it, the caller checks). A ring round [x z] adds only the points its lines
  share with the disc (and wedge), so the work is the rings walked plus the points taken."
  ([origin at spacing max-distance] (nearest-points origin at spacing max-distance nil))
  ([[ox oz] [x z] spacing max-distance side]
   (let [o [(js/Math.floor ox) (js/Math.floor oz)]
         p [(- x (o 0)) (- z (o 1))]
         [ci cj] (mapv #(js/Math.round (/ % spacing)) p)
         ;; [lo hi] offsets from o along the line whose fixed (:x or :z) offset from o is u
         disc (fn [_ u] (when (<= (js/Math.abs u) max-distance)
                          (let [w (js/Math.sqrt (- (* max-distance max-distance) (* u u)))] [(- w) w])))
         wedge (fn [fixed u] (let [[f g] (if (= :x fixed) [0 1] [1 0])]
                               (some->> (side-span side fixed (- u (p f))) (mapv #(+ % (p g))))))
         spans (if side [disc wedge] [disc])
         line (fn [fixed n lo hi]
                (let [[lo hi] (reduce (fn [[lo hi] f]
                                        (if-let [[a b] (f fixed (* n spacing))]
                                          [(max lo (dec (js/Math.ceil (/ a spacing)))) (min hi (inc (js/Math.floor (/ b spacing))))]
                                          (reduced [1 0])))
                                      [lo hi] spans)]
                  (for [m (range lo (inc hi))] (if (= :x fixed) [n m] [m n]))))
         ring (fn [k]
                (if (zero? k)
                  [[ci cj]]
                  (concat (line :z (- cj k) (- ci k) (+ ci k))
                          (line :z (+ cj k) (- ci k) (+ ci k))
                          (line :x (- ci k) (- cj k -1) (+ cj k -1))
                          (line :x (+ ci k) (- cj k -1) (+ cj k -1)))))
         pt (fn [[a b]] [(+ (o 0) (* a spacing)) (+ (o 1) (* b spacing))])
         in? (fn [[a b]] (and (not (and (zero? a) (zero? b))) (<= (js/Math.hypot (* a spacing) (* b spacing)) max-distance)))
         entry (fn [ab] [(into [(js/Math.hypot (- ((pt ab) 0) x) (- ((pt ab) 1) z))] (spiral-key ab)) (pt ab)])
         ;; every point of a ring past k lies more than (k + 1/2) spacings from [x z]; none past last is in the disc
         last-k (inc (js/Math.ceil (/ (+ (js/Math.hypot (p 0) (p 1)) max-distance) spacing)))
         step (fn step [k buf]
                (lazy-seq
                  (loop [k k buf buf]
                    (if (> k last-k)
                      (map second buf)
                      (let [buf (sort-by first (into buf (map entry) (filter in? (ring k))))
                            [now later] (split-with #(< ((first %) 0) (* (+ k 0.5) spacing)) buf)]
                        (if (seq now)
                          (concat (map second now) (step (inc k) (vec later)))
                          (recur (inc k) (vec later))))))))]
     (step 0 []))))
