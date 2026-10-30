(ns jobs.lib.seen-sections
  "The body's memory a section (16 cells a side) at a time: the fewest steps from a cell to any cell it remembers and the
  frontier it could walk to nearest a goal."
  (:require [engine.perception.store :as store]
            [jobs.lib.walk.rock :as rock]))

(defn- st-of [p] (let [^js st (some-> (aget p "perception") :st)] (when (some-> st .-stores) st)))

(defn- sections [^js st] (store/store-of st (.-dim st)))

(defn- gap
  "Cells between a and the 16 from lo, 0 inside."
  [a lo]
  (max 0 (- lo a) (- a (+ lo 15))))

(defn- steps
  "The fewest steps (one cell across, one up or max-fall down each) between cells dx dy dz apart, either way."
  [dx dy dz mf]
  (max (+ dx dz) (js/Math.ceil (/ dy mf))))

(defn steps-fn
  "A fn [x y z] of the fewest steps (each at most one cell across, one up or max-fall down) between the cell and any cell
  the body remembers, by the sections that hold them (##Inf with none): within one of a neighbour cell's, so a search may
  price on it. nil without a memory. The sections are the memory's when first asked."
  [p max-fall]
  (when-let [st (st-of p)]
    (let [mf (max 1 max-fall)
          boxes (delay (let [a #js []] (.forEach (sections st) (fn [^js s] (.push a #js [(* 16 (.-cx s)) (* 16 (.-sy s)) (* 16 (.-cz s))]))) a))
          box-gap (fn [o lo] (max 0 (- lo (+ o 15)) (- o (+ lo 15))))
          ;; by a cell's section, the boxes nearest it first, each with the fewest steps from the section to it: a cell's
          ;; nearest box is one before the first whose steps from the section pass the best found
          orders (js/Map.)]
      (fn [x y z]
        (let [cx (bit-shift-right x 4) sy (bit-shift-right y 4) cz (bit-shift-right z 4)
              k (store/section-key cx sy cz)
              ^js order (or (.get orders k)
                            (let [ox (* 16 cx) oy (* 16 sy) oz (* 16 cz)
                                  o (.map ^js @boxes (fn [^js b] #js [(steps (box-gap ox (aget b 0)) (box-gap oy (aget b 1)) (box-gap oz (aget b 2)) mf) b]))]
                              (.sort o (fn [a b] (- (aget a 0) (aget b 0))))
                              (.set orders k o)
                              o))]
          (loop [i 0 best ##Inf]
            (if (or (>= i (.-length order)) (>= (aget order i 0) best))
              best
              (let [^js b (aget order i 1)]
                (recur (inc i) (min best (steps (gap x (aget b 0)) (gap y (aget b 1)) (gap z (aget b 2)) mf)))))))))))

(defn- section-keys
  "The keys of the sections a box [lo hi] of cells touches."
  [[lx ly lz] [hx hy hz]]
  (for [cx (range (bit-shift-right lx 4) (inc (bit-shift-right hx 4)))
        sy (range (bit-shift-right ly 4) (inc (bit-shift-right hy 4)))
        cz (range (bit-shift-right lz 4) (inc (bit-shift-right hz 4)))]
    (store/section-key cx sy cz)))

(defn- each-cell!
  "(f x y z i) over the 4096 cells of section s."
  [^js s f]
  (let [bx (* 16 (.-cx s)) by (* 16 (.-sy s)) bz (* 16 (.-cz s))]
    (dotimes [i 4096] (f (+ bx (bit-and i 15)) (+ by (bit-shift-right i 8)) (+ bz (bit-and (bit-shift-right i 4) 15)) i))))

(defn- open-fn []
  (let [^js open (rock/open-ids)] (fn [v] (and (pos? v) (== 1 (aget open (dec v)))))))

(defn toward
  "The remembered frontier cell (open, its head open, a seen floor that is not, a face neighbour never seen) to walk to and
  look from nearest goal: fewer steps from goal than from, within the memory's reach of from (perception's
  max-seen-radius), and a way through it under max-s were the unseen open (step-s a step); of those the one whose way is
  cheapest with each step past it priced extra-s more. nil when none."
  [p from goal max-fall step-s extra-s max-s]
  (when-let [st (st-of p)]
    (let [mf (max 1 max-fall)
          open? (open-fn)
          id (fn [x y z] (inc (store/remembered-id st x y z)))
          [fx fy fz] from [gx gy gz] goal
          apart (fn [x y z ax ay az] (steps (js/Math.abs (- x ax)) (js/Math.abs (- y ay)) (js/Math.abs (- z az)) mf))
          left (apart fx fy fz gx gy gz)
          r store/max-seen-radius
          frontier? (fn [x y z]
                      (and (open? (id x (inc y) z))
                           (let [f (id x (dec y) z)] (and (pos? f) (not (open? f))))
                           (or (zero? (id (inc x) y z)) (zero? (id (dec x) y z)) (zero? (id x y (inc z))) (zero? (id x y (dec z)))
                               (zero? (id x (+ y 2) z)))))
          secs (sections st)
          best #js [##Inf nil]]
      (doseq [k (section-keys [(- fx r) (- fy r) (- fz r)] [(+ fx r) (+ fy r) (+ fz r)])
              :let [^js s (.get secs k)]
              :when (and s (< (steps (gap gx (* 16 (.-cx s))) (gap gy (* 16 (.-sy s))) (gap gz (* 16 (.-cz s))) mf) left))]
        (let [^js ids (.-ids s)]
          (each-cell! s (fn [x y z i]
                          (when (open? (aget ids i))
                            (let [on (apart x y z gx gy gz) to (apart fx fy fz x y z)
                                  price (+ (* step-s to) (* (+ step-s extra-s) on))]
                              (when (and (< on left) (<= to r) (<= (* step-s (+ to on)) max-s) (< price (aget best 0))
                                         (frontier? x y z))
                                (aset best 0 price)
                                (aset best 1 [x y z]))))))))
      (aget best 1))))
