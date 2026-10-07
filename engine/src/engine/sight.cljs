(ns engine.sight
  "Line of sight over a block grid: Amanatides-Woo traversal of the cells a segment passes through.")

(defn line-clear
  "True when no cell the segment from (fx fy fz) to (tx ty tz) crosses is solid, `(solid? x y z)` with integer cell
  coordinates. The start and end cells never count. The walk is bounded by the segment, so its length is the cap."
  [fx fy fz tx ty tz solid?]
  (let [dx (- tx fx) dy (- ty fy) dz (- tz fz)
        cx (js/Math.floor fx) cy (js/Math.floor fy) cz (js/Math.floor fz)
        ex (js/Math.floor tx) ey (js/Math.floor ty) ez (js/Math.floor tz)
        step (fn [d] (js/Math.sign d))
        t-delta (fn [d] (if (zero? d) js/Infinity (js/Math.abs (/ 1 d))))
        t-max (fn [d c f td] (cond (zero? d) js/Infinity (pos? d) (* (- (inc c) f) td) :else (* (- f c) td)))
        [sx sy sz] [(step dx) (step dy) (step dz)]
        [tdx tdy tdz] [(t-delta dx) (t-delta dy) (t-delta dz)]
        budget (+ (js/Math.abs (- ex cx)) (js/Math.abs (- ey cy)) (js/Math.abs (- ez cz)))]
    (loop [i 0 cx cx cy cy cz cz mx (t-max dx cx fx tdx) my (t-max dy cy fy tdy) mz (t-max dz cz fz tdz)]
      (if (>= i budget)
        true
        (let [axis (cond (and (<= mx my) (<= mx mz)) :x (<= my mz) :y :else :z)
              t (case axis :x mx :y my :z mz)]
          (if (> t 1)
            true
            (let [cx (if (= axis :x) (+ cx sx) cx) cy (if (= axis :y) (+ cy sy) cy) cz (if (= axis :z) (+ cz sz) cz)
                  mx (if (= axis :x) (+ mx tdx) mx) my (if (= axis :y) (+ my tdy) my) mz (if (= axis :z) (+ mz tdz) mz)]
              (cond (and (= cx ex) (= cy ey) (= cz ez)) true
                    (solid? cx cy cz) false
                    :else (recur (inc i) cx cy cz mx my mz)))))))))

(defn segment-hits-box?
  "Whether the segment from `from` to `to` ([x y z] each) meets the box [x0 y0 z0 x1 y1 z1]: slab method."
  [from to box]
  (loop [i 0 lo 0 hi 1]
    (if (= i 3)
      true
      (let [o (nth from i) d (- (nth to i) o) b0 (nth box i) b1 (nth box (+ i 3))]
        (if (zero? d)
          (and (<= b0 o b1) (recur (inc i) lo hi))
          (let [t0 (/ (- b0 o) d) t1 (/ (- b1 o) d)
                lo (max lo (min t0 t1)) hi (min hi (max t0 t1))]
            (and (<= lo hi) (recur (inc i) lo hi))))))))

(defn ray-clear
  "Like line-clear, but a crossed cell blocks only where the segment meets one of its collision boxes.
  `(shapes-at x y z)` gives the cell's boxes in local coordinates (0..1 across the cell; a fence post reaches 1.5)."
  [fx fy fz tx ty tz shapes-at]
  (let [from [fx fy fz] to [tx ty tz]]
    (line-clear fx fy fz tx ty tz
                (fn [x y z] (boolean (some (fn [b] (segment-hits-box? from to [(+ (nth b 0) x) (+ (nth b 1) y) (+ (nth b 2) z)
                                                                                (+ (nth b 3) x) (+ (nth b 4) y) (+ (nth b 5) z)]))
                                           (shapes-at x y z)))))))
