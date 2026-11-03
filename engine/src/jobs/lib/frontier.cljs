(ns jobs.lib.frontier
  "The distance from an unseen cell to the nearest frontier cell the body remembers (open, with a face neighbour it has
  not seen): where sight was cut by light or reach, not by a wall, and whether that frontier cell is lit. The measured
  unseen-cell prior (base-rates/unseen-solid) reads both, the distance capped at base-rates/unseen-d-max."
  (:require [engine.perception.store :as store]
            [jobs.lib.cost.base-rates :as base-rates]
            [jobs.lib.walk.rock :as rock]))

(defn- id-fn [^js sections]
  (fn [cx cy cz] (or (some-> (.get sections (store/section-key (bit-shift-right cx 4) (bit-shift-right cy 4) (bit-shift-right cz 4)))
                             .-ids (aget (store/cell-index cx cy cz)))
                     0)))

(def ^:private side "The edge of a block of cells whose distances are worked out together." 8)

(defn block-field
  "Int8Array of side^3 (x fastest, then z, then y): each cell's 2 * distance to the nearest frontier, capped at
  base-rates/unseen-d-max, plus 1 when that frontier cell is lit (lit-fn of its x y z; of several equally near, lit only
  if all are; nil lit-fn: none), in the block whose lowest corner is ox oy oz; 0 for a frontier cell itself. A capped Chebyshev
  distance transform, one axis at a time: a frontier farther off than the cap cannot count, so only a margin of
  cap - 1 round the block is read."
  [^js sections ox oy oz lit-fn]
  (let [cap base-rates/unseen-d-max
        m (dec cap)
        n (+ side m m)
        r (+ n 2)
        x0 (- ox m 1) y0 (- oy m 1) z0 (- oz m 1)
        ^js open (rock/open-ids)
        ;; 0 never seen, 1 seen open, 2 seen not open; over the margin and one cell more for the face neighbours
        ^js kind (js/Uint8Array. (* r r r))
        _ (dotimes [j r]
            (dotimes [k r]
              (let [cy (+ y0 j) cz (+ z0 k) row (* r (+ k (* r j)))]
                (loop [i 0 sx nil ^js ids nil]
                  (when (< i r)
                    (let [cx (+ x0 i)
                          s (bit-shift-right cx 4)
                          ^js ids (if (identical? s sx)
                                    ids
                                    (some-> ^js (.get sections (store/section-key s (bit-shift-right cy 4) (bit-shift-right cz 4))) .-ids))
                          v (if ids (aget ids (store/cell-index cx cy cz)) 0)]
                      (when (pos? v) (aset kind (+ i row) (if (== 1 (aget open (dec v))) 1 2)))
                      (recur (inc i) s ids)))))))
        rr (* r r)
        ^js litc (js/Uint8Array. (* r r r))
        lit-at (fn [q]
                 (let [c (aget litc q)]
                   (if (pos? c)
                     (== 2 c)
                     (let [l (boolean (and lit-fn (lit-fn (+ x0 (mod q r)) (+ y0 (quot q rr)) (+ z0 (mod (quot q r) r)))))]
                       (aset litc q (if l 2 1))
                       l))))
        frontier? (fn [q] (and (== 1 (aget kind q))
                               (or (zero? (aget kind (inc q))) (zero? (aget kind (dec q)))
                                   (zero? (aget kind (+ q r))) (zero? (aget kind (- q r)))
                                   (zero? (aget kind (+ q rr))) (zero? (aget kind (- q rr))))))
        ;; along x: the distance to the nearest frontier in the row, for the block's x only
        ^js dx (js/Int8Array. (* side n n))
        _ (dotimes [j n]
            (dotimes [k n]
              (let [row (+ 1 (* r (+ (inc k) (* r (inc j))))) to (* side (+ k (* n j)))]
                (dotimes [a side]
                  (aset dx (+ to a) (loop [t 0]
                                      (let [up (+ row m a t) dn (- (+ row m a) t)
                                            fu (frontier? up) fd (frontier? dn)]
                                        (cond (> t m) (* 2 cap)
                                              (or fu fd) (+ (* 2 t) (if (and (or (not fu) (lit-at up)) (or (not fd) (lit-at dn))) 1 0))
                                              :else (recur (inc t))))))))))
        ;; then along y and along z: the nearest of max(|t|, the distance t off on the axis before)
        nearest (fn [^js from base stride]
                  (loop [t (- m) best (* 2 cap)]
                    (if (> t m)
                      best
                      (let [e (aget from (+ base (* t stride)))]
                        (recur (inc t) (js/Math.min best (+ (* 2 (js/Math.max (js/Math.abs t) (bit-shift-right e 1))) (bit-and e 1))))))))
        ^js dy (js/Int8Array. (* side side n))
        _ (dotimes [b side]
            (dotimes [k n]
              (dotimes [a side]
                (aset dy (+ a (* side (+ k (* n b)))) (nearest dx (+ a (* side (+ k (* n (+ m b))))) (* side n))))))
        ^js out (js/Int8Array. (* side side side))]
    (dotimes [b side]
      (dotimes [e side]
        (dotimes [a side]
          (aset out (+ a (* side (+ e (* side b)))) (nearest dy (+ a (* side (+ m e (* n b)))) side)))))
    out))

(defonce ^:private fields (js/WeakMap.))

(defn- block-key [bx by bz]
  (+ (* (+ (* (+ bx 4194304) 8388608) (+ bz 4194304)) 128) (+ by 64)))

(defn field-value
  "block-field's value at the cell: 2 * distance (at base-rates/unseen-d-max for a cell that is not itself a frontier) plus
  1 for a lit nearest frontier, from blocks worked out a time and kept while the body's memory takes no write and light-key
  (what lit-fn depends on, as the sky's darkening) stays: for pricing many cells."
  ([p cell] (field-value p cell nil nil))
  ([p [x y z] lit-fn light-key]
   (when-let [^js st (some-> (aget p "perception") :st)]
     (when (.-stores st)
       (let [^js kept (let [^js c (.get fields st)]
                        (if (and c (== (.-writes c) (.-writes st)) (identical? (.-dim c) (.-dim st)) (= (.-light c) light-key))
                          c
                          (let [c #js {:writes (.-writes st) :dim (.-dim st) :light light-key :blocks (js/Map.)}] (.set fields st c) c)))
             bx (js/Math.floor (/ x side)) by (js/Math.floor (/ y side)) bz (js/Math.floor (/ z side))
             k (block-key bx by bz)
             ^js blocks (.-blocks kept)
             ^js field (or (.get blocks k)
                           (let [f (block-field (store/store-of st (.-dim st)) (* side bx) (* side by) (* side bz) lit-fn)]
                             (.set blocks k f)
                             f))]
         (aget field (+ (- x (* side bx)) (* side (+ (- z (* side bz)) (* side (- y (* side by))))))))))))

(defn distance
  "The cell's distance to the nearest frontier (field-value's half)."
  [p cell]
  (some-> (field-value p cell) (bit-shift-right 1)))
