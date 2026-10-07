(ns engine.path.planner.bends
  "Search methods: the bends of a path through tight cells. A body crosses a tight cell from the point it enters by to the
   point it leaves by, in a straight line; where a stalk or post stands in the way of that line, the route runs round it
   through the cell's free positions, and the path carries its corners as extra steps in the cell (marked bend)."
  (:require [engine.path.planner.base :refer [GRID MOVE-WALK]]
            [engine.path.planner.search :refer [Search]]))

(set! *warn-on-infer* true)

(defn- free-at? [^js mask i j]
  (and (>= i 0) (< i GRID) (>= j 0) (< j GRID) (not (zero? (aget mask (+ (* j GRID) i))))))

(defn- blocked-at? [^js mask i j]
  (and (>= i 0) (< i GRID) (>= j 0) (< j GRID) (zero? (aget mask (+ (* j GRID) i)))))

(defn line-free?
  "Is every mask position the segment (ai, aj) - (bi, bj) passes (one per 1/16 along its longer axis, rounded) free?
   A leg with a blocked end (a boundary point snapped to the nearest region, at a step or a climbable) is not judged."
  [^js mask ai aj bi bj]
  (let [di (- bi ai)
        dj (- bj aj)
        n (js/Math.max (js/Math.abs di) (js/Math.abs dj))]
    (or (blocked-at? mask ai aj)
        (blocked-at? mask bi bj)
        (loop [k 1]
          (cond
            (>= k n) true
            (free-at? mask (js/Math.round (+ ai (/ (* di k) n))) (js/Math.round (+ aj (/ (* dj k) n)))) (recur (inc k))
            :else false)))))

(defn- shortest
  "The free positions of a shortest 8-connected route (no corner cut) from a to b over the mask, as indices a..b, or nil."
  [^js mask a b]
  (let [parents (js/Int16Array. (* GRID GRID))
        queue (js/Int16Array. (* GRID GRID))]
    (.fill parents -2)
    (aset parents a -1)
    (aset queue 0 a)
    (loop [head 0 tail 1]
      (cond
        (== (aget parents b) -2)
        (if (>= head tail)
          nil
          (let [k (aget queue head)
                i (js-mod k GRID)
                j (/ (- k i) GRID)
                [tail] (reduce (fn [[tail] [di dj]]
                                 (let [ni (+ i di) nj (+ j dj) nk (+ (* nj GRID) ni)]
                                   (if (and (free-at? mask ni nj) (== (aget parents nk) -2)
                                            (or (zero? di) (zero? dj) (and (free-at? mask (+ i di) j) (free-at? mask i (+ j dj)))))
                                     (do (aset parents nk k) (aset queue tail nk) [(inc tail)])
                                     [tail])))
                               [tail] [[1 0] [-1 0] [0 1] [0 -1] [1 1] [1 -1] [-1 1] [-1 -1]])]
            (recur (inc head) tail)))

        :else
        (loop [k b out (list)]
          (if (== k -1) (vec out) (recur (aget parents k) (conj out k))))))))

(defn route
  "The corners (indices into the mask) of the shortest-looking straight-legged route from a to b, string-pulled over the
   mask: empty when the leg is straight and free, nil when b cannot be reached."
  [^js mask a b]
  (let [ij (fn [k] [(js-mod k GRID) (js/Math.floor (/ k GRID))])
        free? (fn [p q] (let [[pi pj] (ij p) [qi qj] (ij q)] (line-free? mask pi pj qi qj)))]
    (if (free? a b)
      []
      (when-some [cells (shortest mask a b)]
        (loop [from 0 out []]
          (let [far (last (filter #(free? (nth cells from) (nth cells %)) (range (inc from) (count cells))))]
            (if (or (nil? far) (== far (dec (count cells))))
              out
              (recur far (conj out (nth cells far))))))))))

(defn- crossing-index
  "Index in a cell's mask of the world point (wx, wz), or -1 when it is not a free position of the mask."
  [^js mask x z wx wz]
  (let [i (js/Math.round (* 16 (- wx x)))
        j (js/Math.round (* 16 (- wz z)))]
    (if (free-at? mask i j) (+ (* j GRID) i) -1)))

(extend-type Search
  Object

  ;; the leg of a walked tight cell from where the body comes in (the step's crossing) to where it leaves (the next step's
  ;; crossing, or the stand point of the last step): the extra steps round whatever stands in its straight way, nil where
  ;; the leg is no business of this namespace, and the point the leg first runs to
  (bendsFor [s ^js step out-x out-z]
    (when (and (== (.-move step) MOVE-WALK) (some? (.-cx step)) ^boolean (.isTight s (.-x step) (.-y step) (.-z step)))
      (let [x (.-x step) z (.-z step)
            mask (.-mask ^js (.shapeOf s x (.-y step) z (+ (* (.-y step) 16) (.-h step))))
            a (crossing-index mask x z (.-cx step) (.-cz step))
            b (crossing-index mask x z out-x out-z)]
        (when (and (>= a 0) (>= b 0))
          (vec (for [k (route mask a b)]
                 #js {:x x :y (.-y step) :z z :h (.-h step) :move MOVE-WALK :corner false :bend true
                      :px (+ x (/ (js-mod k GRID) 16)) :pz (+ z (/ (js/Math.floor (/ k GRID)) 16))}))))))

  ;; the steps with the bends of every tight cell walked straight on into the next step's crossing (or to the last step's
  ;; stand point). A step heads for its first bend, or for the crossing out, not for the cell's representative point; a last
  ;; step with bends comes again after them, at its stand point.
  (withBends [s ^js steps]
    (let [out #js []
          n (.-length steps)]
      (loop [k 0]
        (when (< k n)
          (let [^js step (aget steps k)
                ^js next-step (when (< (inc k) n) (aget steps (inc k)))
                walked (or (nil? next-step) (and (== (.-move next-step) MOVE-WALK) (some? (.-cx next-step))))
                bends (when walked
                        (.bendsFor s step (if (some? next-step) (.-cx next-step) (.-px step)) (if (some? next-step) (.-cz next-step) (.-pz step))))
                ^js head (first bends)]
            (cond
              (nil? bends) (.push out step)
              (empty? bends) (do (when (some? next-step) (set! (.-px step) (.-cx next-step)) (set! (.-pz step) (.-cz next-step)))
                                 (.push out step))
              :else (let [final (when (nil? next-step) (let [^js c (js/Object.assign #js {} step)] (js-delete c "cx") (js-delete c "cz") (js-delete c "opens") (js-delete c "hatch") c))]
                      (set! (.-px step) (.-px head))
                      (set! (.-pz step) (.-pz head))
                      (.push out step)
                      (doseq [b bends] (.push out b))
                      (when (some? final) (.push out final))))
            (recur (inc k)))))
      out)))
