(ns engine.path.space-test
  "engine.path.space: free space for the body centre (ported from the JS space.test.mjs), and the bamboo box the planner and
  the body's physics must agree on (offset-shared.test.mjs)."
  (:require ["prismarine-block" :as prismarine-block]
            [cljs.test :refer [deftest is testing]]
            [engine.path.blocks :as b]
            [engine.path.fixture :as fx]
            [engine.path.space :as space]))

(def table (b/default-state-table))
(def W 0.31)
(def N 17)
(def require-here fx/require-here)
(def offsets (require-here "./js/offsets.mjs"))

(defn free-at
  "free centre positions of the cell at (x, z) with feet at y, as [px pz] pairs"
  ([snapshot x y z] (free-at snapshot x y z y (+ y 1.8)))
  ([snapshot x y z lo hi]
   (let [mask (space/free-mask (space/boxes-near snapshot table x y z lo hi) x z)]
     (vec (keep (fn [k] (when (= 1 (aget mask k)) [(+ x (/ (rem k N) 16)) (+ z (/ (quot k N) 16))])) (range (* N N)))))))

(defn- fill-world [fill & [blocks]] (fx/fixture-snapshot {:fill fill :blocks (or blocks [])}))

(deftest an-empty-cell-is-free-everywhere
  (is (= (* N N) (count (free-at (fill-world [[-2 0 -2 2 0 2 "stone"]]) 0 1 0)))))

(deftest stone-to-the-east-blocks-positions-flush-with-or-beyond-1-minus-w
  (let [free (free-at (fill-world [[-2 0 -2 2 0 2 "stone"] [1 1 -1 1 3 1 "stone"]]) 0 1 0)]
    (is (= (* 12 N) (count free)))
    (is (= [0 0.6875] [(apply min (map first free)) (apply max (map first free))]))))

(deftest a-floor-box-touching-the-feet-and-a-ceiling-above-the-head-do-not-intrude
  (is (= (* N N) (count (free-at (fill-world [[-2 0 -2 2 0 2 "stone"] [-2 3 -2 2 3 2 "stone"]]) 0 1 0 1 2.8)))))

(deftest an-unloaded-neighbour-counts-as-a-full-box
  (is (= 15.6875 (apply max (map first (free-at (fx/fixture-snapshot {:blocks [[0 0 0 "stone"]]}) 15 1 5))))))

(deftest water-has-no-boxes
  (is (zero? (.-length (space/boxes-near (fill-world [[-2 1 -2 2 2 2 "water"]]) table 0 1 0 1 2.8)))))

(defn- mask-of [cells]
  (let [mask (js/Uint8Array. (* N N))]
    (doseq [[i j] cells] (aset mask (+ (* j N) i) 1))
    mask))

(defn- regions-of [mask] (mapv #(js->clj % :keywordize-keys true) (array-seq (space/regions mask))))

(deftest regions-one-component-for-an-open-grid-centre-most-point
  (is (= [{:size (* N N) :px 8 :pz 8}] (regions-of (.fill (js/Uint8Array. (* N N)) 1)))))

(deftest regions-split-into-4-connected-components-diagonal-does-not-join
  (is (= [{:size 1 :px 0 :pz 0} {:size 1 :px 1 :pz 1} {:size 2 :px 15 :pz 16}]
         (sort-by (juxt :size :px) (regions-of (mask-of [[0 0] [1 1] [16 16] [15 16]]))))))

(deftest regions-ties-go-to-the-lowest-j-then-i
  (is (= [{:size 5 :px 8 :pz 7}] (regions-of (mask-of [[8 7] [9 7] [9 8] [9 9] [8 9]])))))

(deftest label-regions-follow-the-regions-order-and-are-minus-1-where-blocked
  (let [mask (mask-of [[0 0] [1 1] [16 16] [15 16]])
        r (space/label-regions mask)
        labels (.-labels r)]
    (is (= (regions-of mask) (mapv #(js->clj % :keywordize-keys true) (array-seq (.-regs r)))))
    (is (= [0 1 2 2 -1] (mapv #(aget labels %) [0 (+ N 1) (+ (* 16 N) 16) (+ (* 16 N) 15) 5])))))

;; cocoa between two jungle logs 2 apart (gap cells x = 1, 2), pods in the row z = 0. Pod extents in x from the log face:
;; age 0 0.3125, age 1 0.4375, age 2 0.5625.
(defn- gap-world [age sides pod-y]
  (fx/fixture-snapshot
   {:fill [[0 0 -1 3 0 1 "stone"] [0 1 -1 0 3 1 "jungle_log"] [3 1 -1 3 3 1 "jungle_log"]]
    :blocks (concat (when (contains? sides :left) [[1 pod-y 0 "cocoa" {:age age :facing "west"}]])
                    (when (contains? sides :right) [[2 pod-y 0 "cocoa" {:age age :facing "east"}]]))}))

(defn- sixteenths-range [from to] (mapv #(+ from (/ % 16)) (range (inc (js/Math.round (* (- to from) 16))))))
(defn- gap-free [snapshot] (into (free-at snapshot 1 1 0) (free-at snapshot 2 1 0)))
(defn- unique-sorted [values] (vec (sort (set values))))

;; narrow pods (age 0, 1) leave a sliver at the cell's z edges where the body slides past them, so look at the middle rows
;; pz in 0.25..0.75, which every pod's z extent overlaps
(defn- middle [free] (filter (fn [[_ pz]] (<= 0.25 (mod pz 1) 0.75)) free))

(deftest cocoa-gap
  (doseq [age [0 1 2]
          :let [left ([1.625 1.75 1.875] age) right ([2.375 2.25 2.125] age)]
          [sides mn mx] [[#{:left :right} left right] [#{:left} left 2.6875] [#{:right} 1.3125 right]]
          pod-y [1 2]]
    (testing (str "age " age " pods " sides " pod row y=" pod-y)
      (let [free (middle (gap-free (gap-world age sides pod-y)))]
        (is (= (sixteenths-range mn mx) (unique-sorted (map first free))))
        (is (= (* 9 (count (sixteenths-range mn mx))) (count (set free))))))))

(deftest cocoa-age-2-both-sides-free-positions-lie-within-0-127-of-the-shared-boundary
  (let [world (gap-world 2 #{:left :right} 1)]
    (is (every? (fn [[px]] (<= (js/Math.abs (- px 2)) 0.127)) (gap-free world)))
    (is (>= (.-length (space/regions (space/free-mask (space/boxes-near world table 1 1 0 1 2.8) 1 0))) 1))))

(deftest cocoa-pod-above-the-head-is-ignored
  (is (= (sixteenths-range 1.3125 2.6875) (unique-sorted (map first (gap-free (gap-world 2 #{:left :right} 3)))))))

(deftest a-one-wide-gap-between-logs-with-an-age-2-pod-has-no-free-position
  (is (= [] (free-at (fx/fixture-snapshot
                      {:fill [[0 0 -1 2 0 1 "stone"] [0 1 -1 0 3 1 "jungle_log"] [2 1 -1 2 3 1 "jungle_log"]]
                       :blocks [[1 1 0 "cocoa" {:age 2 :facing "west"}]]})
                     1 1 0))))

(defn- bamboo-box [x z] (vec (.bambooBox ^js offsets x z)))

(deftest bamboo-grove-free-space-matches-a-brute-force-check-against-bamboo-box
  (let [world (fill-world [[0 0 0 2 0 2 "stone"] [0 1 0 2 3 2 "bamboo"]])
        mask (space/free-mask (space/boxes-near world table 1 1 1 1 2.8) 1 1)
        cells (for [x [0 1 2] z [0 1 2]] {:x x :z z :box (bamboo-box x z)})
        expected (mapv (fn [k]
                         (let [px (+ 1 (/ (rem k N) 16)) pz (+ 1 (/ (quot k N) 16))]
                           (if (some (fn [{:keys [x z box]}]
                                       (and (> (+ px W) (- (+ x (box 0)) 1e-4)) (< (- px W) (+ x (box 3) 1e-4))
                                            (> (+ pz W) (- (+ z (box 2)) 1e-4)) (< (- pz W) (+ z (box 5) 1e-4))))
                                     cells)
                             0 1)))
                       (range (* N N)))
        free (reduce + expected)]
    (is (= expected (vec (array-seq mask))))
    (is (< 0 free (* N N)))))

;; segments in the age 2 / both sides gap: [from to expected]
(deftest segment-free
  (doseq [[from to expected] [[{:x 2 :z -0.5} {:x 2 :z 1.5} true]
                              [{:x 1.9 :z 0.5} {:x 2.1 :z 0.5} true]
                              [{:x 1.2 :z 0.5} {:x 1.9 :z 0.5} false]
                              [{:x 2 :z 0.5} {:x 2.6 :z 0.5} false]
                              [{:x 2 :z 0.5} {:x 2 :z 0.5} true]]]
    (testing (str from " -> " to)
      (is (= expected (space/segment-free? (gap-world 2 #{:left :right} 1) table (clj->js from) (clj->js to) 1 2.8))))))

;; the planner's bamboo (boxes-near) and the body's physics (offset-shapes) must put the stalk at the same world box
(deftest bamboo-planner-box-is-the-servers-box
  (let [Block (prismarine-block fx/registry)
        server-shapes (.-serverShapes ^js (require-here "./js/offset-shapes.mjs"))]
    (doseq [[x z] [[0 0] [1 1] [5 -3] [-1 -1] [-7 4] [-33 -50] [16 16] [123 -456] [-300 299] [2857 3216]]]
      (testing (str "bamboo at " x ", " z)
        (let [snapshot (fx/fixture-snapshot {:blocks [[x 64 z "bamboo"]]})
              ;; (cells in unloaded neighbour chunks come back as whole solid cubes: the stalk is the one thin box)
              boxes (space/boxes-near snapshot table x 64 z 64 66)
              at (first (filter #(< (- (aget boxes (+ % 3)) (aget boxes %)) 0.5) (range 0 (.-length boxes) 6)))
              planner (mapv #(aget boxes (+ at %)) (range 6))
              block (.fromStateId ^js Block (fx/default-state "bamboo") 0)
              _ (set! (.-position block) #js {:x x :y 64 :z z})
              [s] (array-seq (server-shapes block))
              world [(+ x (aget s 0)) (+ 64 (aget s 1)) (+ z (aget s 2)) (+ x (aget s 3)) (+ 64 (aget s 4)) (+ z (aget s 5))]]
          (is (every? true? (map #(< (js/Math.abs (- %1 %2)) 1e-3) planner world))))))))
