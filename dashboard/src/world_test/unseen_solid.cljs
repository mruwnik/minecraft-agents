(ns world-test.unseen-solid
  "tools/event-counts.mjs unseen-solid: counts the measured prior jobs.lib.cost.base-rates/unseen-solid from the recorded
  chunk columns (worlds/<world>/chunks): P(solid) of a cell a body standing there has not seen, by the section of the body's feet, the cell's
  Chebyshev distance d (1..d-max, farther as d-max) to the nearest frontier cell (seen open with an unseen face neighbour:
  a sight ray cut by light or :near, not by a wall), whether that frontier is lit, the stand's q-bucket and the cell's height over the feet (dy-bucket). Sight as the body's (engine.perception):
  rays out to r blocks every way from the eye of each stand (open feet and head over a solid floor) of the sampled columns;
  a cell is made out when its light passes the visible table by day or it lies within near; a sight-blocking cell is lit by
  the brighter of itself and the cell before it and ends the ray. Solid is the engine's (jobs.lib.walk.rock/open-ids).
  Every stride-th column file in name order is read, the fixture plot grid left out. Per chunk the shares are kept so each
  bucket's Beta is fitted by moments over chunks (a body sees one place). Pure parts: sight, tally, report."
  (:require ["fs" :as fs]
            ["path" :as path]
            [engine.path.offsets :as offsets]
            [engine.perception.light :as light]
            [jobs.lib.walk.rock :as rock]
            [world-test.field-passes :as field-passes]
            [world-test.water-lags :as water-lags]))

(def r "Box half-size and ray length, blocks." 16)
(def near "engine.perception :near: dark cells this close are made out." 4)
(def d-max "The farthest distance bucket (it holds the farther cells too)." 8)
(def rays "Sight rays per stand, spread evenly over the sphere." 6000)
(def columns "Local x z of the sampled columns of a chunk." [[4 4] [12 12]])
(def bottom-y -64)
(def top-y 319)
(def min-chunks "Fewest chunks a bucket's own fit is taken from." 3)
(def q-edges
  "Bucket edges of q, the solid share of the cells seen within near of the eye (a body sees them in any light): a floor
  under open air reads about 0.25, a cave with walls in reach more."
  [0.3 0.5 0.7])

(defn open-ids
  "rock/open-ids, the planner's block table found from the repo root or the dashboard dir (it loads engine/js)."
  []
  (when-let [root (first (filter #(fs/existsSync (path/join % "js" "table-cache.mjs"))
                                 (map #(path/resolve (js/process.cwd) %) ["engine" "../engine" "."])))]
    (offsets/set-root! root))
  (rock/open-ids))

(def side (inc (* 2 r)))
(def ^:private n3 (* side side side))
(defn- idx [x y z] (+ x (* side (+ z (* side y)))))

(defn box
  "#js {:solid :light :loaded} over the side^3 cells round feet x y z (local r r r) by at ((fn [x y z]) -> {:id :block :sky}
  or nil): Uint8Arrays, solid by open (rock/open-ids), light packed sky << 4 | block."
  [at ^js open x y z]
  (let [solid (js/Uint8Array. n3) lit (js/Uint8Array. n3) loaded (js/Uint8Array. n3)]
    (dotimes [ly side]
      (dotimes [lz side]
        (dotimes [lx side]
          (when-let [c (and (<= bottom-y (+ y ly (- r)) top-y) (at (+ x lx (- r)) (+ y ly (- r)) (+ z lz (- r))))]
            (let [i (idx lx ly lz)]
              (aset loaded i 1)
              (aset lit i (bit-or (bit-shift-left (or (:sky c) 0) 4) (or (:block c) 0)))
              (when-not (and (:id c) (== 1 (aget open (:id c)))) (aset solid i 1)))))))
    #js {:solid solid :light lit :loaded loaded}))

(defn sight
  "Uint8Array of the box's cells the body at its centre makes out (1), visible: Uint8Array over packed light."
  [^js b ^js visible]
  (let [^js solid (.-solid b) ^js lit (.-light b) ^js loaded (.-loaded b)
        seen (js/Uint8Array. n3)
        ex (+ r 0.5) ey (+ r 1.62) ez (+ r 0.5)
        golden (* js/Math.PI (- 3 (js/Math.sqrt 5)))]
    (dotimes [k rays]
      (let [dy (- 1 (/ (* 2 (+ k 0.5)) rays))
            rr (js/Math.sqrt (- 1 (* dy dy)))
            a (* golden k)
            dx (* rr (js/Math.cos a)) dz (* rr (js/Math.sin a))
            step (fn [d] (cond (pos? d) 1 (neg? d) -1 :else 0))
            sx (step dx) sy (step dy) sz (step dz)
            inv (fn [d] (if (zero? d) js/Infinity (/ 1 (js/Math.abs d))))
            tdx (inv dx) tdy (inv dy) tdz (inv dz)
            first-t (fn [e d s td] (if (zero? s) js/Infinity (* td (if (pos? s) (- (inc (js/Math.floor e)) e) (- e (js/Math.floor e))))))]
        (loop [x (js/Math.floor ex) y (js/Math.floor ey) z (js/Math.floor ez)
               tx (first-t ex dx sx tdx) ty (first-t ey dy sy tdy) tz (first-t ez dz sz tdz)
               t 0 prev 0]
          (when (and (< -1 x side) (< -1 y side) (< -1 z side) (<= t r))
            (let [i (idx x y z)]
              (when (== 1 (aget loaded i))
                (let [own (aget lit i)
                      l (if (== 1 (aget solid i)) (light/max-light own prev) own)]
                  (when (or (<= t near) (== 1 (aget visible l))) (aset seen i 1))
                  (when (zero? (aget solid i))
                    (cond (and (<= tx ty) (<= tx tz)) (recur (+ x sx) y z (+ tx tdx) ty tz tx own)
                          (<= ty tz) (recur x (+ y sy) z tx (+ ty tdy) tz ty own)
                          :else (recur x y (+ z sz) tx ty (+ tz tdz) tz own))))))))))
    seen))

(def ^:private faces (into-array (map into-array [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])))
(def ^:private round (into-array (for [fx [-1 0 1] fy [-1 0 1] fz [-1 0 1] :when (not= 0 fx fy fz)] (into-array [fx fy fz]))))

(defn- frontier?
  "Whether cell i (x y z) is seen open with a loaded unseen face neighbour."
  [^js solid ^js loaded ^js seen i x y z]
  (and (== 1 (aget seen i)) (zero? (aget solid i))
       (loop [f 0]
         (and (< f 6)
              (let [^js o (aget faces f) nx (+ x (aget o 0)) ny (+ y (aget o 1)) nz (+ z (aget o 2))]
                (or (and (< -1 nx side) (< -1 ny side) (< -1 nz side)
                         (let [j (idx nx ny nz)] (and (== 1 (aget loaded j)) (zero? (aget seen j)))))
                    (recur (inc f))))))))

(defn distances
  "#js {:d :lit} over the box: Int8Array of each cell's Chebyshev distance to the nearest frontier cell (-1: no frontier
  in the box), and Uint8Array 1 where that frontier is lit by visible (a tie goes to the dark one)."
  [^js b ^js seen ^js visible]
  (let [^js solid (.-solid b) ^js lit (.-light b) ^js loaded (.-loaded b)
        d (js/Int8Array. n3) lit-of (js/Uint8Array. n3)
        q (js/Int32Array. n3)
        tail (volatile! 0)]
    (.fill d -1)
    (doseq [pass [0 1]]
      (dotimes [y side]
        (dotimes [z side]
          (dotimes [x side]
            (let [i (idx x y z)]
              (when (and (== pass (aget visible (aget lit i))) (frontier? solid loaded seen i x y z))
                (aset d i 0) (aset lit-of i pass) (aset q @tail i) (vswap! tail inc)))))))
    (loop [head 0]
      (when (< head @tail)
        (let [i (aget q head)
              x (mod i side) z (mod (quot i side) side) y (quot i (* side side))
              nd (inc (aget d i))]
          (dotimes [f 26]
            (let [^js o (aget round f) nx (+ x (aget o 0)) ny (+ y (aget o 1)) nz (+ z (aget o 2))]
              (when (and (< -1 nx side) (< -1 ny side) (< -1 nz side))
                (let [j (idx nx ny nz)]
                  (when (neg? (aget d j))
                    (aset d j nd) (aset lit-of j (aget lit-of i)) (aset q @tail j) (vswap! tail inc))))))
          (recur (inc head)))))
    #js {:d d :lit lit-of}))

(defn q-bucket "The q-edges bucket (0..3) of share q." [q] (count (filter #(>= q %) q-edges)))

(defn near-share
  "The solid share of the box's seen cells whose centre lies within near of the eye."
  [^js b ^js seen]
  (let [^js solid (.-solid b)
        ex (+ r 0.5) ey (+ r 1.62) ez (+ r 0.5)]
    (loop [i 0 n 0 k 0]
      (if (== i n3)
        (if (pos? n) (/ k n) 0)
        (let [x (mod i side) z (mod (quot i side) side) y (quot i (* side side))
              dx (- (+ x 0.5) ex) dy (- (+ y 0.5) ey) dz (- (+ z 0.5) ez)]
          (if (and (== 1 (aget seen i)) (<= (+ (* dx dx) (* dy dy) (* dz dz)) (* near near)))
            (recur (inc i) (inc n) (+ k (aget solid i)))
            (recur (inc i) n k)))))))

(def dy-edges
  "Bucket edges of a cell's height over the feet: under the floor, the band the body walks in (feet -1 to head +1), above."
  [-1 3])

(defn dy-bucket "The dy-edges bucket (0..2) of height dy over the feet." [dy] (count (filter #(>= dy %) dy-edges)))

(def pooled-q "The q bucket index that pools every q." (inc (count q-edges)))

(defn tally
  "{[section-y d lit? q-bucket dy-bucket] {:solid n :open n}} of the unseen loaded cells of the box round feet y (world
  height) that have a frontier, section-y and q-bucket the stand's (its feet's section, near-share: the body asks about
  cells round it), each also under q-bucket pooled-q."
  [^js b ^js seen ^js dist y]
  (let [^js solid (.-solid b) ^js loaded (.-loaded b) ^js d (.-d dist) ^js lit (.-lit dist)
        qb (q-bucket (near-share b seen))
        ;; [d 1..d-max][dy bucket][lit][solid]
        counts (js/Int32Array. (* d-max 3 4))]
    (dotimes [i n3]
      (when (and (== 1 (aget loaded i)) (zero? (aget seen i)) (pos? (aget d i)))
        (let [at (+ (* 12 (dec (min d-max (aget d i)))) (* 4 (dy-bucket (- (quot i (* side side)) r))) (* 2 (aget lit i)) (aget solid i))]
          (aset counts at (inc (aget counts at))))))
    (into {}
          (for [dd (range 1 (inc d-max)) dyb (range 3) l [0 1]
                :let [at (+ (* 12 (dec dd)) (* 4 dyb) (* 2 l))
                      open (aget counts at) solid (aget counts (inc at))]
                :when (pos? (+ open solid))
                q [qb pooled-q]]
            [[(* 16 (js/Math.floor (/ y 16))) dd (== 1 l) q dyb] {:solid solid :open open}]))))

(def visible-by-day (light/visible-table 1 0.2))

(defn stand-tally
  "tally from the stand at feet x y z by at."
  [at x y z]
  (let [b (box at (open-ids) x y z)
        seen (sight b visible-by-day)]
    (tally b seen (distances b seen visible-by-day) y)))

(defn chunk-tally
  "The tallies of every stand of the sampled columns of chunk cx cz by at, summed."
  [at cx cz]
  (let [^js open (open-ids)
        solid? (fn [x y z] (when-let [c (at x y z)] (not (and (:id c) (== 1 (aget open (:id c)))))))]
    (apply merge-with (partial merge-with +) {}
           (for [[lx lz] columns
                 :let [x (+ (* 16 cx) lx) z (+ (* 16 cz) lz)]
                 y (range -63 318)
                 :when (and (true? (solid? x (dec y) z)) (false? (solid? x y z)) (false? (solid? x (inc y) z)))]
             (stand-tally at x y z)))))

(defn- fit-of
  "{:prior [a b] :n :chunks} of the rows ({:solid :open} per chunk) of one bucket."
  [rows min-n]
  (let [solid (reduce + (map :solid rows))
        open (reduce + (map :open rows))
        shares (keep (fn [{:keys [solid open]}] (when (>= (+ solid open) min-n) (/ solid (+ solid open)))) rows)]
    {:prior (if (> (count shares) 1) (field-passes/beta-fit shares solid (+ solid open)) [solid open])
     :n (+ solid open) :chunks (count shares)}))

(defn report
  "Of chunk tallies: {:q-edges :dy-edges :sections [{:y :lit :dark :n-lit :n-dark}]}, :lit and :dark per d 1..d-max per
  q-bucket (the last pooled-q) per dy-bucket the Beta pseudo-counts [solid open] fitted by moments over the chunks with
  at least min-n cells of the bucket (fit-of); a bucket fitted over fewer than min-chunks chunks takes its pooled-q fit."
  [tallies min-n]
  (let [ys (sort (distinct (map first (mapcat keys tallies))))
        fit (fn [k] (fit-of (map #(get % k {:solid 0 :open 0}) tallies) min-n))
        one (fn [y d lit qb dyb]
              (let [f (fit [y d lit qb dyb])]
                (if (or (= qb pooled-q) (>= (:chunks f) min-chunks)) f (fit [y d lit pooled-q dyb]))))]
    {:q-edges q-edges
     :dy-edges dy-edges
     :sections (vec (for [y ys]
                      (into {:y y}
                            (for [[lit k] [[true :lit] [false :dark]]
                                  :let [fits (vec (for [d (range 1 (inc d-max))]
                                                    (vec (for [qb (range (inc pooled-q))] (vec (for [dyb (range 3)] (one y d lit qb dyb)))))))]
                                  [k' f] [[k (mapv (fn [by-q] (mapv #(mapv :prior %) by-q)) fits)]
                                          [(keyword (str "n-" (name k))) (mapv (fn [by-q] (mapv #(mapv :n %) by-q)) fits)]]]
                              [k' f]))))}))

(defn count-in
  "report of every stride-th recorded column file in dir, read by at."
  [dir at stride]
  (let [cols (->> (array-seq (fs/readdirSync dir))
                  (keep #(when-let [[_ cx cz] (re-find #"^(-?\d+)\.(-?\d+)\.bin$" %)] [(js/parseInt cx) (js/parseInt cz)]))
                  (remove (fn [[cx cz]] (water-lags/reserved? cx cz)))
                  sort
                  (take-nth stride))]
    (assoc (report (map (fn [[cx cz]] (chunk-tally at cx cz)) cols) 200) :files (count cols) :stride stride)))
