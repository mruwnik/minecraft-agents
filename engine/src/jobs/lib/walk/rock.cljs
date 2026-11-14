(ns jobs.lib.walk.rock
  "A body's guess at the cells it never saw. A buried body's (the prior): under ground they are mostly rock, so a cave
  opening it saw is no portal to a cheap way through the rock round it: an unseen cell more than k blocks (Chebyshev)
  from every seen open cell and every seed cell (the feet, the goal) reads as stone; nearer it stays unseen (priced as
  the mask prices it). Any body's, of a goal: an unseen cell under ground whose top it saw from above, more than k
  steps through unseen cells from all it saw open (buried-section), is in the ground (ground-cells)."
  (:require [engine.path.blocks :as blocks]
            [engine.perception.store :as store]))

(def open-ids*
  (delay (let [^js t (blocks/default-state-table)
               kind (.-kind t)
               a (js/Uint8Array. (.-length kind))]
           (dotimes [id (.-length kind)]
             (let [k (aget kind id)]
               (when-not (or (== k blocks/SOLID) (== k blocks/NARROW)) (aset a id 1))))
           a)))

(defn open-ids
  "1 by state id for a block a body sees or walks through (no collision, a fluid, a climbable, a door): a seen one
  means the space round it may go on."
  []
  @open-ids*)

(def climb-ids*
  (delay (let [^js t (blocks/default-state-table)
               climb (.-climb t)
               a (js/Uint8Array. (.-length climb))]
           (dotimes [id (.-length climb)]
             (when (== 1 (aget climb id)) (aset a id 1)))
           a)))

(def ^:const shaft-sections "Sections below one that a climbable's shaft goes on up through." 4)

(defn- shaft!
  "Out cells (a 4096-cell section array, 1 rock) of the section cx sy cz that lie straight above a remembered climbable (a
  ladder, vines) through unseen cells only are set 0: a shaft goes on until seen otherwise. Returns out, or nil when no
  climbable lies in it or within shaft-sections sections below."
  [known ^js out cx sy cz]
  (let [^js climb @climb-ids*
        lower (keep (fn [d] (known cx (- sy d) cz)) (range shaft-sections -1 -1))
        any-climb? (some (fn [^js ids] (some #(and (pos? %) (== 1 (aget climb (dec %)))) ids)) lower)]
    (when any-climb?
      (let [out (js/Uint8Array. out)]
        (dotimes [lz 16]
          (dotimes [lx 16]
            (loop [secs (range shaft-sections -1 -1) in? false]
              (when-let [d (first secs)]
                (let [ids (known cx (- sy d) cz)]
                  (recur (rest secs)
                         (loop [ly 0 in? in?]
                           (if (== ly 16)
                             in?
                             (let [cell (bit-or (bit-shift-left ly 8) (bit-shift-left lz 4) lx)
                                   v (if ids (aget ids cell) 0)]
                               (cond
                                 (pos? v) (recur (inc ly) (== 1 (aget climb (dec v))))
                                 (and in? (zero? d)) (do (aset out cell 0) (recur (inc ly) in?))
                                 :else (recur (inc ly) in?)))))))))))
        out))))

(defn- dilate!
  "b: a dilated by k along one axis of the n^3 grid, s that axis' stride and o1 o2 the other two."
  [^js a ^js b n k s o1 o2]
  (dotimes [i n]
    (dotimes [j n]
      (let [base (+ (* i o1) (* j o2))]
        (loop [t 0 cnt (loop [t 0 cnt 0] (if (< t (min k n)) (recur (inc t) (+ cnt (aget a (+ base (* t s))))) cnt))]
          (when (< t n)
            (let [add (+ t k)
                  rem (- t k 1)
                  cnt (cond-> cnt
                        (< add n) (+ (aget a (+ base (* add s))))
                        (>= rem 0) (- (aget a (+ base (* rem s)))))]
              (aset b (+ base (* t s)) (if (pos? cnt) 1 0))
              (recur (inc t) cnt))))))))

(defn- spare-heights!
  "out (a 4096-cell section array, 1 rock) of the section at section y sy with every cell set 0 at a height where spare?
  (fn [y]) holds: there the prior holds an unseen cell no likelier solid than open. Returns a new array, or out when no
  height is spared."
  [^js out sy spare?]
  (let [spared (filterv #(spare? (+ (* 16 sy) %)) (range 16))]
    (if (empty? spared)
      out
      (let [res (js/Uint8Array. out)]
        (doseq [ly spared] (.fill res 0 (bit-shift-left ly 8) (bit-shift-left (inc ly) 8)))
        res))))

(def all-rock (js/Uint8Array. 4096))
(.fill all-rock 1)

(defn- rock-section*
  [known ^js open k cx sy cz seeds]
  (let [n (+ 16 (* 2 k))
        nn (* n n)
        seed (js/Uint8Array. (* n nn))
        x0 (- (* cx 16) k) y0 (- (* sy 16) k) z0 (- (* cz 16) k)
        any? (volatile! false)]
    (doseq [dy [-1 0 1] dz [-1 0 1] dx [-1 0 1]]
      (when-let [^js ids (known (+ cx dx) (+ sy dy) (+ cz dz))]
        (let [lo #(max 0 (- (- k) (* 16 %))) hi #(min 16 (- n k (* 16 %)))]
          (doseq [ly (range (lo dy) (hi dy)) lz (range (lo dz) (hi dz))]
            (let [row (+ (* (+ (* 16 dy) ly k) nn) (* (+ (* 16 dz) lz k) n) (* 16 dx) k)
                  cells (bit-or (bit-shift-left ly 8) (bit-shift-left lz 4))]
              (loop [lx (lo dx)]
                (when (< lx (hi dx))
                  (let [v (aget ids (bit-or cells lx))]
                    (when (and (pos? v) (== 1 (aget open (dec v))))
                      (aset seed (+ row lx) 1)
                      (vreset! any? true)))
                  (recur (inc lx)))))))))
    (doseq [[x y z] seeds
            :let [px (- x x0) py (- y y0) pz (- z z0)]
            :when (and (< -1 px n) (< -1 py n) (< -1 pz n))]
      (aset seed (+ (* py nn) (* pz n) px) 1)
      (vreset! any? true))
    (if-not @any?
      (or (shaft! known all-rock cx sy cz) all-rock)
      (let [b (js/Uint8Array. (* n nn))
            out (js/Uint8Array. 4096)]
        (dilate! seed b n k 1 nn n)
        (dilate! b seed n k n nn 1)
        (dilate! seed b n k nn n 1)
        (dotimes [ly 16]
          (dotimes [lz 16]
            (let [row (+ (* (+ ly k) nn) (* (+ lz k) n) k)
                  cells (bit-or (bit-shift-left ly 8) (bit-shift-left lz 4))]
              (dotimes [lx 16]
                (aset out (bit-or cells lx) (- 1 (aget b (+ row lx))))))))
        (or (shaft! known out cx sy cz) out)))))

(defn rock-section
  "The section (cx sy cz, sy the world's section y) as a 4096-cell Uint8Array (store/cell-index), 1 where an unseen cell
  is guessed rock: more than k blocks from every remembered open cell (known: (fn [cx sy cz]) the memory's ids + 1, or
  nil; open: open-ids) and every one of seeds ([x y z] cells), and with spare? not at a height it spares (spare-heights!)."
  ([known open k cx sy cz seeds] (rock-section known open k cx sy cz seeds nil))
  ([known ^js open k cx sy cz seeds spare?]
   (let [out (rock-section* known open k cx sy cz seeds)]
     (if spare? (spare-heights! out sy spare?) out))))

(def ground-ids*
  (delay (let [^js t (blocks/default-state-table)
               kind (.-kind t)
               partial (.-partial t)
               a (js/Uint8Array. (.-length kind))]
           (dotimes [id (.-length kind)]
             (when (and (== (aget kind id) blocks/SOLID) (zero? (aget partial id))) (aset a id 1)))
           a)))

(def ^:const top-section "The highest section y a column is read up to (the world's top, y 320)." 20)

(defn- column-step
  "A column read down one cell of id + 1 v (0 unseen): from code (state * 4 + kind) to the next. state 1: ground whose top
  the body saw from above (seen open over it) lies over the cell, else 0 or 2; kind the cell's (0 unseen, 1 open, 2
  ground, 3 other): a ceiling seen only from below, or leaves, tell nothing of the ground under them."
  [^js open ^js ground code v]
  (cond (zero? v) (bit-and code 12)
        (== 1 (aget open (dec v))) 9
        (== 1 (aget ground (dec v))) (case (bit-and code 3) 1 6 2 (bit-or (bit-and code 12) 2) 10)
        :else 11))

(defn- column-above
  "By column (lz << 4 | lx) of the section cx sy cz, the column-step code of the cell over its top cell. kept: a Map by
  section key."
  [known ^js kept cx sy cz]
  (let [key (store/section-key cx sy cz)]
    (or (.get kept key)
        (let [out (js/Uint8Array. 256)]
          (when (< sy top-section)
            (let [^js up (column-above known kept cx (inc sy) cz)
                  ^js ids (known cx (inc sy) cz)
                  ^js open @open-ids*
                  ^js ground @ground-ids*]
              (.set out up)
              (when ids
                (dotimes [col 256]
                  (loop [ly 15 code (aget up col)]
                    (if (< ly 0)
                      (aset out col code)
                      (recur (dec ly) (column-step open ground code (aget ids (bit-or (bit-shift-left ly 8) col))))))))))
          (.set kept key out)
          out))))

(defn- reach-through-unseen!
  "seed (an n^3 grid, 1 at a seen open cell) grown k steps through face neighbours that are unseen (unseen, 1 each)."
  [^js seed ^js unseen n k]
  (let [nn (* n n)
        size (* n nn)]
    (dotimes [_ k]
      (let [prev (js/Uint8Array. seed)]
        (dotimes [i size]
          (when (and (== 1 (aget unseen i)) (zero? (aget prev i)))
            (let [x (mod i n) z (mod (quot i n) n) y (quot i nn)]
              (when (or (and (> x 0) (== 1 (aget prev (dec i)))) (and (< x (dec n)) (== 1 (aget prev (inc i))))
                        (and (> z 0) (== 1 (aget prev (- i n)))) (and (< z (dec n)) (== 1 (aget prev (+ i n))))
                        (and (> y 0) (== 1 (aget prev (- i nn)))) (and (< y (dec n)) (== 1 (aget prev (+ i nn)))))
                (aset seed i 1)))))))
    seed))

(defn buried-section
  "The section (cx sy cz) as a 4096-cell Uint8Array, 1 where an unseen cell lies under remembered ground in its column
  (column-above, kept its Map) more than k steps through unseen cells from every remembered open cell and every one of
  seeds (cells the body stands or stood in); nil when no cell of it does. A goal is no seed: one under a seen top, away
  from all seen open, is in the ground, to be dug to."
  [known ^js open k cx sy cz seeds ^js kept]
  (let [^js ground @ground-ids*
        ^js own (known cx sy cz)
        ^js above (column-above known kept cx sy cz)
        under (js/Uint8Array. 4096)
        any? (volatile! false)]
    (dotimes [col 256]
      (loop [ly 15 code (aget above col)]
        (when (>= ly 0)
          (let [cell (bit-or (bit-shift-left ly 8) col)
                v (if own (aget own cell) 0)]
            (when (and (zero? v) (== 4 (bit-and code 12))) (aset under cell 1) (vreset! any? true))
            (recur (dec ly) (column-step open ground code v))))))
    (when @any?
      (let [n (+ 16 (* 2 k))
            nn (* n n)
            seed (js/Uint8Array. (* n nn))
            unseen (js/Uint8Array. (* n nn))
            x0 (- (* cx 16) k) y0 (- (* sy 16) k) z0 (- (* cz 16) k)]
        (doseq [dy [-1 0 1] dz [-1 0 1] dx [-1 0 1]
                :let [^js ids (known (+ cx dx) (+ sy dy) (+ cz dz))
                      lo #(max 0 (- (- k) (* 16 %))) hi #(min 16 (- n k (* 16 %)))
                      lx0 (lo dx) lx1 (hi dx)]
                ly (range (lo dy) (hi dy)) lz (range (lo dz) (hi dz))]
          (let [row (+ (* (+ (* 16 dy) ly k) nn) (* (+ (* 16 dz) lz k) n) (* 16 dx) k)
                cells (bit-or (bit-shift-left ly 8) (bit-shift-left lz 4))]
            (loop [lx lx0]
              (when (< lx lx1)
                (let [v (if ids (aget ids (bit-or cells lx)) 0)]
                  (cond (zero? v) (aset unseen (+ row lx) 1)
                        (== 1 (aget open (dec v))) (aset seed (+ row lx) 1)))
                (recur (inc lx))))))
        (doseq [[x y z] seeds
                :let [px (- x x0) py (- y y0) pz (- z z0)]
                :when (and (< -1 px n) (< -1 py n) (< -1 pz n))]
          (aset seed (+ (* py nn) (* pz n) px) 1))
        (reach-through-unseen! seed unseen n k)
        (dotimes [ly 16]
          (dotimes [lz 16]
            (let [row (+ (* (+ ly k) nn) (* (+ lz k) n) k)
                  cells (bit-or (bit-shift-left ly 8) (bit-shift-left lz 4))]
              (dotimes [lx 16]
                (when (== 1 (aget seed (+ row lx)))
                  (aset under (bit-or cells lx) 0))))))
        (or (shaft! known under cx sy cz) under)))))

(defn rock-at
  "(fn [x y z]) true where an unseen cell is guessed rock (rock-section, built once a section)."
  [{:keys [k known seeds spare?]}]
  (let [open (open-ids)
        cache (js/Map.)
        last-key (volatile! nil)
        last-rock (volatile! nil)]
    (fn [x y z]
      (let [cx (bit-shift-right x 4) sy (bit-shift-right y 4) cz (bit-shift-right z 4)
            key (store/section-key cx sy cz)
            ^js r (if (== key @last-key)
                    @last-rock
                    (let [r (or (.get cache key)
                                (let [r (rock-section known open k cx sy cz seeds spare?)] (.set cache key r) r))]
                      (vreset! last-key key)
                      (vreset! last-rock r)))]
        (== 1 (aget r (store/cell-index x y z)))))))

(defn ground-cells
  "The cells of cells ([x y z]) that buried-section reads as ground (stood: the cells the body stands or stood in), as a
  Set of \"x,y,z\" keys; nil when none is."
  [{:keys [k known stood cells]}]
  (let [open (open-ids)
        kept (js/Map.)
        sections (js/Map.)
        out (js/Set.)]
    (doseq [[x y z] cells
            :let [cx (bit-shift-right x 4) sy (bit-shift-right y 4) cz (bit-shift-right z 4)
                  key (store/section-key cx sy cz)
                  ^js b (if (.has sections key)
                          (.get sections key)
                          (let [b (buried-section known open k cx sy cz stood kept)] (.set sections key b) b))]
            :when (and b (== 1 (aget b (store/cell-index x y z))))]
      (.add out (str x "," y "," z)))
    (when (pos? (.-size out)) out)))

(defn with-rock
  "snapshot read with every cell that reads mask (the unseen cells' id) and is guessed rock read as stone (an id)
  instead: by rock-at when k is set, and every cell of ground (a Set of \"x,y,z\" keys, ground-cells). With edge, stone is
  the unloaded id and the view is the planner's loaded edge (snapshot edge): the guessed rock is where the land may go
  on, a frontier."
  [^js snapshot {:keys [mask stone edge k ^js ground] :as guess}]
  (let [rock-at (if k (rock-at guess) (constantly false))
        view (js/Object.create snapshot)]
    (when edge (set! (.-edge view) true))
    (set! (.-stateAt view) (fn [x y z]
                             (let [s (.stateAt snapshot x y z)]
                               (if (and (== s mask) (or (rock-at x y z) (and ground (.has ground (str x "," y "," z))))) stone s))))
    view))
