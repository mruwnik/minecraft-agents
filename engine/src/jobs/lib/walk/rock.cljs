(ns jobs.lib.walk.rock
  "A buried body's guess at the cells it never saw: under ground they are mostly rock, so a cave opening it saw is no
  portal to a cheap way through the rock round it. An unseen cell more than k blocks (Chebyshev) from every seen open
  cell and every seed cell (the feet, the goal) reads as stone; nearer it stays unseen (priced as the mask prices it)."
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

(def all-rock (js/Uint8Array. 4096))
(.fill all-rock 1)

(defn rock-section
  "The section (cx sy cz, sy the world's section y) as a 4096-cell Uint8Array (store/cell-index), 1 where an unseen cell
  is guessed rock: more than k blocks from every remembered open cell (known: (fn [cx sy cz]) the memory's ids + 1, or
  nil; open: open-ids) and every one of seeds ([x y z] cells)."
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

(defn with-rock
  "snapshot read with every cell that reads mask (the unseen cells' id) and is guessed rock (rock-section, built once a
  section) read as stone instead."
  [^js snapshot {:keys [mask stone k known seeds]}]
  (let [open (open-ids)
        cache (js/Map.)
        last-key (volatile! nil)
        last-rock (volatile! nil)
        rock-at (fn [x y z]
                  (let [cx (bit-shift-right x 4) sy (bit-shift-right y 4) cz (bit-shift-right z 4)
                        key (store/section-key cx sy cz)
                        ^js r (if (== key @last-key)
                                @last-rock
                                (let [r (or (.get cache key)
                                            (let [r (rock-section known open k cx sy cz seeds)] (.set cache key r) r))]
                                  (vreset! last-key key)
                                  (vreset! last-rock r)))]
                    (== 1 (aget r (store/cell-index x y z)))))
        view (js/Object.create snapshot)]
    (set! (.-stateAt view) (fn [x y z]
                             (let [s (.stateAt snapshot x y z)]
                               (if (and (== s mask) (rock-at x y z)) stone s))))
    view))
