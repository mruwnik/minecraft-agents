(ns jobs.lib.pen-world
  "The world a pen check reads, for jobs.lib.pen: the per-cell block reader and collision columns, what an animal can stand on and step to (the walking model in jobs.lib.pen's doc), and the surfaces a fill starts from."
  (:require [engine.args :as a]
            [jobs.lib.pen-blocks :refer [collision]]
            [engine.settings :as settings]))

(a/defargs settings
  {::max-drop {:default 3.0 :doc "Blocks a penned animal may drop when it walks." :spec (a/num-in 0 nil)}
   ::body-height {:default 0.7 :doc "Height of the smallest herded animal (a chicken): a pen must hold it, so a gap this high is a way out." :spec (a/num-in 0 nil)}})

(def step-up 1.0)
(defn max-drop [] (settings/get settings ::max-drop))
(defn body-height [] (settings/get settings ::body-height))
(def eps 1e-6)

;; ------------------------------------------------------------------ the world, read once per cell

(def absent ::absent)

(defn store
  "A fresh cell store: a JS Map from pack, with the origin the keys are relative to (set by the first key packed)."
  []
  #js {:m (js/Map.) :ox nil :oy 0 :oz 0})

(defn pack
  "x y z as one number, the key of the per-cell JS maps (hashing a [x y z] vector per lookup was a fifth of a pen check's
  time). Within 1024 columns and 128 cells of the store's origin the key is a small integer (V8 hashes those fast);
  further out it is a negative number that is still unique."
  [o x y z]
  (when (nil? (.-ox o)) (set! (.-ox o) x) (set! (.-oy o) y) (set! (.-oz o) z))
  (let [dx (+ (- x (.-ox o)) 1024) dz (+ (- z (.-oz o)) 1024) dy (+ (- y (.-oy o)) 128)]
    (if (and (>= dx 0) (< dx 2048) (>= dz 0) (< dz 2048) (>= dy 0) (< dy 256))
      (+ (* (+ (* dx 2048) dz) 256) dy)
      (- -1 (+ (* (+ (* (+ x 1048576) 2097152) (+ z 1048576)) 1024) (+ y 256))))))

(defn make-world
  "Memoised reader over block-at: {:block f :cell f :unloaded volatile-of-set :head volatile-of-set}; both take x y z.
  The cells in sealed ([x y z]) read as a wall two high. :head collects the unloaded cells clear? read as head room.
  blocks (a JS Map from pack) holds the blocks read, shared by the fills of one check: block-at is asked once per cell."
  [block-at sealed blocks]
  (let [cells (js/Map.)
        walls (when (seq sealed) (js/Set. (to-array (map (fn [[x y z]] (pack blocks x y z)) sealed))))
        unloaded (volatile! #{})
        head (volatile! #{})
        block (fn [x y z]
                (let [k (pack blocks x y z)
                      known (.get (.-m blocks) k)
                      b (if (undefined? known) (block-at {:x x :y y :z z}) known)]
                  (when (undefined? known) (.set (.-m blocks) k b))
                  (when (nil? b) (vswap! unloaded conj [x y z]))
                  b))
        read (fn [x y z]
               (if-let [b (block x y z)]
                 (collision b)
                 {:lo 0 :hi 1 :unloaded? true}))]
    {:store blocks
     :block block
     :unloaded unloaded
     :head head
     :max-drop (max-drop)
     :body-height (body-height)
     :cell (fn [x y z]
             (let [k (pack blocks x y z)
                   known (.get cells k)]
               (cond
                 (and walls (.has walls k)) {:lo 0 :hi 2}
                 (not (undefined? known)) known
                 :else (let [c (read x y z)] (.set cells k c) c))))}))

(defn floor-cell [n] (js/Math.floor (+ n eps)))

(defn overlaps? [{:keys [lo hi]} y bottom top]
  (and (< (+ y lo) (- top eps)) (> (+ y hi) (+ bottom eps))))

(defn clear?
  "True when nothing in column x z collides with the open height interval (bottom, top). An unloaded cell read goes
  in the world's :head."
  [{:keys [cell head]} x z bottom top]
  (let [last-y (js/Math.floor (- top eps))]
    (loop [y (js/Math.floor bottom)]
      (or (> y last-y)
          (let [c (cell x y z)]
            (when (:unloaded? c) (vswap! head conj [x y z]))
            (and (or (nil? c) (not (overlaps? c y bottom top)))
                 (recur (inc y))))))))

(defn standable?
  [{:keys [body-height] :as world} x z c y]
  (and c
       (not (:hazard? c))
       (not (:unloaded? c))
       (clear? world x z (+ y (:hi c)) (+ y (:hi c) body-height))))

(defn reach
  "Where an animal standing at height s steps into column x z: {:top :perch?}, or nil."
  [{:keys [cell max-drop body-height] :as world} x z s]
  (let [highest (js/Math.floor (+ s step-up eps))
        lowest (js/Math.floor (- s max-drop 1 eps))]
    (loop [y highest]
      (when (>= y lowest)
        (let [c (cell x y z)
              t (when c (+ y (:hi c)))]
          (if-not (and c (<= t (+ s step-up eps)))
            (recur (dec y))
            (when (and (not (:hazard? c))
                       (not (:unloaded? c))
                       (>= t (- s max-drop eps))
                       (clear? world x z t (+ (max s t) body-height)))
              {:top t :perch? (> (:hi c) 1)})))))))

;; An animal in water this deep floats clear of the bottom and steps from the surface.
(def swim-depth 2)
(def water-surface 0.89)

(defn swim-top
  "Height an animal standing at s in column x z steps from: the water surface when the water over the floor is
  swim-depth cells deep or more, else s."
  [{:keys [block]} x z s]
  (let [y0 (floor-cell s)
        y (first (drop-while #(some-> (block x % z) .-name (= "water")) (iterate inc y0)))]
    (if (>= (- y y0) swim-depth) (+ y -1 water-surface) s)))

(def orthogonal [[1 0] [-1 0] [0 1] [0 -1]])
(def diagonal [[1 1] [1 -1] [-1 1] [-1 -1]])
;; each diagonal: the indices in orthogonal of its two straight neighbours
(def diagonal-via [[0 2] [0 3] [1 2] [1 3]])

(defn moves
  "[[dx dz] {:top :perch?}] for every step from column x z at height s; a diagonal needs both orthogonals."
  [world x z floor]
  (let [s (swim-top world x z floor)
        at (fn [[dx dz]] (reach world (+ x dx) (+ z dz) s))
        straight (mapv at orthogonal)
        corners (for [i (range 4)
                      :let [[a b] (nth diagonal-via i)]
                      :when (and (nth straight a) (nth straight b))
                      :let [d (nth diagonal i)
                            r (at d)]
                      :when r]
                  [d r])]
    (concat (keep-indexed (fn [i r] (when r [(nth orthogonal i) r])) straight) corners)))
;; ------------------------------------------------------------------ where the fill starts

(defn surfaces-in
  "{:key :s :perch?} for each surface in column x z whose feet cell is within [lo-y hi-y], highest first."
  [{:keys [cell] :as world} x z lo-y hi-y]
  (for [y (range (inc hi-y) (- lo-y 2) -1)
        :let [c (cell x y z)]
        :when (and c (standable? world x z c y))
        :let [t (+ y (:hi c))
              feet (floor-cell t)]
        :when (<= lo-y feet hi-y)]
    {:key [x feet z] :s t :perch? (> (:hi c) 1)}))

(defn seeds-at [world {:keys [x y z]}]
  (take 1 (surfaces-in world x z y y)))

(defn outside?
  "True when column x z is outside box: beyond its corners, or, when it has :cols (see with-cols), not one of them."
  [{:keys [min max cols]} x z]
  (not (and (<= (:x min) x (:x max)) (<= (:z min) z (:z max)) (or (nil? cols) (contains? cols [x z])))))

(defn with-cols
  "box with :cols, the set of [x z] columns in it that a walker outside the box cannot reach without crossing a
  column of barriers (positions [x y z]): the pen of an L-shaped plan leaves its notch out. No :cols when the
  barriers do not close."
  [{:keys [min max] :as box} barriers]
  (let [walls (into #{} (map (fn [[x _ z]] [x z])) barriers)
        in? (fn [[x z]] (and (<= (dec (:x min)) x (inc (:x max))) (<= (dec (:z min)) z (inc (:z max)))))
        reached (loop [seen #{[(dec (:x min)) (dec (:z min))]} stack [[(dec (:x min)) (dec (:z min))]]]
                  (if-let [[x z] (peek stack)]
                    (let [next (->> [[(inc x) z] [(dec x) z] [x (inc z)] [x (dec z)]]
                                    (filter #(and (in? %) (not (walls %)) (not (seen %)))))]
                      (recur (into seen next) (into (pop stack) next)))
                    seen))]
    (let [cols (set (for [x (range (:x min) (inc (:x max)))
                          z (range (:z min) (inc (:z max)))
                          :when (not (reached [x z]))]
                      [x z]))]
      ;; only barrier columns left: the walls do not close, so check the whole box
      (if (every? walls cols) box (assoc box :cols cols)))))

(defn seeds-in-box [world {:keys [min max] :as box}]
  (for [x (range (:x min) (inc (:x max)))
        z (range (:z min) (inc (:z max)))
        :when (not (outside? box x z))
        s (surfaces-in world x z (:y min) (:y max))
        :when (not (:perch? s))]
    s))
