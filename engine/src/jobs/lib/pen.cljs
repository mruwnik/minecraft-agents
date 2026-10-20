(ns jobs.lib.pen
  "The pen check: from a standing cell or a box, flood-fill what a chicken (the smallest herded animal) can walk
  and say whether it stays inside. Pure over a block-at function that takes
  {:x :y :z} and returns a block (#js {:name :properties}) or nil (unloaded).

  The model, after vanilla's ground pathing as far as it is known:
  - every block has a collision column [lo hi] inside its cell (fence, wall and
    closed gate 1.5, slab 0.5 or 1, carpet 1/16, most others 1); an open gate or
    door, a plant, air and water have none; lava, fire and the like are
    hazards an animal never enters; a block this file does not know counts as
    a full block;
  - an animal stands on a surface, the top of a column with 0.7 free above it (a fence hung a block above the floor is a way out);
  - from a surface it steps to the neighbouring column's highest surface at
    most 1 above (so a block, a slab or a carpet is climbed, a fence top is
    not, from level ground) and falls to the first surface below, at most 3
    down, never through one; 4 down is not walked;
  - in water two or more deep it steps from the surface, not the bottom;
  - a diagonal step needs both orthogonal neighbours steppable.

  Leaks: with a :box, any step out of it. Without one, the fill runs into open
  ground until :max-cells and the way out is read off the walk to its far end
  (an open gate, a gap or a ridge climbed over). A leaky pen is refilled with
  those cells walled off, so :inside and :gates are the pen's own."
  (:require [engine.args :as a]
            [jobs.lib.pen-blocks :refer [collision gate? on?]]
            [engine.settings :as settings]))

(a/defargs settings
  {::max-drop {:default 3.0 :doc "Blocks a penned animal may drop when it walks." :spec (a/num-in 0 nil)}
   ::body-height {:default 0.7 :doc "Height of the smallest herded animal (a chicken): a pen must hold it, so a gap this high is a way out." :spec (a/num-in 0 nil)}
   ::gate-scan {:default 600 :doc "The most cells searched for gates when the flood was cut short." :spec (a/int-in 1 nil)}
   ::max-seals {:default 4 :doc "Rounds of leaks shut when planning a pen." :spec (a/int-in 0 nil)}
   ::max-listed {:default 12 :doc "Leaks listed in a pen report." :spec (a/int-in 1 nil)}})

(def step-up 1.0)
(defn max-drop [] (settings/get settings ::max-drop))
(defn body-height [] (settings/get settings ::body-height))
(def eps 1e-6)
(def default-max-cells 2000)
(defn gate-scan [] (settings/get settings ::gate-scan))
(defn max-seals [] (settings/get settings ::max-seals))

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

;; ------------------------------------------------------------------ the fill

(defn fill*
  "fill with the seen cells in a JS Map from pack: {:seen map :order [key] :exits [...] :truncated? bool :last key}."
  [world seeds box max-cells]
  (let [seen (js/Map.)
        o (:store world)
        put! (fn [[x y z] v] (.set seen (pack o x y z) v))]
    (doseq [{:keys [key s]} seeds] (put! key {:s s :from nil}))
    (loop [queue (into cljs.core/PersistentQueue.EMPTY (map :key) seeds)
           order (mapv :key seeds)
           exits []]
      (cond
        (empty? queue) {:seen seen :order order :exits exits :truncated? false}
        (>= (.-size seen) max-cells) {:seen seen :order order :exits exits :truncated? true :last (peek order)}
        :else
        (let [[x y z :as k] (peek queue)
              s (:s (.get seen (pack o x y z)))
              [order' exits' queue']
              (reduce (fn [[order exits queue] [[dx dz] {:keys [top perch?]}]]
                        (let [nx (+ x dx) nz (+ z dz)
                              ny (floor-cell top)]
                          (cond
                            (and box (outside? box nx nz)) [order (conj exits {:from k :to [nx ny nz] :top top :perch? perch?}) queue]
                            (.has seen (pack o nx ny nz)) [order exits queue]
                            :else (let [nk [nx ny nz]]
                                    (put! nk {:s top :from k :perch? perch?})
                                    [(conj order nk) exits (conj queue nk)]))))
                      [order exits (pop queue)]
                      (moves world x z s))]
          (recur queue' order' exits'))))))

(defn fill
  "Breadth first from seeds ({:key :s}) until the queue is empty or max-cells are seen. With a box, a step
  into a column outside it is recorded as an exit and not taken. {:seen {key {:s :from}} :order [key]
  :exits [{:from key :to key}] :truncated? bool :last key}."
  [world seeds box max-cells]
  (let [{:keys [seen order] :as r} (fill* world seeds box max-cells)]
    (assoc r :seen (persistent! (reduce (fn [m [x y z :as k]] (assoc! m k (.get seen (pack (:store world) x y z)))) (transient {}) order)))))

(defn path-to
  "The cells from a seed to key, each {:key :s}."
  [seen key]
  (loop [k key acc ()]
    (if-not k
      (vec acc)
      (recur (get-in seen [k :from]) (conj acc (assoc (select-keys (get seen k) [:s :perch?]) :key k))))))

;; ------------------------------------------------------------------ where it leaks

(defn barrier-at?
  "True when cell x y z holds something that blocks an animal standing there (more than half a block high)."
  [{:keys [cell]} x y z]
  (let [c (cell x y z)]
    (boolean (and c (not (:unloaded? c)) (> (:hi c) 0.5)))))

(defn barrier-both-sides?
  "True when a barrier stands within d cells on both opposite sides of x y z, along one axis."
  [world [x y z] d]
  (let [side (fn [dx dz] (some #(barrier-at? world (+ x (* dx %)) y (+ z (* dz %))) (range 1 (inc d))))]
    (boolean (or (and (side 1 0) (side -1 0))
                 (and (side 0 1) (side 0 -1))))))

(defn open-gate-at? [{:keys [block]} [x y z]]
  (let [b (block x y z)]
    (boolean (and (gate? b) (on? b "open")))))

(defn ridge?
  "True when the path climbs onto cell i, stays up for any run of cells and steps down off it again: a wall or fence top
  crossed or walked along."
  [path i]
  (let [s (fn [j] (:s (nth path j)))
        up (when (< 0 i (dec (count path))) (s i))]
    (and up
         (> up (+ (s (dec i)) 0.25))
         (let [after (first (drop-while #(<= (Math/abs (- (s %) up)) 0.25) (range (inc i) (count path))))]
           (boolean (and after (< (s after) (- up 0.25))))))))

(defn evidence
  "The first cell of path that shows how the animal got out, {:index :why}, or nil: an open gate, then a gap
  with a barrier either side at 1 cell, then a ridge climbed over, then a gap at 2 cells."
  [world path]
  (let [first-where (fn [pred] (first (filter pred (range (count path)))))
        key-at (fn [i] (:key (nth path i)))]
    (or (some->> (first-where #(open-gate-at? world (key-at %))) (hash-map :why :open-gate :index))
        (some->> (first-where #(barrier-both-sides? world (key-at %) 1)) (hash-map :why :gap :index))
        (some->> (first-where #(ridge? path %)) (hash-map :why :climb :index))
        (some->> (first-where #(barrier-both-sides? world (key-at %) 2)) (hash-map :why :gap :index)))))

(defn climb-at
  "The leak {:pos :why :climb} of the first fence, wall or gate top path climbs over (a one-block step is a divider
  the animals cross inside), or nil. For a fill that ended on unseen cells:
  a gap or an open gate on the path may be an inner chokepoint, so only a look at the unseen cells can tell."
  [path]
  (when-let [i (first (filter #(and (ridge? path %) (:perch? (nth path %))) (range (count path))))]
    (let [[x y z] (:key (nth path i))] {:pos {:x x :y y :z z} :why :climb})))

(defn perch-entry
  "The fence, wall or gate cell where path first steps onto a block top higher than a block, {:x :y :z}, or nil."
  [path]
  (when-let [i (first (filter #(and (:perch? (nth path %)) (not (:perch? (get path (dec %))))) (range (count path))))]
    (let [{[x _ z] :key s :s} (nth path i)] {:x x :y (dec (floor-cell s)) :z z})))

(defn open-ground?
  "True when the ground seed ({:key :s}) stands on, walked without climbing fence tops, spreads over more than
  limit cells: a field, not a pocket the pen's own fences wall in."
  [{:keys [cell] :as world} seed limit]
  (let [walled (assoc world :cell (fn [x y z] (let [c (cell x y z)] (if (and c (> (:hi c) 1)) (assoc c :hi 3) c))))]
    (boolean (:truncated? (fill* walled [seed] nil limit)))))

(defn perch-ins
  "Leaks {:pos :why :perch-in} of the pen's fences, walls and gates (a cell higher than a block beside an inside
  cell) that open ground outside lies within a block of the top of: an animal steps onto the top there and drops in.
  world is a fresh one: cells read here are no unloaded leaks."
  [{:keys [cell max-drop] :as world} seen]
  (let [inside-cols (into #{} (map (fn [[x _ z]] [x z])) (keys seen))
        limit (+ 50 (* 2 (count seen)))
        open (memoize #(open-ground? world %1 limit))]
    (->> (keys seen)
         (mapcat (fn [[x y z]]
                   (for [[dx dz] orthogonal
                         :let [fx (+ x dx) fz (+ z dz) c (cell fx y fz)]
                         :when (and c (> (:hi c) 1) (not (:hazard? c)) (not (:unloaded? c))
                                    (not (contains? inside-cols [fx fz])))
                         :let [t (+ y (:hi c)) ox (+ fx dx) oz (+ fz dz)]
                         :when (and (not (contains? inside-cols [ox oz]))
                                    (some #(and (not (:perch? %)) (<= (- t 1 eps) (:s %) (+ t max-drop eps)) (open %))
                                          (surfaces-in world ox oz (floor-cell (- t 1)) (floor-cell (+ t max-drop)))))]
                     {:pos {:x fx :y y :z fz} :why :perch-in})))
         (sort-by (juxt (comp :x :pos) (comp :y :pos) (comp :z :pos)))
         vec)))

(defn leak-at
  "The leak {:pos :why} on path, a path that ends outside. With box? a wall top stood on is the answer (the
  floor reached it a step below its top), unless an open gate shows; else the crossing itself when nothing shows
  better; without box?, nil when nothing shows."
  [world path box?]
  (let [tail (if box? (vec (take-last 3 path)) path)
        found (evidence world tail)
        end (last path)
        perch (when box? (perch-entry path))
        pos (fn [[x y z]] {:x x :y y :z z})]
    (cond
      (= :open-gate (:why found)) {:pos (pos (:key (nth tail (:index found)))) :why :open-gate}
      perch {:pos perch :why :climb}
      found {:pos (pos (:key (nth tail (:index found)))) :why (:why found)}
      (not box?) nil
      (apply not= (map :s tail)) {:pos (pos (:key end)) :why :climb}
      :else {:pos (pos (:key end)) :why :open})))

;; ------------------------------------------------------------------ gates

(defn gates-near
  "Every fence gate in or beside the cells (a step up or down too: the floor inside may be a step off the fence line), {:pos :open?}, in position order."
  [{:keys [block]} cells]
  (->> cells
       (mapcat (fn [[x y z]] (for [[dx dz] (cons [0 0] orthogonal) dy [0 1 -1]] [(+ x dx) (+ y dy) (+ z dz)])))
       distinct
       (keep (fn [[x y z :as k]]
               (let [b (block x y z)]
                 (when (gate? b) {:pos {:x x :y y :z z} :open? (on? b "open")}))))
       (sort-by (juxt (comp :x :pos) (comp :y :pos) (comp :z :pos)))
       vec))

;; ------------------------------------------------------------------ a drop inside the gate

(defn level-region
  "The cells of inside reached from seeds by orthogonal steps of at most one block up or down."
  [inside seeds]
  (loop [region (set seeds) queue (into cljs.core/PersistentQueue.EMPTY seeds)]
    (if (empty? queue)
      region
      (let [[x y z] (peek queue)
            next (for [[dx dz] orthogonal dy [0 -1 1]
                       :let [k [(+ x dx) (+ y dy) (+ z dz)]]
                       :when (and (contains? inside k) (not (contains? region k)))]
                   k)
            fresh (distinct next)]
        (recur (into region fresh) (into (pop queue) fresh))))))

(defn gate-drop
  "{:gate :from :to :drop} for a gate whose inside side reaches fewer than half the inside cells by steps of one block
  and borders a cell more than one lower (an animal will not step down two, so it never leaves the gate side), else nil."
  [inside {{:keys [x y z]} :pos :as gate}]
  (let [seeds (for [[dx dz] orthogonal dy [0 -1 1]
                    :let [k [(+ x dx) (+ y dy) (+ z dz)]]
                    :when (contains? inside k)]
                k)
        region (when (seq seeds) (level-region inside seeds))
        edges (when (and region (< (* 2 (count region)) (count inside)))
                (for [[kx ky kz :as from] region
                      [dx dz] orthogonal
                      ty (range (- ky 2) (- ky (+ 2 (max-drop))) -1)
                      :let [to [(+ kx dx) ty (+ kz dz)]]
                      :when (contains? inside to)]
                  {:gate (:pos gate) :from from :to to :drop (- ky ty)}))]
    (first (sort-by (juxt :from :to) edges))))

(defn gate-drops [inside gates]
  (vec (keep #(gate-drop inside %) gates)))

;; ------------------------------------------------------------------ the answer

(defn answer [closed? reason inside leaks gates]
  {:closed? closed? :reason reason :inside inside :leaks leaks :gates gates})

(defn covered?
  "Whether a known block standing on the floor of the cell over [x y z] (a fence, a wall, a block) covers it: no animal
  stands in it or on it, so what it holds does not matter. Not for a cell read as head room (a doorway under a lintel):
  air there may be a way out."
  [{:keys [cell head]} [x y z :as k]]
  (let [c (cell x (inc y) z)]
    (boolean (and c (not (:unloaded? c)) (zero? (:lo c)) (not (contains? @head k))))))

(defn unloaded-leaks
  "The unloaded (or unseen) cells the fill read, but not those covered?: the 5 nearest to near [x y z] when given,
  else the first 5 by coordinate."
  [{:keys [unloaded] :as world} near]
  (->> @unloaded (remove #(covered? world %))
       (sort-by (fn [[x y z :as k]]
                  (if near
                    (let [[nx ny nz] near] [(+ (* (- x nx) (- x nx)) (* (- y ny) (- y ny)) (* (- z nz) (- z nz))) k])
                    k)))
       (take 5) (mapv (fn [[x y z]] {:pos {:x x :y y :z z} :why :unloaded}))))

(defn dedupe-leaks [leaks]
  (vec (first (reduce (fn [[out seen] l]
                        (if (contains? seen (:pos l)) [out seen] [(conj out l) (conj seen (:pos l))]))
                      [[] #{}] leaks))))

(defn analyse
  "One fill: {:world :fill :leaks} with the leaks found (the unloaded cells too); nil when there is no start and no
  unloaded cell was read looking for one."
  [{:keys [block-at at box max-cells sealed near blocks final?]}]
  (let [scan (make-world block-at sealed blocks)
        seeds (cond
                at (seeds-at scan at)
                box (seeds-in-box scan box))
        ;; the scan of a tall box reads every cell under the floors; only what the fill reads can leak
        world (if (and box (seq seeds)) (make-world block-at sealed blocks) scan)]
    (when (or (seq seeds) (seq @(:unloaded world)))
      (let [{:keys [seen exits truncated?] end :last :as fill} (fill world seeds box max-cells)
            found (cond
                    (seq exits) (keep #(leak-at world (conj (path-to seen (:from %)) {:key (:to %) :s (:top %) :perch? (:perch? %)}) true) exits)
                    (and truncated? (not box)) (keep identity [(leak-at world (path-to seen end) false)])
                    (and (not box) (seq @(:unloaded world)))
                    (let [path (path-to seen (last (:order fill)))]
                      (keep identity [(if final? (leak-at world path false) (climb-at path))]))
                    :else (perch-ins (make-world block-at sealed blocks) seen))]
        {:world world
         :fill fill
         :leaks (dedupe-leaks (concat found (unloaded-leaks world near)))}))))

(defn pen-without-leaks
  "The cells the pen would enclose with its leaks shut, or nil when shutting up to max-seals rounds of them does
  not close it. Each pass walls off the leaks the last fill found; a climbed wall is no cell to shut."
  [opts first-leaks]
  (loop [sealed #{} leaks first-leaks n 0]
    (let [real (remove #(= :unloaded (:why %)) leaks)]
      (when (and (seq real) (not-any? #(= :climb (:why %)) real) (< n (max-seals)))
        (let [sealed (into sealed (map (fn [{{:keys [x y z]} :pos}] [x y z])) real)
              {:keys [fill leaks]} (analyse (assoc opts :sealed sealed))]
          (if (:truncated? fill)
            (recur sealed leaks (inc n))
            fill))))))

(defn check
  "The pen check.
  opts: :block-at (required), :at {:x :y :z} (feet cell to start from) and/or :box {:min :max} (every surface in it
  starts the fill and a step out of it is a leak), :max-cells (default 2000), :near [x y z] (the unloaded leaks
  listed are the nearest to it, else the first by coordinate), :final? (no look is left: a fill that ran into unseen
  cells names the gap, open gate or wall top on its way out as a leak instead of waiting for a look).
  Returns {:closed? :reason :inside :leaks :gates :gate-drops}.
  :reason is nil when closed, else :leak, :unbounded (more than :max-cells reached and no way out pinned down; give
  a :box for exact crossings), :unloaded (a needed cell was not loaded or seen, the start's floor too) or :no-start (the start is no floor).
  :inside is a set of [x y z] feet cells. For a leaky pen without a box it is what the pen encloses with its leaks
  shut, or empty when that does not close it.
  :leaks is [{:pos {:x :y :z} :why}], :why one of :open-gate :gap :climb :perch-in :open :unloaded.
  :gate-drops is [{:gate :from :to :drop}]: a gate whose side of the pen is cut off from the rest by a drop of more than 1."
  [{:keys [max-cells] :or {max-cells default-max-cells} :as opts}]
  (let [opts (assoc opts :max-cells max-cells :sealed #{} :blocks (store))
        {:keys [world fill leaks]} (analyse opts)]
    (if (or (not world) (and (empty? (:order fill)) (empty? leaks)))
      (answer false :no-start #{} [] [])
      (let [{:keys [order truncated?]} fill
            real? (some #(not= :unloaded (:why %)) leaks)
            reason (cond real? :leak
                         truncated? :unbounded
                         (seq leaks) :unloaded)
            closed? (nil? reason)
            shut (when (and (not closed?) (not (:box opts)) truncated?) (pen-without-leaks opts leaks))
            cells (cond
                    (or closed? (:box opts) (not truncated?)) order
                    shut (:order shut)
                    :else [])]
        (let [inside (set cells)
              gates (gates-near world (if (and truncated? (not shut)) (take (gate-scan) order) cells))]
          (assoc (answer closed? reason inside leaks gates) :gate-drops (gate-drops inside gates)))))))

(def ledge-height 3)

(defn in-pen?
  "True when position pos ({:x :y :z}, floats) stands on a feet cell of the pen's inside, or up to ledge-height
  above one (a sill or ledge inside the fence that the fill does not walk, a hop)."
  [{:keys [inside]} {:keys [x y z]}]
  (let [cx (js/Math.floor x) cz (js/Math.floor z) fy (js/Math.floor (+ y 0.01))]
    (boolean (some #(contains? inside [cx % cz]) (range fy (- fy (inc ledge-height)) -1)))))

(defn max-listed [] (settings/get settings ::max-listed))

(defn summary
  "The answer as the event and result carry it: the cells counted, the leaks capped."
  [{:keys [closed? reason inside leaks gates gate-drops]}]
  (cond-> {:closed? closed? :reason reason :cells (count inside) :leaks (vec (take (max-listed) leaks)) :gates gates}
    (seq gate-drops) (assoc :gate-drops gate-drops)
    (> (count leaks) (max-listed)) (assoc :leaks-total (count leaks))))
