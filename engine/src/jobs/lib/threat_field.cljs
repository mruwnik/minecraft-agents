(ns jobs.lib.threat-field
  "Where a known mob can hurt the body, the one reach every danger price reads (Q16: its way and line of fire, the cells
  the body has not seen the worse case): its field, the cells [x y z] a body standing in is within its reach, each with
  a weight 0-1.
  A walker's field is its way (jobs.lib.reach/forward over mob-lookup: a cell the body has not seen is ground) out to its
  follow range in steps (cost.threat/follow-range: vanilla navigation goes no farther), weight 1 within ::close steps,
  falling to 0 at the range. A ranged mob adds the standing cells within danger/ranged-radius it has a line of fire to
  (weight 1). A mob that lives in water (water-mobs) keeps to the water it was met in: every seen cell of that body of
  water (water-body), weight 1. At most ::max-cells cells, read once per check-cache window (field-of)."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.path.planner.base :as base]
            [engine.settings :as settings]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.reach :as reach]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::close {:default 2 :doc "Steps of a mob's way within which its field weighs 1." :spec (a/int-in 0 nil)}
   ::max-cells {:default 4000 :doc "The most cells one mob's field holds, the nearest first." :spec (a/int-in 1 nil)}
   ::fire-rise {:default 6 :doc "Blocks above or below a ranged mob that its line of fire is tried to." :spec (a/int-in 0 nil)}
   ::water-cells {:default 8000 :doc "The most seen water cells one water mob's body of water is costed over (water-body)." :spec (a/int-in 1 nil)}})

(def water-mobs "Mobs that live in water: they stay in the body of water they were met in." #{"drowned" "guardian" "elder_guardian"})

(defn max-cells [] (settings/get settings ::max-cells))

;; ---------------------------------------------------------------- walkers

(defn walk-field
  "The cells #js [x y z w] a walker at start [x y z] steps to over kind-at within range steps (forward), at most n,
  nearest first: w 1 within close steps, falling linearly to 0 at range (a cell at range is left out)."
  [kind-at [sx sy sz] range close n]
  (let [key (reach/keyer sx sz)
        seen (js/Set.)
        out #js []
        weight (fn [steps] (if (<= steps close) 1 (/ (- range steps) (- range close))))]
    (.add seen (key sx sy sz))
    (loop [queue #js [#js [sx sy sz 0]] head 0]
      (if (or (>= head (.-length queue)) (>= (.-length out) n))
        out
        (let [c (aget queue head)
              x (aget c 0) y (aget c 1) z (aget c 2) steps (aget c 3)]
          (.push out #js [x y z (weight steps)])
          (when (< (inc steps) range)
            (let [nexts (reach/forward kind-at x y z)]
              (dotimes [i (.-length nexts)]
                (let [nc (aget nexts i)
                      k (key (aget nc 0) (aget nc 1) (aget nc 2))]
                  (when-not (.has seen k)
                    (.add seen k)
                    (.push queue #js [(aget nc 0) (aget nc 1) (aget nc 2) (inc steps)]))))))
          (recur queue (inc head)))))))

(defn fire-cells
  "The cells #js [x y z 1] within radius of a ranged mob at pos ({:x :y :z}, feet) and ::fire-rise of its height where a
  body stands (reach/standable? over kind-at) in its line of fire (reach/line-of-fire? over arrow-at), the nearest
  columns first, at most n."
  [kind-at arrow-at pos radius n]
  (let [[mx my mz] (reach/cell-of pos)
        rise (settings/get settings ::fire-rise)
        r (js/Math.floor radius)
        columns (->> (for [dx (range (- r) (inc r)) dz (range (- r) (inc r))
                           :let [d2 (+ (* dx dx) (* dz dz))]
                           :when (<= d2 (* radius radius))]
                       [d2 (+ mx dx) (+ mz dz)])
                     (sort-by first))]
    (into-array
     (take n (for [[_ x z] columns
                   y (range (- my rise) (inc (+ my rise)))
                   :when (and (reach/standable? kind-at x y z)
                              (reach/line-of-fire? arrow-at pos {:x (+ x 0.5) :y y :z (+ z 0.5)}))]
               #js [x y z 1])))))

;; ---------------------------------------------------------------- water mobs

(defn water? [p x y z] (= "water" (u/seen-name p {:x x :y y :z z})))

(defn water-surface
  "[x y z] of the top seen water cell of the column at or next to pos (the cell, up to 2 below it, or a side neighbour's:
  a body hit from the water stood on its shore), nil when no water is seen there."
  [p {:keys [x y z]}]
  (let [x (js/Math.floor x) y (js/Math.floor y) z (js/Math.floor z)]
    (some (fn [[dx dz]]
            (when-let [wy (some #(when (water? p (+ x dx) % (+ z dz)) %) [y (dec y) (- y 2)])]
              [(+ x dx) (loop [wy wy] (if (water? p (+ x dx) (inc wy) (+ z dz)) (recur (inc wy)) wy)) (+ z dz)]))
          [[0 0] [1 0] [-1 0] [0 1] [0 -1]])))

(defn water-body
  "The seen water cells [x y z] of the body of water whose top cell is surface: the columns whose cell at its level is seen
  water, side by side from it, each from that level down while seen water; at most n cells, the nearest columns first."
  [p [sx sy sz] n]
  (loop [queue #queue [[sx sz]] seen #{[sx sz]} cells (transient [])]
    (if (or (empty? queue) (>= (count cells) n))
      (persistent! cells)
      (let [[x z] (peek queue)
            column (take-while #(water? p x % z) (range sy (- sy 384) -1))
            more (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]]
                       :let [k [(+ x dx) (+ z dz)]]
                       :when (and (not (contains? seen k)) (water? p (first k) sy (second k)))]
                   k)]
        (recur (into (pop queue) more) (into seen more) (reduce #(conj! %1 [x %2 z]) cells (take (- n (count cells)) column)))))))

;; ---------------------------------------------------------------- the field

(defn field
  "The field of mob {:mob :pos} known to the body of primitives p (see the ns doc): {:cells #js [#js [x y z w] ...]
  :weights (a Map of base/cell-key to w)}; a water mob met where no water is seen, and a mob of no name or place, has none.
  lookups {:kind-at :arrow-at}: reach/mob-lookup and reach/arrow-lookup of p, shared by the fields of one window."
  ([p threat] (field p threat {:kind-at (reach/mob-lookup p) :arrow-at (reach/arrow-lookup p)}))
  ([p {:keys [mob pos]} {:keys [kind-at arrow-at]}]
   (let [cells (cond
                 (not (and (string? mob) pos)) #js []
                 (contains? water-mobs mob)
                 (if-let [top (water-surface p pos)]
                   (into-array (map (fn [[x y z]] #js [x y z 1]) (water-body p top (settings/get settings ::water-cells))))
                   #js [])
                 :else
                 (let [walk (walk-field kind-at (reach/cell-of pos) (threat/follow-range mob) (settings/get settings ::close) (max-cells))]
                   (if (= :projectile (threat/damage-type mob))
                     (.concat walk (fire-cells kind-at arrow-at (update pos :y js/Math.floor) (danger-q/ranged-radius) (max-cells)))
                     walk)))
         weights (js/Map.)]
     (doseq [^js c cells]
       (let [k (base/cell-key (aget c 0) (aget c 1) (aget c 2))]
         (.set weights k (max (aget c 3) (or (.get weights k) 0)))))
     {:cells cells :weights weights})))

(defn field-key [{:keys [mob pos]}] [mob (some-> pos (update-vals js/Math.floor))])

(defonce bodies (js/WeakMap.))

(defn shared
  "The value of (thunk) for key k, kept per body (the primitives of c) for every job of it while younger than the
  check-cache ttl (never under check-cache/fresh): what the world shows a field does not depend on the job asking."
  [c k thunk]
  (let [p (:primitives c)
        kept (or (.get bodies p) (let [m (js/Map.)] (.set bodies p m) m))
        now (ctx/now c)
        hit (.get kept (pr-str k))]
    (if (and hit (not (::check-cache/fresh c)) (<= 0 (- now (:t hit)) (check-cache/ttl-ms)))
      (:v hit)
      (let [v (thunk)]
        (doseq [[key e] (es6-iterator-seq (.entries kept))] (when (> (- now (:t e)) (* 10 (check-cache/ttl-ms))) (.delete kept key)))
        (.set kept (pr-str k) {:t now :v v})
        v))))

(defn field-of
  "field of threat {:mob :pos} for job c, read once per check-cache window for the same mob and cell (shared)."
  [c threat]
  (shared c [::field (field-key threat)]
          #(field (:primitives c) threat (shared c [::lookups] (fn [] {:kind-at (reach/mob-lookup (:primitives c)) :arrow-at (reach/arrow-lookup (:primitives c))})))))

(defn reach-fn
  "A fn of a cell x y z: its weight in field (0-1), nil outside it."
  [{:keys [weights]}]
  (fn [x y z] (.get weights (base/cell-key x y z))))
