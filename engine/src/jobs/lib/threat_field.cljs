(ns jobs.lib.threat-field
  "Where a known mob can hurt the body, the one reach every danger price reads (Q16: its way and line of fire, the cells
  the body has not seen the worse case): its field, the cells [x y z] a body standing in is within its reach, each with
  a weight 0-1.
  A walker's field is its way (jobs.lib.reach/forward over mob-lookup: a cell the body has not seen is ground) out to its
  follow range in steps (cost.threat/follow-range: vanilla navigation goes no farther), weighted by cost.threat/way-weight. A ranged mob adds the standing cells within danger/ranged-radius it has a line of fire to
  (weight 1). A mob that lives in water (water-mobs) keeps to the water it was met in: every seen cell of that body of
  water (water-body), weight 1. At most ::max-cells cells, kept per body while what it read stays (field-of)."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.path.planner.base :as base]
            [engine.perception.store :as store]
            [engine.settings :as settings]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.reach :as reach]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::max-cells {:default 4000 :doc "The most cells one mob's field holds, the nearest first." :spec (a/int-in 1 nil)}
   ::fire-per-column {:default 3 :doc "The most standing cells of one column a ranged mob's line of fire is tried to, the nearest its height first (unseen ground stands at every height)." :spec (a/int-in 1 nil)}
   ::fire-rise {:default 6 :doc "Blocks above or below a ranged mob that its line of fire is tried to." :spec (a/int-in 0 nil)}
   ::water-cells {:default 8000 :doc "The most seen water cells one water mob's body of water is costed over (water-body)." :spec (a/int-in 1 nil)}})

(def water-mobs "Mobs that live in water: they stay in the body of water they were met in." #{"drowned" "guardian" "elder_guardian"})

(defn max-cells [] (settings/get settings ::max-cells))

;; ---------------------------------------------------------------- reads

(defn boxed
  "kind-at read through a dense cache over the cells within half blocks across and rise up or down of [cx cy cz]: a
  cell's kind is asked of kind-at once, a field asks each cell some 25 times. kind-at as it is outside the box."
  [kind-at [cx cy cz] half rise]
  (let [w (inc (* 2 half)) h (inc (* 2 rise))
        x0 (- cx half) y0 (- cy rise) z0 (- cz half)
        codes (js/Uint8Array. (* w w h))
        kinds #js [nil]]
    (fn [x y z]
      (let [dx (- x x0) dy (- y y0) dz (- z z0)]
        (if (and (< -1 dx w) (< -1 dy h) (< -1 dz w))
          (let [i (+ (* (+ (* dx w) dz) h) dy)
                c (aget codes i)]
            (if (zero? c)
              (let [v (kind-at x y z)
                    j (.findIndex kinds #(= v %))
                    j (if (and (neg? j) (< (.-length kinds) 255)) (do (.push kinds v) (dec (.-length kinds))) j)]
                (when (pos? j) (aset codes i j))
                v)
              (aget kinds c)))
          (kind-at x y z))))))

;; ---------------------------------------------------------------- walkers

(defn walk-field
  "The cells #js [x y z w steps] a walker at start [x y z] steps to over kind-at within range steps (forward), at most
  n, nearest first: w threat/way-weight of its steps (close the field-close steps), none from range on."
  [kind-at [sx sy sz :as start] range close n]
  (let [kind-at (boxed kind-at start range 16)
        key (reach/keyer sx sz)
        seen (js/Set.)
        out #js []]
    (.add seen (key sx sy sz))
    (loop [queue #js [#js [sx sy sz 0]] head 0]
      (if (or (>= head (.-length queue)) (>= (.-length out) n))
        out
        (let [c (aget queue head)
              x (aget c 0) y (aget c 1) z (aget c 2) steps (aget c 3)]
          (.push out #js [x y z (threat/way-weight close steps) steps])
          (when (< (inc steps) range)
            (let [nexts (reach/forward kind-at x y z)]
              (dotimes [i (.-length nexts)]
                (let [nc (aget nexts i)
                      k (key (aget nc 0) (aget nc 1) (aget nc 2))]
                  (when-not (.has seen k)
                    (.add seen k)
                    (.push queue #js [(aget nc 0) (aget nc 1) (aget nc 2) (inc steps)]))))))
          (recur queue (inc head)))))))

(def columns-within
  "The columns #js [dx dz] within radius of 0 0, the nearest first (ties by dx, then dz): the same for every mob."
  (memoize
   (fn [radius]
     (let [r (js/Math.floor radius)]
       (->> (for [dx (range (- r) (inc r)) dz (range (- r) (inc r))
                  :let [d2 (+ (* dx dx) (* dz dz))]
                  :when (<= d2 (* radius radius))]
              [d2 dx dz])
            (sort-by first)
            (map (fn [[_ dx dz]] #js [dx dz]))
            into-array)))))

(defn fire-cells
  "The cells #js [x y z 1] within radius of a ranged mob at pos ({:x :y :z}, feet) and ::fire-rise of its height where a
  body stands (reach/standable? over kind-at; ::fire-per-column of each column, the nearest its height first) in its
  line of fire (reach/line-of-fire? over arrow-at), the nearest columns first, at most n."
  [kind-at arrow-at pos radius n]
  (let [[mx my mz :as cell] (reach/cell-of pos)
        rise (settings/get settings ::fire-rise)
        per-column (settings/get settings ::fire-per-column)
        r (js/Math.floor radius)
        kind-at (boxed kind-at cell r (+ rise 3))
        arrow-at (boxed arrow-at cell r (+ rise 3))
        ^js columns (columns-within radius)
        out #js []]
    (loop [i 0]
      (when (and (< i (.-length columns)) (< (.-length out) n))
        (let [x (+ mx (aget (aget columns i) 0)) z (+ mz (aget (aget columns i) 1))]
          (loop [k 0 found 0]
            (when (and (<= k (* 2 rise)) (< found per-column) (< (.-length out) n))
              (let [y (if (odd? k) (+ my (/ (inc k) 2)) (- my (/ k 2)))]
                (if (reach/standable? kind-at x y z)
                  (do (when (reach/line-of-fire? arrow-at pos {:x (+ x 0.5) :y y :z (+ z 0.5)})
                        (.push out #js [x y z 1]))
                      (recur (inc k) (inc found)))
                  (recur (inc k) found)))))
          (recur (inc i)))))
    out))

;; ---------------------------------------------------------------- water mobs

(defn water? [kind-at x y z] (keyword-identical? :water (kind-at x y z)))

(defn water-surface
  "[x y z] of the top seen water cell of the column at or next to pos (the cell, up to 2 below it, or a side neighbour's:
  a body hit from the water stood on its shore), nil when no water is seen there."
  [kind-at {:keys [x y z]}]
  (let [x (js/Math.floor x) y (js/Math.floor y) z (js/Math.floor z)]
    (some (fn [[dx dz]]
            (when-let [wy (some #(when (water? kind-at (+ x dx) % (+ z dz)) %) [y (dec y) (- y 2)])]
              [(+ x dx) (loop [wy wy] (if (water? kind-at (+ x dx) (inc wy) (+ z dz)) (recur (inc wy)) wy)) (+ z dz)]))
          [[0 0] [1 0] [-1 0] [0 1] [0 -1]])))

(defn water-body
  "The seen water cells [x y z] of the body of water whose top cell is surface: the columns whose cell at its level is seen
  water, side by side from it, each from that level down while seen water; at most n cells, the nearest columns first."
  [kind-at [sx sy sz] n]
  (loop [queue #queue [[sx sz]] seen #{[sx sz]} cells (transient [])]
    (if (or (empty? queue) (>= (count cells) n))
      (persistent! cells)
      (let [[x z] (peek queue)
            column (take-while #(water? kind-at x % z) (range sy (- sy 384) -1))
            more (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]]
                       :let [k [(+ x dx) (+ z dz)]]
                       :when (and (not (contains? seen k)) (water? kind-at (first k) sy (second k)))]
                   k)]
        (recur (into (pop queue) more) (into seen more) (reduce #(conj! %1 [x %2 z]) cells (take (- n (count cells)) column)))))))

;; ---------------------------------------------------------------- the field

(defn field
  "The field of mob {:mob :pos} known to the body of primitives p (see the ns doc): {:cells #js [#js [x y z w] ...]
  :weights (a Map of base/cell-key to w) :steps (a walker's: a Map of base/cell-key to the steps of its way there)
  :whole? (its way shut in: ended short of its follow range and of ::max-cells, from a cell it stands on)}; a water mob met
  where no water is seen, and a mob of no name or place, has none.
  lookups {:kind-at :arrow-at}: reach/mob-lookup and reach/arrow-lookup of p, shared by the fields of one window."
  ([p threat] (field p threat {:kind-at (reach/mob-lookup p) :arrow-at (reach/arrow-lookup p)}))
  ([p {:keys [mob pos]} {:keys [kind-at arrow-at]}]
   (let [cells (cond
                 (not (and (string? mob) pos)) #js []
                 (contains? water-mobs mob)
                 (if-let [top (water-surface kind-at pos)]
                   (into-array (map (fn [[x y z]] #js [x y z 1]) (water-body kind-at top (settings/get settings ::water-cells))))
                   #js [])
                 :else
                 (let [walk (walk-field kind-at (reach/cell-of pos) (threat/follow-range mob) (threat/field-close) (max-cells))]
                   (if (= :projectile (threat/damage-type mob))
                     (.concat walk (fire-cells kind-at arrow-at (update pos :y js/Math.floor) (danger-q/ranged-radius) (max-cells)))
                     walk)))
         weights (js/Map.)
         steps (js/Map.)]
     (doseq [^js c cells]
       (let [k (base/cell-key (aget c 0) (aget c 1) (aget c 2))]
         (.set weights k (max (aget c 3) (or (.get weights k) 0)))
         (when-let [n (aget c 4)] (when-not (.has steps k) (.set steps k n)))))
     {:cells cells :weights weights :steps steps
      :whole? (and (string? mob)
                   (< 1 (.-size steps) (max-cells))
                   (< (reduce max 0 (es6-iterator-seq (.values steps))) (dec (threat/follow-range mob))))})))

(defn field-key
  "What one field is kept under: the mob and its place to 2 blocks across (a mob stepping about keeps its field, and the
  tolls built from it stay one vector)."
  [{:keys [mob pos]}]
  [mob (some-> pos (update-vals #(js/Math.floor %)) (update :x #(js/Math.floor (/ % 2))) (update :z #(js/Math.floor (/ % 2))))])

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

;; ---------------------------------------------------------------- reads kept across checks

;; the kinds the code arrays of kept reads hold, by code (0: not read)
(defonce codes-of #js [nil])

(defn code-of [v]
  (let [i (.findIndex codes-of #(= v %))]
    (cond (pos? i) i
          (< (.-length codes-of) 255) (do (.push codes-of v) (dec (.-length codes-of)))
          :else 0)))

(def max-sections "Sections of kept reads (16 KB each) one body keeps." 512)
(def max-fields "Fields one body keeps (a mob roaming leaves one per place)." 64)

(defn ageing-step
  "The clock in half :mutable-max-ms steps when section cx sy cz or one beside it holds a mutable state's time
  (store/section-stamp odd: a remembered fluid or door, and a cell beside a fluid, age to unknown), else nil."
  [{:keys [opts st]} cx sy cz]
  (let [dim (.-dim ^js st)]
    (when (some (fn [[dx dy dz]] (let [s (store/section-stamp st dim (+ cx dx) (+ sy dy) (+ cz dz))] (and (pos? s) (odd? s))))
                [[0 0 0] [1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])
      (js/Math.floor (/ ((:now opts)) (quot (:mutable-max-ms opts) 2))))))

(defn forget-cell!
  "Drops the kept kinds of cell x y z from its section's reads in secs; bumps the section's version when one was kept."
  [^js secs x y z]
  (when-let [^js e (.get secs (store/section-key (bit-shift-right x 4) (bit-shift-right y 4) (bit-shift-right z 4)))]
    (let [i (store/cell-index x y z)]
      (when (or (pos? (aget (.-walk e) i)) (pos? (aget (.-arrow e) i)))
        (aset (.-walk e) i 0)
        (aset (.-arrow e) i 0)
        (set! (.-version e) (inc (.-version e)))))))

(defn refresh!
  "Brings the kept reads e of a section in secs up to date with the body's memory (perception per): when its stamp moved,
  the kinds of every cell whose remembered state changed, and of the cells beside it (a fluid beside a cell ages it), are
  dropped; all of them when the section came or went, or its ageing step moved. Returns e."
  [per ^js secs ^js e]
  (let [^js st (:st per)
        dim (.-dim st)
        cx (.-cx e) sy (.-sy e) cz (.-cz e)
        stamp (store/section-stamp st dim cx sy cz)
        step (ageing-step per cx sy cz)]
    (when-not (and (== stamp (.-stamp e)) (= step (.-step e)))
      (let [ids (store/section-ids st dim cx sy cz)
            ^js old (.-ids e)]
        (if (or (nil? ids) (nil? old) (not= step (.-step e)))
          (do (.fill (.-walk e) 0) (.fill (.-arrow e) 0) (set! (.-version e) (inc (.-version e))))
          (dotimes [i 4096]
            (when-not (== (aget old i) (aget ids i))
              (let [x (+ (* 16 cx) (bit-and i 15)) y (+ (* 16 sy) (bit-shift-right i 8)) z (+ (* 16 cz) (bit-and (bit-shift-right i 4) 15))]
                (forget-cell! secs x y z)
                (forget-cell! secs (inc x) y z) (forget-cell! secs (dec x) y z)
                (forget-cell! secs x (inc y) z) (forget-cell! secs x (dec y) z)
                (forget-cell! secs x y (inc z)) (forget-cell! secs x y (dec z))))))
        (set! (.-ids e) (some-> ids .slice))
        (set! (.-stamp e) stamp)
        (set! (.-step e) step)))
    e))

(defn adopt!
  "Takes memory as it is now as the base of the kept reads e of a section: after the reads of one field, whose glances
  wrote the cells they saw (synchronously, so nothing else wrote meanwhile) as they read them."
  [{:keys [st]} ^js e]
  (let [^js st st
        dim (.-dim st)
        cx (.-cx e) sy (.-sy e) cz (.-cz e)]
    (set! (.-ids e) (some-> (store/section-ids st dim cx sy cz) .slice))
    (set! (.-stamp e) (store/section-stamp st dim cx sy cz))))

(defn section-reads
  "#js [kind-at arrow-at deps unloaded]: reach/mob-lookup and reach/arrow-lookup of p, each cell's kinds kept in secs
  (the body's, per 16^3 section of its memory: refresh!); deps: the sections read, key -> their kept reads; unloaded:
  the cell keys read that were not loaded (not kept: such a cell reads otherwise once its chunk loads, with no memory
  write; nor is a cell that unloads after it was read: remembered terrain stays known). flags.cleared: secs was cleared
  during the read, so entries of deps are no longer in it."
  [p ^js secs]
  (let [per (aget p "perception")
        deps (js/Map.)
        unloaded (js/Set.)
        flags #js {:cleared false}
        last #js [nil nil nil nil]
        section (fn [cx sy cz]
                  (if (and (identical? cx (aget last 0)) (identical? sy (aget last 1)) (identical? cz (aget last 2)))
                    (aget last 3)
                    (let [k (store/section-key cx sy cz)
                          ^js e (or (.get deps k)
                                    (let [^js e (or (.get secs k)
                                                    (let [e #js {:cx cx :sy sy :cz cz :stamp nil :step nil :ids nil :version 0
                                                                 :walk (js/Uint8Array. 4096) :arrow (js/Uint8Array. 4096)}]
                                                      (when (>= (.-size secs) max-sections) (.clear secs) (set! (.-cleared flags) true))
                                                      (.set secs k e)
                                                      e))]
                                      (refresh! per secs e)
                                      (.set deps k e)
                                      e))]
                      (aset last 0 cx) (aset last 1 sy) (aset last 2 cz) (aset last 3 e)
                      e)))
        ;; one read of a cell keeps both its kinds: the line of fire reads the cells the way does
        read (fn [walk?]
               (fn [x y z]
                 (let [^js e (section (bit-shift-right x 4) (bit-shift-right y 4) (bit-shift-right z 4))
                       i (store/cell-index x y z)
                       c (aget (if walk? (.-walk e) (.-arrow e)) i)]
                   (cond
                     (pos? c) (aget codes-of c)
                     (.has unloaded (base/cell-key x y z)) (if walk? :ground :open)
                     :else
                     (let [b (u/sensed p {:x x :y y :z z})
                           unseen? (or (nil? b) (true? (.-unknown b)))
                           walk (if unseen? :ground (reach/kind-of b))
                           arrow (if unseen? :open (reach/arrow-kind-of b))]
                       (if (some? b)
                         (do (aset (.-walk e) i (code-of walk)) (aset (.-arrow e) i (code-of arrow)))
                         (.add unloaded (base/cell-key x y z)))
                       (if walk? walk arrow))))))]
    #js [(read true) (read false) deps unloaded flags]))

(defonce memories (js/WeakMap.))

(defn kept
  "(thunk lookups) for field-key k of the body of c, kept while no cell of memory its reads kept (section-reads) changed,
  under check-cache/fresh too: then it reads the same. lookups {:kind-at :arrow-at}. Without a perception or offline:
  shared, per check-cache window."
  [c k thunk]
  (let [p (:primitives c)
        per (aget p "perception")]
    (if-not (some-> per :raw .eye)
      (shared c [::field k]
              #(thunk (shared c [::lookups] (fn [] {:kind-at (reach/mob-lookup p) :arrow-at (reach/arrow-lookup p)}))))
      (let [^js mem (or (.get memories p) (let [m #js {:secs (js/Map.) :fields (js/Map.) :dim nil}] (.set memories p m) m))
            dim (.-dim ^js (:st per))
            _ (when-not (= dim (.-dim mem))
                (.clear (.-secs mem)) (.clear (.-fields mem)) (set! (.-dim mem) dim))
            ^js secs (.-secs mem)
            ^js fields (.-fields mem)
            ks (pr-str k)
            ^js hit (.get fields ks)]
        (if (and hit (every? (fn [^js d] (let [^js e (refresh! per secs (aget d 0))] (== (aget d 1) (.-version e)))) (aget hit 1)))
          (aget hit 0)
          (let [reads (section-reads p secs)
                v (thunk {:kind-at (aget reads 0) :arrow-at (aget reads 1)})
                cleared? (.-cleared ^js (aget reads 4))]
            ;; secs cleared (past max-sections): the kept fields' reads are no longer refreshed
            (when (or cleared? (>= (.-size fields) max-fields)) (.clear fields))
            (if (or cleared? (pos? (.-size (aget reads 3))))
              (.delete fields ks)
              (let [es (vec (es6-iterator-seq (.values (aget reads 2))))]
                (run! #(adopt! per %) es)
                (.set fields ks #js [v (mapv (fn [^js e] #js [e (.-version e)]) es)])))
            v))))))

(defn field-of
  "field of threat {:mob :pos} for job c, read once for the same mob and field-key while the world reads the same (kept)."
  [c threat]
  (kept c (field-key threat) #(field (:primitives c) threat %)))

(defn reach-fn
  "A fn of a cell x y z: its weight in field (0-1), nil outside it."
  [{:keys [weights]}]
  (fn [x y z] (.get weights (base/cell-key x y z))))

(def cells-within
  "The offsets [dx dy dz d] of the cells within r blocks of a cell (d its distance), the cell itself first."
  (memoize (fn [r]
             (let [k (js/Math.floor r)]
               (->> (for [dx (range (- k) (inc k)) dy (range (- k) (inc k)) dz (range (- k) (inc k))
                          :let [d (js/Math.hypot dx dy dz)] :when (<= d r)]
                      [dx dy dz d])
                    (sort-by peek)
                    vec)))))

(defn way-fn
  "A fn of a cell x y z: the blocks the walker of field walks from its cell start to within reach blocks of it (its
  blow): to a cell of its way, its walk there (the straight line when its steps took no detour, no more than the
  cells between, else its steps) less reach; else its walk to the nearest cell of its way within reach; ##Inf with
  none and the field :whole? (the walker cannot get there), nil when that is not known (the straight line stands)."
  [{:keys [steps whole?]} [sx sy sz] reach]
  (let [memo (js/Map.)
        walked (fn [x y z]
                 (when-let [n (.get steps (base/cell-key x y z))]
                   (let [dx (js/Math.abs (- x sx)) dy (js/Math.abs (- y sy)) dz (js/Math.abs (- z sz))]
                     (if (<= n (+ dx dy dz 1)) (js/Math.hypot dx dy dz) n))))
        way (fn [x y z]
              (if-let [d (walked x y z)]
                (max 0 (- d reach))
                (let [near (keep (fn [[ox oy oz]] (walked (+ x ox) (+ y oy) (+ z oz))) (cells-within reach))]
                  (cond
                    (seq near) (reduce min near)
                    whole? ##Inf))))]
    (fn [x y z]
      (let [k (base/cell-key x y z)]
        (if (.has memo k)
          (.get memo k)
          (let [w (way x y z)] (.set memo k w) w))))))

(defn reaches?
  "Whether the field of threat {:mob :pos} (field-of c) holds the cell of pos {:x :y :z}."
  [c threat {:keys [x y z]}]
  (some? ((reach-fn (field-of c threat)) (js/Math.floor x) (js/Math.floor y) (js/Math.floor z))))
