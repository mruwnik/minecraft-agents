(ns jobs.access.tunnel.line
  "The geometry of a tunnel line: choosing it (approach), its cells. Reads no settings, memory or ctx."
  (:require [jobs.lib.block-sets :as bs]
            [jobs.lib.stair-cells :as geo]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.pillar :as pillar]
            [plan.shape :as shape]))

(def heading-order [:north :east :south :west])


(defn ahead
  "The cell n along heading from cell, same height."
  [cell heading n]
  (let [[dx dz] (shape/headings heading)]
    (shape/add cell [(* n dx) 0 (* n dz)])))

(defn surface
  "The feet height of the top stand of column [x z], looking down from hi to lo: the first cell that is not open must
  be a solid floor with two open cells over it. nil when it is not (a fluid, a cave roof above hi), or a cell on the
  way is not loaded. With seen-at (nil for a cell not seen) the floor and the two cells over it must be seen: unseen
  cells read as air above the surface, so the first seen stone of a hidden column is no surface. The scan ends (nil)
  at the first unseen cell under a seen one: it is the surface still to look at, and the rock under it is hidden, so
  reading it would spend the looks on cells no sight pass can show."
  ([block-at column lo hi] (surface block-at nil column lo hi))
  ([block-at seen-at [x z] lo hi]
   (loop [y hi above-seen? false]
     (when (>= y lo)
       (let [n (block-at [x y z])
             seen? (or (nil? seen-at) (some? (seen-at [x y z])))]
         (cond
           (nil? n) nil
           (and above-seen? (not seen?)) nil
           (pillar/clear? n) (recur (dec y) seen?)
           (and (<= (+ y 2) hi) (or (nil? seen-at) (every? seen-at [[x y z] [x (+ y 1) z] [x (+ y 2) z]])) (rules/solid-floor? block-at [x y z])) (inc y)
           :else nil))))))

(defn line
  "The steps of a line, as [feet cells] each: the stair from entry, then the run; with :end (the stair's last cell)
  and :stand."
  [{:keys [entry heading dir steps run]}]
  (let [stair-feet (take (inc steps) (iterate #(:next (shape/step-cells % dir heading)) entry))
        end (last stair-feet)
        run-feet (take (inc run) (iterate #(ahead % heading 1) end))]
    {:steps (concat (map (fn [f] [f (shape/step-cells f dir heading)]) (butlast stair-feet))
                    (map (fn [f] [f (geo/step-cells f :level heading)]) (butlast run-feet)))
     :end end
     :stand (last run-feet)}))

(defn line-cells
  "The feet cells of the line of plan: index 0 the entry to n (steps + run) the stand, then the target as n+1."
  [plan]
  (let [{:keys [steps stand]} (line plan)]
    (-> (mapv first steps) (conj stand) (conj (:target plan)))))

(defn head-cell [cell] (shape/add cell [0 1 0]))

(defn open-cells
  "The cells of the line's inside, feet and head, that light passes through."
  [plan]
  (let [cells (line-cells plan)]
    (into (set cells) (map head-cell) cells)))

(defn step-stop
  "jobs.lib.stair-cells/stop-of for a step of the line. With a building block carried (in :filler?) air under or in the floor is no stop:
  the walk bridges it."
  [in feet cells accept]
  (let [filler? (:filler? in)
        stop (geo/stop-of (assoc in :feet feet) (assoc cells :filler? filler?) accept)]
    (when-not (and filler? (= :no-floor (:reason stop)) (bs/open-floor? (:block stop)))
      stop)))

(defn line-stop
  "The first stop on the line of plan, or nil when every step and the target may be cut. stop-at: (fn [feet cells]) ->
  the step's stop (step-stop)."
  [stop-at plan]
  (let [{:keys [steps stand]} (line plan)]
    (or (some (fn [[feet cells]] (stop-at feet cells)) steps)
        (stop-at stand (geo/step-cells stand :level (:heading plan))))))

(defn line-clear?
  "Whether no step of the line of plan stops. Judged from the target back: the run near the target is shared by every
  line along a heading, so with a cached stop-at a refused run ends each longer line at its first step judged."
  [stop-at plan]
  (let [{:keys [steps stand]} (line plan)]
    (and (nil? (stop-at stand (geo/step-cells stand :level (:heading plan))))
         (not-any? (fn [[feet cells]] (stop-at feet cells)) (reverse steps)))))

(defn standable?
  "Whether the body can stand at feet: it and the head cell open, a solid floor under."
  [block-at [x y z]]
  (and (pillar/clear? (block-at [x y z])) (pillar/clear? (block-at [x (inc y) z]))
       (boolean (rules/solid-floor? block-at [x (dec y) z]))))

(def walk-in-max
  "Blocks the body walks to an entry before a longer line from where it stands is taken instead."
  24)

(defn open-row?
  "Whether every cell from feet to [ex fy ez] along one row is standable: the body walks it to an entry there."
  [block-at [fx fy fz :as feet] [ex ez]]
  (let [dx (- ex fx) dz (- ez fz)
        n (+ (js/Math.abs dx) (js/Math.abs dz))]
    (and (or (zero? dx) (zero? dz)) (<= n walk-in-max)
         (every? #(standable? block-at [(+ fx (* % (js/Math.sign dx))) fy (+ fz (* % (js/Math.sign dz)))]) (range (inc n))))))

(defn stand-in?
  "Whether column [x z] holds a cell surface could answer with feet from..to: a solid floor under two open cells, all
  seen. Reads the range only: surface reads every cell from its hi down, so a column that cannot answer in the range is
  not scanned from the top."
  [block-at seen-at [x z] from to]
  (boolean (some (fn [fy] (let [f [x (dec fy) z] a [x fy z] b [x (inc fy) z]
                                nb (block-at b) na (block-at a) nf (block-at f)]
                            (and nb na nf (pillar/clear? nb) (pillar/clear? na) (not (pillar/clear? nf))
                                 (or (nil? seen-at) (every? seen-at [f a b])) (rules/solid-floor? block-at f))))
                 (range from (inc to)))))

(defn entry-heights
  "The feet heights from..to an entry in column [ex ez] may take: the surface (scanned from hi to lo), and the body's
  own height when it stands in that column or can walk to it along an open row (a cave or tunnel roof hides the surface
  from a body underground)."
  [block-at seen-at [ex ez] lo hi from to feet]
  (let [[_ fy _] feet
        own (when (and feet (<= from fy to) (open-row? block-at feet [ex ez])) fy)
        top (when (stand-in? block-at seen-at [ex ez] from to) (surface block-at seen-at [ex ez] lo hi))]
    (distinct (keep identity [(when (and top (<= from top to)) top) own]))))

(defn fits
  "The plans of the lines along heading whose entry is n before the target, when the stair fits in it (entry: the
  surface, or the body's cell feet); else ()."
  [block-at seen-at target heading n max-length feet]
  (let [[tx ty tz] target
        [ex _ ez] (ahead target heading (- n))]
    (for [y (entry-heights block-at seen-at [ex ez] (- ty max-length) (+ ty max-length 1) (- ty (dec n)) (+ ty (dec n)) feet)]
      (let [d (- y ty)
            plan {:entry [ex y ez] :heading heading :dir (if (neg? d) :up :down) :steps (js/Math.abs d)
                  :run (- n 1 (js/Math.abs d)) :length n :target [tx ty tz]}]
        (assoc plan :stand (:stand (line plan)))))))

(defn dist2 [[x y z] [a b c]] (+ (* (- x a) (- x a)) (* (- y b) (- y b)) (* (- z c) (- z c))))

(defn far-entry?
  "Whether the entry of plan is more than walk-in-max from feet: such a line loses to any nearer one."
  [plan feet]
  (> (dist2 feet (:entry plan)) (* walk-in-max walk-in-max)))

(defn level-from?
  "Whether plan is a level run from the body's own cell: a body at the target's height on its line digs level, not a stair from the surface."
  [plan feet]
  (and (zero? (:steps plan)) (= (:entry plan) feet)))

(defn fitting-lines
  "The plans along heading with an entry n before the target, n in ns, in order. Lazy, so ns unchunked: a caller that
  stops at the first clear line reads no column past it."
  [in target heading ns max-length feet]
  (mapcat #(fits (or (:column-at in) (:block-at in)) (:floor-at in) target heading % max-length feet) ns))

(defn walk-in-ns
  "The entry distances n whose entry column lies within walk-in-max of feet: the only lines far-entry? lets win by being near."
  [target heading max-length [fx _ fz]]
  (filter (fn [n] (let [[ex _ ez] (ahead target heading (- n))]
                    (<= (+ (* (- ex fx) (- ex fx)) (* (- ez fz) (- ez fz))) (* walk-in-max walk-in-max))))
          (take max-length (iterate inc 1))))

(defn best-on-heading
  "{:plan p} for the shortest valid line along heading, else {:stop s}: the stop of the last step, on the cell before
  the target, which every line shares (no line is read then), else the shortest fitting line's stop, or :too-far when
  none fits. The :near and :level lines are looked for among the entries the body can walk to
  only. Entry columns are read while (more?) holds."
  [in stop-at more? target heading max-length feet]
  (let [lines (fn [ns] (fitting-lines in target heading (take-while (fn [_] (more?)) ns) max-length feet))
        stand (ahead target heading -1)]
    (if-let [stop (stop-at stand (geo/step-cells stand :level heading))]
      {:stop stop}
      (let [fitting (lines (take max-length (iterate inc 1)))]
        (if-let [p (first (filter #(line-clear? stop-at %) fitting))]
          (let [nearby (filter #(line-clear? stop-at %) (lines (walk-in-ns target heading max-length feet)))]
            {:plan p :near (first (remove #(far-entry? % feet) nearby))
             :level (first (filter #(level-from? % feet) nearby))})
          {:stop (if-let [q (first fitting)] (line-stop stop-at q) {:reason (if (more?) :too-far :read-bound)})})))))

(defn cuts-floor?
  "Whether the line of plan cuts the floor under feet (the body would come back to a hole where it stood)."
  [plan feet]
  (let [floor (shape/add feet [0 -1 0])]
    (boolean (some (fn [[_ {:keys [cut]}]] (some #{floor} cut)) (:steps (line plan))))))


(defn cached-reads
  "block-at reading each cell once, counting the reads in volatile n: the world stands still during one sync approach,
  and its lines share most cells."
  [n block-at]
  (let [m (js/Map.)]
    (fn [[x y z :as cell]]
      (let [k (str x "," y "," z)]
        (if (.has m k)
          (.get m k)
          (let [b (block-at cell)] (vswap! n inc) (.set m k b) b))))))

(defn approach
  "The line to cut to target (the plan map, see fits) from rules input in (without :cell and :feet), the body at
  feet; else {:reason ...} (see the doc), with :headings when the headings fail for different reasons. Only is a heading
  to take, else all four are tried."
  ([in target feet max-length accept] (approach in target feet max-length accept nil))
  ([in target feet max-length accept only]
  (if (and (nil? (:zones in)) (not (:ignore-zones? in)))
    {:reason :no-zones}
    (let [reads (volatile! 0)
          cached (partial cached-reads reads)
          in (cond-> in (:block-at in) (update :block-at cached) (:column-at in) (update :column-at cached)
                     (:floor-at in) (update :floor-at cached))
          stop-at (memoize (fn [feet cells] (step-stop in feet cells accept)))
          judge (fn [h] (let [limit (+ @reads (:heading-reads in))]
                          (best-on-heading in stop-at #(< @reads limit) target h max-length feet)))
          per (into {} (map (fn [h] [h (judge h)])) (if only [only] heading-order))
          plans (distinct (mapcat (fn [h] (keep identity [(:plan (per h)) (:near (per h)) (:level (per h))])) (keys per)))]
      (if (seq plans)
        (first (sort-by (juxt #(not (level-from? % feet)) #(far-entry? % feet) :length #(cuts-floor? % feet) #(dist2 feet (:entry %))) plans))
        (let [stops (into {} (map (fn [[h r]] [h (:stop r)])) per)
              real (remove #(#{:too-far :read-bound} (:reason %)) (vals stops))
              reasons (set (map :reason real))]
          (cond
            (empty? real) {:reason (if (some #(= :read-bound (:reason %)) (vals stops)) :read-bound :too-far)}
            (= 1 (count reasons)) (first real)
            :else {:reason :no-approach :headings (into {} (map (fn [[h s]] [h (:reason s)])) stops)})))))))

(defn leg-target
  "The waypoint of the first leg of a target far from feet underground. Diagonal: on the body's row, one past the
  target's column, so the first leg stands in that column; the shorter axis goes first (swap? takes the longer axis
  first). On the body's row or column: the target's column or row on it (farther than max-length: max-length along it). nil when the body stands on
  the surface or a leg is longer than max-length."
  [block-at [fx fy fz :as feet] [tx _ tz] max-length swap?]
  (let [dx (- tx fx) dz (- tz fz)
        x-first? (not= (boolean swap?) (<= (js/Math.abs dx) (js/Math.abs dz)))
        [a b] (if x-first? [dx dz] [dz dx])]
    (when (not= fy (surface block-at [fx fz] (- fy 1) (+ fy max-length 1)))
      (cond
        (and (zero? dz) (> (js/Math.abs dx) max-length)) [(+ fx (* max-length (js/Math.sign dx))) fy fz]
        (and (zero? dx) (> (js/Math.abs dz) max-length)) [fx fy (+ fz (* max-length (js/Math.sign dz)))]
        (zero? dz) (when-not (zero? dx) [tx fy fz])
        (zero? dx) [fx fy tz]
        (and (not (zero? dx)) (not (zero? dz)) (<= (inc (js/Math.abs a)) max-length) (<= (js/Math.abs b) max-length))
        (if x-first?
          [(+ tx (js/Math.sign dx)) fy fz]
          [fx fy (+ tz (js/Math.sign dz))])))))
