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
    (when-not (and filler? (= :no-floor (:reason stop)) (bs/air (:block stop)))
      stop)))

(defn line-stop
  "The first stop on the line of plan, or nil when every step and the target may be cut."
  [in plan accept]
  (let [{:keys [steps stand]} (line plan)]
    (or (some (fn [[feet cells]] (step-stop in feet cells accept)) steps)
        (step-stop in stand (geo/step-cells stand :level (:heading plan)) accept))))

(defn standable?
  "Whether the body can stand at feet: it and the head cell open, a solid floor under."
  [block-at [x y z]]
  (and (pillar/clear? (block-at [x y z])) (pillar/clear? (block-at [x (inc y) z]))
       (boolean (rules/solid-floor? block-at [x (dec y) z]))))

(defn entry-heights
  "The feet heights an entry in column [ex ez] may take: the surface, and the body's own cell when it stands in
  that column (a cave or tunnel roof hides the surface from a body underground)."
  [block-at seen-at [ex ez] lo hi feet]
  (let [[fx fy fz] feet
        own (when (and feet (= [fx fz] [ex ez]) (<= lo fy hi) (standable? block-at feet)) fy)]
    (distinct (keep identity [(surface block-at seen-at [ex ez] lo hi) own]))))

(defn fits
  "The plans of the lines along heading whose entry is n before the target, when the stair fits in it (entry: the
  surface, or the body's cell feet); else ()."
  [block-at seen-at target heading n max-length feet]
  (let [[tx ty tz] target
        [ex _ ez] (ahead target heading (- n))]
    (for [y (entry-heights block-at seen-at [ex ez] (- ty max-length) (+ ty max-length 1) feet)
          :when (<= (js/Math.abs (- y ty)) (dec n))]
      (let [d (- y ty)
            plan {:entry [ex y ez] :heading heading :dir (if (neg? d) :up :down) :steps (js/Math.abs d)
                  :run (- n 1 (js/Math.abs d)) :length n :target [tx ty tz]}]
        (assoc plan :stand (:stand (line plan)))))))

(defn dist2 [[x y z] [a b c]] (+ (* (- x a) (- x a)) (* (- y b) (- y b)) (* (- z c) (- z c))))

(def walk-in-max
  "Blocks the body walks to an entry before a longer line from where it stands is taken instead."
  24)

(defn far-entry?
  "Whether the entry of plan is more than walk-in-max from feet: such a line loses to any nearer one."
  [plan feet]
  (> (dist2 feet (:entry plan)) (* walk-in-max walk-in-max)))

(defn level-from?
  "Whether plan is a level run from the body's own cell: a body at the target's height on its line digs level, not a stair from the surface."
  [plan feet]
  (and (zero? (:steps plan)) (= (:entry plan) feet)))

(defn best-on-heading
  "{:plan p} for the shortest valid line along heading, else {:stop s} (the shortest fitting line's stop, or
  :too-far when none fits)."
  [in target heading max-length accept feet]
  (let [fitting (mapcat #(fits (or (:column-at in) (:block-at in)) (:floor-at in) target heading % max-length feet) (range 1 (inc max-length)))
        judged (map (fn [p] [p (line-stop in p accept)]) fitting)]
    (if-let [[p] (first (filter (comp nil? second) judged))]
      {:plan p :near (some (fn [[q stop]] (when (and (nil? stop) (not (far-entry? q feet))) q)) judged)
       :level (some (fn [[q stop]] (when (and (nil? stop) (level-from? q feet)) q)) judged)}
      {:stop (or (second (first judged)) {:reason :too-far})})))

(defn cuts-floor?
  "Whether the line of plan cuts the floor under feet (the body would come back to a hole where it stood)."
  [plan feet]
  (let [floor (shape/add feet [0 -1 0])]
    (boolean (some (fn [[_ {:keys [cut]}]] (some #{floor} cut)) (:steps (line plan))))))


(defn approach
  "The line to cut to target (the plan map, see fits) from rules input in (without :cell and :feet), the body at
  feet; else {:reason ...} (see the doc), with :headings when the headings fail for different reasons. Only is a heading
  to take, else all four are tried."
  ([in target feet max-length accept] (approach in target feet max-length accept nil))
  ([in target feet max-length accept only]
  (if (and (nil? (:zones in)) (not (:ignore-zones? in)))
    {:reason :no-zones}
    (let [per (into {} (map (fn [h] [h (best-on-heading in target h max-length accept feet)])) (if only [only] heading-order))
          plans (distinct (mapcat (fn [h] (keep identity [(:plan (per h)) (:near (per h)) (:level (per h))])) (keys per)))]
      (if (seq plans)
        (first (sort-by (juxt #(not (level-from? % feet)) #(far-entry? % feet) :length #(cuts-floor? % feet) #(dist2 feet (:entry %))) plans))
        (let [stops (into {} (map (fn [[h r]] [h (:stop r)])) per)
              real (remove #(= :too-far (:reason %)) (vals stops))
              reasons (set (map :reason real))]
          (cond
            (empty? real) {:reason :too-far}
            (= 1 (count reasons)) (first real)
            :else {:reason :no-approach :headings (into {} (map (fn [[h s]] [h (:reason s)])) stops)})))))))

(defn leg-target
  "The waypoint of the first leg of a target far from feet underground. Diagonal: on the body's row, one past the
  target's column, so the first leg stands in that column; the shorter axis goes first (swap? takes the longer axis
  first). On the body's row or column but farther than max-length: max-length along it. nil when the body stands on
  the surface or a leg is longer than max-length."
  [block-at [fx fy fz :as feet] [tx _ tz] max-length swap?]
  (let [dx (- tx fx) dz (- tz fz)
        x-first? (not= (boolean swap?) (<= (js/Math.abs dx) (js/Math.abs dz)))
        [a b] (if x-first? [dx dz] [dz dx])]
    (when (not= fy (surface block-at [fx fz] (- fy 1) (+ fy max-length 1)))
      (cond
        (and (zero? dz) (> (js/Math.abs dx) max-length)) [(+ fx (* max-length (js/Math.sign dx))) fy fz]
        (and (zero? dx) (> (js/Math.abs dz) max-length)) [fx fy (+ fz (* max-length (js/Math.sign dz)))]
        (and (not (zero? dx)) (not (zero? dz)) (<= (inc (js/Math.abs a)) max-length) (<= (js/Math.abs b) max-length))
        (if x-first?
          [(+ tx (js/Math.sign dx)) fy fz]
          [fx fy (+ tz (js/Math.sign dz))])))))
