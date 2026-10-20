(ns jobs.movement.go-to.column
  "go-to's goal y. A column goal ([x z] or {:x :z}, no y) stands on the surface of that column: its highest seen stand with
  no seen solid over it (trees skipped), why :surface; else (the column seen only under cover) the stand nearest the body's
  feet, why :nearest-level; over the whole build height. A column with no ground seen aims provisionally at an
  estimate (estimate: the seen ground of the nearest column around it within estimate-reach of the feet, else the feet
  y; seen open there down to an unseen cell: the lowest open cell over it, unseen-under), or, when the column is seen there
  and has no floor, keeps a stand-in y (the body's feet); go-to aims again every step, so it resolves from
  seen cells on approach; a provisional one it first walks to within approach-range of. A y with no standable cell within the
  range is re-aimed to its column's standable cell nearest it, at most :y-slack blocks off, never past a solid (leads-to?;
  seen cells, an unseen one from the world the plans read).

  Rules of go-to: :pos [x z] or {:x :z} (no y) walks to the seen ground of that
  column nearest the body's feet (unseen: first to within 8 of a height estimated from the seen columns around it, then looks); a y the planner finds no standable cell within :range of, or under :unknown a y seen in the ground with no seen stand within :range (asked before the first walk, pre-aim!), is re-aimed once to its column's
  ground nearest it, at most the y-slack setting (4) blocks off and never past a solid (info goal-resolved {:goal :to}); see jobs.movement.go-to.column. A
  column whose ground is not made out is looked at after a fruitless walk (at most 3 a call); with no stand in reach of its guess it walks to seen stands toward it (column/waypoint, off the line past a cliff), until ::max-waypoint-misses bring it no nearer. A cell over a short floor (farmland, a path, a bottom slab) stands for that
  block's own cell, where the body's feet are. :place (a name such as :home, set by jobs.memory.set-place) walks to
  that place's recorded position instead of :pos; a name not in memory falls back to the shared marker of that name (jobs.lib.world/marker)."
  (:require [jobs.lib.block-sets :as bs]
            [engine.args :as a]
            [jobs.lib.places :as places]
            [jobs.lib.reach :as reach]
            [jobs.lib.surface :as surface]
            [jobs.lib.util :as u]
            [jobs.movement.go-to.tuning :refer [off-line-cone off-line-radius]]))

(def approach-range
  "Blocks from a column's provisional cell go-to first walks to while no ground of it is seen, and looks: a guess out of
  reach (a floating block, a tree's leaves) does not stop it short of seeing the column."
  8)

(def top-y "The highest feet y a column goal looks for ground at (the head cell above is the build limit's last)." (- places/max-y 2))

(defn column-xz
  "[x z] of a column pos ([x z] or {:x :z} with no :y), else nil."
  [pos]
  (cond
    (and (sequential? pos) (= 2 (count pos))) (vec pos)
    (and (map? pos) (nil? (:y pos)) (contains? pos :x) (contains? pos :z)) [(:x pos) (:z pos)]))

(def pos-or-column?
  (a/pred "[x y z], {:x :y :z}, [x z] or {:x :z} of numbers (no y: the ground of that column)"
          #(or (some? (a/cell %)) (boolean (some->> (column-xz %) (every? a/finite?))))))

(defn parse
  "{:pos {:x :z}} (floored) for a column pos, else nil."
  [pos]
  (when-let [[x z] (column-xz pos)]
    (let [parsed (places/parse-pos [x 0 z])]
      (if (:reason parsed) parsed {:pos (dissoc (:pos parsed) :y)}))))

(defn kind
  "A block as a stand test reads it: :unknown (nil: unseen or unloaded), :hazard, else reach/kind-of."
  [b]
  (cond (nil? b) :unknown
        (bs/harms-in-cell (.-name b)) :hazard
        :else (reach/kind-of b)))

(defn surface-kind-at
  "As seen-kind-at, a log or leaf block read as :open: a tree is not the surface."
  [p]
  (if (some? (.-sensedAt p))
    (fn [x y z] (let [b (u/seen-block p {:x x :y y :z z})]
                  (if (and b (surface/canopy? (.-name b))) :open (kind b))))
    (constantly :unknown)))

(defn seen-kind-at
  "(fn [x y z]) -> kind of the block the body of primitives p sees or remembers there, :unknown when never seen (or when
  p senses nothing)."
  [p]
  (if (some? (.-sensedAt p))
    (fn [x y z] (kind (u/seen-block p {:x x :y y :z z})))
    (constantly :unknown)))

(defn kind-at
  "(fn [x y z]) -> as seen-kind-at; an unseen cell from the raw world (reach/lookup) when raw?."
  [p raw?]
  (let [seen (seen-kind-at p)
        raw (when raw? (reach/lookup p kind))]
    (if raw
      (fn [x y z] (let [k (seen x y z)] (if (keyword-identical? :unknown k) (raw x y z) k)))
      seen)))

(defn free? [k] (or (keyword-identical? :open k) (keyword-identical? :water k)))

(defn stands?
  "Feet and head cells open or water, and a solid floor under open feet."
  [kind-at x y z]
  (let [feet (kind-at x y z)]
    (and (free? feet) (free? (kind-at x (inc y) z))
         (or (keyword-identical? :water feet) (keyword-identical? :solid (kind-at x (dec y) z))))))

(defn ground
  "The y from lo to hi nearest y (the higher of two) where the column x z stands (and ok? of it, when given), else nil."
  ([kind-at x z y lo hi] (ground kind-at x z y lo hi (constantly true)))
  ([kind-at x z y lo hi ok?]
   (some (fn [d] (let [up (+ y d) down (- y d)]
                   (cond (and (<= up hi) (stands? kind-at x up z) (ok? up)) up
                         (and (pos? d) (>= down lo) (stands? kind-at x down z) (ok? down)) down)))
         (range 0 (inc (max (- hi y) (- y lo)))))))

(defn top
  "The y of the highest stand of the column x z (from lo to hi: default the build height) with no solid or hazard over it
  up to hi, else nil."
  ([kind-at x z] (top kind-at x z places/min-y top-y))
  ([kind-at x z lo hi]
   ;; one read per cell, as stands? reads them: the feet of y are the head of y - 1
   (loop [y hi head (kind-at x (inc y) z) feet (kind-at x y z)]
     (when (>= y lo)
       (let [floor (kind-at x (dec y) z)]
         (cond (and (free? feet) (free? head) (or (keyword-identical? :water feet) (keyword-identical? :solid floor))) y
               (#{:solid :hazard} feet) nil
               :else (recur (dec y) feet floor)))))))

(defn cached
  "kind-at reading each cell once: one aim reads the cells of a column over the whole build height, and again for its
  nearest ground."
  [kind-at]
  (let [m (js/Map.)]
    (fn [x y z]
      (let [k (str x "," y "," z)
            v (.get m k)]
        (if (undefined? v) (let [v (kind-at x y z)] (.set m k v) v) v)))))

(defn pick
  "[y why] of the stand of the column x z by the cells of kind-at: :surface, else :nearest-level to body-y; nil if none."
  [kind-at x z body-y]
  (if-let [y (top kind-at x z)]
    [y :surface]
    (when-let [y (ground kind-at x z body-y places/min-y top-y)] [y :nearest-level])))

(defn leads-to?
  "Whether the column x z leads from a goal at y to the stand at g without passing a solid: up only from a goal cell
  that is not free (a position in the ground: its surface), down only through free cells. An open cell under a ceiling
  (a sealed pocket, a cave) is never re-aimed onto the ground over it."
  [kind-at x z y g]
  (if (> g y)
    (not (free? (kind-at x y z)))
    (every? #(free? (kind-at x % z)) (range (inc g) y))))

(def estimate-reach "Blocks above and below the feet a neighbouring column's seen ground is looked for (estimate)." 32)

(defn estimate
  "The y an unseen column x z is guessed to stand at: the surface (else the seen ground nearest the feet) of the nearest
  column around it, looked for in 8 directions out to approach-range, else body-y; within estimate-reach of the feet, as
  64 full-height columns read per aim cost 100s of ms."
  [kind-at x z body-y]
  (or (some (fn [[d dx dz]] (let [cx (+ x (* d dx)) cz (+ z (* d dz)) lo (- body-y estimate-reach) hi (+ body-y estimate-reach)]
                              (or (top kind-at cx cz lo hi) (ground kind-at cx cz body-y lo hi))))
            (for [d (range 1 (inc approach-range)) [dx dz] [[1 0] [-1 0] [0 1] [0 -1] [1 1] [1 -1] [-1 1] [-1 -1]]] [d dx dz]))
      body-y))

(defn unseen-under
  "The guess y of column x z at y: y when unseen, else, seen open down to an unseen cell (its ground not seen yet), the
  lowest open cell over it; nil on a seen floor or none."
  [kind-at x z y]
  (loop [g y]
    (let [k (kind-at x g z)]
      (cond (keyword-identical? :unknown k) (min y (inc g))
            (and (free? k) (> g places/min-y)) (recur (dec g))))))

(defn waypoint
  "A cell to walk to toward the column x z from the body's feet [bx by bz] when its estimate has no stand in reach: the
  seen ground (nearest by, within estimate-reach; a head cell not seen counts as open, as a seen slope's air) at least
  gain blocks (default 2) nearer x z across than the body: of the column on the straight line from the body to x z
  nearest x z, else (a cliff face on the line, its top unseen) of any column within off-line-radius of the body and
  off-line-cone of the line, the one nearest x z; else nil. Stands in tried (feet cells) are skipped."
  ([kind-at x z feet] (waypoint kind-at x z feet 2 #{}))
  ([kind-at x z feet gain] (waypoint kind-at x z feet gain #{}))
  ([kind-at x z [bx by bz] gain tried]
   (let [h (js/Math.hypot (- x bx) (- z bz))
         ux (/ (- bx x) (max h 1))
         uz (/ (- bz z) (max h 1))
         ka (fn [cx cy cz] (let [k (kind-at cx cy cz)]
                             (if (and (keyword-identical? :unknown k) (free? (kind-at cx (dec cy) cz))) :open k)))
         stand (fn [[cx cz]] (when-let [g (ground ka cx cz by (- by estimate-reach) (+ by estimate-reach))]
                               (when-not (tried [cx g cz]) {:x cx :y g :z cz})))
         across (fn [[cx cz]] (js/Math.hypot (- x cx) (- z cz)))
         cos-max (js/Math.cos (* (off-line-cone) (/ js/Math.PI 180)))
         in-cone? (fn [[cx cz]] (let [dx (- cx bx) dz (- cz bz) d (js/Math.hypot dx dz)]
                                  (and (pos? d) (>= (/ (+ (* dx (- x bx)) (* dz (- z bz))) (* d (max h 1))) cos-max))))
         r (off-line-radius)]
     (or (some stand (distinct (for [k (range 1 (inc (- h gain)))] [(js/Math.floor (+ x 0.5 (* k ux))) (js/Math.floor (+ z 0.5 (* k uz)))])))
         (some stand (sort-by (juxt across (fn [[cx cz]] (js/Math.hypot (- cx bx) (- cz bz))))
                              (for [cx (range (- bx r) (+ bx r 1)) cz (range (- bz r) (+ bz r 1))
                                    :let [col [cx cz]]
                                    :when (and (<= (across col) (- h gain)) (in-cone? col))]
                                col)))))))

(defn aim
  "{:pos cell} go-to walks to for goal ({:x :y :z}, or {:x :z} for a column): :resolved true when the y was chosen here,
  :provisional too when a column's y is an estimate (its cell or floor there unseen). body-y: the body's feet y; slack:
  the :y-slack arg; raw?: whether unseen cells of a y goal's column read from the raw world (a column reads seen cells
  only). A y goal comes here only once the planner found no cell in its range stands."
  [p {:keys [x y z] :as goal} slack body-y raw?]
  (if (nil? y)
    (let [seen (cached (surface-kind-at p))]
      (if-let [[g why] (pick seen x z body-y)]
        {:pos {:x x :y g :z z} :resolved true :why why}
        (if-let [g (unseen-under seen x z (estimate seen x z body-y))]
          {:pos {:x x :y g :z z} :resolved true :provisional true}
          {:pos {:x x :y body-y :z z}})))
    (if-let [g (when (pos? slack) (let [ka (kind-at p raw?)] (ground ka x z y (- y slack) (+ y slack) #(leads-to? ka x z y %))))]
      {:pos (assoc goal :y g) :resolved true :why :explicit}
      {:pos goal})))
