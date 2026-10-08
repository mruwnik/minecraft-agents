(ns jobs.movement.go-to.column
  "go-to's goal y. A column goal ([x z] or {:x :z}, no y) stands on the ground of that column nearest the body's feet,
  read from seen cells only, over the whole build height. A column with no ground seen aims provisionally at an
  estimate (estimate: the seen ground of the nearest column around it, else the body's feet y), or, when the column is
  seen there and does not stand, keeps a stand-in y (the body's feet); go-to aims again every step, so it resolves from
  seen cells on approach; a provisional one it first walks to within approach-range of. A y with no standable cell within the
  range is re-aimed to its column's standable cell nearest it, at most :y-slack blocks off, never past a solid (leads-to?;
  seen cells, an unseen one from the world the plans read)."
  (:require [engine.args :as a]
            [jobs.lib.places :as places]
            [jobs.lib.reach :as reach]
            [jobs.lib.util :as u]))

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
        (reach/hazard-blocks (.-name b)) :hazard
        :else (reach/kind-of b)))

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
  "The y an unseen column x z is guessed to stand at: the seen ground nearest the feet (within estimate-reach) of the
  nearest column around it, looked for in 8 directions out to approach-range, else body-y."
  [kind-at x z body-y]
  (or (some (fn [[d dx dz]] (ground kind-at (+ x (* d dx)) (+ z (* d dz)) body-y (- body-y estimate-reach) (+ body-y estimate-reach)))
            (for [d (range 1 (inc approach-range)) [dx dz] [[1 0] [-1 0] [0 1] [0 -1] [1 1] [1 -1] [-1 1] [-1 -1]]] [d dx dz]))
      body-y))

(defn waypoint
  "A cell to walk to toward the column x z from the body's feet [bx by bz] when its estimate has no stand in reach: the
  seen ground (nearest by, within estimate-reach; a head cell not seen counts as open, as a seen slope's air) of the
  column on the straight line from the body to x z that is nearest x z and at least 2 blocks nearer it than the body,
  else nil."
  [kind-at x z [bx by bz]]
  (let [h (js/Math.hypot (- x bx) (- z bz))
        ux (/ (- bx x) (max h 1))
        uz (/ (- bz z) (max h 1))
        ka (fn [cx cy cz] (let [k (kind-at cx cy cz)]
                            (if (and (keyword-identical? :unknown k) (free? (kind-at cx (dec cy) cz))) :open k)))]
    (some (fn [[cx cz]] (when-let [g (ground ka cx cz by (- by estimate-reach) (+ by estimate-reach))] {:x cx :y g :z cz}))
          (distinct (for [k (range 1 (- h 1))] [(js/Math.floor (+ x 0.5 (* k ux))) (js/Math.floor (+ z 0.5 (* k uz)))])))))

(defn aim
  "{:pos cell} go-to walks to for goal ({:x :y :z}, or {:x :z} for a column): :resolved true when the y was chosen here,
  :provisional too when a column's y is an estimate (its cell or floor there unseen). body-y: the body's feet y; slack:
  the :y-slack arg; raw?: whether unseen cells of a y goal's column read from the raw world (a column reads seen cells
  only). A y goal comes here only once the planner found no cell in its range stands."
  [p {:keys [x y z] :as goal} slack body-y raw?]
  (if (nil? y)
    (let [seen (seen-kind-at p)]
      (if-let [g (ground seen x z body-y places/min-y top-y)]
        {:pos {:x x :y g :z z} :resolved true}
        (let [e (estimate seen x z body-y)]
          (if (or (keyword-identical? :unknown (seen x e z)) (keyword-identical? :unknown (seen x (dec e) z)))
            {:pos {:x x :y e :z z} :resolved true :provisional true}
            {:pos {:x x :y body-y :z z}}))))
    (if-let [g (when (pos? slack) (let [ka (kind-at p raw?)] (ground ka x z y (- y slack) (+ y slack) #(leads-to? ka x z y %))))]
      {:pos (assoc goal :y g) :resolved true}
      {:pos goal})))
