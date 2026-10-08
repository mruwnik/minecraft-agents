(ns jobs.lib.breath
  "What the body's head and feet cells hold, shared by the suffocating and wedged triggers and their jobs
  (jobs.survival.breathe, jobs.survival.unwedge). Sensing only.

  Two ways to suffocate:
  - :drowning: in water, oxygen below :min-oxygen, and the head cell is not
    air (a swimmer at the surface breathes and is not drowning).
  - :enclosed: the cell holding the eyes (feet y + 1.62) holds a block whose
    collision shape fills the cell (blockAt's :fullCube), as in Minecraft.
    Air, fluids, plants, slabs and the like never do. An unloaded cell
    (blockAt null) is read as not enclosed, never as a reason to dig.

  Wedged: the feet cell holds such a full block, e.g. sand that fell on the body. Farmland, slabs, snow layers, soul
  sand, paths, plants and fluids never do. The eye cell is the suffocating trigger's business."
  (:require [engine.game :as game]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.dig-look :as look]
            [jobs.lib.util :as u]))

(def default-min-oxygen 12)

(defn air? [block-name] (contains? #{"air" "cave_air" "void_air"} block-name))


(defn eye-cell
  "The cell holding the eyes of a body whose feet are at self's pos. The feet
  are not at an integer y on farmland, slabs, paths or soul sand."
  [self]
  {:x (js/Math.floor (.. self -pos -x))
   :y (js/Math.floor (+ (.. self -pos -y) game/eye-height))
   :z (js/Math.floor (.. self -pos -z))})

(defn suffocates-here?
  "Whether the cell the body touches (its eye or feet cell) fills with a block, as it feels: no raw read, any light."
  [p cell]
  (boolean (some-> (u/feel p cell) .-fullCube)))

(defn suffocates?
  "Whether the block at cell makes a head inside it suffocate: sensing reports
  its collision shape as filling the cell (:fullCube). Plants, slabs, fluids
  and air do not. An unloaded cell does not; a cell never seen counts as solid (not assumed safe)."
  [p cell]
  (let [b (u/sensed p cell)]
    (boolean (and b (or (true? (.-unknown b)) (.-fullCube b))))))

(defn head-under?
  "Whether the body is in water with its eye cell not air (an unfelt cell counts as water): its breath runs."
  [p]
  (let [self (.self p)]
    (boolean (and (.-inWater self) (not (air? (or (u/feel-name p (eye-cell self)) "water")))))))

(defn situation
  "Why the body at p is suffocating, :drowning or :enclosed, or nil."
  [p min-oxygen]
  (let [self (.self p)]
    (cond
      (and (< (.-oxygen self) min-oxygen) (head-under? p)) :drowning
      (suffocates-here? p (eye-cell self)) :enclosed
      :else nil)))

(def defaults {:min-oxygen default-min-oxygen})

(def blocked-policy
  "Policy of the :unwedge-blocked entries written by jobs.survival.unwedge: one per cell it could not free."
  {:cap 10 :ttl (* 10 60 1000)})

(defn feet-cell
  "The cell holding the feet (a small epsilon keeps a body at y 63.9999 on a floor out of the floor block)."
  [self]
  {:x (js/Math.floor (.. self -pos -x))
   :y (js/Math.floor (+ (.. self -pos -y) 0.001))
   :z (js/Math.floor (.. self -pos -z))})

(defn in-water?
  "Whether the body is in water: it says so, or its feet cell feels like water (afloat at the surface)."
  [p]
  (let [self (.self p)]
    (boolean (or (.-inWater self) (= "water" (u/feel-name p (feet-cell self)))))))

(defn wedged-cell
  "The feet cell when its block fills the cell, else nil."
  [p]
  (let [cell (feet-cell (.self p))]
    (when (suffocates-here? p cell) cell)))

(defn only-feet-cell
  "wedged-cell, unless the eye cell is solid too (suffocating's business, breathe digs out); else nil."
  [p]
  (when-not (suffocates-here? p (eye-cell (.self p)))
    (wedged-cell p)))

(defn blocked-here?
  "Whether the job already reported cell as one it cannot free (a recent :unwedge-blocked entry): no refire then."
  [memory cell]
  (boolean (and memory (some #(= cell (:cell (:data %))) (mem/entries memory :unwedge-blocked)))))

(def swimmable
  "Fluid and water plants a body swims through as water: kelp, seagrass and an upward bubble column (see seen-water-name:
  a downward one drags to the floor and is never swum into)."
  #{"water" "kelp" "kelp_plant" "seagrass" "tall_seagrass" "bubble_column"})

(defn seen-water-name
  "u/seen-name, but a bubble column with drag true (it pulls down, over magma) reads as \"bubble_column_down\", which is
  not swimmable."
  [p cell]
  (let [b (u/seen-block p cell)
        drag? (and b (= "bubble_column" (.-name b))
                   (let [props (or (.-properties b) (some-> (.-getProperties b) (.call b)))]
                     (or (true? (some-> props .-drag)) (= "true" (some-> props .-drag str)))))]
    (if drag? "bubble_column_down" (some-> b .-name))))

(defn passable-water-or-air? [name]
  (or (air? name) (contains? swimmable name)))

(defn column-open?
  "No cell seen over the feet cell up to reach is a block: what is not seen is not a cap, so a drowning body may swim up it."
  [p x z fy reach]
  (every? (fn [k] (let [n (seen-water-name p {:x x :y (+ fy k) :z z})] (or (nil? n) (passable-water-or-air? n))))
          (range (inc (inc reach)))))

(defn columns
  "Column offsets within radius, the own column first, then by distance."
  [radius]
  (->> (for [dx (range (- radius) (inc radius))
             dz (range (- radius) (inc radius))
             :when (<= (+ (* dx dx) (* dz dz)) (* radius radius))]
         [dx dz])
       (sort-by (fn [[dx dz]] (+ (* dx dx) (* dz dz))))))

(defn drag-between?
  "A downward bubble column lies in a cell the straight walk from the feet column (fx, fz) to (x, z) crosses, at the feet's
  height or the one above (the target column itself included). A diagonal step crosses both cells beside it too: the
  body may cut either corner."
  [p fx fz x z fy]
  (let [n (max (js/Math.abs (- x fx)) (js/Math.abs (- z fz)))
        at (fn [i] [(js/Math.round (+ fx (* (- x fx) (/ i n)))) (js/Math.round (+ fz (* (- z fz) (/ i n))))])
        cells (mapcat (fn [i]
                        (let [[ax az] (at (dec i)) [bx bz :as b] (at i)]
                          (if (and (not= ax bx) (not= az bz)) [[bx az] [ax bz] b] [b])))
                      (range 1 (inc n)))]
    (boolean
     (some (fn [[cx cz]]
             (some #(= "bubble_column_down" (seen-water-name p {:x cx :y % :z cz})) [fy (inc fy)]))
           cells))))

(defn surface-in-column
  "The feet cell in column x z, at or above fy and within reach, whose head
  cell is air and which is reached through water or air only; nil if the
  column is capped or leaves the loaded world. name-at (default seen-water-name) reads the cells; a cell never seen ends the column."
  ([p x z fy reach] (surface-in-column p x z fy reach seen-water-name))
  ([p x z fy reach name-at]
   (loop [k 0]
     (when (<= k reach)
       (let [y (+ fy k)
             feet (name-at p {:x x :y y :z z})
             head (name-at p {:x x :y (inc y) :z z})]
         (cond
           (not (and feet head (passable-water-or-air? feet))) nil
           (air? head) {:x x :y y :z z}
           (contains? swimmable head) (recur (inc k))
           :else nil))))))

(defn nearest-air
  "The surface cell of the nearest column within radius that reaches air, not one of failed (a seq of cells); nil if none.
  Every column is read only through cells the body has seen or feels, never through stone. With straight? (the default)
  a column whose straight walk crosses a downward bubble column (drag-between?) is skipped: a raw move walks straight; a
  go-to leg passes false, as its planner routes round one."
  ([p self-pos radius reach] (nearest-air p self-pos radius reach nil true))
  ([p self-pos radius reach failed straight?]
   (let [fx (js/Math.floor (:x self-pos))
         fy (js/Math.floor (:y self-pos))
         fz (js/Math.floor (:z self-pos))
         skip (set failed)]
     (some (fn [[dx dz]]
             (let [cell (surface-in-column p (+ fx dx) (+ fz dz) fy reach)]
               (when-not (or (contains? skip cell) (and straight? (drag-between? p fx fz (+ fx dx) (+ fz dz) fy))) cell)))
           (columns radius)))))

(defn nearest-pocket
  "The nearest seen cell below the feet, within radius sideways and depth down, whose head cell is air and feet cell
  water or air (the way a drowning body came, e.g. its tunnel), not one of failed; nil if none. Only cells seen."
  [p self-pos radius depth failed]
  (let [fx (js/Math.floor (:x self-pos)) fy (js/Math.floor (:y self-pos)) fz (js/Math.floor (:z self-pos))
        skip (set failed)
        d2 (fn [{:keys [x y z]}] (+ (* (- x fx) (- x fx)) (* (- y fy) (- y fy)) (* (- z fz) (- z fz))))]
    (->> (for [[dx dz] (columns radius) dy (range 1 (inc depth))] {:x (+ fx dx) :y (- fy dy) :z (+ fz dz)})
         (filter #(and (not (contains? skip %))
                       (air? (seen-water-name p (update % :y inc)))
                       (passable-water-or-air? (seen-water-name p %))))
         (sort-by d2)
         first)))

(defn air-left-ms
  "Wall-clock ms a drowning body has before it dies: 15 ticks a point of oxygen, then 2 health a second."
  [self]
  (settings/ticks->ms (+ (* 15 (max 0 (.-oxygen self))) (* 10 (max 0 (or (.-health self) 20))))))

(defn ^:async look-column!
  "Look up the column x z from fy, one look per cell not yet seen, until a seen cell that is neither water nor air or
  reach cells are done (at most reach + 1 looks; with first-only? one look, at the first unseen cell). A look glances
  the cell and its neighbours, so cells in the dark are learned."
  [c x z fy reach first-only?]
  (loop [k 0]
    (when (<= k (inc reach))
      (let [cell {:x x :y (+ fy k) :z z}
            p (:primitives c)]
        (if (look/unknown? p [x (:y cell) z])
          (do (await (look/look-at! c [x (:y cell) z]))
              (when-not first-only? (recur (inc k))))
          (when (passable-water-or-air? (seen-water-name p cell))
            (recur (inc k))))))))

(defn ^:async find-air!
  "The nearest air target after looking: up the own column first, then, only when a block is seen over the own column,
  once at each other column in radius (the first cell not seen there); nil if none."
  [c pos]
  (let [{:keys [radius reach]} (:args c)
        p (:primitives c)
        fx (js/Math.floor (:x pos)) fy (js/Math.floor (:y pos)) fz (js/Math.floor (:z pos))]
    (await (look-column! c fx fz fy reach false))
    (or (nearest-air p pos radius reach)
        (when-not (column-open? p fx fz fy reach)
          (doseq [[dx dz] (rest (columns radius))]
              (await (look-column! c (+ fx dx) (+ fz dz) fy reach true)))
            (nearest-air p pos radius reach)))))
