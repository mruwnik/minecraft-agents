(ns jobs.lib.breath
  "What the body's head and feet cells hold, shared by the suffocating and wedged triggers and their jobs
  (jobs.survival.breathe, jobs.survival.unwedge). Sensing only.

  Two ways to suffocate:
  - :drowning: in water, the head cell not air (a swimmer at the surface breathes and is not drowning), and the air
    left plus the allowed drowning (::drown-hp) is less than the swim to the nearest air plus ::margin-s (see drowning?).
    Water Breathing and Conduit Power never drown.
  - :enclosed: the cell holding the eyes (feet y + 1.62) holds a block whose
    collision shape fills the cell (blockAt's :fullCube), as in Minecraft.
    Air, fluids, plants, slabs and the like never do. An unloaded cell
    (blockAt null) is read as not enclosed, never as a reason to dig.

  Wedged: the feet cell holds such a full block, e.g. sand that fell on the body. Farmland, slabs, snow layers, soul
  sand, paths, plants and fluids never do. The eye cell is the suffocating trigger's business."
  (:require [jobs.lib.access.rules :as rules]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.cost.planner :as planner]
            [engine.args :as a]
            [engine.game :as game]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.dig-look :as look]
            [jobs.lib.util :as u]))

(declare settings)

(def air? bs/air)

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

(defn walk-cells
  "The columns [x z] the straight walk from (fx, fz) to (x, z) crosses, the target included. A diagonal step crosses both
  cells beside it too: the body may cut either corner."
  [fx fz x z]
  (let [n (max (js/Math.abs (- x fx)) (js/Math.abs (- z fz)))
        at (fn [i] [(js/Math.round (+ fx (* (- x fx) (/ i n)))) (js/Math.round (+ fz (* (- z fz) (/ i n))))])]
    (mapcat (fn [i]
              (let [[ax az] (at (dec i)) [bx bz :as b] (at i)]
                (if (and (not= ax bx) (not= az bz)) [[bx az] [ax bz] b] [b])))
            (range 1 (inc n)))))

(defn drag-between?
  "A downward bubble column lies in a cell the straight walk from the feet column (fx, fz) to (x, z) crosses, at the feet's
  height or the one above (the target column itself included)."
  [p fx fz x z fy]
  (boolean
   (some (fn [[cx cz]]
           (some #(= "bubble_column_down" (seen-water-name p {:x cx :y % :z cz})) [fy (inc fy)]))
         (walk-cells fx fz x z))))

(defn walk-open?
  "Every cell the straight walk from (fx, fz) to (x, z) crosses is seen water or air at the feet's height and the one
  above: a window or a wall between the columns is no way there."
  [p fx fz x z fy]
  (every? (fn [[cx cz]]
            (every? #(passable-water-or-air? (seen-water-name p {:x cx :y % :z cz})) [fy (inc fy)]))
          (walk-cells fx fz x z)))

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

(def neighbours rules/neighbour-deltas)

(defn joined-cells
  "The seen water and air cells joined to the feet by a walk through such cells, at most radius sideways of the feet and
  from one below to reach above."
  [p fx fy fz radius reach]
  (let [open? (fn [{:keys [x y z] :as cell}]
                (and (<= (js/Math.abs (- x fx)) radius) (<= (js/Math.abs (- z fz)) radius) (<= (dec fy) y (+ fy reach))
                     (passable-water-or-air? (seen-water-name p cell))))
        start {:x fx :y fy :z fz}]
    (loop [frontier [start] seen #{start}]
      (if (empty? frontier)
        seen
        (let [next (->> (for [c frontier [dx dy dz] neighbours] (-> c (update :x + dx) (update :y + dy) (update :z + dz)))
                        (remove seen)
                        distinct
                        (filter open?))]
          (recur next (into seen next)))))))

(defn nearest-air
  "The surface cell of the nearest column within radius that reaches air, not one of failed (a seq of cells); nil if none.
  Every column is read only through cells the body has seen or feels, never through stone, and joined to the body by seen
  water and air. With straight? (the default) a column whose straight walk crosses a downward bubble column
  (drag-between?) or a cell that is not seen water or air (walk-open?) is skipped: a raw move walks straight; a go-to
  leg passes false, as its planner routes round one."
  ([p self-pos radius reach] (nearest-air p self-pos radius reach nil true))
  ([p self-pos radius reach failed straight?]
   (let [fx (js/Math.floor (:x self-pos))
         fy (js/Math.floor (:y self-pos))
         fz (js/Math.floor (:z self-pos))
         skip (set failed)
         joined (when-not straight? (joined-cells p fx fy fz radius reach))]
     (some (fn [[dx dz]]
             (let [cell (surface-in-column p (+ fx dx) (+ fz dz) fy reach)]
               (when-not (or (contains? skip cell)
                             (if straight?
                               (or (drag-between? p fx fz (+ fx dx) (+ fz dz) fy)
                                   (not (walk-open? p fx fz (+ fx dx) (+ fz dz) fy)))
                               (not (contains? joined cell))))
                 cell)))
           (columns radius)))))

(a/defargs settings
  {::margin-s {:default 3 :doc "Seconds of spare breath a body in water with its head submerged keeps on reaching air: it counts as drowning once its air left (plus ::drown-hp) is under the swim to air plus this." :spec (a/num-in 0 nil)}
   ::drown-hp {:default 0 :doc "Health the body may lose to drowning before it counts as drowning (an unusual job may allow some)." :spec (a/num-in 0 nil)}
   ::ticks-per-oxygen {:default 15 :doc "Game ticks a drowning body loses one point of oxygen in (the game's rule)." :spec (a/int-in 1 nil)}
   ::ticks-per-health {:default 10 :doc "Game ticks between the 2 health a drowning body loses once the oxygen is gone (the game's rule)." :spec (a/int-in 1 nil)}
   ::swim-ms {:default 500 :doc "Milliseconds a swimmer takes per block, to price a swim to a pocket of air." :spec (a/num-in 0 nil)}})

(defn swim-ms [] (settings/get settings ::swim-ms))

(defn effect? [self names]
  (boolean (some #(contains? names (.-name %)) (array-seq (.-effects self)))))

(defn air-left-s
  "Seconds of breath the body has: its oxygen at the helmet's drain, plus the helmet's grace (planner/air-profile)."
  [self]
  (let [{:keys [air-drain air-grace]} (planner/air-profile (.-equipment self) (array-seq (.-effects self)))]
    (+ (or air-grace 0)
       (/ (/ (settings/ticks->ms (* (settings/get settings ::ticks-per-oxygen) (max 0 (.-oxygen self)))) 1000)
          (or air-drain 1)))))

(defn drown-allowance-s
  "Seconds of drowning the body may take: ::drown-hp at 2 hp every ::ticks-per-health."
  []
  (/ (settings/ticks->ms (* (settings/get settings ::ticks-per-health) (/ (settings/get settings ::drown-hp) 2))) 1000))

(defn straight-up-reaches-air?
  "Whether the own column, seen water all the way, has air over it within steps cells: the usual lake, found without a flood."
  [p fx fy fz steps]
  (loop [k 0]
    (when (<= k steps)
      (let [cell {:x fx :y (+ fy k) :z fz}]
        (cond
          (not (passable-water-or-air? (seen-water-name p cell))) false
          (air? (seen-water-name p (update cell :y inc))) true
          :else (recur (inc k)))))))

(defn flood-reaches-air?
  "Whether a flood of at most steps blocks through seen water and air reaches a cell whose head cell is air."
  [p self-pos steps]
  (let [fx (js/Math.floor (:x self-pos)) fy (js/Math.floor (:y self-pos)) fz (js/Math.floor (:z self-pos))
        open? (fn [{:keys [x y z] :as cell}]
                (and (<= (js/Math.abs (- x fx)) steps) (<= (js/Math.abs (- y fy)) steps) (<= (js/Math.abs (- z fz)) steps)
                     (passable-water-or-air? (seen-water-name p cell))))
        start {:x fx :y fy :z fz}]
    (loop [frontier [start] seen #{start} k 0]
      (cond
        (empty? frontier) false
        (some #(air? (seen-water-name p (update % :y inc))) frontier) true
        (>= k steps) false
        :else (let [next (->> (for [c frontier [dx dy dz] neighbours] (-> c (update :x + dx) (update :y + dy) (update :z + dz)))
                              (remove seen)
                              distinct
                              (filter open?))]
                (recur next (into seen next) (inc k)))))))

(defn reaches-air-within?
  "Whether a swim of at most steps blocks through seen water and air reaches a cell whose head cell is air."
  [p self-pos steps]
  (or (straight-up-reaches-air? p (js/Math.floor (:x self-pos)) (js/Math.floor (:y self-pos)) (js/Math.floor (:z self-pos)) steps)
      (flood-reaches-air? p self-pos steps)))

(defn drowning?
  "Whether a body in water with its head under must go for air now: its air left plus the allowed drowning is less than
  the swim to the nearest seen air plus the margin. The search only goes as far as the air left allows, so it is short;
  air not seen counts as out of reach."
  [p]
  (let [self (.self p)]
    (and (head-under? p)
         (number? (.-oxygen self))
         (not (effect? self #{"water_breathing" "conduit_power"}))
         (let [budget-s (- (+ (air-left-s self) (drown-allowance-s)) (settings/get settings ::margin-s))]
           (or (neg? budget-s)
               (not (reaches-air-within? p {:x (.. self -pos -x) :y (.. self -pos -y) :z (.. self -pos -z)} (js/Math.floor (/ (* 1000 budget-s) (swim-ms))))))))))

(defn situation
  "Why the body at p is suffocating, :drowning or :enclosed, or nil."
  [p]
  (cond
    (drowning? p) :drowning
    (suffocates-here? p (eye-cell (.self p))) :enclosed
    :else nil))

(defn nearest-pocket
  "The nearest seen pocket of air below the feet: {:cell :steps}, the cell (head cell air, feet cell water or air, not
  one of failed) found by a walk through seen water and air cells from the feet, at most radius sideways, depth down
  and 3 up, steps cells long; nil if none is joined to the body that way. Only cells seen."
  [p self-pos radius depth failed]
  (let [fx (js/Math.floor (:x self-pos)) fy (js/Math.floor (:y self-pos)) fz (js/Math.floor (:z self-pos))
        skip (set failed)
        open? (fn [{:keys [x y z] :as cell}]
                (and (<= (js/Math.abs (- x fx)) radius) (<= (js/Math.abs (- z fz)) radius) (<= (- fy depth) y (+ fy 3))
                     (passable-water-or-air? (seen-water-name p cell))))
        pocket? (fn [cell] (and (< (:y cell) fy) (not (contains? skip cell)) (air? (seen-water-name p (update cell :y inc)))))
        start {:x fx :y fy :z fz}]
    (loop [frontier [start] seen #{start} steps 0]
      (when (seq frontier)
        (if-let [cell (first (filter pocket? frontier))]
          {:cell cell :steps steps}
          (let [next (->> (for [c frontier [dx dy dz] neighbours] (-> c (update :x + dx) (update :y + dy) (update :z + dz)))
                          (remove seen)
                          distinct
                          (filter open?))]
            (recur next (into seen next) (inc steps))))))))

(defn air-left-ms
  "Wall-clock ms a drowning body has before it dies: a point of oxygen lasts ::ticks-per-oxygen ticks, then 2 health
  every ::ticks-per-health."
  [self]
  (settings/ticks->ms (+ (* (settings/get settings ::ticks-per-oxygen) (max 0 (.-oxygen self)))
                         (* (settings/get settings ::ticks-per-health) (max 0 (or (.-health self) 20))))))

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
  [c pos radius reach]
  (let [p (:primitives c)
        fx (js/Math.floor (:x pos)) fy (js/Math.floor (:y pos)) fz (js/Math.floor (:z pos))]
    (await (look-column! c fx fz fy reach false))
    (or (nearest-air p pos radius reach)
        (when-not (column-open? p fx fz fy reach)
          (doseq [[dx dz] (rest (columns radius))]
              (await (look-column! c (+ fx dx) (+ fz dz) fy reach true)))
            (nearest-air p pos radius reach)))))

(defn side-cell
  "A horizontal neighbour of the feet cell where the body fits (feet and head
  cells passable and not harmful: lava, fire, cactus...) and can stand (the cell below is neither air, water, a water
  plant, a bubble column, lava nor magma); nil if none. Unloaded cells do not count."
  [p self]
  (let [fx (js/Math.floor (.. self -pos -x))
        fy (js/Math.floor (.. self -pos -y))
        fz (js/Math.floor (.. self -pos -z))
        fits? (fn [cell]
                (let [feet (seen-water-name p cell)
                      head (seen-water-name p (update cell :y inc))
                      below (seen-water-name p (update cell :y dec))]
                  (and feet head below
                       (not (suffocates? p cell)) (not (suffocates? p (update cell :y inc)))
                       (not (contains? bs/harms-in-cell feet)) (not (contains? bs/harms-in-cell head))
                       (not (air? below)) (not (contains? swimmable below)) (not (contains? #{"lava" "magma_block"} below))
                       (not-any? #{"bubble_column_down"} [feet head below]))))]
    (->> [[1 0] [-1 0] [0 1] [0 -1]]
         (map (fn [[dx dz]] {:x (+ fx dx) :y fy :z (+ fz dz)}))
         (filter fits?)
         first)))
