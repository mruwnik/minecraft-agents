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
