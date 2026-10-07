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
  (:require [engine.memory :as mem]
            [jobs.lib.util :as u]))

(def default-min-oxygen 12)

(defn air? [block-name] (contains? #{"air" "cave_air" "void_air"} block-name))

(def eye-height 1.62)

(defn eye-cell
  "The cell holding the eyes of a body whose feet are at self's pos. The feet
  are not at an integer y on farmland, slabs, paths or soul sand."
  [self]
  {:x (js/Math.floor (.. self -pos -x))
   :y (js/Math.floor (+ (.. self -pos -y) eye-height))
   :z (js/Math.floor (.. self -pos -z))})

(defn suffocates?
  "Whether the block at cell makes a head inside it suffocate: sensing reports
  its collision shape as filling the cell (:fullCube). Plants, slabs, fluids
  and air do not. An unloaded cell does not."
  [p cell]
  (boolean (some-> (.blockAt p (clj->js cell)) .-fullCube)))

(defn situation
  "Why the body at p is suffocating, :drowning or :enclosed, or nil."
  [p min-oxygen]
  (let [self (.self p)
        head (u/block-name p (eye-cell self))]
    (cond
      (and (.-inWater self) (< (.-oxygen self) min-oxygen) (not (air? (or head "water")))) :drowning
      (suffocates? p (eye-cell self)) :enclosed
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
    (when (some-> (.blockAt p (clj->js cell)) .-fullCube) cell)))

(defn only-feet-cell
  "wedged-cell, unless the eye cell is solid too (suffocating's business, breathe digs out); else nil."
  [p]
  (when-not (suffocates? p (eye-cell (.self p)))
    (wedged-cell p)))

(defn blocked-here?
  "Whether the job already reported cell as one it cannot free (a recent :unwedge-blocked entry): no refire then."
  [memory cell]
  (boolean (and memory (some #(= cell (:cell (:data %))) (mem/entries memory :unwedge-blocked)))))
