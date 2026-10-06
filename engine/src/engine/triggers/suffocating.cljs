(ns engine.triggers.suffocating
  "The suffocating trigger, and the sensing it shares with jobs.survival.breathe.

  Two situations, both read from sensing only:
  - :drowning: in water, oxygen below :min-oxygen, and the head cell is not
    air (a swimmer at the surface breathes and is not drowning).
  - :enclosed: the cell holding the eyes (feet y + 1.62) holds a block whose
    collision shape fills the cell (blockAt's :fullCube), as in Minecraft.
    Air, fluids, plants, slabs and the like never do. An unloaded cell
    (blockAt null) is read as not enclosed, never as a reason to dig."
  (:require [engine.jobs.util :as u]))

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

(defn suffocating
  "Holds when situation says the body is drowning or enclosed.
  :min-oxygen (default 12 of 20) is the drowning threshold; the job's own :min-oxygen is set in the entry's :job spec.
  A danger reflex: cooldown 0 and breathe has no backoff, so a spent or declined job fires again at once while the
  danger lasts."
  [world _memory args]
  (some? (situation world (:min-oxygen args default-min-oxygen))))
