(ns triggers.survival.wedged
  "The wedged trigger: the cell holding the feet holds a block whose collision shape fills the cell (blockAt's
  :fullCube), e.g. sand that fell on the body. Farmland, slabs, snow layers, soul sand, paths, plants and fluids
  never do, so a body standing on or in them is not wedged. The eye cell is the suffocating trigger's business
  (it is registered first and wins); an unloaded cell reads as not wedged.
  Sensing only: the block the body stands in, as a player feels it."
  (:require [engine.memory :as mem]))

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

(defn blocked-here?
  "Whether the job already reported cell as one it cannot free (a recent :unwedge-blocked entry): no refire then."
  [memory cell]
  (boolean (and memory (some #(= cell (:cell (:data %))) (mem/entries memory :unwedge-blocked)))))

(defn wedged
  "Holds when the feet cell holds a full block, unless unwedge recently found it cannot free that cell.
  A danger reflex: cooldown 0; the :unwedge-blocked entry (10 min) is what stops a refire flood."
  [world memory _args]
  (let [cell (wedged-cell world)]
    (and (some? cell) (not (blocked-here? memory cell)))))
