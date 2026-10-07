(ns triggers.survival.wedged
  "The wedged trigger: the cell holding the feet holds a block whose collision shape fills the cell (jobs.lib.breath).
  The eye cell is the suffocating trigger's business (it owns the body when both are solid); an unloaded cell reads
  as not wedged. Sensing only: the block the body stands in, as a player feels it."
  (:require [jobs.lib.breath :as breath]))

(defn wedged
  "Holds when the feet cell holds a full block, unless unwedge recently found it cannot free that cell.
  A danger reflex: cooldown 0; the :unwedge-blocked entry (10 min) is what stops a refire flood."
  [world memory _args]
  (let [cell (breath/only-feet-cell world)]
    (and (some? cell) (not (breath/blocked-here? memory cell)))))
