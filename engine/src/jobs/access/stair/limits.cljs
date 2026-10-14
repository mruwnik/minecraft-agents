(ns jobs.access.stair.limits
  "The settings the stair steps read (keys stay under :jobs.access.stair/), which tunnel runs too."
  (:require [engine.args :as a]
            [engine.settings :as settings]))

(a/defargs settings
  {:jobs.access.stair.limits/max-cell-digs {:default 3 :doc "Digs of one cell before the stair stops :refills." :spec (a/int-in 0 nil)}
   :jobs.access.stair.limits/max-fall-digs {:default 12 :doc "Digs of one cell a falling block (gravel, sand) keeps refilling before the stair stops :refills." :spec (a/int-in 0 nil)}
   :jobs.access.stair.limits/bridge-depth {:default 2 :doc "How many cells back along a stair down a missing floor may be bridged from a solid cell (the cave under its last floor)." :spec (a/int-in 0 nil)}
   :jobs.access.stair.limits/take-back-floors? {:default true :doc "A floor block a go-to stair (:note) places goes into the scaffold ledger for jobs.access.cleanup to take back; false keeps it." :spec boolean?}})

(defn max-cell-digs [] (settings/get settings :jobs.access.stair.limits/max-cell-digs))
(defn max-fall-digs [] (settings/get settings :jobs.access.stair.limits/max-fall-digs))
(defn bridge-depth [] (settings/get settings :jobs.access.stair.limits/bridge-depth))
(defn take-back-floors? [] (settings/get settings :jobs.access.stair.limits/take-back-floors?))
