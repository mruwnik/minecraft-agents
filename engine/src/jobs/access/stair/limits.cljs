(ns jobs.access.stair.limits
  "The settings the stair steps read (keys under :jobs.access.stair.limits/), which tunnel runs too."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.access.rules :as rules]))

(a/defargs settings
  {:jobs.access.stair.limits/max-cell-digs {:default 3 :doc "Digs of one cell before a stair, tunnel run or clear-path stops :refills." :spec (a/int-in 0 nil)}
   :jobs.access.stair.limits/max-fall-digs {:default 12 :doc "Digs of one cell a falling block (gravel, sand) keeps refilling before the stair stops :refills." :spec (a/int-in 0 nil)}
   :jobs.access.stair.limits/bridge-depth {:default 2 :doc "How many cells back along a stair down a missing floor may be bridged from a solid cell (the cave under its last floor)." :spec (a/int-in 0 nil)}
   :jobs.access.stair.limits/dig-plugs? {:default false :doc "A cell this body plugged against water may be dug (the stair stops :plugged otherwise)." :spec boolean?}
   :jobs.access.stair.limits/take-back-floors? {:default true :doc "A floor block a go-to stair (:note) places goes into the scaffold ledger for jobs.access.cleanup to take back; false keeps it." :spec boolean?}})

(defn max-cell-digs [] (settings/get settings :jobs.access.stair.limits/max-cell-digs))
(defn max-fall-digs [] (settings/get settings :jobs.access.stair.limits/max-fall-digs))
(defn max-digs
  "The digs of one cell block may take before a call stops :refills (stair, tunnel run, clear-path)."
  [block]
  (if (rules/falling? block) (max-fall-digs) (max-cell-digs)))
(defn bridge-depth [] (settings/get settings :jobs.access.stair.limits/bridge-depth))
(defn take-back-floors? [] (settings/get settings :jobs.access.stair.limits/take-back-floors?))
(defn dig-plugs? [] (settings/get settings :jobs.access.stair.limits/dig-plugs?))
