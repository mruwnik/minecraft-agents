(ns jobs.access.stair.limits
  "The settings the stair steps read (keys stay under :jobs.access.stair/), which tunnel runs too."
  (:require [engine.args :as a]
            [engine.settings :as settings]))

(a/defargs settings
  {:jobs.access.stair.limits/max-cell-digs {:default 3 :doc "Digs of one cell before the stair stops :refills." :spec (a/int-in 0 nil)}
   :jobs.access.stair.limits/max-fall-digs {:default 12 :doc "Digs of one cell a falling block (gravel, sand) keeps refilling before the stair stops :refills." :spec (a/int-in 0 nil)}
   :jobs.access.stair.limits/bridge-depth {:default 2 :doc "How many cells back along a stair down a missing floor may be bridged from a solid cell (the cave under its last floor)." :spec (a/int-in 0 nil)}
   :jobs.access.stair.limits/make-room? {:default true :doc "A stair whose pack has no room for the next drop runs a jobs.storage.make-room child (it tosses junk blocks, keeps building blocks); false waits :no-free-slot." :spec boolean?}
   :jobs.access.stair.limits/dig-plugs? {:default false :doc "A cell this body plugged against water may be dug (the stair stops :plugged otherwise)." :spec boolean?}
   :jobs.access.stair.limits/take-back-floors? {:default true :doc "A floor block a go-to stair (:note) places goes into the scaffold ledger for jobs.access.cleanup to take back; false keeps it." :spec boolean?}})

(defn max-cell-digs [] (settings/get settings :jobs.access.stair.limits/max-cell-digs))
(defn max-fall-digs [] (settings/get settings :jobs.access.stair.limits/max-fall-digs))
(defn bridge-depth [] (settings/get settings :jobs.access.stair.limits/bridge-depth))
(defn take-back-floors? [] (settings/get settings :jobs.access.stair.limits/take-back-floors?))
(defn make-room? [] (settings/get settings :jobs.access.stair.limits/make-room?))
(defn dig-plugs? [] (settings/get settings :jobs.access.stair.limits/dig-plugs?))
