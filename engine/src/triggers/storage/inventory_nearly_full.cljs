(ns triggers.storage.inventory-nearly-full
  "The inventory-nearly-full trigger: few free slots are left."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::free {:default 2 :doc "Empty main and hotbar slots at or below which the inventory counts as nearly full." :spec (a/int-in 0 36)}})
(defn nearly-full-free [] (settings/get settings ::free))

(def defaults {})

(defn inventory-nearly-full
  "Holds when at most :free (args, default 2) of the 36 main and hotbar slots are empty.
  make-room puts things in a known chest, else tosses the least valuable stacks.
  The 120 s cooldown stops a body with nothing it may toss from retrying every tick."
  [world _memory args]
  (<= (u/free-slots world) (:free args (nearly-full-free))))
