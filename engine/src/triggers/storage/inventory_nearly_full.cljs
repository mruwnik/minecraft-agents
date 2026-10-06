(ns triggers.storage.inventory-nearly-full
  "The inventory-nearly-full trigger: few free slots are left."
  (:require [jobs.lib.util :as u]))

(def nearly-full-free 2)

(def defaults {:free nearly-full-free})

(defn inventory-nearly-full
  "Holds when at most :free (args, default 2) of the 36 main and hotbar slots are empty.
  make-room puts things in a known chest, else tosses the least valuable stacks.
  The 120 s cooldown stops a body with nothing it may toss from retrying every tick."
  [world _memory args]
  (<= (u/free-slots world) (:free args nearly-full-free)))
