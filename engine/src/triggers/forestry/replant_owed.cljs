(ns triggers.forestry.replant-owed
  "The replant-owed trigger: a replant debt (:forestry/replant, written when a tree is felled) whose sapling the body
  carries. Its job, jobs.forestry.plant-sapling with the carried saplings only, plants the oldest such debt, then the
  next, until none is left. A debt whose sapling is not carried waits until one is: the trigger never sends the body
  to fetch."
  (:require [engine.memory :as mem]
            [jobs.lib.body :as body]
            [jobs.lib.trees :as trees :refer [replant-kind]]
            [jobs.lib.util :as u]))

(def defaults {})

(defn replant-owed
  "Holds alive, on the ground, while a replant debt's sapling is carried."
  [p view _args _kn _live]
  (boolean
   (and (not (body/recovery-pending? view :replant-owed))
        (not (false? (.-onGround (.self p))))
        (seq (trees/with-sapling (map :data (mem/entries view replant-kind)) (u/inventory p))))))
