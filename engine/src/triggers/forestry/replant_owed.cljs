(ns triggers.forestry.replant-owed
  "The replant-owed trigger: a replant debt (:forestry/replant, written when a tree is felled) whose sapling the body
  carries. Its job, jobs.forestry.plant-sapling with the carried saplings only, plants the oldest such debt, then the
  next, until none is left. A debt whose sapling is not carried waits until one is: the trigger never sends the body
  to fetch. A debt whose cell is not loaded is no reason to act (the walk would end :not-loaded and refire): it waits
  until the body is near enough for the chunk to load, and for two minutes after plant-sapling failed there (a walk that ended :not-loaded, :unreachable ..).
  Nor does a debt where the body would be due home before night (jobs.lib.shelter/home-due-at?, setting
  jobs.lib.shelter/home-due): the walk home owns the dusk."
  (:require [engine.memory :as mem]
            [jobs.lib.body :as body]
            [jobs.lib.shelter :as sh]
            [jobs.lib.trees :as trees :refer [replant-kind]]
            [jobs.lib.util :as u]))

(defn replant-owed
  "Holds alive, on the ground, while a replant debt's sapling is carried, its cell is loaded and the body is not due home from it."
  [p view _args _kn _live]
  (boolean
   (and (not (body/recovery-pending? view :replant-owed))
        (not (false? (.-onGround (.self p))))
        (some #(and (u/block-at p (:pos %)) (not (sh/home-due-at? p view (:pos %)))) (trees/with-sapling (trees/without-stalled view (map :data (mem/entries view replant-kind))) (u/inventory p))))))
