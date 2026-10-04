(ns engine.triggers.scaffold-left
  "The scaffold-left trigger: holds while the scaffold ledger (engine.access.ledger) has an open entry in a loaded cell
  whose placing job is no longer live (its job memory is gone: done, cancelled, discarded) and that no cleanup held in
  the last 10 minutes, and a zone list has been read. Runs jobs.access.cleanup, which takes exactly those entries.
  Persistence :stop: a cleanup that leaves entries holds them, the condition turns false, and it fires again only for
  new ones or once the hold has run out."
  (:require [engine.access.ledger :as ledger]
            [engine.jobs.util :as u]
            [engine.world :as world]))

(defn block-at-of [p] (fn [[x y z]] (u/block-name p {:x x :y y :z z})))

(def trigger
  {:name :scaffold-left
   :when (fn [p memory _args kn]
           (boolean (and (some? (world/zones kn))
                         (seq (ledger/offered memory (block-at-of p) nil)))))
   :job '(jobs.access.cleanup)
   :args {}
   :persistence :stop})
