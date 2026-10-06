(ns engine.triggers.scaffold-left
  "The scaffold-left trigger. It holds when a zone list has been read and the scaffold ledger (engine.access.ledger)
  has an open entry that:
    lies in a loaded cell
    belongs to a job that is no longer live (its job memory is gone: done, cancelled, discarded)
    no cleanup held in the last 10 minutes
  Runs jobs.access.cleanup, which takes exactly those entries.
  Persistence :stop: a cleanup that leaves entries holds them, so the condition turns false.
  It fires again only for new entries or once the hold has run out."
  (:require [engine.access.ledger :as ledger]
            [engine.jobs.util :as u]
            [engine.world :as world]))

(defn block-at-of [p] (fn [[x y z]] (u/block-name p {:x x :y y :z z})))

(defn scaffold-left
  [p memory _args kn]
  (boolean (and (some? (world/zones kn))
                (seq (ledger/offered memory (block-at-of p) nil)))))
