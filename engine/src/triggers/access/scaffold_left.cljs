(ns triggers.access.scaffold-left
  "The scaffold-left trigger. It holds when a zone list has been read and the scaffold ledger (jobs.lib.ledger)
  has an open entry that:
    lies in a loaded cell
    belongs to a job that is no longer live (its job memory is gone: done, cancelled, discarded)
    no cleanup held in the last 10 minutes
    within :within (32) blocks of the body (cleanup's :within), so a far scaffold never drags the body back to it
    not deeper below the feet than jobs.lib.ledger/max-below: cleanup would only hold it again
  It holds only by day, past the dawn grace and with no known hostile near (jobs.lib.dawn/calm?), and not within
  jobs.lib.threats/fled-memory-ms of a flight, unless ::at-night / ::with-hostile are set: the night's seal is scaffold
  too, and the body is inside it.
  Runs jobs.access.cleanup, which takes exactly those entries.
  Persistence :stop: a cleanup that leaves entries holds them, so the condition turns false.
  It fires again only for new entries or once the hold has run out."
  (:require [jobs.lib.body :as body]
            [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.dawn :as dawn]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter :as sh]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]
            [jobs.lib.world-files :as world]))

(a/defargs settings
  {::at-night {:default false :doc "Whether the trigger may fire at night." :spec boolean?}
   ::with-hostile {:default false :doc "Whether the trigger may fire in the first light, while a known hostile is near, or just after a flight." :spec boolean?}})

(def defaults {:within 32})

(defn block-at-of [p] (fn [[x y z]] (u/block-name p {:x x :y y :z z})))

(defn too-deep?
  "Whether the entry lies deeper below the feet than cleanup walks to."
  [p {[_ y _] :cell}]
  (> (- (second (reach/standing-vec p)) y) (ledger/max-below)))

(defn scaffold-left
  [p memory args kn]
  (boolean (and (not (body/recovery-pending? memory :scaffold-left))
                (some? (world/zones kn))
                (or (settings/get settings ::at-night) (not (sh/night? p)))
                (or (settings/get settings ::with-hostile)
                    (and (dawn/calm? p) (not (threats/fled-recently? memory))))
                (some (complement #(too-deep? p %))
                      (ledger/offered memory (block-at-of p) nil
                                      (ledger/near-of-args {:primitives p} args))))))
