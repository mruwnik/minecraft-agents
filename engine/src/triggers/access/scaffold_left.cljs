(ns triggers.access.scaffold-left
  "The scaffold-left trigger. It holds when a zone list has been read and the scaffold ledger (jobs.lib.ledger)
  has an open entry that:
    lies in a loaded cell
    belongs to a job that is no longer live (its job memory is gone: done, cancelled, discarded)
    no cleanup held in the last 10 minutes
    within :near's :radius (32) of the body, so a far scaffold never drags the body back to it
    not deeper below the feet than jobs.lib.ledger/max-below, not within jobs.lib.ledger/died-radius of a place the body died, not roofed (a solid block within jobs.lib.ledger/roof-height above it) while the body is in the open (jobs.access.cleanup.step/walk-blocker)
  It holds only in the light (a retreat plug is taken back in the dark too: the body sits inside it, but not while it opens onto a dark cell, which cleanup keeps), past the dawn grace and with no known hostile near (jobs.lib.dawn/calm?), and not within
  jobs.lib.threats/fled-memory-ms of a flight, unless ::at-night / ::with-hostile are set: the night's seal is scaffold
  too, and the body is inside it.
  Runs jobs.access.cleanup, which takes exactly those entries.
  Persistence :stop: a cleanup that leaves entries holds them, so the condition turns false.
  It fires again only for new entries or once the hold has run out."
  (:require [jobs.lib.body :as body]
            [engine.args :as a]
            [engine.settings :as settings]
            [jobs.access.cleanup :as cleanup]
            [jobs.access.cleanup.step :as step]
            [jobs.lib.dawn :as dawn]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.reach :as reach]
            [jobs.lib.access :as access]
            [jobs.lib.shelter.keep :as stay]
            [jobs.lib.look :as look]
            [jobs.lib.threats :as threats]
            [jobs.lib.world-files :as world]))

(a/defargs settings
  {::at-night {:default false :doc "Whether the trigger may fire in the dark." :spec boolean?}
   ::with-hostile {:default false :doc "Whether the trigger may fire in the first light, while a known hostile is near, or just after a flight." :spec boolean?}})

(def defaults {:near {:radius 32}})

(defn block-at-of
  "Names only loaded cells: an unseen one is offered too (the run looks at it)."
  [p]
  (access/sensed-at p access/hidden-guess))

(defn walk-blocked?
  "Whether the entry e, out of the body's dig reach, is not worth a walk: far below, near a death, roofed (step/walk-blocker)."
  [p memory entries e]
  (boolean (and (> (step/distance (cleanup/eye-of {:primitives p}) (step/centre (:cell e))) (cleanup/dig-reach))
                (step/walk-blocker {:feet (reach/standing-vec p) :max-below (ledger/max-below) :roof-height (ledger/roof-height)
                               :died-near? (ledger/died-near-of memory) :block-at (access/sensed-at p access/hidden-guess)
                               :ledger (ledger/cells entries)}
                              e))))

(defn scaffold-left
  [p memory args kn]
  (let [dark-ok? (or (settings/get settings ::at-night) (not (look/dark-here? p)))
        opens-dark? (stay/opens-dark? p)
        plug-kept? #(and (stay/at-night?) (opens-dark? (:cell %)))
        entries (ledger/open-entries memory)]
    (boolean (and (not (body/recovery-pending? memory :scaffold-left))
                  (some? (world/zones kn))
                  (or (settings/get settings ::with-hostile)
                      (and (dawn/calm? p) (not (threats/fled-recently? p memory))))
                  (some #(and (not (walk-blocked? p memory entries %))
                              (or dark-ok? (and (= :retreat-plug (:purpose %)) (not (plug-kept? %)))))
                        (ledger/offered memory (block-at-of p) nil
                                        (ledger/near-of-args {:primitives p} args)))))))
