(ns triggers.access.scaffold-left
  "The scaffold-left trigger. It holds when a zone list has been read and the scaffold ledger (jobs.lib.ledger)
  has an open entry that:
    lies in a loaded cell
    is past its :until (jobs.lib.ledger ::ttl-s after it was written; its placer takes it back itself when it ends)
    no cleanup held in the last 10 minutes
    within :near's :radius (32) of the body, so a far scaffold never drags the body back to it
    not deeper below the feet than jobs.lib.ledger/max-below, not where a remembered death not seen clear since counts (jobs.lib.death-spot/covers?), not roofed (a solid block within jobs.lib.ledger/roof-height above it) while the body is in the open (jobs.lib.ledger/walk-blocker)
  It holds only in the light (a retreat plug is taken back in the dark too: the body sits inside it, but not while it opens onto a dark cell, which cleanup keeps; so is scaffold in the body's own column under its feet: a body perched on its pillar comes down by digging under itself, a step at a time, whatever the light or its health), past the dawn grace and with no known hostile near (jobs.lib.dawn/calm?), and not within
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
            [jobs.lib.access :as access]
            [jobs.lib.shelter.keep :as stay]
            [jobs.lib.look :as look]
            [jobs.lib.threats :as threats]
            [jobs.lib.world-files :as world]))

(a/defargs settings
  {::at-night {:default false :doc "Whether the trigger may fire in the dark." :spec boolean?}
   ::with-hostile {:default false :doc "Whether the trigger may fire in the first light, while a known hostile is near, or just after a flight." :spec boolean?}})

(def defaults {:near {:radius 32} :checks {:safe? '(not (threatened)) :fit? '(armed)}})

(defn perched-on?
  "Whether the entry e lies in the column under the body's feet."
  [feet e]
  (let [[fx fy fz] feet [x y z] (:cell e)]
    (and (= [x z] [fx fz]) (< y fy))))

(defn block-at-of
  "Names loaded cells: an unseen one read by the measured prior (access/guessed-at)."
  [p]
  (access/guessed-at p))

(defn walk-blocked?
  "Whether the entry e, out of the body's dig reach, is not worth a walk: far below, near a death, roofed (ledger/walk-blocker). A roof is one the body has seen: the cells over a far block are seldom looked at, and the prior would read them as rock."
  [p died-near? entries e]
  (boolean (and (> (ledger/distance (ledger/eye-of {:primitives p}) (ledger/centre (:cell e))) (ledger/dig-reach))
                (ledger/walk-blocker {:feet (reach/standing-vec p) :max-below (ledger/max-below) :roof-height (ledger/roof-height)
                               :died-near? died-near? :block-at (access/sensed-at p "air")
                               :ledger (ledger/cells entries)}
                              e))))

(defn scaffold-left
  [p memory args kn]
  (let [dark-ok? (or (settings/get settings ::at-night) (not (look/dark-here? p)))
        opens-dark? (stay/opens-dark? p)
        plug-kept? #(and (stay/at-night?) (opens-dark? (:cell %)))
        entries (ledger/open-entries memory)
        feet (reach/standing-vec p)
        died-near? (ledger/died-near-of {:primitives p :view (constantly memory)})]
    (boolean (and (not (body/recovery-pending? memory :scaffold-left))
                  (some? (world/zones kn))
                  (or (settings/get settings ::with-hostile)
                      (and (not (dawn/hostile-known? p)) (not (threats/fled-recently? p memory))))
                  (some #(and (not (walk-blocked? p died-near? entries %))
                              (or dark-ok? (perched-on? feet %) (and (= :retreat-plug (:purpose %)) (not (plug-kept? %)))))
                        (ledger/offered memory (block-at-of p) nil
                                        (ledger/near-of-args {:primitives p} args)))))))
