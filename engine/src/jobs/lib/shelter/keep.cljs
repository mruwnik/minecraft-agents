(ns jobs.lib.shelter.keep
  "Whether a shelter's block stays: a shelter or retreat-plug block opens onto a dark cell (cleanup keeps it, the scaffold-left trigger skips it, blocks.dig waits for the light)."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.shelter.sense :as sh-sense]))

(a/defargs settings
  {::at-night? {:default true :doc "A shelter's block (:shelter, :retreat-plug) stays while it opens onto a dark cell; false takes it back anyway." :spec boolean?}})

(defn at-night? [] (settings/get settings ::at-night?))

(def shelter-purposes #{:shelter :retreat-plug})

(defn opens-dark?
  "A fn of a cell: whether a neighbour of it is not solid and dark (sh-sense/lit-here?: a cell the body has not seen by
  the guess), unless the light reaches the
  body's own cell while it is not under a roof: a half-opened seal shows the far side is lit, and a body standing in the
  light outside an emptied shelter has no use for its dark inside. The body's own two cells do not
  count: the pocket it sealed itself into is dark by the seal, not by what lies outside."
  [p]
  (let [own (sh-sense/feet p)
        own? #{own (update own :y inc)}
        own-lit? (sh-sense/lit-here? p own)
        sheltered? (sh-sense/roofed? p 2)
        dark? (fn [n] (if (sh-sense/seen-cell? p n)
                        (and (not (sh-sense/lit-here? p n)) (or sheltered? (not own-lit?)))
                        (and (not own-lit?) (not (sh-sense/lit-here? p n)))))]
    (fn [[x y z]]
      (boolean (some (fn [[dx dy dz]]
                       (let [n {:x (+ x dx) :y (+ y dy) :z (+ z dz)}]
                         (and (not (own? n)) (not (sh-sense/solid-at? p n)) (dark? n))))
                     [[1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [0 1 0] [0 -1 0]])))))

(defn kept-tonight
  "The body's open ledger entry of a shelter's block at cell [x y z] that no dig may take while the night lasts: the sky
  dark outside, the body awake, the block opening onto a dark cell (opens-dark?). Else nil."
  [p view cell]
  (when (and (at-night?) (sh-sense/dark-outside? p) (not (sh-sense/sleeping? p)))
    (when-let [e (first (filter #(and (= cell (:cell %)) (shelter-purposes (:purpose %))) (ledger/open-entries view)))]
      (when ((opens-dark? p) cell) e))))
