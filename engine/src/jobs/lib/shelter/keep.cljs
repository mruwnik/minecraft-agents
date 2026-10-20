(ns jobs.lib.shelter.keep
  "Whether a shelter's block stays: a shelter or retreat-plug block opens onto a dark cell (cleanup keeps it, the scaffold-left trigger skips it)."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.look :as look]
            [jobs.lib.shelter.sense :as sh-sense]))

(a/defargs settings
  {::at-night? {:default true :doc "A shelter's block (:shelter, :retreat-plug) stays while it opens onto a dark cell; false takes it back anyway." :spec boolean?}})

(defn at-night? [] (settings/get settings ::at-night?))

(defn opens-dark?
  "A fn of a cell: whether a neighbour of it is not solid and dark: a cell the body has seen by its light, one it has not
  by the guess (the sky's darkening, or the body underground: unseen cave cells are dark)."
  [p]
  (let [buried? (sh-sense/buried-at? p (sh-sense/feet p))
        dark? (fn [n] (if (sh-sense/seen-cell? p n)
                        (not (sh-sense/lit-here? p n))
                        (or buried? (look/dark-guess? p))))]
    (fn [[x y z]]
      (boolean (some (fn [[dx dy dz]]
                       (let [n {:x (+ x dx) :y (+ y dy) :z (+ z dz)}]
                         (and (not (sh-sense/solid-at? p n)) (dark? n))))
                     [[1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [0 1 0] [0 -1 0]])))))
