(ns jobs.lib.cost.rise
  "How far up the sky is for a body that has seen none, from what it knows. Pure."
  (:require [engine.args :as a]
            [engine.settings :as settings]))

(a/defargs settings
  {::surface-prior {:default [[63 0.25] [67 0.25] [72 0.25] [80 0.15] [95 0.10]]
                    :doc "The overworld's ground height over a spot nothing is known of, as [y weight] pairs (sea level 63; most land a few blocks above, hills and mountains a tail)."
                    :spec (a/coll-of (a/coll-of (a/num-in 0 nil)))}})

(defn surface-prior [] (settings/get settings ::surface-prior))

(defn expected-rise
  "Blocks up from feet y to the open sky: at least `least` (the rock seen in the way), and when `known` (a surface y the body
  knows: the top of its own stair, a sky-lit stand it remembers) up to that; else the mean over the surface prior of the
  rise to each height, none less than `least`."
  [y least known]
  (let [rise-to (fn [s] (max least (- s y)))]
    (if known
      (rise-to known)
      (let [prior (surface-prior)]
        (/ (transduce (map (fn [[s w]] (* w (rise-to s)))) + 0 prior)
           (transduce (map second) + 0 prior))))))
