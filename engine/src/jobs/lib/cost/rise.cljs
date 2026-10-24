(ns jobs.lib.cost.rise
  "How far up the sky is for a body that has seen none, from what it knows. Pure."
  (:require [engine.args :as a]
            [engine.settings :as settings]))

(a/defargs settings
  {::surface-prior {:default [[2 2] [10 1] [14 1] [18 4] [22 69] [26 127] [30 64] [34 17]
                             [38 42] [42 108] [46 197] [50 778] [54 792] [58 2729] [62 78729] [66 215653]
                             [70 160189] [74 60410] [78 36840] [82 20259] [86 17870] [90 20127] [94 24568] [98 14607]
                             [102 18933] [106 10739] [110 13033] [114 10502] [118 10171] [122 9222] [126 6324] [130 2989]
                             [134 2606] [138 1219] [142 1304] [146 821] [150 5534] [154 1115] [158 991] [162 538]
                             [166 225] [170 561] [174 86] [178 47] [182 103] [186 111] [190 2] [202 1046]
                             [206 6]]
                    :doc "The overworld's ground height over a spot nothing is known of, as [y weight] pairs: the y a body stands at (one over the highest block that is no air, plant or tree; a column whose ground is water or lava is left out) in 4-block bins named by their middle, weighted by the columns counted (1028352 columns of 4017 recorded chunk files, every 20th, all 256 columns of each, the fixture plot grid left out; 2026-10-24; the few columns below y 47 are real pits and ravines in complete chunks, kept). Recount: node tools/event-counts.mjs surface-heights --stride 20. The recorded land is where the trial bodies went."
                    :spec (a/coll-of (a/coll-of (a/num-in 0 nil)))}
   ::surface-floor {:default 1
                    :doc "A pseudo-count added to every 4-block bin from y 0 to 320 of ::surface-prior, so heights nobody recorded (mountains, 190-200, above 206) keep a little weight; weak against the million counted columns."
                    :spec (a/num-in 0 nil)}})

(defn surface-prior
  "[y weight] of the counted prior plus ::surface-floor on every bin."
  []
  (let [floor (settings/get settings ::surface-floor)
        counted (into {} (settings/get settings ::surface-prior))]
    (for [y (range 2 320 4)] [y (+ floor (get counted y 0))])))

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
