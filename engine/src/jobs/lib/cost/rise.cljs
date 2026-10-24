(ns jobs.lib.cost.rise
  "How far up the sky is for a body that has seen none, from what it knows. Pure."
  (:require [engine.args :as a]
            [engine.settings :as settings]))

(a/defargs settings
  {::surface-prior {:default [[2 2] [10 1] [14 1] [18 4] [22 135] [26 152] [30 65] [34 19]
                             [38 44] [42 113] [46 221] [50 854] [54 841] [58 3063] [62 352690] [66 215919]
                             [70 160457] [74 60515] [78 36919] [82 20368] [86 17932] [90 20194] [94 24568] [98 14608]
                             [102 18933] [106 10741] [110 13034] [114 10508] [118 10193] [122 9222] [126 6327] [130 3012]
                             [134 2628] [138 1271] [142 1329] [146 831] [150 5536] [154 1304] [158 999] [162 543]
                             [166 227] [170 562] [174 86] [178 47] [182 103] [186 111] [190 2] [202 1046]
                             [206 6]]
                    :doc "The overworld's ground height over a spot nothing is known of, as [y weight] pairs: the y a body stands at (one over the highest block that is no air, plant or tree; water counts) in 4-block bins named by their middle, weighted by the columns counted (1028352 columns of 4017 recorded chunk files, every 20th, the fixture plot grid left out; 2026-10-24). Recount: node tools/event-counts.mjs surface-heights --stride 20. The recorded land is where the trial bodies went."
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
