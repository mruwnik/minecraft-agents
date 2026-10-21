(ns jobs.lib.cost.ways
  "What go-to's ways that change the world cost in seconds, so it takes the cheapest: a stair's step (two cuts and a
  step), a block of a pillar or a bridge (place, step), a block missing from the pack (fetched). Digging is priced by
  jobs.lib.cost/dig-cost per block."
  (:require [engine.args :as a]
            [engine.settings :as settings]))

(a/defargs settings
  {::stair-step-s {:default 3 :doc "Seconds one step of a stair, up or down, is reckoned to take." :spec (a/num-in 0 nil)}
   ::pillar-block-s {:default 1.5 :doc "Seconds one block of a pillar is reckoned to take (jump, place)." :spec (a/num-in 0 nil)}
   ::bridge-block-s {:default 1.5 :doc "Seconds one block of a bridge is reckoned to take (place, step)." :spec (a/num-in 0 nil)}
   ::fetch-block-s {:default 3 :doc "Seconds one block the pack lacks for a pillar or a bridge adds (fetching it)." :spec (a/num-in 0 nil)}})

(defn stair-step-s [] (settings/get settings ::stair-step-s))

(defn stair-cost "Seconds a stair of `steps` steps takes." [steps] (* steps (stair-step-s)))

(defn pillar-cost
  "Seconds a pillar of `blocks` blocks takes, `short` of them to be fetched."
  ([blocks] (pillar-cost blocks 0))
  ([blocks short] (+ (* blocks (settings/get settings ::pillar-block-s)) (* short (settings/get settings ::fetch-block-s)))))

(defn bridge-cost
  "Seconds a bridge of `blocks` blocks takes, `short` of them to be fetched."
  ([blocks] (bridge-cost blocks 0))
  ([blocks short] (+ (* blocks (settings/get settings ::bridge-block-s)) (* short (settings/get settings ::fetch-block-s)))))
