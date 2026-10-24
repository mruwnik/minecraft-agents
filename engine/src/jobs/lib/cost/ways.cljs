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
   ::fill-place-s {:default 1.5 :doc "Seconds laying one carried block over a gap or a drop is reckoned to take (place, step)." :spec (a/num-in 0 nil)}
   ::fill-spent-s {:default 1 :doc "Seconds the one block a fill spends is reckoned to be worth (the block is not recovered)." :spec (a/num-in 0 nil)}
   ::fill-scarce {:default 2 :doc "A fill leaving fewer than this many blocks carried adds ::fetch-block-s: the pack is nearly out." :spec (a/num-in 0 nil)}
   ::seal-place-s {:default 3 :doc "Seconds being sealed in is reckoned to cost on top of digging out (turn, step to the wall, climb out)." :spec (a/num-in 0 nil)}
   ::fetch-block-s {:default 3 :doc "Seconds one block the pack lacks for a pillar or a bridge adds (fetching it)." :spec (a/num-in 0 nil)}})

(defn stair-step-s [] (settings/get settings ::stair-step-s))

(defn stair-cost "Seconds a stair of `steps` steps takes." [steps] (* steps (stair-step-s)))

(defn stair-leg-cost
  "Seconds one step up of a stair takes when walking it would take `s0` (a walk step of `step-s`, plus its cuts and unseen
  cells): the cuts are paid on top, and the step itself is at least `stair-s`."
  [s0 step-s stair-s]
  (+ (- s0 step-s) (max step-s stair-s)))

(defn pillar-cost
  "Seconds a pillar of `blocks` blocks takes, `short` of them to be fetched."
  ([blocks] (pillar-cost blocks 0))
  ([blocks short] (+ (* blocks (settings/get settings ::pillar-block-s)) (* short (settings/get settings ::fetch-block-s)))))

(defn bridge-cost
  "Seconds a bridge of `blocks` blocks takes, `short` of them to be fetched."
  ([blocks] (bridge-cost blocks 0))
  ([blocks short] (+ (* blocks (settings/get settings ::bridge-block-s)) (* short (settings/get settings ::fetch-block-s)))))

(defn fill-cost
  "Seconds one carried block laid over a gap or a drop takes, with `carried` blocks of the kind in the pack: the placing,
  the block spent, and a fetch when the pack is nearly emptied."
  [carried]
  (+ (settings/get settings ::fill-place-s) (settings/get settings ::fill-spent-s)
     (if (< (dec carried) (settings/get settings ::fill-scarce)) (settings/get settings ::fetch-block-s) 0)))

(defn seal-cost
  "Seconds being sealed in by a place costs: digging out through the cheapest of `dig-seconds` (what each wall block takes
  with the carried kit), plus ::seal-place-s; nil when there is none (a real trap)."
  [dig-seconds]
  (when-let [dig (some->> dig-seconds seq (apply min))]
    (+ dig (settings/get settings ::seal-place-s))))
