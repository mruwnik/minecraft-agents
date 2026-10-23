(ns jobs.lib.built
  "The cells this body placed a block in, in body memory (one durable entry, the newest max-cells kept): a pen it fenced by hand
  is its own whatever the zones say. jobs.lib.tidy/place! writes it on every placed block; the scaffold ledger holds only
  temporary blocks, which a cleanup takes back, so a kept build cannot live there."
  (:require [engine.ctx :as ctx]
            [engine.memory :as mem]))

(def kind :built-cells)

(def policy {:cap 1 :ttl :forever :durable? true})

(def max-cells "Most placed cells kept; the oldest go first." 4000)

(defn cells
  "The set of [x y z] cells placed, from a body-memory view {:data :now} (ctx/view in a job)."
  [view]
  (set (:cells (:data (mem/latest view kind)))))

(defn remember!
  "Add cell [x y z] to the cells placed, from a job's round."
  [c cell]
  (let [old (:cells (:data (mem/latest (ctx/view c) kind)))]
    (when-not (some #{cell} old)
      (ctx/remember! c kind {:cells (vec (take-last max-cells (conj (vec old) cell)))} policy))))
