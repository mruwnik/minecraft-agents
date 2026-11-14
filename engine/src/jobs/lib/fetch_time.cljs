(ns jobs.lib.fetch-time
  "The seconds fetching may take: the default :spend :job-time of a job that fetches (items.obtain, animals.leash,
  items.anvil-combine, items.enchant)."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]))

(a/defargs settings
  {::one-unit-s {:default 600 :doc "Seconds a fetch of one unit, or of one experience level, may run: the least a fetch of items gets." :spec (a/num-in 0 nil)}})

(defn one-unit-s [] (settings/get settings ::one-unit-s))

(defn items-s
  "Seconds a fetch of n (default 1) of any one of names may take: the least over them of its goal-value (the usual cost
  from nothing: measured gathers and the recipe tree, so an anvil counts its 31 iron) plus one sd of it (Q184), and at
  least one-unit-s: a fetch's own looks, walks and crafts beside the gathers are not in the measured spread."
  ([names] (items-s names 1))
  ([names n]
   (let [s (if (seq names) (apply min (map #(let [{:keys [value sd]} (cost/goal-value [{:name % :count (or n 1)}])] (+ value sd)) names)) 0)]
     (max (one-unit-s) (if (js/isFinite s) s 0)))))
