(ns jobs.lib.goal
  "Where a walk that was cut is going: the :goal an open go-to keeps in its job memory (go-to drops :open and :goal when its
  walk ends), found in the memory of the other listed jobs."
  (:require [engine.args :as a]
            [engine.memory :as mem]))

(def pos-spec
  "The spec of a :goal arg: a position map or vector."
  (a/pred "{:x :y :z} or [x y z]" #(or (map? %) (vector? %))))

(def errands
  "The comfort errands' jobs (the trigger defaults at :comfort): their walks are not where the body was bound."
  #{"jobs.access.cleanup" "jobs.maintenance.shut-doors" "jobs.storage.make-room" "jobs.survival.restore-broken"
    "jobs.animals.shut-gate" "jobs.items.pick-up-loose" "jobs.movement.leave-vehicle"})

(defn open-goals
  "The :goal of every open go-to in a job memory map, its children included."
  [m]
  (concat (when (and (:open m) (:goal m)) [(:goal m)])
          (mapcat open-goals (vals (:children m)))))

(defn interrupted-goal
  "The goal of the most recently written job memory, other than root-id's own and an errands job's, that holds an open go-to; nil when none."
  [view root-id]
  (->> (keys (:entries (:data view)))
       (filter #(and (mem/job-kind? %) (not= % (mem/job-kind root-id))))
       (remove #(errands (:job (:data (mem/latest view %)))))
       (keep #(let [e (mem/latest view %)] (when-let [g (first (open-goals (:data e)))] [(:t e) g])))
       (sort-by first >)
       first
       second))
