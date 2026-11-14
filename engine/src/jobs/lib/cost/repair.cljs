(ns jobs.lib.cost.repair
  "What mending the cells of a plan seen broken is worth: what having each cell was worth to whoever built it, and the use
  lost while it stays broken where a measured loss exists (a herd's escapes, a crop growing dry)."
  (:require [jobs.lib.cost.destroy :as destroy]
            [jobs.lib.cost.farm :as farm]
            [jobs.lib.cost.pen :as pen]
            [jobs.lib.cost.table :as table]
            [jobs.lib.crops :as crops]
            [plan.shape :as shape]))

(defn cell-worth
  "Seconds a broken cell ({:want :found}) was worth to its builder: a wanted block's put-back (destroy/put-back-s), a
  :clear cell's dig of what fills it (table/dig-cost)."
  [{:keys [want found]}]
  (if (= :clear want)
    (or (table/dig-cost found) 0)
    (destroy/put-back-s (shape/want-block want))))

(defn dry-loss
  "Seconds a crop loses growing on dry farmland instead of wet: sown-worth at the wet growth rate less at the dry."
  [crop]
  (- (farm/sown-worth crop (crops/stages-per-s true nil)) (farm/sown-worth crop (crops/stages-per-s false nil))))

(defn repair-worth
  "Seconds mending is worth: cell-worth of each broken cell of :cells; for each of :herds {:mob :target :gaps}, what the
  herd loses through its gaps (pen/gap-loss); for each crop of :dried (a planned crop left with no water), dry-loss."
  [{:keys [cells herds dried]}]
  (+ (reduce + 0 (map cell-worth cells))
     (reduce + 0 (map (fn [{:keys [mob target gaps]}] (pen/gap-loss mob target gaps)) herds))
     (reduce + 0 (map dry-loss dried))))
