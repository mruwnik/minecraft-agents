(ns jobs.gather.mine.way-set
  "The :ways set of jobs.gather.mine and jobs.gather.dig-for: which ways the dig may use to get the item. A way is also off when its limit is 0.")

(def ordered [:seen :descend-stair :descend-shaft :strip-tunnel])
(def all (set ordered))

(defn ways [args] (or (:ways args) all))

(defn seen? [args] (contains? (ways args) :seen))
(defn stair? [args] (contains? (ways args) :descend-stair))
(defn shaft? [args] (contains? (ways args) :descend-shaft))
(defn tunnel? [args] (and (contains? (ways args) :strip-tunnel) (pos? (:tunnel-length args))))
(defn descends? [args] (or (stair? args) (shaft? args)))

(defn stay-level?
  "A run that may tunnel but never go down keeps the level it starts at: seen ore only beside the tunnel, breaches sealed."
  [args]
  (and (contains? (ways args) :strip-tunnel) (not (descends? args))))
