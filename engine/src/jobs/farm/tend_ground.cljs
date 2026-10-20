(ns jobs.farm.tend-ground
  "Whether a ground cell of a field wants tilling or sits under a block."
  (:require [jobs.lib.block-sets :as bs]
            [jobs.farm.till :as till]))

(defn untilled?
  "A ground cell {:name :above} of dirt or grass with nothing but air or ground cover over it."
  [{:keys [name above]}]
  (boolean (and (till/tillable name)
                (or (bs/air above) (till/ground-cover above)))))

(defn covered?
  "A ground cell {:name :above} of dirt or grass with a block over it that is neither air nor ground cover."
  [{:keys [name above]}]
  (boolean (and (till/tillable name) above
                (not (bs/air above)) (not (till/ground-cover above)))))
