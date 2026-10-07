(ns jobs.farm.tend-stock
  "What jobs.farm.tend keeps carried and what it stores or composts: the lists of seeds and farm goods, the keep map,
  the surplus over it, and whether a ground cell wants tilling."
  (:require [jobs.lib.cost :as cost]
            [jobs.farm.till :as till]))

(def waste-seeds ["wheat_seeds" "beetroot_seeds" "melon_seeds" "pumpkin_seeds"])

(def seed-backup "A stack of each seed type stays carried; the rest is composted." 64)

(def farm-goods
  ["wheat_seeds" "beetroot_seeds" "melon_seeds" "pumpkin_seeds" "carrot" "potato" "wheat" "beetroot" "melon_slice"
   "melon" "pumpkin" "poisonous_potato" "hay_block" "cocoa_beans"])

(defn carried
  "{name count} of an inventory ([{:name :count}])."
  [inventory]
  (reduce (fn [acc {:keys [name count]}] (update acc name (fnil + 0) count)) {} inventory))

(defn surplus
  "The names (in the order of names) of which more is carried than the keep map holds back."
  [inventory keep names]
  (let [have (carried inventory)]
    (filterv #(> (get have % 0) (get keep % 0)) names)))

(defn keeps
  "{name count} kept carried: the sowing reserve, a backup stack of each seed type, the body's 3-day food
  (cost/food-reserve) and the :keep entries, whichever is largest for a name."
  [sow inventory keep]
  (merge-with max sow (zipmap waste-seeds (repeat seed-backup)) (cost/food-reserve inventory) keep))

(defn untilled?
  "A ground cell {:name :above} of dirt or grass with nothing but air or ground cover over it."
  [{:keys [name above]}]
  (boolean (and (till/tillable name)
                (or (till/air above) (till/ground-cover above)))))
