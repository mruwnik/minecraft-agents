(ns jobs.farm.tend-stock
  "What jobs.farm.tend keeps carried and what it stores or composts: the lists of seeds and farm goods, the keep map,
  the surplus over it, and whether a ground cell wants tilling."
  (:require [jobs.lib.crops :as crops]
            [engine.args :as a]
            [jobs.lib.block-sets :as bs]
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]
            [jobs.lib.storage :as storage]
            [jobs.farm.till :as till]))

(def waste-seeds (into (filterv #(.endsWith % "_seeds") crops/field-seeds) crops/other-seeds))

(a/defargs settings
  {::seed-backup {:default 64 :doc "A stack of each seed type stays carried; the rest is composted." :spec (a/int-in 0 nil)}})
(defn seed-backup [] (settings/get settings ::seed-backup))

(def farm-goods
  (vec (distinct (concat waste-seeds crops/produce-items ["melon_slice" "melon" "pumpkin" "hay_block" "cocoa_beans"]))))

(defn surplus
  "The names (in the order of names) of which more is carried than the keep map holds back."
  [inventory keep names]
  (let [have (storage/carried-totals inventory)]
    (filterv #(> (get have % 0) (get keep % 0)) names)))

(defn keeps
  "{name count} kept carried: the sowing reserve, a backup stack of each seed type, the body's 3-day food
  (cost/food-reserve) and the :keep entries, whichever is largest for a name."
  [sow inventory keep]
  (merge-with max sow (zipmap waste-seeds (repeat (seed-backup))) (cost/food-reserve inventory) keep))

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
