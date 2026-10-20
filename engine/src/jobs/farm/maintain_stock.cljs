(ns jobs.farm.maintain-stock
  "What jobs.farm.maintain keeps carried and what it stores or composts: the sowing reserve, the keep map and the
  surplus over it."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.crops :as crops]
            [jobs.lib.storage :as storage]
            [jobs.farm.tend-ground :as gr]))

(a/defargs settings
  {::seed-backup {:default 64 :doc "A stack of each seed type stays carried; the rest is composted." :spec (a/int-in 0 nil)}})
(defn seed-backup [] (settings/get settings ::seed-backup))

(defn surplus
  "The names (in the order of names) of which more is carried than the keep map holds back."
  [inventory keep names]
  (let [have (storage/carried-totals inventory)]
    (filterv #(> (get have % 0) (get keep % 0)) names)))

(defn keeps
  "{name count} kept carried: the sowing reserve, a backup stack of each seed type, the units the reserve register
  holds (held: jobs.lib.reserve/held) and the :keep entries, whichever is largest for a name."
  [sow held keep]
  (merge-with max sow (zipmap crops/waste-seeds (repeat (seed-backup))) held keep))

(defn box-reserve
  "{seed count} to keep: twice the beds in all, taken one at a time from each carried seed that has more than is reserved already."
  [beds inventory]
  (let [have (storage/carried-totals inventory)]
    (loop [budget (* 2 beds) res {}]
      (let [open (filterv #(> (get have % 0) (get res % 0)) crops/field-seeds)]
        (if (or (zero? budget) (empty? open))
          res
          (let [[res budget] (reduce (fn [[r b] item]
                                       (if (zero? b)
                                         (reduced [r b])
                                         [(update r item (fnil inc 0)) (dec b)]))
                                     [res budget]
                                     open)]
            (recur budget res)))))))

(defn beds
  "How many ground cells ({:name :above}) are farmland or untilled."
  [ground]
  (count (filter #(or (= "farmland" (:name %)) (gr/untilled? %)) ground)))

(defn plan-reserve
  "{seed count}: twice the planned cells ({pos crop}) of each crop."
  [cells]
  (into {} (for [[crop n] (frequencies (vals cells))] [(crops/seed-of crop) (* 2 n)])))
