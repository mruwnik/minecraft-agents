(ns jobs.lib.standing
  "A standing job (one kept in the queue under repeat, or a site job) declines while a job that is not standing can run
  (away-cause), so it does not walk the body home between trips; standing jobs are the listed ones under repeat (standing-ids)."
  (:require [clojure.string :as str]))

(defn root-id [c] (first (str/split (:id c) #"/")))

(def site-jobs
  "The jobs that decline here: they must not hold each other off, or two far ones wait for ever."
  #{'jobs.farm.tend 'jobs.animals.maintain 'jobs.items.kitchen})

(defn standing-ids
  "The ids of the listed jobs kept under repeat, and of the site jobs."
  [c]
  (into #{}
        (comp (filter (fn [[_ inst]] (let [spec (:spec inst)] (or (= :repeat (:op spec)) (site-jobs (:job spec))))))
              (map key))
        (:instances @(:state (:engine c)))))

(defn runnable-others
  "The ids of the listed jobs that are not standing (nor this one's root), not waiting on a declining check and not failed:
  the ones that run or can start now."
  [c]
  (let [eng (:engine c)
        standing (conj (standing-ids c) (root-id c))
        st @(:state eng)
        waiting @(:waiting eng)]
    (into [] (remove #(or (standing %) (waiting %) (contains? (:failed st) %))) (:list st))))

(defn away-cause
  "nil, or the cause {:reason :away :others ids} when site (a position, or nil) is set and a job that is not standing
  runs or can start now: the body's time is spoken for, so a standing job does not walk it to its site. With nothing
  else to run the body walks there, however far or deep."
  [c site]
  (let [others (runnable-others c)]
    (when (and site (seq others))
      {:reason :away :site site :others others})))
