(ns jobs.lib.standing
  "A standing job (one kept in the queue under repeat, or a site job) declines far from its site while a job that is not standing
  is listed, or deep below it (away-cause), so it does not walk the body home between or after trips;
  standing jobs are the listed ones under repeat (standing-ids)."
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.loose :as loose]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::away-limit {:default 48 :doc "How far from its site a standing job still starts while another job is listed, in blocks." :spec (a/num-in 0 nil)}
   ::away-depth {:default 8 :doc "How far below its site a standing job still starts, in blocks, while another job is listed or this job is under repeat (the gap after a trip job)." :spec (a/num-in 0 nil)}})
(defn away-limit [] (settings/get settings ::away-limit))
(defn away-depth [] (settings/get settings ::away-depth))

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

(defn under-repeat?
  "True when this ctx's root job is kept under repeat."
  [c]
  (= :repeat (:op (:spec (get (:instances @(:state (:engine c))) (root-id c))))))

(defn away-cause
  "nil, or the cause {:reason :away ..} when site (a position, or nil) is farther than
  ::away-limit from the body and a listed job other than this one and the standing jobs is listed, or when the body is
  more than ::away-depth below it and this job is under repeat or such a job is listed (a lone one-shot site job walks back)."
  [c site]
  (let [view (ctx/view c)
        standing (conj (standing-ids c) (root-id c))
        others? (seq (remove standing (loose/live-ids view nil)))]
    (when (and site
               (or (and (> (- (:y site) (:y (u/self-pos c))) (away-depth))
                        (or others? (under-repeat? c)))
                   (and (> (u/dist (u/self-pos c) site) (away-limit)) others?)))
      {:reason :away :site site :limit (away-limit)})))
