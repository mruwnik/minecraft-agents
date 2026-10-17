(ns jobs.lib.standing
  "A standing job (one kept in the queue under repeat) declines far from its site while a job that is not standing
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
   ::away-depth {:default 8 :doc "How far below its site a standing job still starts, in blocks, even with no other job listed (the gap after a trip job ends)." :spec (a/num-in 0 nil)}})
(defn away-limit [] (settings/get settings ::away-limit))
(defn away-depth [] (settings/get settings ::away-depth))

(defn root-id [c] (first (str/split (:id c) #"/")))

(defn standing-ids
  "The ids of the listed jobs kept under repeat."
  [c]
  (into #{} (comp (filter (comp #{:repeat} :op :spec val)) (map key)) (:instances @(:state (:engine c)))))

(defn away-cause
  "nil, or the cause {:reason :away ..} when site (a position, or nil) is farther than
  ::away-limit from the body and a listed job other than this one and the standing jobs is listed, or when the body is
  more than ::away-depth below it."
  [c site]
  (let [view (ctx/view c)
        standing (conj (standing-ids c) (root-id c))]
    (when (and site
               (or (> (- (:y site) (:y (u/self-pos c))) (away-depth))
                   (and (> (u/dist (u/self-pos c) site) (away-limit))
                        (seq (remove standing (loose/live-ids view nil))))))
      {:reason :away :site site :limit (away-limit)})))
