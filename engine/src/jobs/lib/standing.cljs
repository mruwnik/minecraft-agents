(ns jobs.lib.standing
  "Targets a standing job (one kept in the queue under repeat) rests after a fruitless pass, so a stuck crop or refused
  ground never keeps its check true for ever and starves the other jobs. Body memory :standing/rested
  {:job :key :until :why}; a target rests ::rest-ms, then is tried once more. A standing job also declines far from
  its site while a job that is not standing is listed, or deep below it (away-wait), so it does not walk the body home
  between or after trips;
  standing jobs are the listed ones under repeat (standing-ids)."
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.loose :as loose]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::rest-ms {:default 600000 :doc "How long a target rests after a pass that left it undone, in ms." :spec (a/int-in 1 nil)}
   ::away-limit {:default 48 :doc "How far from its site a standing job still starts while another job is listed, in blocks." :spec (a/num-in 0 nil)}
   ::away-depth {:default 8 :doc "How far below its site a standing job still starts, in blocks, even with no other job listed (the gap after a trip job ends)." :spec (a/num-in 0 nil)}})
(defn rest-ms [] (settings/get settings ::rest-ms))
(defn away-limit [] (settings/get settings ::away-limit))
(defn away-depth [] (settings/get settings ::away-depth))

(def kind :standing/rested)

(defn rested
  "{key {:until ms :why}} of the targets of job (symbol) resting now."
  [c job]
  (let [view (ctx/view c)]
    (into {} (keep (fn [{:keys [data]}]
                     (when (and (= job (:job data)) (< (:now view) (:until data)))
                       [(:key data) (select-keys data [:until :why])])))
          (mem/entries view kind))))

(defn rest!
  "Rest target key of job for ::rest-ms, with why (a keyword) kept for the status text."
  [c job key why]
  (ctx/forget-where! c kind #(and (= job (:job %)) (= key (:key %))))
  (ctx/remember! c kind {:job job :key key :why why :until (+ (ctx/now c) (rest-ms))} {:cap 32 :ttl (rest-ms)}))

(defn root-id [c] (first (str/split (:id c) #"/")))

(defn standing-ids
  "The ids of the listed jobs kept under repeat."
  [c]
  (into #{} (comp (filter (comp #{:repeat} :op :spec val)) (map key)) (:instances @(:state (:engine c)))))

(defn away-wait
  "For a check: nil, or the declined wait (false, reason :away) when site (a position, or nil) is farther than
  ::away-limit from the body and a listed job other than this one and the standing jobs is listed, or when the body is
  more than ::away-depth below it."
  [c site]
  (let [view (ctx/view c)
        standing (conj (standing-ids c) (root-id c))]
    (when (and site
               (or (> (- (:y site) (:y (u/self-pos c))) (away-depth))
                   (and (> (u/dist (u/self-pos c) site) (away-limit))
                        (seq (remove standing (loose/live-ids view nil))))))
      (ctx/wait c {:reason :away :site site :limit (away-limit)}))))
