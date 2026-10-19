(ns jobs.lib.build.text
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [plan.shape :as shape]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]
            [jobs.lib.build.plan :refer [owed kept-ground shortage left-text shortage-text]]))

(defn summary
  "What a build of cells left, from the job memory m: {:placed :missing :short :given-up :wrong :refused}."
  [c cells m sturdy-ground]
  (let [kept? (kept-ground c cells sturdy-ground)
        left (owed cells)
        given-up (merge (select-keys (:unplaceable m) (map :pos left)) (:given-up m {}))
        wrong (mapv (fn [{:keys [pos found want short?]}]
                      (if-let [placed (get-in m [:misplaced pos])]
                        {:pos pos :found placed :want (shape/want-text want) :placed true}
                        (cond-> {:pos pos :found found :want (shape/want-text want)}
                          short? (assoc :short true))))
                    (filter #(and (#{:wrong :extra} (:answer %)) (not (kept? %))) cells))
        refused (->> (:refused m) (sort-by key) (mapv (fn [[pos why]] (assoc why :pos pos))))
        wood (:wood m)]
    (cond-> {:placed (:placed m 0) :missing (mapv :pos left) :short (shortage left (storage/carried-totals (u/inventory (:primitives c))))
             :given-up given-up :wrong wrong :refused refused}
      wood (assoc :wood wood))))

(defn fetch-failed-text
  "The booked failed fetches of the short items, in words: 'could not fetch birch_log: no-source (why)'."
  [fails]
  (str/join "; " (map (fn [[item {:keys [failed why]}]]
                        (str "could not fetch " item ": " (name (or failed :failed)) (when why (str " (" why ")"))))
                      fails)))

(defn stopped-text
  "Why a build that left cells ended stopped: the plan, how many were placed and are missing, and what it left."
  [plan {:keys [placed missing fetch-failed wrong] :as result}]
  (let [left (str/join "; " (remove str/blank? [(left-text result) (fetch-failed-text fetch-failed)
                                                 (when (seq wrong) (str (count wrong) " blocks wrong"))]))]
    (str "build of " plan " stopped: placed " placed ", still missing " (count missing) (when (seq left) (str " (" left ")")))))

(defn assoc-fetch-failed
  "result with :fetch-failed {item {:failed :why}} for each short item whose fetch failed in this call."
  [c result]
  (let [booked (:fetch-failed (ctx/mem c))
        fails (into (sorted-map) (keep (fn [item] (when-let [f (get booked item)] [item f]))) (keys (:short result)))]
    (cond-> result (seq fails) (assoc :fetch-failed fails))))
