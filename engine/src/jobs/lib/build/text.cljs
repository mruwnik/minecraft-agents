(ns jobs.lib.build.text
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [plan.shape :as shape]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]
            [jobs.lib.build.wood :as wood]
            [jobs.lib.build.plan :refer [owed kept-ground shortage left-text shortage-text]]))

(defn summary
  "What a build of cells left, from the job memory m: {:placed :missing :short :given-up :wrong :refused}, with :filled (the fill blocks placed) when any."
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
      wood (assoc :wood wood)
      (seq (:filled m)) (assoc :filled (count (:filled m))))))

(defn fetch-failed-text
  "The booked failed fetches of the short items, in words: 'could not fetch birch_log: no-source (why)'."
  [fails]
  (str/join "; " (map (fn [[item {:keys [failed why hint]}]]
                        (str "could not fetch " item ": " (name (or failed :failed)) (when why (str " (" why ")")) (when hint (str " - " hint))))
                      fails)))

(defn wood-hint
  "For an exact wood item that no source gave while other wood is carried: the carried wood and the generic name the blueprint can use."
  [item carried]
  (let [sp (wood/species-of item)
        kind (some-> sp (as-> s (str/replace item (re-pattern (str "^(stripped_)?" s "_")) "")))
        others (when sp (sort (filter #(some-> (wood/species-of %) (not= sp)) (keys carried))))
        generic (some (fn [[g k]] (when (= k kind) g)) wood/generic)]
    (when (and (seq others) generic)
      (str "carries " (str/join ", " (map #(str % " " (carried %)) others)) "; the blueprint can use generic " generic))))

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
        carried (storage/carried-totals (u/inventory (:primitives c)))
        hinted (fn [item f] (if-let [hint (and (= :no-source (:failed f)) (wood-hint item carried))] (assoc f :hint hint) f))
        fails (into (sorted-map) (keep (fn [item] (when-let [f (get booked item)] [item (hinted item f)]))) (keys (:short result)))]
    (cond-> result (seq fails) (assoc :fetch-failed fails))))
