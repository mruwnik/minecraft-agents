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

(def listed 8)

(defn cells-text
  "The count of items and the first few, as text for an event; the whole list goes in the event's data. Never clipped
  with an ellipsis: it says how many more there are."
  [items show]
  (let [shown (take listed items)
        more (- (count items) (count shown))]
    (str (str/join ", " (map show shown)) (when (pos? more) (str ", and " more " more (all in :cells)")))))

(defn announce!
  "Emit the build warns of a summary: build.short, build.gave-up, build.refused and build.wrong."
  [c {:keys [short given-up refused wrong]}]
  (let [plan (:plan (:args c))]
    (when (seq short)
      (ctx/emit! c :build.short :warn {:plan plan :short short
                                       :text (str "build of " plan " is short of " (shortage-text short))}))
    (when (seq given-up)
      (ctx/emit! c :build.gave-up :warn {:plan plan :cells given-up
                                         :text (str "build of " plan " gave up " (count given-up) " cells: "
                                                    (cells-text (sort-by key given-up) (fn [[pos why]] (str (pr-str pos) " " (name why)))))}))
    (doseq [[reason group] (group-by :reason refused)]
      (ctx/emit! c :build.refused :warn {:plan plan :reason reason :cells group
                                         :text (str "build of " plan " refused " (count group) " cells (" (name reason) "): "
                                                    (cells-text group #(str (pr-str (:pos %)) " " (or (:zone %) (:plan %) (str/join "," (map name (:hazards %)))))))}))
    (when (seq wrong)
      (ctx/emit! c :build.wrong :warn {:plan plan :cells wrong
                                       :text (str "build of " plan " left " (count wrong) " wrong blocks: "
                                                  (cells-text wrong #(str (pr-str (:pos %)) " " (:found %)
                                                                          (when (:short %) (str ", needs " (:want %) " (short)")))))}))))

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
