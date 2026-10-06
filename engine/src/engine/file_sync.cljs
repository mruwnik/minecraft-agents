(ns engine.file-sync
  "Keeping a folder of <id>.edn files in memory, re-read only when changed: the pure bookkeeping (due?, stale-ids,
  drop-gone, absorb) and the stamps of a folder. engine.notes and the world's plan files use it."
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]))

(defn due?
  "Whether the files should be stat-ed again: never checked, or :every-ms has passed."
  [{:keys [checked-at every-ms]} now]
  (or (nil? checked-at) (>= (- now checked-at) every-ms)))

(defn stale-ids
  "Ids of stamps {id stamp} that are new or whose stamp differs from the entry's."
  [entries stamps]
  (keep (fn [[id stamp]] (when (not= stamp (get-in entries [id :stamp])) id)) stamps))

(defn drop-gone
  "Entries without a file any more are forgotten."
  [entries stamps]
  (select-keys entries (keys stamps)))

(defn absorb
  "Fold one parsed file ({:value v} or {:errors [text ..]}) into entries: [entries warn], warn being nil or
  {:id :error :kept} (kept: a last good copy is still used)."
  [entries id stamp {:keys [value errors]}]
  (if (empty? errors)
    [(assoc entries id {:stamp stamp :value value}) nil]
    (let [error (str/join "; " errors)
          old (get-in entries [id :value])]
      [(assoc entries id (cond-> {:stamp stamp :error error} old (assoc :value old)))
       {:id id :error error :kept (some? old)}])))

(defn stamps
  "{id [mtime size]} of the <id>.edn files of dir; a missing dir has none."
  [dir]
  (let [names (try (vec (.readdirSync fs dir)) (catch :default _ []))]
    (into {} (keep (fn [f]
                     (when (str/ends-with? f ".edn")
                       (when-let [st (try (.statSync fs (path/join dir f)) (catch :default _ nil))]
                         [(str/replace f #"\.edn$" "") [(.-mtimeMs st) (.-size st)]]))))
          names)))
