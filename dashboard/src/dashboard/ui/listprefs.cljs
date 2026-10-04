(ns dashboard.ui.listprefs
  "Which right-hand lists are shown, kept in the browser's localStorage under one key so the choice survives page
  changes and reloads. The merge and the text are pure; the storage access is the two small functions at the end."
  (:require [re-frame.core :as rf]))

(def storage-key "dashboard.lists-open")

(def list-names {:chat-open? "chat" :places-open? "places" :players-open? "players"})

(defn parse-stored
  "The stored text as a map, or {} for anything that is not a JSON object."
  [text]
  (let [parsed (try (js->clj (js/JSON.parse text)) (catch :default _ nil))]
    (if (map? parsed) parsed {})))

(defn open-flags
  "`defaults` (db key -> shown?) with each list's stored boolean laid over it; other stored values are ignored."
  [defaults text]
  (let [stored (parse-stored text)]
    (reduce-kv (fn [flags open-key list-name]
                 (let [value (get stored list-name)]
                   (cond-> flags (boolean? value) (assoc open-key value))))
               defaults
               list-names)))

(defn stored-text [flags]
  (js/JSON.stringify (clj->js (into {} (map (fn [[open-key list-name]] [list-name (boolean (flags open-key))])) list-names))))

(defn read-stored []
  (try (.getItem js/localStorage storage-key) (catch :default _ nil)))

(defn write-stored! [flags]
  (try (.setItem js/localStorage storage-key (stored-text flags)) (catch :default _ nil)))

(rf/reg-fx :store-lists write-stored!)
