(ns dashboard.ui.statusfilter
  "Which body states the top bar's chips filter the bodies by, kept in the browser's sessionStorage so the choice
  survives page changes and reloads of this tab. Everything is pure except the two small storage functions at the end."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [dashboard.ui.trouble :as trouble]))

(def storage-key "dashboard.status-filter")

(def states [[:manual "manual"] [:working "working"] [:idle "idle"] [:trouble "in trouble"] [:offline "offline"]])

(defn only-states
  "The bodies whose state (the one the counts use) is in `pressed`; all of them when nothing is pressed. Bodies named in
  `keep-names` are always kept."
  ([bodies pressed now] (only-states bodies pressed now #{}))
  ([bodies pressed now keep-names]
   (if (empty? pressed)
     bodies
     (filterv #(or (contains? pressed (trouble/status % now)) (contains? keep-names (:name %))) bodies))))

(defn toggle [pressed k]
  (let [pressed (set pressed)]
    (if (contains? pressed k) (disj pressed k) (conj pressed k))))

(defn toggle-fx
  "The :toggle-status-filter event's effects: the db with the state toggled, and the new set to store."
  [db k]
  (let [pressed (toggle (:status-filter db) k)]
    {:db (assoc db :status-filter pressed) :store-status-filter pressed}))

(defn chip-title [k label pressed?]
  (cond
    (and pressed? (= k :trouble)) "stop filtering by trouble"
    pressed? (str "stop filtering by " label)
    (= k :trouble) "show only bodies in trouble"
    :else (str "show only " label " bodies")))

(defn chips
  "One map per state for the top bar: the full count (never the filtered one), whether it is pressed, whether it is dimmed
  because another is."
  [counts pressed]
  (mapv (fn [[k label]]
          (let [pressed? (contains? pressed k)]
            {:state k :label label :count (get counts k 0) :pressed? pressed?
             :dim? (boolean (and (seq pressed) (not pressed?))) :title (chip-title k label pressed?)}))
        states))

(defn join-or [words]
  (if (< (count words) 2)
    (str/join words)
    (str (str/join ", " (butlast words)) " or " (last words))))

(defn empty-text [pressed]
  (str "no body is " (join-or (for [[k label] states :when (contains? pressed k)] label))))

(def state-names (into {} (map (fn [[k _]] [(name k) k])) states))

(defn parse-stored
  "The stored text as a set of states: the known names of a JSON array, anything else ignored."
  [text]
  (let [parsed (try (js->clj (js/JSON.parse text)) (catch :default _ nil))]
    (if (sequential? parsed)
      (into #{} (keep #(when (string? %) (get state-names %))) parsed)
      #{})))

(defn stored-text [pressed]
  (js/JSON.stringify (clj->js (for [[k _] states :when (contains? pressed k)] (name k)))))

(defn read-stored []
  (try (.getItem js/sessionStorage storage-key) (catch :default _ nil)))

(defn write-stored! [pressed]
  (try (.setItem js/sessionStorage storage-key (stored-text pressed)) (catch :default _ nil)))

(rf/reg-fx :store-status-filter write-stored!)
