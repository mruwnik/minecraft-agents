(ns engine.scenario
  "Scenario files: EDN {:register [...] :queue [...]}. See README.md."
  (:require [cljs.reader :as reader]
            ["fs" :as fs]))

(defn parse [text]
  (reader/read-string text))

(defn read-file [file]
  (parse (fs/readFileSync file "utf8")))

(defn problems
  "Human-readable problems with scenario s against catalog; empty when fine."
  [catalog {:keys [register queue]}]
  (let [jobs (:jobs catalog)
        triggers (:triggers catalog)]
    (vec
     (concat
      (mapcat (fn [{:keys [trigger job]}]
                (cond
                  (not (contains? triggers trigger)) [(str "unknown trigger " trigger)]
                  (and job (not (contains? jobs job))) [(str "unknown job " job)]
                  :else []))
              register)
      (mapcat (fn [i {:keys [job]}]
                (cond
                  (nil? job) [(str "queue entry " i " has no :job")]
                  (not (contains? jobs job)) [(str "unknown job " job)]
                  :else []))
              (range) queue)))))
