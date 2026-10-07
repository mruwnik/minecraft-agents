(ns dashboard.goal
  "A body's goal: one short line its controller (a game agent, the world-test runner) sets, kept in the body folder as
  goal.edn {:text :by :since}. The dashboard shows it; the engine never reads it."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            ["fs" :as fs]
            ["path" :as path]))

(def max-text 200)

(defn goal-file [body-dir] (path/join body-dir "goal.edn"))

(defn clean-text
  "s as one line of at most max-text characters, nil when blank."
  [s]
  (let [t (some-> s str (str/replace #"[\x00-\x1f\x7f]" " ") str/trim)]
    (when-not (str/blank? t) (subs t 0 (min max-text (count t))))))

(defn read-goal
  "The goal in body-dir, or nil when there is none or the file is not a goal."
  [body-dir]
  (let [v (try (reader/read-string (fs/readFileSync (goal-file body-dir) "utf8")) (catch :default _ nil))]
    (when (and (map? v) (string? (:text v))) v)))

(defn write-goal!
  "Sets body-dir's goal to text, by who, since now (ms); returns the goal. Written to a temp file and renamed, so a
  reader never sees half a goal. Throws on blank text."
  [body-dir text by now]
  (let [t (clean-text text)
        _ (when-not t (throw (js/Error. "goal text is empty")))
        goal (cond-> {:text t :since now} by (assoc :by (str by)))
        file (goal-file body-dir)
        tmp (str file "." (.-pid js/process) ".tmp")]
    (fs/writeFileSync tmp (str (pr-str goal) "\n"))
    (fs/renameSync tmp file)
    goal))

(defn clear-goal! [body-dir]
  (fs/rmSync (goal-file body-dir) #js {:force true}))
