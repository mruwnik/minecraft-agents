(ns dashboard.plan
  "Reading plan and blueprint files: state/worlds/<world>/plans/<id>.edn, one plan per file, and blueprints/<id>.edn
  at the repo root, one blueprint per file (the .blueprint.json documents there are the legacy library's). Only the
  file side lives here; parsing the text is plan.parse, what plans and blueprints are is plan.shape."
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]
            [plan.parse :as parse]))

(defn read-edn-dir
  "Every <id>.edn file of a directory parsed with (parse text id): {:found {id value} :errors [{:file name :errors [...]}]}.
  A missing directory has none."
  [dir parse k]
  (let [files (try (->> (.readdirSync fs dir) (filter #(str/ends-with? % ".edn")) sort) (catch :default _ []))
        results (for [f files
                      :let [id (str/replace f #"\.edn$" "")
                            text (try (.readFileSync fs (.join path dir f) "utf8") (catch :default e (ex-message e)))]]
                  (assoc (parse text id) :file f :id id))]
    {:found (into {} (keep (fn [r] (when (k r) [(:id r) (k r)]))) results)
     :errors (vec (keep (fn [{:keys [file errors]}] (when errors {:file file :errors errors})) results))}))

(defn read-dir
  "Every plan file of a directory: {:plans {id plan} :errors [{:file name :errors [...]}]}."
  [dir]
  (let [{:keys [found errors]} (read-edn-dir dir parse/parse :plan)]
    {:plans found :errors errors}))

(defn read-blueprints
  "Every blueprint file of a directory: {:blueprints {id blueprint} :errors [{:file name :errors [...]}]}."
  [dir]
  (let [{:keys [found errors]} (read-edn-dir dir parse/parse-blueprint :blueprint)]
    {:blueprints found :errors errors}))
