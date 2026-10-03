(ns dashboard.plan
  "Reading plan files: state/worlds/<world>/plans/<id>.edn, one plan per file (DRAFT, see README). Only the file and EDN
  side lives here; what a plan is (validation, children, expansion, cell predicates) is plan.shape."
  (:require ["fs" :as fs]
            ["path" :as path]
            [cljs.reader :as reader]
            [clojure.string :as str]
            [plan.shape :as shape]))

(defn parse
  "Text of a plan file -> {:plan p} or {:errors [string ...]}; never throws."
  [text file-id]
  (let [read (try {:value (reader/read-string text)} (catch :default e {:error (str "unreadable EDN: " (ex-message e))}))]
    (if (:error read)
      {:errors [(:error read)]}
      (let [errors (shape/plan-errors (:value read) file-id)]
        (if (seq errors) {:errors errors} {:plan (:value read)})))))

(defn read-dir
  "Every plan file of a directory: {:plans {id plan} :errors [{:file name :errors [...]}]}. A missing directory has none."
  [dir]
  (let [files (try (->> (.readdirSync fs dir) (filter #(str/ends-with? % ".edn")) sort) (catch :default _ []))
        results (for [f files
                      :let [id (str/replace f #"\.edn$" "")
                            text (try (.readFileSync fs (.join path dir f) "utf8") (catch :default e (ex-message e)))]]
                  (assoc (parse text id) :file f :id id))]
    {:plans (into {} (keep (fn [{:keys [id plan]}] (when plan [id plan]))) results)
     :errors (vec (keep (fn [{:keys [file errors]}] (when errors {:file file :errors errors})) results))}))
