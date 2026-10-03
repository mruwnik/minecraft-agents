(ns dashboard.ui.jobsmodel
  "Pure rules of the Jobs page: doc text as paragraphs, the filter, grouping by category, badges."
  (:require [clojure.string :as str]))

(defn paragraphs
  "A docstring as paragraphs: blank lines separate them, hard-wrapped lines join."
  [doc]
  (->> (str/split (or doc "") #"\n\s*\n")
       (map #(str/join " " (remove str/blank? (map str/trim (str/split-lines %)))))
       (remove str/blank?)
       vec))

(defn body-paragraphs
  "The job's `doc`, else the docstring of its namespace."
  [{:keys [doc ns-doc]}]
  (paragraphs (or doc ns-doc)))

(defn matches? [needle job]
  (let [n (str/lower-case (or needle ""))
        hay (str/lower-case (str/join "\n" (concat [(:id job) (:doc job) (:ns-doc job) (:args job)] (:running job) (:reflex job))))]
    (or (str/blank? n) (str/includes? hay n))))

(defn grouped
  "[{:category :jobs}] for the jobs matching the filter, categories and jobs in name order."
  [jobs needle]
  (->> jobs
       (filter #(matches? needle %))
       (group-by :category)
       (sort-by key)
       (mapv (fn [[category items]] {:category category :jobs (vec (sort-by :id items))}))))

(defn badges [{:keys [running reflex backoff]}]
  (vec (concat (when (seq running) [{:kind :running :text (str "running on: " (str/join ", " running))}])
               (when (seq reflex) [{:kind :reflex :text (str "reflex on: " (str/join ", " reflex))}])
               (when backoff [{:kind :backoff :text "backoff"}]))))

(defn count-text [shown total needle]
  (let [noun (if (= 1 total) "job" "jobs")]
    (if (str/blank? needle) (str total " " noun) (str shown " of " total " " noun))))

(def long-doc-chars 420)

(defn long-doc?
  "Docs this long start collapsed on the page."
  [job]
  (> (count (str/join (body-paragraphs job))) long-doc-chars))
