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

(defn badges [{:keys [running reflex]}]
  (vec (concat (when (seq running) [{:kind :running :text (str "running on: " (str/join ", " running))}])
               (when (seq reflex) [{:kind :reflex :text (str "reflex on: " (str/join ", " reflex))}]))))

(defn count-text [shown total needle]
  (let [noun (if (= 1 total) "job" "jobs")]
    (if (str/blank? needle) (str total " " noun) (str shown " of " total " " noun))))

(def long-doc-chars 420)

(defn long-doc?
  "Docs this long start collapsed on the page."
  [job]
  (> (count (str/join (body-paragraphs job))) long-doc-chars))

(def summary-chars 200)

(defn cut-at-word [text limit]
  (if (<= (count text) limit)
    text
    (let [head (subs text 0 (inc limit))
          at (str/last-index-of head " ")]
      (str (subs text 0 (or at limit)) "\u2026"))))

(defn summary
  "{:text :more?}: the first sentence of the doc (cut at a word near summary-chars), and whether there is more."
  [job]
  (let [full (str/join " " (body-paragraphs job))
        first-sentence (or (re-find #"(?s)^.*?[.!?](?=\s|$)" full) full)
        text (cut-at-word first-sentence summary-chars)]
    {:text text :more? (not= text full)}))

(defn arg-count
  "Top-level keys of the pretty-printed args map."
  [args]
  (count (re-seq #"(?:^\{|\n) ?:[^\s]+" (or args ""))))
