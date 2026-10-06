(ns plan.parse
  "The text of a plan or blueprint file -> the checked value or its errors; never throws. Shared by the dashboard
  (dashboard.plan) and the body engine (jobs.lib.world-files). No IO: reading the files is the caller's."
  (:require [cljs.reader :as reader]
            [plan.shape :as shape]))

(defn error-text [{:keys [part error]}]
  (if part (str "part " part ": " error) error))

(defn parse-with
  "Text of an EDN file -> {k value} or {:errors [string ...]}; never throws. check gives [{:part :error}] for a value."
  [text file-id k check]
  (let [read (try {:value (reader/read-string text)} (catch :default e {:error (str "unreadable EDN: " (ex-message e))}))]
    (if (:error read)
      {:errors [(:error read)]}
      (let [errors (check (:value read) file-id)]
        (if (seq errors) {:errors (mapv error-text errors)} {k (:value read)})))))

(defn parse [text file-id] (parse-with text file-id :plan shape/plan-errors))

(defn parse-blueprint [text file-id] (parse-with text file-id :blueprint shape/blueprint-errors))
