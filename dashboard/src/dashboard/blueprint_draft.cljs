(ns dashboard.blueprint-draft
  "EDN editor operations, shared with page event tests."
  (:require [dashboard.edn :as edn]
            [clojure.string :as str]
            [plan.parse :as parse]
            [plan.shape :as shape]))

(defn download [source]
  (let [bp (edn/one-form source) id (:id bp)
        parsed (parse/parse-blueprint source id)]
    (when-not (and (shape/part-id? id) (empty? (:errors parsed)))
      (throw (js/Error. (str/join " · " (or (seq (:errors parsed)) ["blueprint needs a valid :id"])))))
    {:filename (str id ".edn") :text (str (str/trimr source) "\n")}))

(defn preview-body [source stock]
  {:source (or source "") :stock (when-not (str/blank? stock) stock)})

(defn editable-source [detail] (:source detail))
