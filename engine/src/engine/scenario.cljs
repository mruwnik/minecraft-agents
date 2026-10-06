(ns engine.scenario
  "Scenario files: EDN {:register [entry] :queue [job-spec]}. See README.md."
  (:require [cljs.reader :as reader]
            [engine.expr :as expr]
            [engine.trigger-api :as trigger-api]
            [engine.triggers :as triggers]
            ["fs" :as fs]))

(defn parse [text]
  (reader/read-string text))

(defn with-defaults
  "Scenario s (nil is {}) with the default trigger set as its :register when it has no :register key; an explicit
  :register (even []) stays as it is."
  [s]
  (let [s (or s {})]
    (if (contains? s :register)
      s
      (assoc s :register (mapv (fn [id] {:trigger id}) triggers/order)))))

(defn read-file [file]
  (parse (fs/readFileSync file "utf8")))

(defn problems
  "Human-readable problems with scenario s against the job registry and the
  triggers (with :condition for :when entries); empty when fine. The register
  goes through the same validation as POST /triggers."
  [jobs triggers {:keys [register queue]}]
  (vec (concat (trigger-api/register-problems jobs triggers register)
               (keep #(expr/problem jobs %) queue))))
