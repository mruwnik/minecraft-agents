(ns engine.scenario
  "Scenario files: EDN {:register [entry] :queue [job-spec]}. See README.md."
  (:require [cljs.reader :as reader]
            [engine.expr :as expr]
            [engine.trigger-api :as trigger-api]
            ["fs" :as fs]))

(defn parse [text]
  (reader/read-string text))

(defn read-file [file]
  (parse (fs/readFileSync file "utf8")))

(defn problems
  "Human-readable problems with scenario s against the job registry and the
  triggers (with :condition for :when entries); empty when fine. The register
  goes through the same validation as POST /triggers."
  [jobs triggers {:keys [register queue]}]
  (vec (concat (trigger-api/register-problems jobs triggers register)
               (keep #(expr/problem jobs %) queue))))
