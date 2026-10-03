(ns engine.scenario
  "Scenario files: EDN {:register [entry] :queue [job-spec]}. See README.md."
  (:require [cljs.reader :as reader]
            [engine.expr :as expr]
            ["fs" :as fs]))

(defn parse [text]
  (reader/read-string text))

(defn read-file [file]
  (parse (fs/readFileSync file "utf8")))

(defn register-problems [jobs triggers {:keys [trigger job]}]
  (cond
    (not (contains? triggers trigger)) [(str "unknown trigger " trigger)]
    (nil? job) []
    (and (seq? job) (= 'hold (first job))) [(str "hold is not allowed in a register entry, in " (pr-str job))]
    :else (keep identity [(expr/problem jobs job)])))

(defn problems
  "Human-readable problems with scenario s against the job registry and the
  triggers; empty when fine."
  [jobs triggers {:keys [register queue]}]
  (vec (concat (mapcat #(register-problems jobs triggers %) register)
               (keep #(expr/problem jobs %) queue))))
