(ns world-test.far-side-rows
  "tools/event-counts.mjs far-side-rows: counts the measured prior behind jobs.lib.cost.door ::wall-rows from the trial bodies'
  clear-path.far-side events (the probes' walls are staged): a door whose far side it could not see, the rows of wall it had seen then (:seen), all it
  found (:rows), and whether it found the far side :open. Rows by [body time-ms], folded into the counts file's :far-side.
  :setting {:k :n} is ::wall-rows: k unseen rows found wall of n looked at (those and each far side found open; a door
  given up says only that its rows were wall).
  Pure parts: scan-line, fold, report."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]))

(defn scan-line
  "One log line -> [time-ms {:seen :rows :open}] when it is a clear-path.far-side, else nil."
  [line]
  (when (str/includes? line ":kind :clear-path.far-side")
    (let [{:keys [time-ms data]} (reader/read-string line)
          {:keys [seen rows open]} data]
      (when (and time-ms (number? seen) (number? rows))
        [time-ms {:seen seen :rows rows :open (true? open)}]))))

(defn fold
  "counts with rows {[body time-ms] row} merged into :far-side; a row already there stays."
  [counts rows]
  (update counts :far-side #(merge rows %)))

(defn report
  "{:doors :open :setting {:k :n}} of the :far-side rows."
  [rows]
  (let [rs (vals rows)
        k (reduce + (map #(max 0 (- (:rows %) (:seen %))) rs))
        open (count (filter :open rs))]
    {:doors (count rs) :open open :setting {:k k :n (+ k open)}}))
