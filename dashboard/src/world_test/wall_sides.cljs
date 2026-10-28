(ns world-test.wall-sides
  "tools/event-counts.mjs wall-sides: counts the measured prior behind jobs.lib.cost.look ::wall-prior from the trial
  bodies' enter-room.arrived events: a room walked into unseen, its sides {:seen :unseen} as the walk started and whether
  it :closed. Rows by [body time-ms], folded into the counts file's :walls so they outlive the logs.
  report: a closed room had all its unseen sides closed; an open one at least one open, taken as exactly one (the
  event does not say which), so the open count is a lower bound and the chance of a wall reads high. :setting {:k :n} is
  the pseudo-counts ::wall-prior takes: k closed unseen sides of n counted.
  Pure parts: scan-line, fold, report."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]))

(defn scan-line
  "One log line -> [[body-or-nil time-ms] {:seen :unseen :closed}] when it is an enter-room.arrived, else nil."
  [line]
  (when (str/includes? line ":kind :enter-room.arrived")
    (let [{:keys [time-ms data]} (reader/read-string line)
          {:keys [seen unseen closed]} data]
      (when (and time-ms (number? unseen))
        [time-ms {:seen seen :unseen unseen :closed (boolean closed)}]))))

(defn fold
  "counts with rows {[body time-ms] row} merged into :walls; a row already there stays."
  [counts rows]
  (update counts :walls #(merge rows %)))

(defn report
  "{:rooms :closed :open :setting {:k :n}} of the :walls rows (those with an unseen side)."
  [rows]
  (let [rs (filter #(pos? (:unseen %)) (vals rows))
        {closed true open false} (group-by :closed rs)
        k (reduce + (map :unseen closed))]
    {:rooms (count rs) :closed (count closed) :open (count open)
     :setting {:k k :n (+ k (count open))}}))
