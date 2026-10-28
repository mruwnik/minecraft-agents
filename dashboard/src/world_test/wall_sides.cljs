(ns world-test.wall-sides
  "tools/event-counts.mjs wall-sides: counts the measured prior behind jobs.lib.cost.look ::wall-prior from the trial
  bodies' enter-room.arrived events: a room walked into unseen, its sides {:seen :unseen} as the walk started, how many of the
  unseen ones turned out :open-sides and :closed-sides, and whether the room is :closed. Rows by [body time-ms], folded into the counts file's :walls so they outlive the logs.
  Events without the side counts (written before they were recorded) are skipped: their open sides are unknown.
  :setting {:k :n} is the pseudo-counts ::wall-prior takes: k closed unseen sides of n counted.
  Pure parts: scan-line, fold, report."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]))

(defn scan-line
  "One log line -> [[body-or-nil time-ms] {:seen :unseen :open-sides :closed-sides}] when it is an enter-room.arrived, else nil."
  [line]
  (when (str/includes? line ":kind :enter-room.arrived")
    (let [{:keys [time-ms data]} (reader/read-string line)
          {:keys [seen unseen open-sides closed-sides]} data]
      (when (and time-ms (number? open-sides) (number? closed-sides))
        [time-ms {:seen seen :unseen unseen :open-sides open-sides :closed-sides closed-sides}]))))

(defn fold
  "counts with rows {[body time-ms] row} merged into :walls; a row already there stays."
  [counts rows]
  (update counts :walls #(merge rows %)))

(defn report
  "{:rooms :closed :open :setting {:k :n}} of the :walls rows: sides counted closed and open, rooms with an unseen side; rows without the counts are skipped."
  [rows]
  (let [rs (filter #(and (pos? (:unseen %)) (number? (:open-sides %)) (number? (:closed-sides %))) (vals rows))
        k (reduce + (map :closed-sides rs))
        open (reduce + (map :open-sides rs))]
    {:rooms (count rs) :closed k :open open
     :setting {:k k :n (+ k open)}}))
