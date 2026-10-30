(ns world-test.look-seconds
  "tools/event-counts.mjs look-seconds: counts the measured seconds one look takes (jobs.lib.cost.look/look-tick-s) from the
  bodies' logs alone: each :sense.looked event with looks carries the :wall-ms its decide! took on the job clock. Pure parts:
  scan-line, report."
  (:require [clojure.string :as str]
            [cljs.reader :as reader]))

(defn scan-line
  "One log line -> {:looks n :ms wall-ms} of a :sense.looked event that logged its wall time, else nil."
  [line]
  (when (str/includes? line ":kind :sense.looked")
    (let [{:keys [looks wall-ms]} (:data (reader/read-string line))]
      (when (and (pos? (or looks 0)) wall-ms) {:looks looks :ms wall-ms}))))

(defn report
  "{:n events :looks total :median s per look over the events :mean s per look :setting}: the median of each event's
  wall seconds over its looks."
  [rows]
  (let [per (vec (sort (map #(/ (:ms %) 1000 (:looks %)) rows)))
        n (count per)
        looks (reduce + (map :looks rows))
        median (when (pos? n) (/ (js/Math.round (* 1000 (nth per (quot n 2)))) 1000))]
    (cond-> {:n n :looks looks}
      median (assoc :median median :mean (/ (js/Math.round (/ (* 1000 (reduce + (map :ms rows))) 1000 looks)) 1000)
                    :setting median))))
