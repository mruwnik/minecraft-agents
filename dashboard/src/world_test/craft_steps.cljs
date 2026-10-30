(ns world-test.craft-steps
  "tools/event-counts.mjs craft-steps: counts the measured seconds one craft step takes (jobs.lib.cost.table ::craft-step) from
  the bodies' logs alone: the mean seconds from a craft action's start to its end (retry-wins/scan-line's rows of kind :craft).
  Pure parts: report."
  (:require [world-test.retry-wins :as retry-wins]))

(def scan-line retry-wins/scan-line)

(defn report
  "{:n crafts :mean s :setting} of the scanned rows' craft actions, start to end; {:n 0} with none."
  [rows]
  (let [rows (filter #(= :craft (:kind %)) rows)
        starts (into {} (keep #(when (= :start (:edge %)) [(:action %) (:t %)])) rows)
        ss (keep #(when-let [s (and (= :end (:edge %)) (starts (:action %)))] (/ (- (:t %) s) 1000)) rows)
        n (count ss)
        mean (when (pos? n) (/ (js/Math.round (* 1000 (/ (reduce + ss) n))) 1000))]
    (cond-> {:n n}
      mean (assoc :mean mean :setting mean))))
