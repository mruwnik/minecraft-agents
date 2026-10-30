(ns world-test.craft-steps
  "tools/event-counts.mjs craft-steps: counts the measured seconds one craft step takes (jobs.lib.cost.table ::craft-step) from
  the bodies' logs alone: the mean seconds from a craft action's start to its end (retry-wins/scan-line's rows of kind :craft), timeouts left out and counted apart.
  Pure parts: report."
  (:require [world-test.retry-wins :as retry-wins]))

(def scan-line retry-wins/scan-line)

(defn report
  "{:n crafts :timeouts k :timeout-s s :mean s :setting} of the scanned rows' craft actions, start to end: mean and n the finished ones, timeout-s the mean seconds a timed-out one took; {:n 0 :timeouts 0} with none."
  [rows]
  (let [rows (filter #(= :craft (:kind %)) rows)
        starts (into {} (keep #(when (= :start (:edge %)) [(:action %) (:t %)])) rows)
        ends (filter #(and (= :end (:edge %)) (starts (:action %))) rows)
        ss (for [e ends :when (not (:fail? e))] (/ (- (:t e) (starts (:action e))) 1000))
        n (count ss)
        mean-of (fn [xs] (when (seq xs) (/ (js/Math.round (* 1000 (/ (reduce + xs) (count xs)))) 1000)))
        mean (mean-of ss)
        timeout-s (mean-of (for [e ends :when (:fail? e)] (/ (- (:t e) (starts (:action e))) 1000)))]
    (cond-> {:n n :timeouts (count (filter :fail? ends))}
      timeout-s (assoc :timeout-s timeout-s)
      mean (assoc :mean mean :setting mean))))
