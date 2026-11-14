(ns jobs.lib.cost.stock
  "What more of an item already held in quantity is worth: only the part of a gain the expected demand of the carried
  reserve's days (cost.food/days) leaves uncovered. Pure over counts."
  (:require [jobs.lib.cost.food :as food]))

;; Broad guess (Q49/Q115), erring high; card ceee5254 replaces it with a measured figure.
(def log-demand-per-day "Logs the body is expected to use in a day, planks, sticks, tools, fuel and building together." 64)

(defn wanted-share
  "The share (0 to 1) of n more of an item that is still wanted when held of it are in stock and demand-per-day of it is
  used a day: what the days' demand leaves uncovered, over n. 1 for n of 0."
  [held n demand-per-day]
  (if (pos? n)
    (/ (max 0 (min n (- (* food/days demand-per-day) held))) n)
    1))

(defn log-share
  "wanted-share of n more logs against log-demand-per-day."
  [held n]
  (wanted-share held n log-demand-per-day))
