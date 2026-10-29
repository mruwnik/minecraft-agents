(ns world-test.tree-logs
  "tools/event-counts.mjs tree-logs: counts the measured prior behind jobs.lib.trees ::logs-per-tree from the bodies' logs
  alone: the :logs of each jobs.forestry.fell-tree ending :done, as n, mean (the prior: a worth is an expected total) and median.
  Pure parts: scan-line, report.")

(defn scan-line
  "The logs a fell-tree :done ending of one log line reports, else nil."
  [line]
  (when (and (.includes line ":kind :ended") (.includes line "jobs.forestry.fell-tree: done"))
    (some-> (re-find #":logs (\d+)" line) second js/parseInt)))

(defn report
  "{:n :mean :median} of the logs per felled tree, nil with none."
  [logs]
  (when (seq logs)
    (let [v (vec (sort logs)) n (count v)]
      {:n n
       :mean (/ (js/Math.round (* 100 (/ (reduce + v) n))) 100)
       :median (if (odd? n) (v (quot n 2)) (/ (+ (v (dec (quot n 2))) (v (quot n 2))) 2))})))
