(ns world-test.item-costs
  "tools/event-counts.mjs item-costs: counts the measured priors behind jobs.lib.cost.value ::gather-counts from the trial
  bodies' logs: each gather of an obtain that mined its need (a /gather-<item> child that logged a mine.done), the seconds
  from its first event to its last mine.done and the items those mines got (0 for a fruitless mine: its seconds count).
  Rows by [body generation job], folded into the counts file's :item-costs so they outlive the logs, which rotate.
  Pure parts: scan-line, spans, fold, report."
  (:require [clojure.string :as str]))

(defn- field [line re] (second (re-find re line)))

(defn scan-line
  "One log line -> {:key [generation gather-job] :item :t ms :got n|nil} of a line of a gather child's job tree (:got on the
  gather's own mine.done), else nil."
  [line]
  (when-let [[_ job item] (re-find #":job-id \"([^\"]*/gather-([a-z0-9_]+))[/\"]" line)]
    (let [own? (str/includes? line (str ":job-id \"" job "\""))]
      {:key [(field line #":generation-id \"([^\"]*)\"") job] :item item :t (js/parseFloat (field line #":time-ms (\d+)"))
       :got (when (and own? (str/includes? line ":kind :mine.done")) (js/parseInt (or (field line #":got (\d+)") "0")))})))

(defn spans
  "{key {:item :s :got}} of the scanned rows whose gather logged a mine.done: seconds from its first row to its last mine.done."
  [rows]
  (into {} (for [[k rs] (group-by :key rows)
                 :let [dones (filter :got rs)]
                 :when (seq dones)]
             [k {:item (:item (first rs))
                 :s (/ (- (apply max (map :t dones)) (apply min (map :t rs))) 1000)
                 :got (reduce + (map :got dones))}])))

(defn fold
  "counts with rows {[body generation job] row} merged into :item-costs; of a row read twice the longer stays (a rotated
  log may have lost a gather's start)."
  [counts rows]
  (update counts :item-costs #(merge-with (fn [old new] (if (> (:s new) (:s old)) new old)) % rows)))

(defn- r3 [x] (/ (js/Math.round (* 1000 x)) 1000))

(defn report
  "{item {:s total seconds :got items :calls gathers :s-per-item}} of the rows (an item none of whose gathers got one is left out)."
  [rows]
  (into (sorted-map)
        (for [[item rs] (group-by :item (vals rows))
              :let [s (reduce + (map :s rs)) got (reduce + (map :got rs))]
              :when (pos? got)]
          [item {:s (r3 s) :got got :calls (count rs) :s-per-item (r3 (/ s got))}])))
