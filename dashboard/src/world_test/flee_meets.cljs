(ns world-test.flee-meets
  "tools/event-counts.mjs flee-meets: counts the measured prior behind jobs.lib.cost.answer ::flee-meets from the trial bodies'
  logs alone (no fold): of n flights (hostile.fled), k from a mob that an event of another run (job) names again by its id
  within window-ms after. Pure: report."
  (:require [clojure.string :as str]))

(def mob-id #"\b(?:zombie|zombie_villager|husk|drowned|skeleton|stray|creeper|spider|cave_spider|witch|enderman|phantom|slime|silverfish|pillager|vindicator) (\d+)\b")

(defn field [re line] (second (re-find re line)))

(defn report
  "{:k :n} of one body's log lines (any order)."
  [lines window-ms]
  (let [rows (keep (fn [line]
                     (when-let [t (field #":time-ms (\d+)" line)]
                       {:t (js/parseInt t) :job (field #":job-id \"([^\"]+)\"" line) :ids (set (map second (re-seq mob-id line)))
                        :fled? (str/includes? line ":kind :hostile.fled")}))
                   lines)
        named (group-by first (for [r rows id (:ids r)] [id r]))
        other? (fn [job r] (not (or (= job (:job r)) (str/starts-with? (str (:job r)) (str job "/")))))
        flights (filter :fled? rows)]
    {:k (count (filter (fn [{:keys [t job ids]}]
                         (some (fn [id] (some (fn [[_ r]] (and (< t (:t r) (+ t window-ms 1)) (other? job r))) (named id))) ids))
                       flights))
     :n (count flights)}))
