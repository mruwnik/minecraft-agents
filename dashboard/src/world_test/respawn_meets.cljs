(ns world-test.respawn-meets
  "tools/event-counts.mjs respawn-meets: counts the measured prior behind jobs.lib.cost.answer ::respawn-meets from the trial
  bodies' logs alone (no fold): of n deaths to a mob whose respawn lies within the mob's give-up range of the death's spot, k
  where an event after the respawn names that mob's id again within window-ms. Pure: report."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            [jobs.lib.cost.threat :as threat]))

(defn report
  "{:k :n} of one body's log lines (any order)."
  [lines window-ms]
  (let [rows (vec (sort-by :t (keep (fn [line] (when-let [t (second (re-find #":time-ms (\d+)" line))] {:t (js/parseInt t) :line line})) lines)))
        event (fn [{:keys [line]}] (reader/read-string line))
        body? (fn [kind {:keys [line]}] (and (str/includes? line ":source :body") (str/includes? line (str ":kind " kind))))
        dist (fn [a b] (js/Math.hypot (- (:x a) (:x b)) (- (:y a) (:y b)) (- (:z a) (:z b))))
        deaths (keep-indexed
                (fn [i r]
                  (when (body? ":died" r)
                    (let [d (:data (event r))
                          {:keys [id name]} (some :attacker (:damages d))
                          back (first (filter #(body? ":respawned" %) (subvec rows (inc i))))]
                      (when (and id back (<= (dist (:pos d) (get-in (event back) [:data :pos])) (threat/give-up-range name)))
                        (let [named (re-pattern (str "(?::id |\\b" name " )" id "\\b"))]
                          (boolean (some (fn [r] (and (< (:t back) (:t r) (+ (:t back) window-ms 1)) (re-find named (:line r)))) rows)))))))
                rows)]
    {:k (count (filter true? deaths)) :n (count deaths)}))
