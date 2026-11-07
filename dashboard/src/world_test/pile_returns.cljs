(ns world-test.pile-returns
  "tools/event-counts.mjs pile-returns: counts the measured prior behind jobs.lib.cost.death ::pile-returns from the trial
  bodies' logs alone (no fold): of n deaths whose respawn lies within threat/despawn-blocks of the death's spot (the walk
  back beats the despawn), k where at least half the items carried at the death were picked up again within reach-blocks
  of the spot, before the next death and within window-ms; a death with no pick-up logged within an hour of it is left
  out (its log records none). Pure: report."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            [jobs.lib.cost.threat :as threat]))

(def hour-ms 3600000)

(defn report
  "{:k :n} of one body's log lines (any order)."
  [lines window-ms reach-blocks]
  (let [rows (vec (sort-by :t (keep (fn [line] (when-let [t (second (re-find #":time-ms (\d+)" line))] {:t (js/parseInt t) :line line})) lines)))
        event (fn [{:keys [line]}] (reader/read-string line))
        body? (fn [kind {:keys [line]}] (and (str/includes? line ":source :body") (str/includes? line (str ":kind " kind))))
        dist (fn [a b] (js/Math.hypot (- (:x a) (:x b)) (- (:y a) (:y b)) (- (:z a) (:z b))))
        picks (filterv #(body? ":picked-up" %) rows)
        returns (keep-indexed
                 (fn [i r]
                   (when (body? ":died" r)
                     (let [{:keys [pos inventory]} (:data (event r))
                           carried (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} inventory)
                           later (subvec rows (inc i))
                           back (first (filter #(body? ":respawned" %) later))
                           end (min (+ (:t r) window-ms) (or (:t (first (filter #(body? ":died" %) later))) js/Infinity))]
                       (when (and (seq carried) back
                                  (<= (dist pos (get-in (event back) [:data :pos])) threat/despawn-blocks)
                                  (some #(< (js/Math.abs (- (:t %) (:t r))) hour-ms) picks))
                         (let [got (->> picks
                                        (filter #(< (:t r) (:t %) end))
                                        (map (comp :data event))
                                        (filter #(<= (dist (:pos %) pos) reach-blocks))
                                        (reduce (fn [m {:keys [item count]}] (update m item (fnil + 0) count)) {}))
                               back-n (reduce + (map (fn [[n c]] (min c (get got n 0))) carried))]
                           (>= (* 2 back-n) (reduce + (vals carried))))))))
                 rows)]
    {:k (count (filter true? returns)) :n (count returns)}))
