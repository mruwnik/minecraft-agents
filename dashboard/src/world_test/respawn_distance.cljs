(ns world-test.respawn-distance
  "tools/event-counts.mjs respawn-distance: counts the measured prior behind jobs.lib.cost.death ::unknown-respawn-blocks from the
  trial bodies' logs alone (no fold): for each death followed by its respawn, the straight distance in blocks from the spot of the
  death to where the body respawned. Pure parts: scan-line, distances, report."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]))

(defn scan-line
  "One log line -> {:k :died|:respawned :pos {:x :y :z}} for a body's death or respawn, else nil."
  [line]
  (when (and (str/includes? line ":source :body") (re-find #":kind :(died|respawned)\b" line))
    (let [e (reader/read-string line)]
      (when-let [pos (get-in e [:data :pos])]
        {:k (:kind e) :pos pos}))))

(defn distances
  "The blocks from each death to the respawn that follows it in one body's ordered events (a death with another before its respawn
  is counted once, at the last)."
  [events]
  (->> (partition-by :k events)
       (partition 2 1)
       (keep (fn [[deaths respawns]]
               (let [a (:pos (last deaths)) b (:pos (first respawns))]
                 (when (and (= :died (:k (first deaths))) (= :respawned (:k (first respawns))))
                   (js/Math.sqrt (reduce + (map #(js/Math.pow (- (get a %) (get b %)) 2) [:x :y :z])))))))))

(defn report
  "{:n deaths :median blocks :q1 :q3} of the distances (nearest rank)."
  [ds]
  (let [v (vec (sort ds))
        at (fn [q] (when (seq v) (js/Math.round (nth v (min (dec (count v)) (js/Math.floor (* q (count v))))))))]
    {:n (count v) :median (at 0.5) :q1 (at 0.25) :q3 (at 0.75)}))
