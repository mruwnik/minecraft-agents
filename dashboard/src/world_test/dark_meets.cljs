(ns world-test.dark-meets
  "tools/event-counts.mjs dark-meets: the hostile meets per dark second (jobs.lib.cost.value ::spawn-chance-per-s) and the hp
  they cost per dark second, from the trial bodies' event records (world-test.event-counts/read-agent) and the recorded
  chunks. A cell is dark when its block light and sky light are both 0 (where a mob can spawn), read from the recorded
  chunks (at: (fn [x y z]) -> {:block :sky} or nil for a column never recorded).
  Exposure: the seconds between two positions of one body run at most gap-ms apart with the first one's feet cell dark,
  by y band. Meets: the first decision of each respond run about a mob of threat/spawn-weights, not another run's about one
  within window-ms (the same meet going on), counted to the band of the body's cell when dark (its last position within
  gap-ms). Hurt: the hp a spawnable mob (or its arrow) dealt at a dark cell. Pure parts: exposure, meets, hurt, report."
  (:require [jobs.lib.cost.threat :as threat]))

(def gap-ms "Longest gap between two positions whose seconds still count as exposure." 10000)
(def window-ms "Time after a meet within which a run about a mob is the same meet going on." 30000)

(defn band "The y band of height y." [y] (cond (< y 40) :under-40 (< y 56) :40-55 :else :surface))

(defn dark-band
  "The band of position {:x :y :z} when its feet cell is dark by at, else nil."
  [at {:keys [x y z]}]
  (let [x (js/Math.floor x) y (js/Math.floor y) z (js/Math.floor z)
        c (at x y z)]
    (when (and c (zero? (:block c)) (zero? (:sky c))) (band y))))

(defn positions [records] (filter #(#{:pos :at} (:k %)) records))

(defn exposure
  "{band s} of records (sorted by :t): the seconds between close positions of one run, by the band of the first's dark cell."
  [at records]
  (->> (positions records)
       (partition 2 1)
       (keep (fn [[a b]]
               (when (and (= (:gen a) (:gen b)) (<= (- (:t b) (:t a)) gap-ms))
                 (when-let [bd (dark-band at a)] [bd (/ (- (:t b) (:t a)) 1000)]))))
       (reduce (fn [m [bd s]] (update m bd (fnil + 0) s)) {})))

(defn spawned? [mob] (contains? threat/spawn-weights mob))

(defn meets
  "The bands of the meets in records (sorted by :t), one entry per meet: the band of the body's dark cell, or nil in a lit or unrecorded one."
  [at records]
  (let [pos (group-by :gen (positions records))]
    (for [[gen rs] (group-by :gen records)
          [job [d]] (group-by :job (filter #(and (= :decided (:k %)) (spawned? (:mob %))) rs))
          :when (not (some #(and (= :decided (:k %)) (spawned? (:mob %)) (not= job (:job %)) (< (- (:t d) window-ms) (:t %) (:t d))) rs))
          :let [here (last (filter #(< (- (:t d) gap-ms) (:t %) (inc (:t d))) (get pos gen)))]]
      (some->> here (dark-band at)))))

(defn hurt
  "{:hp :hits} the spawnable mobs (and arrows) dealt at dark cells in records, by band of the cell hurt at."
  [at records]
  (->> records
       (filter #(and (= :hurt (:k %)) (:at %) (some (fn [c] (or (spawned? c) (= "arrow" c))) (:causes %))))
       (keep (fn [r] (when-let [bd (dark-band at (:at r))] [bd (:amount r)])))
       (reduce (fn [m [bd a]] (-> m (update-in [bd :hp] (fnil + 0) a) (update-in [bd :hits] (fnil inc 0)))) {})))

(defn r5 [x] (/ (js/Math.round (* 1e5 x)) 1e5))

(defn report
  "{band {:n-s :k :per-s :hp :hp-per-s}} and the pooled :all, over the exposure {band s} maps, meet band lists and hurt maps
  of the bodies: per-s (k + 1) / n-s, hp-per-s hp / n-s."
  [exposures meet-bands hurts]
  (let [secs (apply merge-with + {} exposures)
        ks (frequencies (remove nil? (apply concat meet-bands)))
        hp (apply merge-with (fn [a b] (merge-with + a b)) {} hurts)
        row (fn [n k h] {:n-s (js/Math.round n) :k k :per-s (when (pos? n) (r5 (/ (inc k) n)))
                         :hp (r5 (or (:hp h) 0)) :hp-per-s (when (pos? n) (r5 (/ (or (:hp h) 0) n)))})
        bands (sort (into (set (keys secs)) (keys ks)))]
    {:bands (into (sorted-map) (map (fn [b] [b (row (get secs b 0) (get ks b 0) (get hp b))])) bands)
     :all (row (reduce + 0 (vals secs)) (reduce + 0 (vals ks)) {:hp (reduce + 0 (keep :hp (vals hp)))})}))
