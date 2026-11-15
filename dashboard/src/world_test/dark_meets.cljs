(ns world-test.dark-meets
  "tools/event-counts.mjs dark-meets: the hostile meets per dark second (jobs.lib.cost.value ::spawn-chance-per-s) and the hp
  they cost per dark second, from the trial bodies' event records (world-test.event-counts/read-agent) and the recorded
  chunks. A cell is dark when it is not look/lit-light? (where a mob can spawn): its recorded block light and sky light
  less the sky darkening at the time of day of the record (clear sky; rain only makes more dark), at: (fn [x y z]) ->
  {:block :sky} or nil for a column never recorded, tod: (fn [t-ms]) -> time of day or nil (see clock).
  Exposure: the seconds between two positions of one body run at most gap-ms apart with the first one's feet cell dark,
  by y band and open sky. Meets: the first decision of each respond run about a mob of threat/spawn-weights, not another run's about one
  within window-ms (the same meet going on), counted to the band of the body's cell when dark (its last position within
  gap-ms). Hurt: the hp a spawnable mob (or its arrow) dealt at a dark cell. Pure parts: exposure, meets, hurt, report."
  (:require [jobs.lib.cost.threat :as threat]
            [jobs.lib.look :as look]
            [jobs.lib.solid :as solid]))

(def gap-ms "Longest gap between two positions whose seconds still count as exposure." 10000)
(def window-ms "Time after a meet within which a run about a mob is the same meet going on." 30000)

(defn band "The y band of height y." [y] (cond (< y 40) :under-40 (< y 56) :40-55 :else :surface))

(def ticks-per-ms "The server's 20 ticks a second." (/ 1 50))
(def day-ticks 24000)

(defn clock
  "(fn [t-ms]) -> time of day (ticks) or nil, from anchors [[t-ms time-of-day]] the bodies' memory entries stamped. Between two
  anchors the clock runs 20 ticks a second; where the later anchor is not the time that predicts (a stopped server, a
  time set, a sleep through the night) within a fiftieth of the span and 100 ticks, the span is unknown."
  [anchors]
  (let [as (vec (sort-by first (distinct anchors)))
        ts (mapv first as)]
    (fn [t]
      (let [i (loop [lo 0 hi (count ts)] (if (< lo hi) (let [m (quot (+ lo hi) 2)] (if (<= (ts m) t) (recur (inc m) hi) (recur lo m))) lo))
            [ta wa] (get as (dec i))
            [tb wb] (get as i)]
        (when (and ta tb)
          (let [span (* (- tb ta) ticks-per-ms)
                err (- (mod (+ (- wb wa span) (/ day-ticks 2)) day-ticks) (/ day-ticks 2))]
            (when (<= (js/Math.abs err) (+ 100 (/ span 50)))
              (mod (+ wa (* (- t ta) ticks-per-ms) (* err (/ (- t ta) (- tb ta)))) day-ticks))))))))

(defn enclosed?
  "Whether the standing room at feet cell x y z is shut in: both its cells are recorded room and every cell round them (four
  sides of each, and over the head) is recorded solid, so nothing spawns there and nothing walks in. A cell never recorded
  counts as open."
  [at x y z]
  (let [name-at (fn [x y z] (:name (at x y z)))
        walls [[1 0] [-1 0] [0 1] [0 -1]]]
    (and (solid/room? (name-at x y z))
         (solid/room? (name-at x (inc y) z))
         (solid/solid? (name-at x (+ y 2) z))
         (every? (fn [[dx dz]] (and (solid/solid? (name-at (+ x dx) y (+ z dz)))
                                    (solid/solid? (name-at (+ x dx) (inc y) (+ z dz)))))
                 walls))))

(defn dark-band
  "The band of position {:x :y :z} at time t when its feet cell is dark by at and tod and not shut in (enclosed?), else nil
  (nil too without a time of day): its y band, -sky under the open sky (recorded sky light 15: dark only by the night)."
  [at tod t {:keys [x y z]}]
  (let [x (js/Math.floor x) y (js/Math.floor y) z (js/Math.floor z)
        c (at x y z)
        w (tod t)]
    (when (and c w (not (look/lit-light? (:block c) (:sky c) (look/sky-subtract-at w 0 0))) (not (enclosed? at x y z)))
      (cond-> (band y) (= 15 (:sky c)) (-> name (str "-sky") keyword)))))

(defn positions [records] (filter #(#{:pos :at} (:k %)) records))

(defn exposure
  "{band s} of records (sorted by :t): the seconds between close positions of one run, by the band of the first's dark cell."
  [at tod records]
  (->> (positions records)
       (partition 2 1)
       (keep (fn [[a b]]
               (when (and (= (:gen a) (:gen b)) (<= (- (:t b) (:t a)) gap-ms))
                 (when-let [bd (dark-band at tod (:t a) a)] [bd (/ (- (:t b) (:t a)) 1000)]))))
       (reduce (fn [m [bd s]] (update m bd (fnil + 0) s)) {})))

(defn spawned? [mob] (contains? threat/spawn-weights mob))

(defn meets
  "The bands of the meets in records (sorted by :t), one entry per meet: the band of the body's dark cell, or nil in a lit or unrecorded one."
  [at tod records]
  (let [pos (group-by :gen (positions records))]
    (for [[gen rs] (group-by :gen records)
          [job [d]] (group-by :job (filter #(and (= :decided (:k %)) (spawned? (:mob %))) rs))
          :when (not (some #(and (= :decided (:k %)) (spawned? (:mob %)) (not= job (:job %)) (< (- (:t d) window-ms) (:t %) (:t d))) rs))
          :let [here (last (filter #(< (- (:t d) gap-ms) (:t %) (inc (:t d))) (get pos gen)))]]
      (some->> here (dark-band at tod (:t here))))))

(defn hurt
  "{:hp :hits} the spawnable mobs (and arrows) dealt at dark cells in records, by band of the cell hurt at."
  [at tod records]
  (->> records
       (filter #(and (= :hurt (:k %)) (:at %) (some (fn [c] (or (spawned? c) (= "arrow" c))) (:causes %))))
       (keep (fn [r] (when-let [bd (dark-band at tod (:t r) (:at r))] [bd (:amount r)])))
       (reduce (fn [m [bd a]] (-> m (update-in [bd :hp] (fnil + 0) a) (update-in [bd :hits] (fnil inc 0)))) {})))

(def surface? #{:surface :surface-sky})

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
     :all (row (reduce + 0 (vals secs)) (reduce + 0 (vals ks)) {:hp (reduce + 0 (keep :hp (vals hp)))})
     :underground (let [under (remove surface? (keys secs))]
                    (row (reduce + 0 (map secs under)) (reduce + 0 (map #(get ks % 0) (remove surface? (keys ks))))
                         {:hp (reduce + 0 (keep #(:hp (hp %)) (remove surface? (keys hp))))}))}))
