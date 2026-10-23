(ns world-test.drowned-meets
  "tools/event-counts.mjs drowned-meets: the drowned meet rates in water (jobs.lib.cost.answer ::drowned-meet-per-s,
  ::drowned-meet-other-per-s) and what each meet cost, from the event records of world-test.event-counts.
  Exposure: the seconds between two positions of one body run at most gap-ms apart with its feet in water, counted to the
  first one's class. Meets: the first decision of each respond run about a drowned, counted to the class of the body's
  cell (its last position within gap-ms; the decision's own :pos is the mob's). A class splits what vanilla's drowned spawn rule splits: river or ocean biome or other, the feet cell lit (block
  light), dark with sky light 7 or less, or dark under the open sky, and :deep when its column holds water at block light 0
  over water (jobs.lib.look/water-fn's class 2). Light and biome are those of the recorded chunks when the rows are
  folded (at: the JS adapter's (fn [x y z]) -> {:name :block :sky :biome} or nil for a column never recorded).
  A meet's outcome: the drowned's hp and hits in the run (to its end plus tail-ms) and within window-ms, a death within
  window-ms ([cause t]), the swings tried and landed (debug log), the weapon it fought with.
  check: each meet in water priced by cost.answer for its first decision (threat-answers in water, no shore known)
  against what it cost.
  Pure parts: class-of, exposure, meets, prior, report."
  (:require [clojure.string :as str]
            [jobs.lib.cost.answer :as answer]
            [jobs.lib.cost.fight :as fight]))

(def gap-ms "Longest gap between two positions whose seconds still count as exposure." 10000)
(def tail-ms "Time after a run ends within which a hurt still counts as its own." 1500)
(def window-ms "Time after a meet within which its hp and a death count." 30000)

(defn class-of
  "The class of the feet cell of position {:x :y :z} by at, nil when it is not water or its column was never recorded."
  [at {:keys [x y z]}]
  (let [x (js/Math.floor x) y (js/Math.floor y) z (js/Math.floor z)
        water? #(= "water" (:name %))
        c (at x y z)]
    (when (water? c)
      (let [deep? (loop [y y]
                    (let [here (at x y z) below (at x (dec y) z)]
                      (cond (not (water? below)) false
                            (zero? (:block here)) true
                            :else (recur (dec y)))))]
        (str (if (re-find #"river|ocean" (str (:biome c))) "river-ocean" "other") "/"
             (cond (pos? (:block c)) "lit" (<= (:sky c) 7) "dark-by-day" :else "dark-at-night") "/"
             (if deep? "deep" "shallow"))))))

(defn deep? [cls] (str/ends-with? (str cls) "/deep"))

(defn positions [records] (filter #(#{:pos :at} (:k %)) records))

(defn exposure
  "Rows [{:id [gen minute] :t :seconds {class s}}] of records (sorted by :t): each body run's seconds in water by class,
  a row a minute of its first position."
  [at records]
  (->> (positions records)
       (partition 2 1)
       (keep (fn [[a b]]
               (when (and (= (:gen a) (:gen b)) (<= (- (:t b) (:t a)) gap-ms))
                 (when-let [cls (class-of at a)]
                   [[(:gen a) (js/Math.floor (/ (:t a) 60000))] (:t a) cls (/ (- (:t b) (:t a)) 1000)]))))
       (reduce (fn [m [id t cls s]] (-> m (update-in [id :seconds cls] (fnil + 0) s) (update-in [id :t] #(or % t)) (assoc-in [id :id] id))) {})
       vals))

(defn meets
  "Rows [{:id [gen job] :t :class :health :decisions :seconds :hp :hits :hp-window :died :tried :landed :weapon}] of the respond
  runs whose first decision is about a drowned, in records (sorted by :t)."
  [at records]
  (let [by-gen (group-by :gen records)]
    (for [[gen rs] by-gen
          [job [first-d :as ds]] (group-by :job (filter #(= :decided (:k %)) rs))
          :when (= "drowned" (:mob first-d))
          :let [t (:t first-d)
                end (some #(when (and (= :ended (:k %)) (= job (:job %)) (>= (:t %) t)) (:t %)) rs)
                run-end (+ (or end (+ t window-ms)) tail-ms)
                within (filter #(<= t (:t %) (+ t window-ms)) rs)
                in-run (filter #(<= (:t %) run-end) within)
                drowned-hurt (fn [xs] (filter #(and (= :hurt (:k %)) (contains? (:causes %) "drowned")) xs))
                own (fn [k] (filter #(and (= k (:k %)) (= job (:job %))) in-run))
                swings (concat (own :hit) (own :miss))]]
      {:id [gen job] :t t
       :class (some->> (last (filter #(< (- t gap-ms) (:t %) (inc t)) (positions rs))) (class-of at))
       :health (:health first-d)
       :decisions (mapv :decision ds)
       :seconds (when end (/ (- end t) 1000))
       :hp (reduce + 0 (map :amount (drowned-hurt in-run)))
       :hits (reduce + 0 (map #(or (:hits %) 1) (drowned-hurt in-run)))
       :hp-window (reduce + 0 (map :amount (drowned-hurt within)))
       :died (some #(when (= :died (:k %)) [(:cause %) (:t %)]) within)
       :tried (when (seq swings) (count swings))
       :landed (count (own :hit))
       :weapon (or (some :item (own :equip)) (some #(when (= job (:job %)) (:wear %)) (filter #(= :ended (:k %)) in-run)))})))

(defn prior
  "{:deep {:k :n :per-s} :other {...}} over exposure and meet rows: k meets and n seconds in water a drowned can spawn in
  (deep) and in other water, per-s (k + 1) / n; :rows the same by class."
  [exposure-rows meet-rows]
  (let [secs (apply merge-with + (map :seconds exposure-rows))
        ks (frequencies (keep :class meet-rows))
        rows (into (sorted-map) (map (fn [c] [c {:k (get ks c 0) :n (js/Math.round (get secs c 0))}])) (into (set (keys secs)) (keys ks)))
        pooled (fn [pick] (let [rs (filter (comp pick key) rows)
                                k (reduce + 0 (map (comp :k val) rs))
                                n (reduce + 0 (map (comp :n val) rs))]
                            {:k k :n n :per-s (when (pos? n) (/ (js/Math.round (* 1e5 (/ (inc k) n))) 1e5))}))]
    {:deep (pooled deep?) :other (pooled (complement deep?)) :rows rows}))

(defn priced
  "{:answer :hp :death} cost.answer gives the first decision of meet row (its health and weapon, no armour, in water with no
  shore known): the answer decide takes, and the expected hp and death chance of the answer the body took."
  [{:keys [health weapon decisions]}]
  (let [body {:health health :food 20 :weapon weapon :swim ##Inf}
        answers (answer/threat-answers body "drowned" nil)
        a (get answers (first decisions))]
    {:answer (:answer (answer/decide body answers nil))
     :hp (when a (answer/expected-damage a))
     :death (when a (answer/answer-death-chance body a nil))}))

(defn r2 [x] (when x (/ (js/Math.round (* 100 x)) 100)))

(defn outcome
  "The meets in rows taken by one first decision: what they cost (hp and deaths in window-ms, a death once however many
  meets it followed, hp in the runs, the drowned's hits a second of
  the runs, swings landed of tried) and what cost.answer priced them at."
  [rows]
  (let [n (count rows)
        timed (filter :seconds rows)
        swung (filter :tried rows)
        p (map priced rows)]
    {:n n
     :hp (r2 (/ (reduce + 0 (map #(min (:hp-window %) (:health %)) rows)) (max n 1)))
     :run-hp (r2 (/ (reduce + 0 (map :hp rows)) (max n 1)))
     :deaths (count (distinct (keep #(some->> (:died %) (vector (first (:id %)))) rows)))
     :hits-per-s (when (seq timed) (r2 (/ (reduce + 0 (map :hits timed)) (reduce + 0 (map :seconds timed)))))
     :seconds (r2 (reduce + 0 (map :seconds timed)))
     :swings {:runs (count swung) :tried (reduce + 0 (map :tried swung)) :landed (reduce + 0 (map :landed swung))}
     :weapon-known (count (filter :weapon rows))
     :priced {:hp (r2 (/ (reduce + 0 (keep :hp p)) (max n 1))) :deaths (r2 (reduce + 0 (keep :death p)))
              :answers (frequencies (map :answer p))}}))

(defn wet-fight
  "{:k :n :per-swing} of the meets in rows first fought in water with a sword or axe: k the drowned's blows in the runs,
  n the swings their kills need (fight/swings), per-swing (k + 0.5) / n (fight ::drowned-wet-hits-per-swing)."
  [rows]
  (let [rs (filter #(and (:class %) (= :fight (first (:decisions %))) (some->> (:weapon %) (re-find #"_(sword|axe)$"))) rows)
        k (reduce + 0 (map :hits rs))
        n (reduce + 0 (map #(fight/swings (:weapon %) {:name "drowned"}) rs))]
    {:runs (count rs) :k k :n n :per-swing (when (pos? n) (/ (js/Math.round (* 100 (/ (+ k 0.5) n))) 100))}))

(defn report
  "The prior, the in-water fight's blows per swing (wet-fight), and the outcomes of the meets in water (a class) by first decision, and by it and the weapon (unknown: none in the logs; priced as a fist)."
  [exposure-rows meet-rows]
  (let [wet (filter :class meet-rows)]
    {:prior (prior exposure-rows meet-rows)
     :wet-fight (wet-fight meet-rows)
     :meets (count meet-rows) :in-water (count wet)
     :outcome (into (sorted-map) (map (fn [[d rs]] [d (outcome rs)])) (group-by (comp first :decisions) wet))
     :by-weapon (into (sorted-map) (map (fn [[k rs]] [k (outcome rs)])) (group-by (juxt (comp first :decisions) #(or (:weapon %) "unknown")) wet))}))

(defn better-meet
  "Of two rows of one meet, the one with a class (its chunk recorded), then the one that saw more of it (a rotation may
  have cut the older read short), then the older."
  [a b]
  (let [seen #(+ (or (:tried %) 0) (:hits %))]
    (cond (and (nil? (:class a)) (:class b)) b
          (and (:class a) (nil? (:class b))) a
          (> (seen b) (seen a)) b
          :else a)))

(defn better-exposure "Of two rows of one minute, the one with more seconds." [a b]
  (if (> (reduce + (vals (:seconds b))) (reduce + (vals (:seconds a)))) b a))

(defn fold
  "counts with exposure-rows and meet-rows merged in by id (:drowned-exposure, :drowned-meets) by better-exposure and better-meet."
  [counts exposure-rows meet-rows]
  (let [merge-in (fn [better rows] (fn [m] (reduce (fn [m r] (update m (:id r) #(if % (better % r) r))) (or m {}) rows)))]
    (-> counts
        (update :drowned-exposure (merge-in better-exposure exposure-rows))
        (update :drowned-meets (merge-in better-meet meet-rows)))))
