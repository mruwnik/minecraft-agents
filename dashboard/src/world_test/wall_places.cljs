(ns world-test.wall-places
  "tools/event-counts.mjs wall-places: counts the measured priors behind jobs.survival.plug-way ::place-s,
  jobs.survival.wall-in ::mob-fails and jobs.survival.respond.options ::pillar-mob-fails from the trial bodies' debug logs:
  each walls attempt of a flight (a /plug/walls or /shut-in/walls child under the hostile-near reflex) and each pillar (a
  /pillar child), its class by the melee hostiles of the flee choice (retreat.choice) logged within choice-ms before it
  (:beside one stands in or beside a side cell, within 2 cells across, 1 up or down, of the body's, for walls; in or beside
  the body's cell, within 1 across, for a pillar; :clear none does; no such choice: no class), how it ended (done, or failed
  for a mob in a cell: walls by :hostile-in-cell, :cells-occupied, a place refused as occupied or with a mob named near the
  cell, another failure saying nothing of mobs; a pillar by :place-failed, its jump-place refused for the body's own cell),
  and the seconds between a walls attempt's placements. Rows by [body generation job], folded into the counts file's :wall-places so they
  outlive the debug logs, which rotate in hours.
  Pure parts: scan, fold, report."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            [jobs.lib.cost.threat :as threat]))

(def choice-ms "Longest age of the flee choice an attempt's class is read from." 3000)

(def mob-reasons #{:hostile-in-cell :cells-occupied})

(defn melee?
  "Whether name is a hostile's (threat/mob-hit) that strikes: not a shooter, a creeper or an enderman."
  [name]
  (and (contains? threat/mob-hit name) (not (or (threat/ranged? {:name name}) (#{"creeper" "enderman"} name)))))

(defn beside?
  "Whether a melee hostile of hostiles [[name x y z]] stands within across cells (default 2: in or beside a side cell), 1
  up or down, of a body whose feet are at pos."
  ([pos hostiles] (beside? pos hostiles 2))
  ([{:keys [x y z]} hostiles across]
   (let [fx (js/Math.floor x) fy (js/Math.floor y) fz (js/Math.floor z)]
     (boolean (some (fn [[n hx hy hz]] (and (melee? n) (<= (js/Math.abs (- hx fx)) across) (<= (js/Math.abs (- hz fz)) across) (<= (js/Math.abs (- hy fy)) 1)))
                    hostiles)))))

(defn mob-place?
  "Whether a place action's done data says a mob stood in its cell."
  [{:keys [status reason]}]
  (or (= "occupied" status)
      (and (= "failed" status) (some->> reason (re-find #"entities near the cell: ([^)]*)") second
                                        (re-seq #"([a-z_]+)#") (some (comp melee? second))
                                        boolean))))

(def walls-id #"^(j\d+/(?:(?:plug|shut-in)/walls|pillar))(/fill/place)?$")

(defn scan
  "A body's log lines (any order) -> {[generation job] row} of its walls and pillar attempts that ended: {:t :way :class
  :end :done? :mob? :placed [ms]}."
  [lines]
  (let [events (->> lines
                    (filter #(and (str/includes? % ":hostile-near")
                                  (or (str/includes? % ":kind :retreat.choice") (str/includes? % "/walls") (str/includes? % "/pillar"))))
                    (map reader/read-string)
                    (sort-by :time-ms))]
    (loop [[e & more] events choice nil open {} out {}]
      (if (nil? e)
        out
        (let [{:keys [time-ms kind context data generation-id]} e
              [_ walls place] (some->> (:job-id context) (re-find walls-id))
              k [generation-id walls]
              pillar? (some-> walls (str/ends-with? "/pillar"))]
          (cond
            (= :retreat.choice kind)
            (recur more {:t time-ms :pos (:pos data)
                         :hostiles (keep #(when-let [[_ n x y z] (re-find #"^([a-z_]+) \d+ \[(-?\d+) (-?\d+) (-?\d+)\]" %)]
                                            [n (js/parseInt x) (js/parseInt y) (js/parseInt z)])
                                         (:hostiles data))}
                   open out)
            (nil? walls) (recur more choice open out)
            (and place (= :done kind) (= "place" (:name data)) (contains? open k))
            (recur more choice (update open k #(cond-> (update % :mob? (fn [m] (or m (mob-place? data))))
                                                  (= "placed" (:status data)) (update :placed conj time-ms)))
                   out)
            (and (not place) (= :child_started kind))
            (recur more choice (assoc open k {:t time-ms :way (if pillar? :pillar :walls) :placed [] :mob? false
                                             :class (when (and choice (<= (- time-ms (:t choice)) choice-ms) (:pos choice))
                                                      (if (beside? (:pos choice) (:hostiles choice) (if pillar? 1 2)) :beside :clear))})
                   out)
            (and (not place) (= :child_ended kind) (contains? open k))
            (recur more choice (dissoc open k)
                   (assoc out k (assoc (get open k) :end (:status data) :done? (= :done (:status data))
                                       :mob? (or (:mob? (get open k)) (contains? (if pillar? #{:place-failed} mob-reasons) (:reason data))))))
            :else (recur more choice open out)))))))

(defn fold
  "counts with rows {[body generation job] row} merged into :wall-places; a row already there stays."
  [counts rows]
  (update counts :wall-places #(merge rows %)))

(defn report
  "{:gap {:mean s :n gaps} :beside {:k :n} :clear {:k :n} :pillar {:beside {:k :n} :clear {:k :n}}}: the mean seconds
  between two placements of one walls attempt, and per class the walls attempts (:pillar: the pillars) that failed for a
  mob (k) of those that closed or failed for a mob (n). A row with no :way is a walls attempt."
  [rows]
  (let [{pillars true walls false} (group-by #(= :pillar (:way %)) (vals rows))
        gaps (mapcat (fn [{:keys [placed]}] (map #(/ (- %2 %1) 1000) placed (rest placed))) walls)
        of (fn [rs cls] (let [told (filter #(and (= cls (:class %)) (or (:done? %) (:mob? %))) rs)]
                          {:k (count (filter #(and (:mob? %) (not (:done? %))) told)) :n (count told)}))]
    {:gap {:mean (when (seq gaps) (/ (js/Math.round (* 100 (/ (reduce + gaps) (count gaps)))) 100)) :n (count gaps)}
     :beside (of walls :beside)
     :clear (of walls :clear)
     :pillar {:beside (of pillars :beside) :clear (of pillars :clear)}}))
