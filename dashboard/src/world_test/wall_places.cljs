(ns world-test.wall-places
  "tools/event-counts.mjs wall-places: counts the measured priors behind jobs.survival.plug-way ::place-s,
  jobs.survival.wall-in ::mob-fails and jobs.survival.respond.options ::pillar-mob-fails from the trial bodies' debug logs:
  each walls attempt of a flight (a /plug/walls or /shut-in/walls child under the hostile-near reflex) and each pillar (a
  /pillar child), its class: walls by the melee hostiles of the flee choice (retreat.choice) logged within choice-ms
  before it (class-of; no such choice: no class), a pillar by the engine's own (the retreat.pillar before it,
  respond.pillar/mob-class), how it ended (done, or failed
  for a mob in a cell: walls by :hostile-in-cell, :cells-occupied, a place refused as occupied or with a mob named near the
  cell, another failure saying nothing of mobs; a pillar by :mob-in-cell, or a refused block or knockback whose give-up names a hostile near, :mob),
  and the seconds between a walls attempt's placements. Rows by [body generation job], folded into the counts file's :wall-places so they
  outlive the debug logs, which rotate in hours.
  Pure parts: scan, fold, report."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            [jobs.lib.cost.danger :as danger]
            [jobs.lib.cost.threat :as threat]
            [engine.settings :as settings]
            [jobs.survival.plug-way :as plug-way]))

(def choice-ms "Longest age of the flee choice an attempt's class is read from." 3000)

(def mob-reasons #{:hostile-in-cell :cells-occupied})

(defn melee?
  "Whether name is a hostile's (threat/mob-hit) that strikes: not a shooter, a creeper or an enderman."
  [name]
  (and (contains? threat/mob-hit name) (not (or (threat/ranged? {:name name}) (#{"creeper" "enderman"} name)))))

(defn arrive-s
  "Seconds before the nearest melee hostile of hostiles [[name x y z]] (the cell it stands in) could strike a body at
  pos: danger/arrival-s from the middle of its cell, as the engine's jobs.survival.wall-in/arrive-s; nil with none. The
  log names no baby."
  [pos hostiles]
  (some->> hostiles
           (filter (comp melee? first))
           (map (fn [[n x y z]] (danger/arrival-s n {:x (+ x 0.5) :y y :z (+ z 0.5)} pos)))
           seq
           (reduce min)))

(defn class-of
  "An ended walls attempt's class: :reached when a mob stood in a cell or a melee mob of the choice could strike before
  every open cell is placed (arrive-s under place-s times :open, as jobs.survival.wall-in/mob-fails is read; an attempt
  with no :open count, logged before it was emitted, under its seconds), :clear otherwise."
  [{:keys [arrive-s t end-t mob? open]}]
  (let [placing-s (if open
                    (* open (settings/get plug-way/settings ::plug-way/place-s))
                    (/ (- end-t t) 1000))]
    (if (or mob? (and arrive-s (< arrive-s placing-s))) :reached :clear)))

(defn mob-place?
  "Whether a place action's done data says a mob stood in its cell."
  [{:keys [status reason]}]
  (or (= "occupied" status)
      (and (= "failed" status) (some->> reason (re-find #"entities near the cell: ([^)]*)") second
                                        (re-seq #"([a-z_]+)#") (some (comp melee? second))
                                        boolean))))

(defn pillar-mob?
  "Whether a pillar child's ending data says it failed for a mob: a mob in its cell, or a refused block or a knockback
  with a hostile near the cell (the give-up's :mob)."
  [{:keys [reason ending]}]
  (boolean (or (= :mob-in-cell reason)
               (and (#{:place-failed :off-column} reason) (:mob (:data ending))))))

(def walls-id #"^(j\d+/(?:(?:plug|shut-in)/walls|pillar))(/fill/place)?$")

(defn scan
  "A body's log lines (any order) -> {[generation job] row} of its walls attempts and {[generation job t] row} of its
  pillar attempts (one job, built again each step) that ended: {:t :end-t :way :class :end :done? :mob? :placed [ms]}, a
  walls attempt also :arrive-s, a pillar :situation [generation feet-cell]. A flee step's retreat.search (its :from and
  :chasers) classes like a flee choice."
  [lines]
  (let [events (->> lines
                    (filter #(and (str/includes? % ":hostile-near")
                                  (or (str/includes? % ":kind :retreat.choice") (str/includes? % ":kind :retreat.search,") (str/includes? % ":kind :retreat.pillar,") (str/includes? % "/walls") (str/includes? % "/pillar"))))
                    (map reader/read-string)
                    (sort-by :time-ms))]
    (loop [[e & more] events choice nil pillar nil open {} out {}]
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
                   pillar open out)
            (= :retreat.search kind)
            (recur more {:t time-ms :pos (zipmap [:x :y :z] (:from data)) :hostiles (map (fn [[n cell]] (into [n] cell)) (:chasers data))}
                   pillar open out)
            (= :retreat.pillar kind)
            (recur more choice {:t time-ms :gen generation-id :class (:class data) :at (:at data)} open out)
            (nil? walls) (recur more choice pillar open out)
            (and (= :walls.open kind) (contains? open k))
            (recur more choice pillar (assoc-in open [k :open] (:open data)) out)
            (and place (= :done kind) (= "place" (:name data)) (contains? open k))
            (recur more choice pillar (update open k #(cond-> (update % :mob? (fn [m] (or m (mob-place? data))))
                                                         (= "placed" (:status data)) (update :placed conj time-ms)))
                   out)
            (and (not place) (= :child_started kind))
            (let [chose? (and choice (<= (- time-ms (:t choice)) choice-ms) (:pos choice))
                  begun? (and pillar? pillar (= generation-id (:gen pillar)) (<= (- time-ms (:t pillar)) choice-ms))]
              (recur more choice (if pillar? nil pillar)
                     (assoc open k (cond-> {:t time-ms :way (if pillar? :pillar :walls) :placed [] :mob? false}
                                     begun? (assoc :class (:class pillar) :situation [generation-id (:at pillar)])
                                     (and chose? (not pillar?)) (assoc :choice? true :arrive-s (arrive-s (:pos choice) (:hostiles choice)))))
                     out))
            (and (not place) (= :child_ended kind) (contains? open k))
            (recur more choice pillar (dissoc open k)
                   (assoc out (cond-> k pillar? (conj (:t (get open k))))
                          (let [r (assoc (dissoc (get open k) :choice?) :end-t time-ms :end (:status data) :done? (= :done (:status data))
                                         :mob? (or (:mob? (get open k)) (if pillar? (pillar-mob? data) (contains? mob-reasons (:reason data)))))]
                            (cond-> r (:choice? (get open k)) (assoc :class (class-of r))))))
            :else (recur more choice pillar open out)))))))

(defn fold
  "counts with rows {[body generation job] row} merged into :wall-places; a row already there stays, unless it was
  classed before arrive-s (no :end-t) and the new one is classed."
  [counts rows]
  (update counts :wall-places #(merge-with (fn [new old] (if (and (:class new) (not (:end-t old))) new old)) rows %)))

(defn report
  "{:gap {:mean s :n gaps} :reached {:k :n} :clear {:k :n} :pillar {:beside {:k :n} :clear {:k :n}}}: the mean seconds
  between two placements of one walls attempt, and per class the walls attempts (:pillar: the pillars) that failed for a
  mob (k) of those that closed or failed for a mob (n), a pillar's first of each situation only (a retry follows only a
  failure). A row with no :way is a walls attempt."
  [rows]
  (let [{pillars true walls false} (group-by #(= :pillar (:way %)) (vals rows))
        pillars (concat (remove :situation pillars)
                        (map #(apply min-key :t %) (vals (group-by (juxt :situation :class) (filter :situation pillars)))))
        gaps (mapcat (fn [{:keys [placed]}] (map #(/ (- %2 %1) 1000) placed (rest placed))) walls)
        of (fn [rs cls] (let [told (filter #(and (= cls (:class %)) (or (:done? %) (:mob? %))) rs)]
                          {:k (count (filter #(and (:mob? %) (not (:done? %))) told)) :n (count told)}))]
    {:gap {:mean (when (seq gaps) (/ (js/Math.round (* 100 (/ (reduce + gaps) (count gaps)))) 100)) :n (count gaps)}
     :reached (of walls :reached)
     :clear (of walls :clear)
     :pillar {:beside (of pillars :beside) :clear (of pillars :clear)}}))
