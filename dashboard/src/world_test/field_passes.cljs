(ns world-test.field-passes
  "tools/event-counts.mjs field-passes: counts the measured farm priors jobs.lib.cost.farm ::field-prior and ::field-work from
  the trial bodies' event logs. A pass is one ending of jobs.farm.tend (its text \"farm tend done\"), each part a field.
  Per field at the pass's start: its cells (:crops :bare :untilled at the end), the ripe ones (harvest :cut) and the
  sowable ones (bare farmland or untilled ground: :bare + :untilled at the end + plant :planted). ::field-prior is a Beta per
  share, fitted to the fields' shares by moments (each field one draw: the fields of a body differ more than their cells).
  Per step child of the pass (harvest, till, plant, irrigate, fertilize: child_started to child_ended, debug events) its
  seconds and units (:cut, :tilled, :planted, :poured); ::field-work is each step's least-squares line seconds =
  fixed + per-unit x units, neither below 0.
  Pure parts: scan-line, passes, report."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            ["fs" :as fs]
            ["path" :as path]
            ["zlib" :as zlib]))

(def steps #{"harvest" "till" "plant" "irrigate" "fertilize"})

(def unit-of {:harvest :cut :till :tilled :plant :planted :irrigate :poured :fertilize :used})

(defn- num-of [re line] (some-> (re-find re line) second js/parseFloat))

(defn ending-data
  "The tend ending's data of a parsed event: a child_ended's :ending, else the event's own :data."
  [e]
  (or (get-in e [:data :ending :data]) (:data e)))

(defn scan-line
  "One log line -> {:k :start|:end :t :gen :id :parent :step} of a tend step child, {:k :pass :t :gen :id :fields [{:steps :field}]}
  of a tend ending, or nil."
  [line]
  (let [t (num-of #":time-ms (\d+)" line)
        gen (some-> (re-find #":generation-id \"([^\"]*)\"" line) second)
        id (some-> (re-find #":job-id \"([^\"]*)\"" line) second)]
    (when (and t gen id)
      (cond
        (str/includes? line "farm tend done")
        (when-let [d (ending-data (try (reader/read-string line) (catch :default _ nil)))]
          {:k :pass :t t :gen gen :id id :fields (vec (if (:parts d) (vals (:parts d)) [d]))})
        (re-find #":kind :child_(started|ended)" line)
        (let [i (str/last-index-of id "/")
              step (when i (subs id (inc i)))]
          (when (steps step)
            {:k (if (str/includes? line ":kind :child_started") :start :end) :t t :gen gen :id id :parent (subs id 0 i) :step (keyword step)}))))))

(defn units
  "Units a step's summary s counts: its unit key (a number), or for irrigate a list of poured cells; 0 for a skipped step."
  [step s]
  (let [u (get s (unit-of step))]
    (cond (number? u) u (sequential? u) (count u) :else 0)))

(defn passes
  "Pass rows {:fields [{:cells :ripe :sowable}] :steps {step {:s :units}}} of one body's records (sorted by :t): the step seconds
  are the step children of the tend id run since its previous pass. One ending logged twice (as an ending and as its caller's
  child_ended, within a second) counts once."
  [records]
  (loop [[r & more] records open {} acc {} last-pass {} rows []]
    (if (nil? r)
      rows
      (let [k [(:gen r) (:id r)]]
        (case (:k r)
          :start (recur more (assoc open k (:t r)) acc last-pass rows)
          :end (if-let [t0 (get open k)]
                 (recur more (dissoc open k) (update-in acc [[(:gen r) (:parent r)] (:step r)] (fnil + 0) (/ (- (:t r) t0) 1000)) last-pass rows)
                 (recur more open acc last-pass rows))
          :pass (if (some-> (get last-pass k) (- (:t r)) js/Math.abs (<= 1000))
                  (recur more open acc last-pass rows)
                  (let [secs (get acc k {})
                        fields (for [{:keys [steps field]} (:fields r)
                                     :let [{:keys [crops bare untilled] :or {crops 0 bare 0 untilled 0}} field]]
                                 {:cells (+ crops bare untilled)
                                  :ripe (units :harvest (:harvest steps))
                                  :sowable (+ bare untilled (units :plant (:plant steps)))})
                        done (into {} (for [[step s] secs]
                                        [step {:s s :units (reduce + 0 (map #(units step (get-in % [:steps step])) (:fields r)))}]))]
                    (recur more open (dissoc acc k) (assoc last-pass k (:t r)) (conj rows {:fields (vec fields) :steps done})))))))))

(defn- r3 [x] (/ (js/Math.round (* 1000 x)) 1000))

(defn beta-fit
  "[alpha beta] of shares xs fitted by moments; with no spread among them (or too few), their pooled counts k of n."
  [xs k n]
  (let [m (/ (reduce + xs) (count xs))
        v (/ (reduce + (map #(js/Math.pow (- % m) 2) xs)) (max 1 (dec (count xs))))
        s (when (and (pos? v) (< 0 m 1)) (dec (/ (* m (- 1 m)) v)))]
    (if (and s (pos? s))
      [(r3 (* m s)) (r3 (* (- 1 m) s))]
      [k (- n k)])))

(defn line-fit
  "{:n :fixed-s :per-unit-s} of points [units seconds]: least squares, a negative part set to 0 and the other refitted."
  [pts]
  (let [n (count pts)
        mu (/ (reduce + (map first pts)) n)
        ms (/ (reduce + (map second pts)) n)
        vu (reduce + (map #(js/Math.pow (- (first %) mu) 2) pts))
        b (if (pos? vu) (/ (reduce + (map #(* (- (first %) mu) (- (second %) ms)) pts)) vu) 0)
        a (- ms (* b mu))
        [a b] (cond (neg? b) [ms 0]
                    (neg? a) [0 (/ (reduce + (map #(* (first %) (second %)) pts)) (max 1e-9 (reduce + (map #(* (first %) (first %)) pts))))]
                    :else [a b])]
    {:n n :fixed-s (r3 a) :per-unit-s (r3 b)}))

(defn report
  "Of pass rows: :prior the value of cost.farm/::field-prior ({:fields :cells :ripe [a b] :sowable [a b]}), :work the value of
  ::field-work ({step line-fit}), from the fields with cells and the steps that ran."
  [rows]
  (let [fields (filter #(pos? (:cells %)) (mapcat :fields rows))
        sum #(reduce + (map % fields))
        share #(map (fn [f] (/ (% f) (:cells f))) fields)
        by-step (reduce (fn [m {:keys [steps]}] (reduce (fn [m [step {:keys [s units]}]] (update m step (fnil conj []) [units s])) m steps)) {} rows)]
    {:prior {:fields (count fields) :cells (sum :cells)
             :ripe (beta-fit (share :ripe) (sum :ripe) (sum :cells))
             :sowable (beta-fit (share :sowable) (sum :sowable) (sum :cells))}
     :work (into (sorted-map) (map (fn [[step pts]] [step (line-fit pts)])) by-step)}))

(defn files
  "The event log files of a body's directory listing, the debug ones too, each rotation once (the plain file wins over its .gz)."
  [names]
  (let [logs (filter #(re-find #"^events\.edn(\.debug)?(\.\d+)?(\.gz)?$" %) names)
        have (set logs)]
    (remove #(and (str/ends-with? % ".gz") (have (str/replace % #"\.gz$" ""))) logs)))

(defn read-lines
  "[[file-index debug? line] ...] of the event files of a body's engine dir."
  [dir]
  (->> (files (array-seq (fs/readdirSync dir)))
       (map-indexed (fn [i f]
                      (let [p (path/join dir f)
                            text (if (str/ends-with? f ".gz") (.toString (zlib/gunzipSync (fs/readFileSync p)) "utf8") (fs/readFileSync p "utf8"))]
                        (map (fn [line] [i (str/includes? f "debug") line]) (str/split-lines text)))))
       (apply concat)))

(defn count-in
  "report over the trial bodies under dir (probes left out: their fields are fixtures, not play)."
  [dir probe?]
  (report (mapcat (fn [b] (passes (sort-by :t (keep (fn [[_ _ line]] (scan-line line)) (read-lines (path/join dir b "engine"))))))
                  (filter #(and (not (probe? %)) (fs/existsSync (path/join dir % "engine"))) (array-seq (fs/readdirSync dir))))))
