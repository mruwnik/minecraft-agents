(ns world-test.item-costs
  "tools/event-counts.mjs item-costs: counts the measured priors behind jobs.lib.cost.value ::gather-counts and
  ::gather-prior-items from the trial bodies' logs: each gather (a /gather-<item> job and its children), the seconds it
  held the body (from each of its lines to the next line, until another job's line or the body went off) and the items
  of its kind the body picked up meanwhile, fruitless gathers too. Only gathers that logged a debug line count: pickups
  are debug lines, so a body logging at info has none. Rows by
  [body generation job], folded into the counts file's :item-costs so they outlive the logs, which rotate.
  Pure parts: scan-line, spans, fold, report, prior-items."
  (:require [clojure.string :as str]))

(defn- field [line re] (second (re-find re line)))

(def ^:private off-kinds
  "Kinds after which no job holds the body until the next job line: a job's run ended (it yields or ends), or the body went off."
  #{"child_ended" "offline" "stopping" "disconnected" "died" "logged-out"})

(defn scan-line
  "One log line -> {:gen :seq :t :job :off? :debug? :pick {:item :count}}, nil without a generation."
  [line]
  (when-let [gen (field line #":generation-id \"([^\"]*)\"")]
    (let [kind (field line #":kind :([a-z_.-]+)")]
      {:gen gen :seq (js/parseInt (field line #":seq (\d+)")) :t (js/parseFloat (field line #":time-ms (\d+)"))
       :job (field line #":job-id \"([^\"]*)\"") :off? (contains? off-kinds kind) :debug? (str/includes? line ":level :debug")
       :pick (when (= "picked-up" kind)
               {:item (field line #":item \"([^\"]*)\"") :count (js/parseInt (or (field line #":count (\d+)") "1"))})})))

(defn- gathers
  "[gather-job item] of each /gather-<item> segment of job's id."
  [job]
  (when job
    (let [parts (str/split job #"/")]
      (keep-indexed (fn [i p] (when-let [[_ item] (re-matches #"gather-([a-z0-9_]+)" p)]
                                [(str/join "/" (take (inc i) parts)) item]))
                    parts))))

(defn- gen-spans
  "{gather-job {:item :s :got :debug?}} of one generation's rows in log order."
  [rows]
  (let [row #(or % {:item %2 :s 0 :got 0})]
    (:out (reduce (fn [{:keys [held prev] :as acc} {:keys [t job off? debug? pick]}]
                    (let [acc (reduce (fn [a [j item]] (update-in a [:out j] #(update (row % item) :s + (/ (- t prev) 1000))))
                                      acc held)
                          acc (reduce (fn [a [j item]] (cond-> (update-in a [:out j] row item) debug? (assoc-in [:out j :debug?] true)))
                                      acc (gathers job))
                          held (cond off? nil job (gathers job) :else held)
                          acc (reduce (fn [a [j item]] (cond-> a (= item (:item pick)) (update-in [:out j :got] + (:count pick))))
                                      acc held)]
                      (assoc acc :held held :prev t)))
                  {:out {}} rows))))

(defn spans
  "{[gen gather-job] {:item :s :got}} of one body's scanned rows: each gather that logged a debug line, its seconds holding
  the body and the items of its kind picked up while it did."
  [rows]
  (into {} (for [[gen rs] (group-by :gen rows)
                 [j span] (gen-spans (sort-by :seq rs))
                 :when (:debug? span)]
             [[gen j] (dissoc span :debug?)])))

(defn fold
  "counts with rows {[body generation job] row} merged into :item-costs; of a row read twice the longer stays (a rotated
  log may have lost a gather's start)."
  [counts rows]
  (update counts :item-costs #(merge-with (fn [old new] (if (> (:s new) (:s old)) new old)) % rows)))

(defn- r3 [x] (/ (js/Math.round (* 1000 x)) 1000))

(defn report
  "{item {:s total seconds :got items :calls gathers}} of the rows."
  [rows]
  (into (sorted-map)
        (for [[item rs] (group-by :item (vals rows))]
          [item {:s (r3 (reduce + (map :s rs))) :got (reduce + (map :got rs)) :calls (count rs)}])))

(defn prior-items
  "The items a measured item's prior counts as (jobs.lib.cost.value ::gather-prior-items): per item with two or more
  timed gathers, the shape of a Gamma of the gathers' rates (items a second) about the item's pooled rate, fitted by
  moments less the counts' Poisson spread; the mean over the items that fit, nil with none."
  [rows]
  (let [ks (for [[_ rs] (group-by :item (vals rows))
                 :let [rs (filter #(pos? (:s %)) rs)
                       mu (/ (reduce + (map :got rs)) (max 1e-9 (reduce + (map :s rs))))
                       v (/ (reduce + (map #(js/Math.pow (- (/ (:got %) (:s %)) mu) 2) rs)) (max 1 (dec (count rs))))
                       noise (/ (reduce + (map #(/ mu (:s %)) rs)) (max 1 (count rs)))]
                 :when (and (< 1 (count rs)) (pos? mu) (< noise v))]
             (/ (* mu mu) (- v noise)))]
    (when (seq ks) (r3 (/ (reduce + ks) (count ks))))))
