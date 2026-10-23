(ns world-test.event-counts
  "tools/event-counts.mjs: recounts the measured combat priors from the trial bodies' event logs, so a setting can cite its k and n.
  Commands: fold | creeper-exchange | flight-starts, each [--agents DIR] [--counts FILE] [--since-ms MS] [--sprint BLOCKS-PER-S]; each folds the logs into the durable counts file first (rows by run id, so counts only grow as logs rotate).
  spot-returns [--agents DIR] (world-test.spot-returns) counts from the logs alone, no fold.
  creeper-exchange: respond runs that only decided about creepers and fought (a :fight decision) with at least one swing: hp the creepers
  dealt (hurt events inside the run) per swing (attack that hit or killed), overall and per weapon equipped in the run, and the runs with a blast over 2 hp.
  flight-starts: the first flee decision of each respond run from a creeper: seconds until the body is 1 block from where it stood,
  less that block at sprint speed (action done and planned events carry the body's position, so the stream is sparse); the deciles are the values at
  (i + 0.5) / 10 of the sorted starts.
  Pure parts: scan-line, exchange, starts, deciles."
  (:require ["fs" :as fs]
            ["path" :as path]
            ["zlib" :as zlib]
            [cljs.reader :as reader]
            [clojure.string :as str]
            [world-test.fresh-drop :as fresh-drop]
            [world-test.spot-returns :as spot-returns]))

(def blast-hp "hp over which one hurt event from a creeper is its blast." 2)
(def tail-ms "Time after a run ends within which a hurt still counts as its own." 1500)
(def stay-m "Blocks from where it stood at the decision at which a body has got going." 1)
(def pos-age-ms "Longest age of the position a decision is measured from." 2000)
(def sparse-ms "Longest gap between two positions across which a start is still told (a longer one hides when the body got going)." 1000)
(def give-up-ms "A decision whose body has not moved this long after it is dropped." 10000)

(defn root-job [id] (first (str/split (str id) #"/")))

(defn- num-of [re line] (some-> (re-find re line) second js/parseFloat))

(defn scan-line
  "One log line -> a small record, or nil for a line the counts do not read. Only the few kinds they read are parsed whole."
  [line]
  (let [t (num-of #":time-ms (\d+)" line)
        gen (some-> (re-find #":generation-id \"([^\"]*)\"" line) second)
        whole #(try (reader/read-string line) (catch :default _ nil))]
    (when (and t gen)
      (cond
        (and (str/includes? line ":kind :hostile.decided") (str/includes? line ":hostile-near"))
        (let [e (whole)]
          {:k :decided :t t :gen gen :job (root-job (get-in e [:context :job-id]))
           :mob (get-in e [:data :mob]) :decision (get-in e [:data :decision])})

        (and (str/includes? line ":kind :ended") (str/includes? line ":hostile-near"))
        {:k :ended :t t :gen gen :job (root-job (second (re-find #":job-id \"([^\"]*)\"" line)))}

        (and (str/includes? line ":kind :hurt") (str/includes? line ":source :body"))
        (let [e (whole)]
          {:k :hurt :t t :gen gen :amount (get-in e [:data :amount]) :causes (set (get-in e [:data :causes]))})

        (and (str/includes? line ":kind :started") (str/includes? line ":name \"equip\""))
        {:k :equip :t t :gen gen :job (root-job (second (re-find #":job-id \"([^\"]*)\"" line)))
         :item (second (re-find #"\"item\" \"([^\"]*)\"" line))}

        (and (str/includes? line ":kind :done") (str/includes? line ":name \"attack\"") (re-find #":status \"(hit|killed)\"" line))
        {:k :hit :t t :gen gen :job (root-job (second (re-find #":job-id \"([^\"]*)\"" line)))}

        (or (and (str/includes? line ":source :action") (str/includes? line ":kind :done")) (str/includes? line ":kind :planned"))
        (when-let [[_ x y z] (last (re-seq #":pos \{:x (-?[\d.]+(?:E-?\d+)?), :y (-?[\d.]+(?:E-?\d+)?), :z (-?[\d.]+(?:E-?\d+)?)\}" line))]
          {:k :pos :t t :gen gen :x (js/parseFloat x) :y (js/parseFloat y) :z (js/parseFloat z)})))))

(defn runs
  "{[gen job] {:decisions [{:mob :decision}] :start :end :hits [t] :items [[t item]]}} of the respond runs in records (sorted by :t)."
  [records]
  (reduce (fn [acc {:keys [k gen job t] :as r}]
            (let [id [gen job]]
              (case k
                :decided (-> acc (update-in [id :decisions] (fnil conj []) (select-keys r [:mob :decision])) (update-in [id :start] #(or % t)))
                :ended (cond-> acc (get acc id) (assoc-in [id :end] t))
                :hit (cond-> acc (get acc id) (update-in [id :hits] (fnil conj []) t))
                :equip (cond-> acc (get acc id) (update-in [id :items] (fnil conj []) [t (:item r)]))
                acc)))
          {} records))

(defn exchange
  "Rows [{:id :t :weapon item-or-nil :swings :hp :blast?}] of the creeper-only fights in records: runs of only creeper decisions with a :fight
  one and an end. The hp is the creeper hurt inside the run (start to end plus tail-ms)."
  [records]
  (let [hurts (filter #(= :hurt (:k %)) records)]
    (for [[[gen job] {:keys [decisions start end hits items]}] (runs records)
          :when (and end
                     (every? #(= "creeper" (:mob %)) decisions)
                     (some #(= :fight (:decision %)) decisions))
          :let [mine (filter #(and (= gen (:gen %)) (<= start (:t %) (+ end tail-ms)) (= #{"creeper"} (:causes %))) hurts)
                amounts (map :amount mine)
                first-hit (first (sort hits))]]
      {:id [gen job] :t start
       :weapon (->> items (filter #(<= (first %) first-hit)) (sort-by first) last second)
       :swings (count hits)
       :hp (reduce + 0 amounts)
       :blast? (boolean (some #(> % blast-hp) amounts))})))

(def sword-or-axe? "Whether a weapon is one the body backs off between swings with (fight/fight-damage)." #(boolean (some->> % (re-find #"_(sword|axe)$"))))

(defn tally
  "Runs with a swing, their swings and hp, the runs with a blast, the runs without a swing in the logs (the debug log that holds the attack
  actions rotates away sooner than the plain one). :hp-per-swing is the mean of the Gamma-Poisson posterior of hp per swing under the
  Jeffreys prior (shape 0.5, rate 0): (hp + 0.5) / swings, with :sd sqrt(hp + 0.5) / swings."
  [all-rows]
  (let [rows (filter (comp pos? :swings) all-rows)
        swings (reduce + 0 (map :swings rows))
        hp (reduce + 0 (map :hp rows))
        r3 #(/ (js/Math.round (* 1000 %)) 1000)]
    {:runs (count rows) :no-swings (- (count all-rows) (count rows)) :swings swings :hp (/ (js/Math.round (* 100 hp)) 100) :blasts (count (filter :blast? rows))
     :hp-per-swing (when (pos? swings) (r3 (/ (+ hp 0.5) swings)))
     :sd (when (pos? swings) (r3 (/ (js/Math.sqrt (+ hp 0.5)) swings)))}))

(defn exchange-report
  "The pooled tally over fights with a sword or axe (the weapons fight-damage prices the exchange for), the tally of every fight, and
  the tally per weapon, so a setting splits by weapon only where the counts separate."
  [rows]
  {:armed (tally (filter (comp sword-or-axe? :weapon) rows))
   :all (tally rows)
   :by-weapon (into (sorted-map) (map (fn [[w rs]] [(or w "unknown") (tally rs)])) (group-by :weapon rows))})

(defn dist [a b] (js/Math.sqrt (+ (js/Math.pow (- (:x a) (:x b)) 2) (js/Math.pow (- (:y a) (:y b)) 2) (js/Math.pow (- (:z a) (:z b)) 2))))

(defn starts
  "The first flee decision from a creeper of each respond run (records sorted by :t), as {:id [gen job] :t decision-ms :s seconds :sparse? bool} until the
  body is stay-m from where it stood, less that block at sprint speed; or {:id :t :drop :no-pos|:never}: no position within pos-age-ms before
  the decision (where it stood is unknown), or none moved in give-up-ms. A move first seen across a gap over sparse-ms is kept, told at
  the middle of the gap (the true moment lies in it), flagged :sparse?."
  [records sprint]
  (let [first-flee (->> records
                        (filter #(and (= :decided (:k %)) (= "creeper" (:mob %)) (= :flee (:decision %))))
                        (reduce (fn [acc r] (let [id [(:gen r) (:job r)]] (if (acc id) acc (assoc acc id r)))) {})
                        vals set)
        secs (fn [t0 t] (max 0 (- (/ (- t t0) 1000) (/ stay-m sprint))))]
    (->> records
         (reduce (fn [{:keys [last-pos pending] :as st} {:keys [k gen job t] :as r}]
                   (case k
                     :decided (if-not (first-flee r)
                                st
                                (let [p (get last-pos gen)]
                                  (if (and p (<= (- t (:t p)) pos-age-ms))
                                    (update st :pending conj {:gen gen :id [gen job] :t t :from p})
                                    (update st :out conj {:id [gen job] :t t :drop :no-pos}))))
                     :pos (let [{here true waiting false} (group-by #(= gen (:gen %)) pending)
                                {moved true still false} (group-by #(>= (dist r (:from %)) stay-m) here)
                                {late true wait false} (group-by #(> (- t (:t %)) give-up-ms) still)
                                tl (get-in last-pos [gen :t])
                                sparse? (> (- t tl) sparse-ms)]
                            (-> st
                                (assoc-in [:last-pos gen] r)
                                (assoc :pending (vec (concat waiting wait)))
                                (update :out into (map (fn [m] {:id (:id m) :t (:t m) :drop :never})) late)
                                (update :out into (map (fn [m] {:id (:id m) :t (:t m) :sparse? sparse?
                                                                :s (secs (:t m) (if sparse? (/ (+ (max tl (:t m)) t) 2) t))}))
                                        moved)))
                     st))
                 {:last-pos {} :pending [] :out []})
         :out)))

(defn deciles
  "The values of sorted xs at (i + 0.5) / 10, i = 0..9, rounded to 2 places."
  [xs]
  (let [v (vec (sort xs)) n (count v)]
    (when (pos? n)
      (mapv #(/ (js/Math.round (* 100 (nth v (min (dec n) (js/Math.floor (* (/ (+ % 0.5) 10) n)))))) 100) (range 10)))))

(defn starts-report
  "n, mean and deciles of the kept starts (rows of starts), how many were told across a gap, and the dropped decisions by reason."
  [rows]
  (let [xs (keep :s rows)
        n (count xs)]
    {:n n :mean (when (pos? n) (/ (js/Math.round (* 100 (/ (reduce + 0 xs) n))) 100)) :deciles (deciles xs)
     :sparse (count (filter :sparse? rows)) :no-pos (count (filter #(= :no-pos (:drop %)) rows)) :never (count (filter #(= :never (:drop %)) rows))}))

;; The durable counts: the rows of each prior by run id, so the counts only grow while the raw logs rotate away.
(def empty-counts {:exchange {} :starts {}})

(defn better-exchange "Of two rows of one fight, the one that saw more swings (a rotation may have cut the older read short), then the older." [a b]
  (if (> (:swings b) (:swings a)) b a))

(defn fold
  "counts with the rows of exchange-rows and start-rows merged in by id; a row already there stays unless a fight row saw more swings
  or a start was dropped and is now told."
  [counts exchange-rows start-rows]
  (-> counts
      (update :exchange #(reduce (fn [m r] (update m (:id r) (fn [old] (if old (better-exchange old r) r)))) % exchange-rows))
      (update :starts #(reduce (fn [m r] (update m (:id r) (fn [old] (if (and old (not (:drop old))) old r)))) % start-rows))))

(defn load-counts [file]
  (if (fs/existsSync file) (merge empty-counts (reader/read-string (fs/readFileSync file "utf8"))) empty-counts))

(defn save-counts!
  "Written whole to a temporary file and renamed over, so a reader never sees half a file."
  [file counts]
  (let [tmp (str file ".tmp")]
    (fs/mkdirSync (path/dirname file) #js {:recursive true})
    (fs/writeFileSync tmp (str (pr-str counts) "\n"))
    (fs/renameSync tmp file)))

(defn event-files
  "The event files of a body's directory listing: the plain log (events.edn, its rotations) and the debug log (action events),
  each rotation once (the plain file wins over its .gz)."
  [names]
  (let [logs (filter #(re-find #"^events\.edn" %) names)
        have (set logs)]
    (remove #(and (str/ends-with? % ".gz") (have (str/replace % #"\.gz$" ""))) logs)))

(defn read-agent
  "The records of one body's event files since since-ms, sorted by time."
  [dir since-ms]
  (->> (event-files (array-seq (fs/readdirSync dir)))
       (mapcat (fn [f]
                 (let [p (path/join dir f)
                       text (if (str/ends-with? f ".gz") (.toString (zlib/gunzipSync (fs/readFileSync p)) "utf8") (fs/readFileSync p "utf8"))]
                   (keep scan-line (str/split-lines text)))))
       (filter #(>= (:t %) since-ms))
       (sort-by :t)
       vec))

(def usage "usage: node tools/event-counts.mjs fold | creeper-exchange | flight-starts | spot-returns [--agents DIR] [--counts FILE] [--since-ms MS] [--sprint BLOCKS-PER-S]
  every command first folds the logs into the counts file (default worlds/test/priors-counts.edn), then reports from the file; --since-ms keeps the rows of fights started after it.")

(defn- parse-args [argv]
  (loop [[a & more] argv o {}]
    (case a
      nil o
      "--agents" (recur (rest more) (assoc o :agents (first more)))
      "--counts" (recur (rest more) (assoc o :counts (first more)))
      "--since-ms" (recur (rest more) (assoc o :since-ms (js/parseFloat (first more))))
      "--sprint" (recur (rest more) (assoc o :sprint (js/parseFloat (first more))))
      (recur more (update o :pos (fnil conj []) a)))))

(defn fold-logs!
  "Reads every body's logs under agents-dir and folds the rows into the counts file; returns the new counts."
  [agents-dir counts-file sprint]
  (let [per-agent (for [b (array-seq (fs/readdirSync agents-dir))
                        :when (fs/existsSync (path/join agents-dir b "engine"))]
                    (read-agent (path/join agents-dir b "engine") 0))
        counts (reduce (fn [c records] (fold c (exchange records) (starts records sprint))) (load-counts counts-file) per-agent)]
    (save-counts! counts-file counts)
    counts))

(defn main
  "argv (array) -> promise of the exit code."
  [argv]
  (let [{[cmd] :pos :keys [agents counts since-ms sprint] :or {since-ms 0 sprint 5.6}} (parse-args (vec (array-seq argv)))
        repo (or js/process.env.WORLD_TEST_REPO ".")
        folded (when (#{"fold" "creeper-exchange" "flight-starts"} cmd)
                 (fold-logs! (or agents (path/join repo "worlds" "test" "agents")) (or counts (path/join repo "worlds" "test" "priors-counts.edn")) sprint))
        since (fn [rows] (filter #(>= (:t %) since-ms) (vals rows)))]
    (js/Promise.resolve
     (case cmd
       "fold" (do (println (pr-str {:exchange (count (:exchange folded)) :starts (count (:starts folded))})) 0)
       "creeper-exchange" (do (println (pr-str (assoc (exchange-report (since (:exchange folded))) :since-ms since-ms))) 0)
       "flight-starts" (do (println (pr-str (assoc (starts-report (since (:starts folded))) :since-ms since-ms :sprint sprint))) 0)
       "spot-returns" (do (println (pr-str (spot-returns/count-in (or agents (path/join repo "worlds" "test" "agents"))))) 0)
       (do (js/console.error usage) 2)))))
