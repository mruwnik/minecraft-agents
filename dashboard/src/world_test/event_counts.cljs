(ns world-test.event-counts
  "tools/event-counts.mjs: recounts the measured combat priors from the trial bodies' event logs, so a setting can cite its k and n.
  Commands: fold | creeper-exchange | flight-starts | notice-rate | drowned-meets (world-test.drowned-meets; trial bodies only) | flee-pace (trial bodies only), each [--agents DIR] [--counts FILE] [--since-ms MS] [--sprint BLOCKS-PER-S]; each folds the logs into the durable counts file first (rows by run id, so counts only grow as logs rotate).
  spot-returns [--agents DIR] (world-test.spot-returns) and vein-seen [--agents DIR] count from the logs alone, no fold.
  vein-seen: the ore blocks one look saw together (26-connected, a vein's plain and deepslate blocks as one ore) per group, from the
  look tool's :look.observed lists; :capped counts the lists the tool cut short (nearest first, so a far group may be cut).
  creeper-exchange: respond runs that only decided about creepers and fought (a :fight decision) with at least one swing: hp the creepers
  dealt (hurt events inside the run) per swing (attack that hit or killed), overall and per weapon equipped in the run, and the runs with a blast over 2 hp.
  flight-starts: the first flee decision of each respond run from a creeper: seconds until the body is 1 block from where it stood,
  less that block at sprint speed (action done and planned events carry the body's position, so the stream is sparse); the deciles are the values at
  (i + 0.5) / 10 of the sorted starts.
  notice-rate: the hostiles a watch first saw within their follow range (watch.saw, one per mob that became known; provoked-only mobs left out), each followed until it
  reached the body (caught: a hurt from its kind, or a respond decision that places a seen mob within cost.danger/melee-reach of the body's last
  position), the body more than the mob's follow range from where it was seen, or a position gap over give-up-ms (censored: the seconds up to
  the gap count, no catch): catches per second in its field, each second weighted by cost.threat/way-weight of the body's straight distance
  from that spot (a lower bound on the mob's steps, so an upper bound on the weight: the rate counts low). The setting death/::notice-rate
  stores {:k catches :n-s weighted seconds} for each class: :other (reach: danger/melee-reach) and :ranged (a mob of threat/ranged?, caught within
  its threat/fire-range, whose line of fire the logs do not hold, so its weight is an upper bound too). A catch ends one sighting: the events
  carry no entity id, so it is the open sighting of the kind nearest the event's spot.
  flee-pace: the flee steps (retreat.step-speed) of flights of two steps or more: blocks over the steps' own seconds, and the seconds
  stood between two steps (cost.danger/::flee-walk-speed, ::flee-stop-s).
  Pure parts: scan-line, exchange, starts, deciles, notices, groups, vein-seen, flee-steps, pace-report."
  (:require ["fs" :as fs]
            ["path" :as path]
            ["zlib" :as zlib]
            [cljs.reader :as reader]
            [clojure.string :as str]
            [jobs.lib.cost.danger :as danger]
            [jobs.lib.cost.threat :as threat]
            [world-test.drowned-meets :as drowned]
            [world-test.fresh-drop :as fresh-drop]
            [world-test.spot-returns :as spot-returns]))

(def blast-hp "hp over which one hurt event from a creeper is its blast." 2)
(def tail-ms "Time after a run ends within which a hurt still counts as its own." 1500)
(def stay-m "Blocks from where it stood at the decision at which a body has got going." 1)
(def pos-age-ms "Longest age of the position a decision is measured from." 2000)
(def sparse-ms "Longest gap between two positions across which a start is still told (a longer one hides when the body got going)." 1000)
(def give-up-ms "A decision whose body has not moved this long after it is dropped." 10000)

(defn root-job [id] (first (str/split (str id) #"/")))

(def pos-re #":pos \{:x (-?[\d.]+(?:E-?\d+)?), :y (-?[\d.]+(?:E-?\d+)?), :z (-?[\d.]+(?:E-?\d+)?)\}")

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
           :mob (get-in e [:data :mob]) :decision (get-in e [:data :decision]) :health (get-in e [:data :health])
           :at (when-not (get-in e [:data :band]) (get-in e [:data :pos]))})

        (str/includes? line ":kind :retreat.step-speed")
        {:k :step :t t :gen gen :job (second (re-find #":job-id \"([^\"]*)\"" line))
         :s (num-of #":seconds (-?[\d.]+)" line) :blocks (num-of #":blocks (-?[\d.]+)" line)}

        (and (str/includes? line ":kind :watch.saw") (str/includes? line ":how :seen"))
        (let [{{:keys [name distance pos id]} :data} (whole)]
          {:k :saw :t t :gen gen :mob name :id id :distance distance :at pos})

        (and (str/includes? line ":kind :ended") (str/includes? line ":hostile-near"))
        {:k :ended :t t :gen gen :job (root-job (second (re-find #":job-id \"([^\"]*)\"" line)))
         :wear (second (re-find #":wear \{\"([^\"]*_(?:sword|axe))\"" line))}

        (and (str/includes? line ":kind :look.observed") (str/includes? line "_ore\""))
        (let [e (whole)]
          {:k :observed :t t :gen gen :capped? (boolean (get-in e [:data :more-blocks?]))
           :blocks (vec (for [{:keys [name pos]} (get-in e [:data :blocks]) :when (str/ends-with? name "_ore")] [name pos]))})

        (and (str/includes? line ":kind :hurt") (str/includes? line ":source :body"))
        (let [e (whole)]
          {:k :hurt :t t :gen gen :amount (get-in e [:data :amount]) :hits (get-in e [:data :hits]) :causes (set (get-in e [:data :causes])) :at (get-in e [:data :pos])})

        (and (str/includes? line ":kind :died") (str/includes? line ":source :body"))
        {:k :died :t t :gen gen :cause (second (re-find #":cause \"([^\"]*)\"" line))}

        (and (str/includes? line ":kind :started") (str/includes? line ":name \"equip\""))
        {:k :equip :t t :gen gen :job (root-job (second (re-find #":job-id \"([^\"]*)\"" line)))
         :item (second (re-find #"\"item\" \"([^\"]*)\"" line))}

        (and (str/includes? line ":kind :done") (str/includes? line ":name \"attack\"") (re-find #":status \"(hit|killed)\"" line))
        {:k :hit :t t :gen gen :job (root-job (second (re-find #":job-id \"([^\"]*)\"" line)))}

        (and (str/includes? line ":kind :done") (str/includes? line ":name \"attack\""))
        {:k :miss :t t :gen gen :job (root-job (second (re-find #":job-id \"([^\"]*)\"" line)))}

        (or (and (str/includes? line ":source :action") (str/includes? line ":kind :done")) (str/includes? line ":kind :planned"))
        (when-let [[_ x y z] (last (re-seq pos-re line))]
          {:k :pos :t t :gen gen :x (js/parseFloat x) :y (js/parseFloat y) :z (js/parseFloat z)})

        :else
        (when-let [[_ x y z] (re-find pos-re line)]
          {:k :at :t t :gen gen :x (js/parseFloat x) :y (js/parseFloat y) :z (js/parseFloat z)})))))

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

(defn flee-steps
  "Rows [{:id [gen t] :flight [gen job] :t :s :blocks}] of the flee steps in records (retreat.step-speed: :s the step's own
  seconds, ending at :t)."
  [records]
  (vec (for [{:keys [k gen job t s blocks]} records :when (and (= :step k) s blocks)]
         {:id [gen t] :flight [gen job] :t t :s s :blocks blocks})))

(defn pace-report
  "The pace of the flights of two steps or more among rows (flee-steps): blocks over the steps' own seconds (:bps), and the
  seconds stood between two steps of a flight (:stop-s: from one step's end to the next one's start, over the gaps)."
  [rows]
  (let [flights (filter #(>= (count %) 2) (map #(sort-by :t %) (vals (group-by :flight rows))))
        steps (apply concat flights)
        gaps (mapcat (fn [f] (map (fn [a b] (max 0 (/ (- (:t b) (* 1000 (:s b)) (:t a)) 1000))) f (rest f))) flights)
        sum #(reduce + 0 %)
        r2 #(/ (js/Math.round (* 100 %)) 100)
        blocks (sum (map :blocks steps)) secs (sum (map :s steps)) stood (sum gaps)]
    {:flights (count flights) :steps (count steps) :blocks (r2 blocks) :seconds (r2 secs)
     :bps (when (pos? secs) (r2 (/ blocks secs))) :gaps (count gaps) :gap-s (r2 stood)
     :stop-s (when (seq gaps) (r2 (/ stood (count gaps))))}))

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
(def empty-counts {:exchange {} :starts {} :notices {} :drowned-exposure {} :drowned-meets {} :flee-steps {}})

(defn better-exchange "Of two rows of one fight, the one that saw more swings (a rotation may have cut the older read short), then the older." [a b]
  (if (> (:swings b) (:swings a)) b a))

(defn fold
  "counts with the rows of exchange-rows, start-rows and notice-rows merged in by id; a row already there stays unless a fight row saw
  more swings, a start was dropped and is now told, or a sighting was still open when its log ended."
  ([counts exchange-rows start-rows] (fold counts exchange-rows start-rows []))
  ([counts exchange-rows start-rows notice-rows]
   (-> counts
       (update :exchange #(reduce (fn [m r] (update m (:id r) (fn [old] (if old (better-exchange old r) r)))) % exchange-rows))
       (update :starts #(reduce (fn [m r] (update m (:id r) (fn [old] (if (and old (not (:drop old))) old r)))) % start-rows))
       (update :notices #(reduce (fn [m r] (update m (:id r) (fn [old] (if (and old (not= :open (:end old))) old r)))) % notice-rows)))))

(defn load-counts [file]
  (if (fs/existsSync file) (merge empty-counts (reader/read-string (fs/readFileSync file "utf8"))) empty-counts))

(defn save-counts!
  "Written whole to a temporary file and renamed over, so a reader never sees half a file."
  [file counts]
  (let [tmp (str file ".tmp")]
    (fs/mkdirSync (path/dirname file) #js {:recursive true})
    (fs/writeFileSync tmp (str (pr-str counts) "\n"))
    (fs/renameSync tmp file)))

(defn ranged-mob? [mob] (threat/ranged? {:name mob}))

(defn reach-of
  "Blocks from the body within which mob can catch it: a ranged one by its line of fire out to its kind's threat/fire-range, any other by melee reach."
  [mob]
  (if (ranged-mob? mob) (threat/fire-range mob) (danger/melee-reach)))

(defn notices
  "Rows [{:id [gen t mob] :t :mob :s :end}] of the sightings in records (sorted by :t): :s the seconds followed, each times the weight
  of the body's distance from the mob's spot over it (a ranged mob within its threat/fire-range weighs 1: whether it has a line of fire is
  not in the logs, so the weight is an upper bound); :end :caught (the mob reached the body), :left (out of its follow range), :gap
  (censored: :s stops at the last position) or :open (the log ends).
  A catch ends one sighting: the events carry no entity id, so the open sighting of the kind whose spot is nearest the event's (a hurt:
  the body's position, a decision: the mob's spot) and not older than give-up-ms. A hurt from a kind catches any mob of it; a decision
  only a mob within reach-of the body."
  [records]
  (let [row (fn [{:keys [id t0 mob s]} end] {:id id :t t0 :mob mob :s s :end end})
        weight (fn [mob d] (if (and (ranged-mob? mob) (<= d (threat/fire-range mob))) 1 (threat/way-weight d)))
        gain (fn [e t] (update e :s + (* (:w e) (/ (min (- t (:t e)) give-up-ms) 1000))))
        reached? (fn [st gen t mob mob-at]
                   (let [p (get-in st [:last gen])]
                     (and mob-at p (<= (- t (:t p)) pos-age-ms) (<= (dist p mob-at) (reach-of mob)))))
        catch (fn [st gen t mob near]
                (let [fresh (filter #(and (= mob (:mob %)) (<= (- t (:t %)) give-up-ms)) (get-in st [:open gen] []))
                      one (when (seq fresh) (apply min-key #(if near (dist near (:at %)) 0) fresh))]
                  (cond-> st
                    one (-> (update-in [:open gen] (fn [os] (vec (remove #(identical? one %) os))))
                            (update :rows conj (row (gain one t) :caught))))))
        out (reduce
             (fn [{:keys [open] :as st} {:keys [k gen t mob] :as r}]
               (let [mine (get open gen [])]
                 (case k
                   :saw (if (or (contains? threat/provoked-only mob) (> (:distance r) (threat/follow-range mob))
                                (and (:id r) (contains? (get-in st [:seen gen]) (:id r))))
                          st
                          (-> st (update-in [:seen gen] (fnil conj #{}) (:id r)) (update-in [:open gen] (fnil conj []) {:id [gen t mob] :t0 t :mob mob :at (:at r) :t t :s 0 :w (weight mob (:distance r))})))
                   :hurt (reduce (fn [st mob] (catch st gen t mob (or (:at r) (get-in st [:last gen])))) st (:causes r))
                   :decided (if (reached? st gen t mob (:at r))
                              (catch st gen t mob (:at r))
                              st)
                   :pos (let [{gone true kept false} (group-by #(> (- t (:t %)) give-up-ms) mine)
                              moved (map #(gain % t) kept)
                              {left true near false} (group-by #(> (dist r (:at %)) (threat/follow-range (:mob %))) moved)]
                          (-> st
                              (assoc-in [:last gen] r)
                              (assoc-in [:open gen] (mapv #(assoc % :t t :w (weight (:mob %) (dist r (:at %)))) near))
                              (update :rows into (concat (map #(row % :gap) gone) (map #(row % :left) left)))))
                   st)))
             {:open {} :rows [] :last {}}
             records)]
    (into (:rows out) (map #(row % :open)) (mapcat val (:open out)))))

(defn notice-tally [rows]
  (let [ends (frequencies (map :end rows))
        k (get ends :caught 0)
        s (reduce + 0 (map :s rows))]
    {:sightings (count rows) :caught k :ends ends :weighted-s (/ (js/Math.round (* 10 s)) 10)
     :rate (when (pos? s) (/ (js/Math.round (* 10000 (/ (+ k 0.5) s))) 10000))}))

(defn notice-report [rows]
  {:all (notice-tally rows)
   :by-class (into (sorted-map) (map (fn [[ranged? rs]] [(if ranged? :ranged :other) (notice-tally rs)])) (group-by (comp ranged-mob? :mob) rows))
   :by-mob (into (sorted-map) (map (fn [[m rs]] [m (notice-tally rs)])) (group-by :mob rows))})

(defn ore-of [block] (-> block (str/replace #"^deepslate_" "") (str/replace #"_ore$" "")))

(defn groups
  "Sizes of the 26-connected groups of cells."
  [cells]
  (loop [left (set cells) sizes []]
    (if-let [start (first left)]
      (let [[n left] (loop [todo [start] left (disj left start) n 0]
                       (if-let [[x y z] (peek todo)]
                         (let [near (for [dx [-1 0 1] dy [-1 0 1] dz [-1 0 1] :let [c [(+ x dx) (+ y dy) (+ z dz)]] :when (left c)] c)]
                           (recur (into (pop todo) near) (reduce disj left near) (inc n)))
                         [n left]))]
        (recur left (conj sizes n)))
      sizes)))

(defn vein-seen
  "{:groups n :blocks k :per-group k/n :capped lists cut short :by-ore {ore {:groups :blocks}}} of the ore seen together in the
  :observed records."
  [records]
  (let [rows (for [{:keys [blocks]} (filter #(= :observed (:k %)) records)
                   [ore cells] (group-by (comp ore-of first) blocks)
                   n (groups (map second cells))]
               [ore n])
        tally (fn [rs] {:groups (count rs) :blocks (reduce + 0 (map second rs))})
        all (tally rows)]
    (assoc all
           :per-group (when (pos? (:groups all)) (/ (js/Math.round (* 100 (/ (:blocks all) (:groups all)))) 100))
           :capped (count (filter #(and (= :observed (:k %)) (:capped? %)) records))
           :by-ore (into (sorted-map) (map (fn [[o rs]] [o (tally rs)])) (group-by first rows)))))

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

(def usage "usage: node tools/event-counts.mjs fold | creeper-exchange | flight-starts | notice-rate | drowned-meets | flee-pace | spot-returns | vein-seen [--agents DIR] [--counts FILE] [--since-ms MS] [--sprint BLOCKS-PER-S]
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

(def probe? "Bodies whose logs the drowned counts leave out: world-test probes, not trial play." #(re-find #"^(Claude)?Probe" %))

(defn fold-logs!
  "Reads every body's logs under agents-dir and folds the rows into the counts file; returns the new counts. at: the
  chunks' cell reader (world-test.drowned-meets/class-of)."
  [agents-dir counts-file sprint at]
  (let [per-agent (for [b (array-seq (fs/readdirSync agents-dir))
                        :when (fs/existsSync (path/join agents-dir b "engine"))]
                    [b (read-agent (path/join agents-dir b "engine") 0)])
        counts (reduce (fn [c [b records]]
                         (cond-> (fold c (exchange records) (starts records sprint) (notices records))
                           (not (probe? b)) (-> (drowned/fold (drowned/exposure at records) (drowned/meets at records))
                                                (update :flee-steps #(reduce (fn [m r] (assoc m (:id r) r)) % (flee-steps records))))))
                       (load-counts counts-file) per-agent)]
    (save-counts! counts-file counts)
    counts))

(defn cell-reader
  "(fn [x y z]) -> {:name :block :sky :biome} or nil of the chunks open-chunks (the JS adapter, nil: none) reads from dir."
  [open-chunks dir]
  (if (and open-chunks (fs/existsSync dir))
    (let [^js read (open-chunks dir)]
      (fn [x y z] (when-let [^js c (read x y z)] {:name (.-name c) :block (.-block c) :sky (.-sky c) :biome (.-biome c)})))
    (constantly nil)))

(defn main
  "argv (array) -> promise of the exit code. open-chunks: tools/event-counts.mjs's reader of the recorded chunks."
  [argv open-chunks]
  (let [{[cmd] :pos :keys [agents counts since-ms sprint] :or {since-ms 0 sprint 5.6}} (parse-args (vec (array-seq argv)))
        repo (or js/process.env.WORLD_TEST_REPO ".")
        agents (or agents (path/join repo "worlds" "test" "agents"))
        folded (when (#{"fold" "creeper-exchange" "flight-starts" "notice-rate" "drowned-meets" "flee-pace"} cmd)
                 (fold-logs! agents (or counts (path/join repo "worlds" "test" "priors-counts.edn")) sprint
                             (cell-reader open-chunks (path/join agents ".." "chunks"))))
        since (fn [rows] (filter #(>= (:t %) since-ms) (vals rows)))]
    (js/Promise.resolve
     (case cmd
       "fold" (do (println (pr-str (update-vals folded count))) 0)
       "creeper-exchange" (do (println (pr-str (assoc (exchange-report (since (:exchange folded))) :since-ms since-ms))) 0)
       "flight-starts" (do (println (pr-str (assoc (starts-report (since (:starts folded))) :since-ms since-ms :sprint sprint))) 0)
       "spot-returns" (do (println (pr-str (spot-returns/count-in agents))) 0)
       "drowned-meets" (do (println (pr-str (assoc (drowned/report (since (:drowned-exposure folded)) (since (:drowned-meets folded))) :since-ms since-ms))) 0)
       "vein-seen" (do (println (pr-str (vein-seen (mapcat #(read-agent (path/join agents % "engine") since-ms)
                                                       (filter #(fs/existsSync (path/join agents % "engine")) (array-seq (fs/readdirSync agents)))))))
                       0)
       "flee-pace" (do (println (pr-str (assoc (pace-report (since (:flee-steps folded))) :since-ms since-ms))) 0)
       "notice-rate" (do (println (pr-str (assoc (notice-report (since (:notices folded))) :since-ms since-ms))) 0)
       (do (js/console.error usage) 2)))))
