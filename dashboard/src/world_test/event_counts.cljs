(ns world-test.event-counts
  "tools/event-counts.mjs: recounts the measured combat priors from the trial bodies' event logs, so a setting can cite its k and n.
  Commands: fold | creeper-exchange | flight-starts | notice-rate | way-on (world-test.way-prior; counts the logs alone, no fold) | drowned-meets (world-test.drowned-meets; trial bodies only) | flee-pace (trial bodies only), each [--agents DIR] [--counts FILE] [--since-ms MS] [--sprint BLOCKS-PER-S]; each folds the logs into the durable counts file first (rows by run id, so counts only grow as logs rotate).
  spot-returns [--agents DIR] (world-test.spot-returns), field-passes [--agents DIR] (world-test.field-passes), arrow-shots [--agents DIR] [--difficulty D] (world-test.arrow-shots; D the
  server's, default 1, easy, the trial server's), caught-flights [--agents DIR] (world-test.caught-flights; trial bodies only), hide-ends [--agents DIR] (world-test.hide-ends; trial bodies only) and vein-seen [--agents DIR] count from the logs alone, no fold;
  water-lags [--agents DIR] [--stride N] (world-test.water-lags) and surface-heights (world-test.surface-heights) from the recorded chunks beside DIR;
  resight [--agents DIR] [--counts FILE] (world-test.resight) folds the bodies' seen memory against those chunks into the counts file; wall-sides likewise folds the trial bodies' enter-room.arrived events (world-test.wall-sides), wall-places their flights' walls attempts and pillars, the probes' pillars too (world-test.wall-places).
  vein-seen: the ore blocks one look saw together (26-connected, a vein's plain and deepslate blocks as one ore) per group, from the
  look tool's :look.observed lists; :capped counts the lists the tool cut short (nearest first, so a far group may be cut).
  creeper-exchange: respond runs that only decided about creepers and fought (a :fight decision) with at least one swing: hp the creepers
  dealt (hurt events inside the run) per swing (attack that hit or killed), overall and per weapon equipped in the run, and the runs with a blast over 2 hp.
  flight-starts: the first flee decision of each respond run from each mob kind: seconds until the body is 1 block from where it stood,
  less that block at sprint speed, known within the gap between two positions (the engine stamps the body's position on some
  events, stamped-re, so the stream is sparse) or right-censored when the flight stopped being watched (a run that held still first is
  no start); per kind and melee kinds pooled, the
  deciles at (i + 0.5) / 10 of their Turnbull fit, and :setting, the value of danger/::flight-starts (starts-by-mob).
  notice-rate: the hostiles a watch first saw within their follow range (watch.saw, one per mob that became known; provoked-only mobs left out), each followed until it
  reached the body (caught: a hurt from its kind, or a respond decision that places a seen mob within cost.danger/melee-reach of the body's last
  position), the body more than the mob's follow range from where it was seen, or a position gap over give-up-ms (censored: the seconds up to
  the gap count, no catch): catches per second in its field, each second weighted by cost.threat/way-weight of the body's straight distance
  from that spot (a lower bound on the mob's steps, so an upper bound on the weight: the rate counts low). The setting death/::notice-rate
  stores {:k catches :n-s weighted seconds} for each class: :other (reach: danger/melee-reach) and :ranged (a mob of threat/ranged?, caught within
  its threat/fire-range, whose line of fire the logs do not hold, so its weight is an upper bound too). A catch ends one sighting: the events
  carry no entity id, so it is the open sighting of the kind nearest the event's spot. :ranged also counts the fixture sightings
  (exposures: engine/fixtures/world/ranged-notice.edn), which the logs of trial play lack: every skeleton there was first seen beyond its range.
  flee-pace: the flee steps (retreat.step-speed, slot :step) of flights of two steps or more: blocks over the steps' own seconds, and the
  seconds stood between two steps while still in flight (a look, a re-decide, a replan, a respond delay; a gap in which another
  answer, action or child ran is left out) (cost.danger/::flee-walk-speed, ::flee-stop-s).
  walk-detour: the steers' blocks over the straight line of each go-to walk (value/::detour-factor).
  closing-rate: of the same flights, the gap to each chaser seen as two steps in a row start (retreat.step-speed :chasers), its growth
  over the seconds between the starts, per mob kind and sprint state (cost.danger/::closing-rates).
  walk-pace: the steers of go-to's walks (a job id ending /walk) outside a flight: blocks over the steers' own seconds (cost/walk-speed).
  gap-miss: the miss rate of gap jumps per [width dy sprint] from go-to's :gap-jump events (world-test.gap-miss; trial bodies only).
  Pure parts: scan-line, exchange, starts, starts-by-mob, notices, groups, vein-seen, flee-steps, pace-report, closing-pieces, closing-report, walk-steers, walk-report, walk-detour."
  (:require ["fs" :as fs]
            ["path" :as path]
            ["zlib" :as zlib]
            [cljs.reader :as reader]
            [clojure.string :as str]
            [jobs.lib.cost.danger :as danger]
            [jobs.lib.danger :as danger-q]
            [engine.settings :as settings]
            [jobs.lib.cost.threat :as threat]
            [world-test.arrow-shots :as arrow-shots]
            [world-test.caught-flights :as caught-flights]
            [world-test.censored :as censored]
            [world-test.drowned-meets :as drowned]
            [world-test.field-passes :as field-passes]
            [world-test.gap-miss :as gap-miss]
            [world-test.fresh-drop :as fresh-drop]
            [world-test.hide-ends :as hide-ends]
            [world-test.resight :as resight]
            [world-test.spot-returns :as spot-returns]
            [world-test.surface-heights :as surface-heights]
            [world-test.water-lags :as water-lags]
            [world-test.pen-escapes :as pen-escapes]
            [world-test.wall-places :as wall-places]
            [world-test.wall-sides :as wall-sides]
            [world-test.way-prior :as way-prior]))

(def blast-hp "hp over which one hurt event from a creeper is its blast." 2)
(def tail-ms "Time after a run ends within which a hurt still counts as its own." 1500)
(def stay-m "Blocks from where it stood at the decision at which a body has got going." 1)
(def pos-age-ms "Longest age of the position a decision is measured from." 2000)
(def give-up-ms "A position gap over which a sighting is censored." 10000)

(defn root-job [id] (first (str/split (str id) #"/")))

(def pos-re #":pos \{:x (-?[\d.]+(?:E-?\d+)?), :y (-?[\d.]+(?:E-?\d+)?), :z (-?[\d.]+(?:E-?\d+)?)\}")

(def flight-actions "Actions a flight runs between its steps (a look, the walk's steering, a wait to respond): a gap with any other was no stop of the flight." #{"steer" "look" "wait"})
(def flight-slots "Children a flight runs between its steps (step, run, walk, pause): a gap with any other (eat, plug, back-off, cornered...) ended the flight." #{"step" "run" "walk" "pause"})

(def stamped-re "Events the engine stamps with the body's position (events/emit!: no emitter's own :pos), besides an action's :done."
  #":kind :(?:planned|started|fired|round_started|memory_written|child_started|child_ended|holding)[,}]")

(def held-re "A respond run's events that it holds still on purpose (hides, seals, is cornered): its flight does not get going."
  #":kind :(?:retreat_sealed|hostile\.sealed-hold|holding, .*:message \"holding: (?:hiding|cornered|under a roof))")

(defn- num-of [re line] (some-> (re-find re line) second js/parseFloat))

(defn off-flight
  "record (scan-line's, nil: none) with :off the line's root job when the line starts, inside a respond run, an action or a
  child no flight runs between its steps (flight-actions, flight-slots): flee-steps ends the flight's stop there."
  [line t gen record]
  (if (and (str/includes? line ":reflex-id :hostile-near")
           (or (and (str/includes? line ":kind :started") (str/includes? line ":source :action")
                    (not (flight-actions (second (re-find #":name \"([^\"]*)\"" line)))))
               (and (str/includes? line ":kind :child_started") (not (flight-slots (second (re-find #":slot :([a-z-]+)" line)))))))
    (assoc (or record {:k :off :t t :gen gen}) :off (root-job (second (re-find #":job-id \"([^\"]*)\"" line))))
    record))

(defn scan-line
  "One log line -> a small record, or nil for a line the counts do not read. Only the few kinds they read are parsed whole."
  [line]
  (let [t (num-of #":time-ms (\d+)" line)
        gen (some-> (re-find #":generation-id \"([^\"]*)\"" line) second)
        whole #(try (reader/read-string line) (catch :default _ nil))]
    (when (and t gen)
      (off-flight line t gen
      (cond
        (str/includes? line ":message \"ranged-notice ")
        (let [[_ mob light d window] (re-find #":message \"ranged-notice (\S+) (\S+) (\S+) (\S+)" line)]
          {:k :exposed :t t :gen gen :mob mob :label (str mob " " light " " d) :window-ms (* 1000 (js/parseFloat window))})

        (and (str/includes? line ":kind :hostile.decided") (str/includes? line ":hostile-near"))
        (let [e (whole)]
          {:k :decided :t t :gen gen :job (root-job (get-in e [:context :job-id]))
           :mob (get-in e [:data :mob]) :decision (get-in e [:data :decision]) :health (get-in e [:data :health]) :near (get-in e [:data :near])
           :at (when-not (get-in e [:data :band]) (get-in e [:data :pos]))})

        (str/includes? line ":kind :retreat.step-speed")
        {:k :step :t t :gen gen :job (root-job (second (re-find #":job-id \"([^\"]*)\"" line)))
         :slot (some-> (re-find #":slot :([a-z-]+)" line) second keyword)
         :s (num-of #":seconds (-?[\d.]+)" line) :blocks (num-of #":blocks (-?[\d.]+)" line)
         :food (num-of #":food (\d+)" line) :chasers (when (str/includes? line ":chasers [[") (get-in (whole) [:data :chasers]))}


        (str/includes? line ":kind :gap-jump")
        (let [{{:keys [width dy sprint out from]} :data} (whole)]
          {:k :gap :t t :gen gen :width width :dy dy :sprint sprint :out out :from from})

        (and (str/includes? line ":kind :watch.saw") (str/includes? line ":how :seen"))
        (let [{{:keys [name distance pos id]} :data} (whole)]
          {:k :saw :t t :gen gen :mob name :id id :distance distance :at pos})

        (and (str/includes? line ":hostile-near") (re-find held-re line))
        {:k :held :t t :gen gen :job (root-job (second (re-find #":job-id \"([^\"]*)\"" line)))}

        (and (str/includes? line ":kind :ended") (str/includes? line ":hostile-near"))
        {:k :ended :t t :gen gen :job (root-job (second (re-find #":job-id \"([^\"]*)\"" line)))
         :wear (second (re-find #":wear \{\"([^\"]*_(?:sword|axe))\"" line))}

        (and (str/includes? line ":kind :look.observed") (str/includes? line "_ore\""))
        (let [e (whole)]
          {:k :observed :t t :gen gen :capped? (boolean (get-in e [:data :more-blocks?]))
           :blocks (vec (for [{:keys [name pos]} (get-in e [:data :blocks]) :when (str/ends-with? name "_ore")] [name pos]))})

        (and (str/includes? line ":kind :hurt") (str/includes? line ":source :body"))
        (let [e (whole)]
          {:k :hurt :t t :gen gen :amount (get-in e [:data :amount]) :hits (get-in e [:data :hits]) :causes (set (get-in e [:data :causes])) :at (get-in e [:data :pos]) :since (get-in e [:data :since-ms])})

        (and (str/includes? line ":kind :died") (str/includes? line ":source :body"))
        {:k :died :t t :gen gen :cause (second (re-find #":cause \"([^\"]*)\"" line))}

        (and (str/includes? line ":kind :started") (str/includes? line ":name \"equip\""))
        {:k :equip :t t :gen gen :job (root-job (second (re-find #":job-id \"([^\"]*)\"" line)))
         :item (second (re-find #"\"item\" \"([^\"]*)\"" line))}

        (and (str/includes? line ":kind :done") (str/includes? line ":name \"attack\"") (re-find #":status \"(hit|killed)\"" line))
        {:k :hit :t t :gen gen :job (root-job (second (re-find #":job-id \"([^\"]*)\"" line)))}

        (and (str/includes? line ":kind :done") (str/includes? line ":name \"attack\""))
        {:k :miss :t t :gen gen :job (root-job (second (re-find #":job-id \"([^\"]*)\"" line)))}

        (or (and (str/includes? line ":source :action") (str/includes? line ":kind :done")) (re-find stamped-re line))
        (when-let [[_ x y z] (last (re-seq pos-re line))]
          (cond-> {:k :pos :t t :gen gen :x (js/parseFloat x) :y (js/parseFloat y) :z (js/parseFloat z)}
            (str/includes? line ":name \"steer\"")
            (assoc :steer {:id (second (re-find #":action-id \"([^\"]*)\"" line))
                           :job (second (re-find #":job-id \"([^\"]*)\"" line))
                           :done? (str/includes? line ":status \"done\"")
                           :flight? (str/includes? line ":reflex-id :hostile-near")})))

        :else
        (when-let [[_ x y z] (re-find pos-re line)]
          {:k :at :t t :gen gen :x (js/parseFloat x) :y (js/parseFloat y) :z (js/parseFloat z)}))))))

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

(defn ranged-mob? [mob] (threat/ranged? {:name mob}))

(defn start-id [{:keys [gen job mob]}] (if (= "creeper" mob) [gen job] [gen job mob]))

(defn starts
  "The first flee decision from each mob kind of each respond run (records sorted by :t), as {:id :mob :t decision-ms :lo :hi}: the
  seconds until the body is stay-m from where it stood, less that block at sprint speed (at least 0), lie in [:lo, :hi]: from the last
  position that still stood there to the first that did not. :hi nil (right-censored) when the flight was no longer watched
  before the body moved: its run ended, decided other than to flee, the body died, or the logs end. :end says which (:moved when it
  did). A switch to fight is censored: the flight was meant and had not got going by then. {:id :mob :t :drop :no-pos}: no position
  within pos-age-ms before the decision (where it stood is unknown); :drop :held: the run held still (held-re) first, an answer that
  never meant to move, so no start.
  :id [gen job] for a creeper and [gen job mob] for any other kind."
  [records sprint]
  (let [first-flee (->> records
                        (filter #(and (= :decided (:k %)) (:mob %) (= :flee (:decision %))))
                        (reduce (fn [acc r] (let [id (start-id r)] (if (acc id) acc (assoc acc id r)))) {})
                        vals set)
        secs (fn [t0 t] (max 0 (- (/ (- t t0) 1000) (/ stay-m sprint))))
        row (fn [{:keys [id mob t still]} hi end] {:id id :mob mob :t t :lo (secs t (max t still)) :hi hi :end end})
        close (fn [st done? end] (let [{gone true kept false} (group-by (comp boolean done?) (:pending st))]
                                   (-> st (assoc :pending (vec kept))
                                       (update :out into (map #(if (= :held end) (assoc (select-keys % [:id :mob :t]) :drop :held) (row % nil end))) gone))))
        run? (fn [gen job] #(= [gen job] [(:gen %) (:job %)]))
        watch (fn [{:keys [last-pos] :as st} {:keys [k gen job t decision] :as r}]
                (case k
                  :decided (cond-> (if (= :flee decision) st (close st (run? gen job) :decided))
                             (first-flee r)
                             (as-> st (let [p (get last-pos gen)]
                                        (if (and p (<= (- t (:t p)) pos-age-ms))
                                          (update st :pending conj {:gen gen :job job :id (start-id r) :mob (:mob r) :t t :from p :still (:t p)})
                                          (update st :out conj {:id (start-id r) :mob (:mob r) :t t :drop :no-pos})))))
                  :pos (let [{here true waiting false} (group-by #(= gen (:gen %)) (:pending st))
                             {moved true still false} (group-by #(>= (dist r (:from %)) stay-m) here)]
                         (-> st
                             (assoc-in [:last-pos gen] r)
                             (assoc :pending (vec (concat waiting (map #(assoc % :still t) still))))
                             (update :out into (map #(row % (secs (:t %) t) :moved)) moved)))
                  (:ended :held) (close st (run? gen job) k)
                  :died (close st #(= gen (:gen %)) :died)
                  st))]
    (:out (close (reduce watch {:last-pos {} :pending [] :out []} records) identity :logs-end))))

(defn flee-steps
  "Rows [{:id [gen t] :flight [gen job] :t :s :blocks :off? :food :chasers}] of the flee steps in records (sorted by :t;
  retreat.step-speed of slot :step: :s the step's own seconds, ending at :t; :chasers [[name id gap]], nil
  in a log before they were kept; gap the blocks to a hostile seen as the step started). :off? when, since the flight's step before,
  another answer's step or an action or child no flight runs between its steps came (:off): the flight had ended, so
  the gap before it was no stop."
  [records]
  (:rows
   (reduce (fn [{:keys [off] :as st} {:keys [k gen job t s blocks slot food chasers] :as r}]
             (let [f [gen job]]
               (cond
                 (:off r) (assoc-in st [:off [gen (:off r)]] true)
                 (not (and (= :step k) s blocks)) st
                 (not= :step slot) (assoc-in st [:off f] true)
                 :else (-> st
                           (update :rows conj (cond-> {:id [gen t] :flight f :t t :s s :blocks blocks :off? (boolean (get off f))}
                                                food (assoc :food food) chasers (assoc :chasers chasers)))
                           (update :off dissoc f)))))
           {:off {} :rows []} records)))

(defn r2 [x] (/ (js/Math.round (* 100 x)) 100))

(defn walk-steers
  "Rows [{:id [gen t] :job :t :s :blocks}] of the steers of walks (a job id ending /walk, go-to's walk child) in records (sorted by
  :t) that ran to their end outside a hostile-near answer: :s the steer's seconds, :blocks the straight line from where it started to
  where it ended (:from, :to those places)."
  [records]
  (:rows
   (reduce (fn [st {:keys [gen t steer] :as r}]
             (cond
               (not steer) st
               (not (:done? steer)) (assoc-in st [:open (:id steer)] r)
               :else (let [from (get-in st [:open (:id steer)])]
                       (cond-> (update st :open dissoc (:id steer))
                         (and from (not (:flight? steer)) (re-find #"(?:^|/)walk$" (str (:job steer))))
                         (update :rows conj {:id [gen (:t from)] :job (:job steer) :t t :s (/ (- t (:t from)) 1000) :blocks (dist from r)
                                                  :from [(:x from) (:y from) (:z from)] :to [(:x r) (:y r) (:z r)]})))))
           {:open {} :rows []} records)))

(defn walk-report
  "The pace of the walks among rows (walk-steers): blocks over the steers' own seconds (:bps), of :walks jobs and :steers steers."
  [rows]
  (let [blocks (reduce + 0 (map :blocks rows)) secs (reduce + 0 (map :s rows))]
    {:walks (count (distinct (map (juxt (comp first :id) :job) rows))) :steers (count rows)
     :blocks (r2 blocks) :seconds (r2 secs) :bps (when (pos? secs) (r2 (/ blocks secs)))}))


(def min-detour-line "Blocks of straight line under which a walk says nothing of its detour." 4)
(def walk-break "Blocks between where a walk job's steer ended and the next began over which the next steer is another go-to call." 3)

(defn pos-of [v] (zipmap [:x :y :z] v))

(defn walk-detour
  "The detour of the walks among rows (walk-steers; rows kept before :from was recorded are left out): a walk is the run of one go-to
  walk job's steers of one body run, each beginning within walk-break of where the last ended; its detour is the blocks the steers
  covered over the straight line from where it started to where it ended (:ratio of the sums, :median of the walks'), over walks of
  at least min-detour-line straight. The steers' chords cut the curves inside a steer, so the ratio is a lower bound."
  [rows]
  (let [runs (mapcat (fn [steers]
                       (reduce (fn [runs r]
                                 (if (and (seq runs) (<= (dist (pos-of (:to (peek (peek runs)))) (pos-of (:from r))) walk-break))
                                   (conj (pop runs) (conj (peek runs) r))
                                   (conj runs [r])))
                               [] (sort-by :t steers)))
                     (vals (group-by (juxt (comp first :id) :job) (filter :from rows))))
        walks (for [steers runs
                    :let [line (dist (pos-of (:from (first steers))) (pos-of (:to (peek steers))))]
                    :when (>= line min-detour-line)]
                [(reduce + 0 (map :blocks steers)) line])
        n (count walks)
        walked (reduce + 0 (map first walks))
        straight (reduce + 0 (map second walks))]
    {:walks n :walked (r2 walked) :straight (r2 straight)
     :ratio (when (pos? n) (r2 (/ walked straight)))
     :median (when (pos? n)
               (let [v (vec (sort (map #(/ (first %) (second %)) walks)))]
                 (r2 (/ (+ (v (quot (dec n) 2)) (v (quot n 2))) 2))))}))

(defn pace-report
  "The pace of the flights of two steps or more among rows (flee-steps): blocks over the steps' own seconds (:bps), and the
  seconds stood between two steps of a flight (:stop-s: from one step's end to the next one's start, over the gaps a
  flight stood in, :off? ones left out and counted as :off-gaps); :by-sprint the same steps' blocks and seconds per :sprint / :walk
  (danger/sprints? of the step's food; :unknown in a log before food was kept)."
  [rows]
  (let [flights (filter #(>= (count %) 2) (map #(sort-by :t %) (vals (group-by :flight rows))))
        steps (apply concat flights)
        pairs (mapcat (fn [f] (map vector f (rest f))) flights)
        gaps (keep (fn [[a b]] (when-not (:off? b) (max 0 (/ (- (:t b) (* 1000 (:s b)) (:t a)) 1000)))) pairs)
        sum #(reduce + 0 %)
        blocks (sum (map :blocks steps)) secs (sum (map :s steps)) stood (sum gaps)]
    {:flights (count flights) :steps (count steps) :blocks (r2 blocks) :seconds (r2 secs)
     :bps (when (pos? secs) (r2 (/ blocks secs))) :gaps (count gaps) :gap-s (r2 stood) :off-gaps (- (count pairs) (count gaps))
     :stop-s (when (seq gaps) (r2 (/ stood (count gaps))))
     :by-sprint (into {} (map (fn [[k ss]] (let [b (sum (map :blocks ss)) s (sum (map :s ss))]
                                             [k {:flights (count (distinct (map :flight ss))) :steps (count ss) :blocks (r2 b) :seconds (r2 s)
                                                 :bps (when (pos? s) (r2 (/ b s)))}])))
                        (group-by #(cond (nil? (:food %)) :unknown (danger/sprints? (:food %)) :sprint :else :walk) steps))}))

(defn closing-pieces
  "Pieces [{:mob :sprint :blocks :s}] of the flights among rows (flee-steps, sorted or not) with gaps kept: from one step's start
  to the next one's (the step and the stop after it; not to an :off? one), the gap to each chaser seen at the first start: :blocks
  its growth, :s the seconds (a chaser not seen at the next start is out of sight, outrun or dead: its gap there at least
  the radius it is a danger within, jobs.lib.danger/kind-radius), :sprint whether the body could sprint (danger/sprints? of its food at the first start)."
  [rows]
  (let [start #(- (:t %) (* 1000 (:s %)))]
    (mapcat (fn [steps]
              (for [[a b] (partition 2 1 (sort-by :t steps)) :when (not (:off? b))
                    :let [s (/ (- (start b) (start a)) 1000)
                          gaps (into {} (map (fn [[_ id g]] [id g])) (:chasers b))]
                    :when (pos? s)
                    [mob id g] (:chasers a) :let [g1 (or (gaps id) (max g (danger-q/kind-radius (threat/ranged? {:name mob}))))]]
                {:mob mob :sprint (danger/sprints? (:food a)) :blocks (- g1 g) :s s}))
            (vals (group-by :flight (filter #(and (:food %) (:chasers %)) rows))))))

(defn closing-report
  "The closing rate of the flights among rows (flee-steps): per mob kind and :sprint / :walk, the gap's growth over the seconds
  (closing-pieces), :bps blocks a second (negative: the mob gains), :pieces counted; :setting, danger/::closing-rates."
  [rows]
  (let [groups (group-by (juxt :mob #(if (:sprint %) :sprint :walk)) (closing-pieces rows))
        sum (fn [ps k] (reduce + 0 (map k ps)))
        tally (fn [ps] {:blocks (r2 (sum ps :blocks)) :s (r2 (sum ps :s))})]
    {:flights (count (distinct (map :flight (filter #(and (:food %) (:chasers %)) rows))))
     :by-mob (reduce (fn [m [[mob sprint] ps]]
                       (let [t (tally ps)]
                         (assoc-in m [mob sprint] (assoc t :pieces (count ps) :bps (when (pos? (:s t)) (r2 (/ (:blocks t) (:s t))))))))
                     (sorted-map) groups)
     :setting (reduce (fn [m [[mob sprint] ps]] (assoc-in m [mob sprint] (tally ps))) {} groups)}))

(def tenths "The levels of the deciles a start prior holds." (mapv #(/ (+ % 0.5) 10) (range 10)))

(defn bounds
  "[lo hi] of a start row (starts; nil: dropped), censored/fit's. lo 0 is starts' floor, so it may be the start itself: no lower
  bound (lo nil). A row folded before the bounds were kept: its :s seen exactly; a :never, which did not move in its first
  give-up-ms, right-censored there."
  [{:keys [lo hi s drop] :as r}]
  (cond (contains? r :lo) [(when (or (pos? lo) (= lo hi)) lo) hi]
        s [s s]
        (= :never drop) [(/ give-up-ms 1000) nil]))

(defn starts-report
  "Of start rows (starts): n of those with bounds, how many were seen exactly, within a gap and right-censored, the decisions with no
  position before them (:no-pos), and the mean and deciles of their Turnbull fit (world-test.censored)."
  [rows]
  (let [xs (keep bounds rows)
        f (when (seq xs) (censored/fit xs))]
    {:n (count xs) :exact (count (filter (fn [[lo hi]] (and hi (= lo hi))) xs)) :interval (count (filter (fn [[lo hi]] (and hi (not= lo hi))) xs))
     :censored (count (remove second xs)) :no-pos (count (filter #(= :no-pos (:drop %)) rows))
     :mean (some-> f censored/mean r2) :deciles (when f (mapv #(r2 (censored/quantile f %)) tenths))}))

(defn mob-of [row] (or (:mob row) "creeper"))

(defn starts-by-mob
  "starts-report of each mob kind's rows (:by-mob; a row counted before kinds were kept is a creeper's) and of the melee kinds' (not
  threat/ranged?, the kinds danger/flight-starts prices) together (:melee); the melee kinds' spread (world-test.censored/between); and
  :setting, danger/::flight-starts: each melee kind's deciles shrunk toward the other melee kinds' (censored/shrink), \"pooled\" all
  melee kinds' together, for a kind with no row; its :n the starts it holds; :spread's :information each kind's (censored/shrink's n)."
  [rows]
  (let [melee (remove #(ranged-mob? (mob-of %)) rows)
        groups (into {} (keep (fn [[mob rs]] (when-let [xs (seq (keep bounds rs))] [mob (vec xs)]))) (group-by mob-of melee))
        shrunk (censored/shrink groups tenths)]
    {:by-mob (into (sorted-map) (map (fn [[mob rs]] [mob (starts-report rs)])) (group-by mob-of rows))
     :melee (starts-report melee)
     :spread (assoc (update-vals (censored/between groups) #(if (js/isFinite %) (/ (js/Math.round (* 1000 %)) 1000) %))
                    :information (update-vals shrunk (comp r2 :n)))
     :setting (into {"pooled" {:n (count (keep bounds melee)) :deciles (:deciles (starts-report melee))}}
                    (map (fn [[mob {:keys [w pooled]}]] [mob {:n (count (groups mob)) :w (r2 w) :deciles (mapv r2 pooled)}]))
                    shrunk)}))

;; The durable counts: the rows of each prior by run id, so the counts only grow while the raw logs rotate away.
(def empty-counts {:exchange {} :starts {} :notices {} :drowned-exposure {} :drowned-meets {} :flight-steps {} :walk-steers {} :gap-jumps {} :resight {} :walls {} :wall-places {} :pen-escapes {}})

(defn better-exchange "Of two rows of one fight, the one that saw more swings (a rotation may have cut the older read short), then the older." [a b]
  (if (> (:swings b) (:swings a)) b a))

(defn start-rank
  "A start row that says how it ended (a held run its drop) over one whose logs ended first, over one with bounds folded before, over
  one folded before bounds were kept, over a dropped one." [r]
  (cond (= :logs-end (:end r)) 3 (or (:end r) (= :held (:drop r))) 4 (contains? r :lo) 2 (:s r) 1 :else 0))

(defn fold
  "counts with the rows of exchange-rows, start-rows and notice-rows merged in by id; a row already there stays unless a fight row saw
  more swings, a later read of a start ranks as high (start-rank), or a sighting was still open when its log ended."
  ([counts exchange-rows start-rows] (fold counts exchange-rows start-rows []))
  ([counts exchange-rows start-rows notice-rows]
   (-> counts
       (update :exchange #(reduce (fn [m r] (update m (:id r) (fn [old] (if old (better-exchange old r) r)))) % exchange-rows))
       (update :starts #(reduce (fn [m r] (update m (:id r) (fn [old] (if (and old (> (start-rank old) (start-rank r))) old r)))) % start-rows))
       (update :notices #(reduce (fn [m r] (update m (:id r) (fn [old] (if (and old (not= :open (:end old))) old r)))) % notice-rows)))))

(defn load-counts
  "The counts file's priors (empty-counts' keys; a retired key such as :flee-steps, rows with no flight class, is dropped)."
  [file]
  (if (fs/existsSync file) (merge empty-counts (select-keys (reader/read-string (fs/readFileSync file "utf8")) (keys empty-counts))) empty-counts))

(defn save-counts!
  "Written whole to a temporary file and renamed over, so a reader never sees half a file."
  [file counts]
  (let [tmp (str file ".tmp")]
    (fs/mkdirSync (path/dirname file) #js {:recursive true})
    (fs/writeFileSync tmp (str (pr-str counts) "\n"))
    (fs/renameSync tmp file)))

(defn reach-of
  "Blocks from the body within which mob can catch it: a ranged one by its line of fire out to its kind's threat/fire-range, any other by melee reach."
  [mob]
  (if (ranged-mob? mob) (threat/fire-range mob) (danger/melee-reach)))

(defn exposures
  "Rows of the fixture sightings in records (sorted by :t): world fixtures that stand a body still (a notify job in place of the
  hostile-near job, text `ranged-notice <mob> <light> <blocks> <window-s>`) with a mob of that kind shooting at it from a clear line
  of fire. The first firing of a run is the sighting (weight 1: in range with a line of fire); :s the seconds until the first hurt
  from the kind (its first hit's time, the hurt's :since-ms), :caught, or the window, :window (censored). A hurt before the firing was a shot from an unseen shooter and leaves
  the run out: the counts price a danger the body has seen."
  [records]
  (for [[_ rs] (group-by :gen records)
        :let [{:keys [t mob gen label window-ms] :as start} (first (filter #(= :exposed (:k %)) rs))]
        :when (and start (not-any? #(and (= :hurt (:k %)) (contains? (:causes %) mob) (<= (:t %) t)) rs))
        :let [hit (first (filter #(and (= :hurt (:k %)) (contains? (:causes %) mob) (< t (:t %))) rs))
              hit-t (when hit (max t (or (:since hit) (:t hit))))
              caught? (and hit (<= (- hit-t t) window-ms))]]
    {:id [gen t mob] :t t :mob mob :label label :end (if caught? :caught :window)
     :s (/ (if caught? (- hit-t t) window-ms) 1000)}))

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
    (-> (:rows out) (into (map #(row % :open)) (mapcat val (:open out))) (into (exposures records)))))

(defn notice-tally [rows]
  (let [ends (frequencies (map :end rows))
        k (get ends :caught 0)
        s (reduce + 0 (map :s rows))]
    {:sightings (count rows) :caught k :ends ends :weighted-s (/ (js/Math.round (* 10 s)) 10)
     :rate (when (pos? s) (/ (js/Math.round (* 10000 (/ (+ k 0.5) s))) 10000))}))

(defn notice-report [rows]
  {:all (notice-tally rows)
   :by-class (into (sorted-map) (map (fn [[ranged? rs]] [(if ranged? :ranged :other) (notice-tally rs)])) (group-by (comp ranged-mob? :mob) rows))
   :by-mob (into (sorted-map) (map (fn [[m rs]] [m (notice-tally rs)])) (group-by :mob rows))
   :by-case (into (sorted-map) (map (fn [[l rs]] [l (notice-tally rs)])) (group-by :label (filter :label rows)))})

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

(defn event-lines
  "The lines of one body's event files (dir)."
  [dir]
  (->> (event-files (array-seq (fs/readdirSync dir)))
       (mapcat (fn [f]
                 (let [p (path/join dir f)
                       text (if (str/ends-with? f ".gz") (.toString (zlib/gunzipSync (fs/readFileSync p)) "utf8") (fs/readFileSync p "utf8"))]
                   (str/split-lines text))))))

(defn way-events
  "The :way-on.* events of one body's files since since-ms, as maps."
  [dir since-ms]
  (->> (event-lines dir)
       (filter #(str/includes? % ":kind :way-on."))
       (map reader/read-string)
       (filter #(>= (:time-ms %) since-ms))))

(defn read-agent
  "The records of one body's event files since since-ms, sorted by time."
  [dir since-ms]
  (->> (event-lines dir)
       (keep scan-line)
       (filter #(>= (:t %) since-ms))
       (sort-by :t)
       vec))

(def usage "usage: node tools/event-counts.mjs fold | creeper-exchange | flight-starts | notice-rate | drowned-meets | flee-pace | closing-rate | walk-pace | walk-detour | gap-miss | way-on | spot-returns | field-passes | arrow-shots | caught-flights | vein-seen | water-lags | surface-heights | resight | wall-sides | pen-escapes | wall-places | hide-ends [--run ID --animals N --secs S --escaped E] [--agents DIR] [--counts FILE] [--since-ms MS] [--sprint BLOCKS-PER-S] [--stride N] [--difficulty D]
  every command first folds the logs into the counts file (default worlds/test/priors-counts.edn), then reports from the file; --since-ms keeps the rows of fights started after it.")

(defn- parse-args [argv]
  (loop [[a & more] argv o {}]
    (case a
      nil o
      "--agents" (recur (rest more) (assoc o :agents (first more)))
      "--counts" (recur (rest more) (assoc o :counts (first more)))
      "--since-ms" (recur (rest more) (assoc o :since-ms (js/parseFloat (first more))))
      "--sprint" (recur (rest more) (assoc o :sprint (js/parseFloat (first more))))
      "--stride" (recur (rest more) (assoc o :stride (js/parseInt (first more))))
      "--run" (recur (rest more) (assoc o :run (first more)))
      "--inside-s" (recur (rest more) (assoc o :inside-s (js/parseFloat (first more))))
      "--step-s" (recur (rest more) (assoc o :step-s (js/parseFloat (first more))))
      "--escaped" (recur (rest more) (assoc o :escaped (js/parseInt (first more))))
      "--difficulty" (recur (rest more) (assoc o :difficulty (js/parseInt (first more))))
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
                         (-> (cond-> (fold c (exchange records) (starts records sprint) (notices records))
                               (not (probe? b)) (-> (drowned/fold (drowned/exposure at records) (drowned/meets at records))
                                                    (update :flight-steps #(reduce (fn [m r] (assoc m (:id r) r)) % (flee-steps records)))
                                                    (update :walk-steers #(reduce (fn [m r] (assoc m (:id r) r)) % (walk-steers records)))
                                                    ; probes' fixture jumps are perfect run-ups: they would bias the miss rate down
                                                    (update :gap-jumps #(reduce (fn [m r] (assoc m (:id r) (assoc r :t (second (:id r))))) % (gap-miss/rows records)))))))
                       (load-counts counts-file) per-agent)]
    (save-counts! counts-file counts)
    counts))

(defn cell-reader
  "(fn [x y z]) -> {:name :block :sky :biome} or nil of the chunks open-chunks (the JS adapter, nil: none) reads from dir."
  [open-chunks dir]
  (if (and open-chunks (fs/existsSync dir))
    (let [^js read (open-chunks dir)]
      (fn [x y z] (when-let [^js c (read x y z)] {:name (.-name c) :id (.-id c) :t (.-t c) :block (.-block c) :sky (.-sky c) :biome (.-biome c)})))
    (constantly nil)))

(defn main
  "argv (array) -> promise of the exit code. open-chunks: tools/event-counts.mjs's reader of the recorded chunks; seen: its
  reader of the seen memory files (.load file, .stateName id)."
  [argv open-chunks ^js seen]
  (let [{[cmd] :pos :keys [agents counts since-ms sprint stride difficulty run inside-s step-s escaped] :or {since-ms 0 sprint 5.6 stride 1 difficulty 1}} (parse-args (vec (array-seq argv)))
        repo (or js/process.env.WORLD_TEST_REPO ".")
        agents (or agents (path/join repo "worlds" "test" "agents"))
        folded (when (#{"fold" "creeper-exchange" "flight-starts" "notice-rate" "drowned-meets" "flee-pace" "closing-rate" "walk-pace" "walk-detour" "gap-miss"} cmd)
                 (fold-logs! agents (or counts (path/join repo "worlds" "test" "priors-counts.edn")) sprint
                             (cell-reader open-chunks (path/join agents ".." "chunks"))))
        since (fn [rows] (filter #(>= (:t %) since-ms) (vals rows)))]
    (js/Promise.resolve
     (case cmd
       "fold" (do (println (pr-str (update-vals folded count))) 0)
       "creeper-exchange" (do (println (pr-str (assoc (exchange-report (since (:exchange folded))) :since-ms since-ms))) 0)
       "flight-starts" (do (println (pr-str (assoc (starts-by-mob (since (:starts folded))) :since-ms since-ms :sprint sprint))) 0)
       "water-lags" (let [dir (path/join agents ".." "chunks")]
                      (println (pr-str (water-lags/count-in dir (cell-reader open-chunks dir) stride)))
                      0)
       "surface-heights" (let [dir (path/join agents ".." "chunks")]
                           (println (pr-str (surface-heights/count-in dir (cell-reader open-chunks dir) stride)))
                           0)
       "resight" (let [file (or counts (path/join repo "worlds" "test" "priors-counts.edn"))
                       at (cell-reader open-chunks (path/join agents ".." "chunks"))
                       folded (reduce (fn [c b]
                                        (let [f (path/join agents b "engine" "seen.bin")
                                              ^js d (when (fs/existsSync f) (.load seen f))]
                                          (cond-> c d (resight/fold (resight/rows b (.-sections d) (.-stateName seen) at)))))
                                      (load-counts file) (array-seq (fs/readdirSync agents)))]
                   (save-counts! file folded)
                   (println (pr-str (resight/report (:resight folded))))
                   0)
       "wall-sides" (let [file (or counts (path/join repo "worlds" "test" "priors-counts.edn"))
                          rows (into {} (for [b (array-seq (fs/readdirSync agents))
                                              :when (and (not (probe? b)) (fs/existsSync (path/join agents b "engine")))
                                              line (event-lines (path/join agents b "engine"))
                                              :let [[t row] (wall-sides/scan-line line)]
                                              :when t]
                                          [[b t] row]))
                          folded (wall-sides/fold (load-counts file) rows)]
                      (save-counts! file folded)
                      (println (pr-str (wall-sides/report (:walls folded))))
                      0)
       "pen-escapes" (let [file (or counts (path/join repo "worlds" "test" "priors-counts.edn"))
                           added (if run (pen-escapes/fold (load-counts file) {run (pen-escapes/row escaped inside-s step-s 1)}) (load-counts file))]
                       (when run (save-counts! file added))
                       (println (pr-str (pen-escapes/report (:pen-escapes added))))
                       0)
       "wall-places" (let [file (or counts (path/join repo "worlds" "test" "priors-counts.edn"))
                           ;; a pillar refused beside a mob is physics given its class, which staged scenes do not skew
                           rows (into {} (for [b (array-seq (fs/readdirSync agents))
                                               :when (fs/existsSync (path/join agents b "engine"))
                                               [k row] (wall-places/scan (event-lines (path/join agents b "engine")))
                                               :when (or (not (probe? b)) (= :pillar (:way row)))]
                                           [(into [b] k) row]))
                           folded (wall-places/fold (load-counts file) rows)]
                       (save-counts! file folded)
                       (println (pr-str (wall-places/report (:wall-places folded))))
                       0)
       "hide-ends" (do (println (pr-str (hide-ends/report (for [b (array-seq (fs/readdirSync agents))
                                                                 :when (and (not (probe? b)) (fs/existsSync (path/join agents b "engine")))
                                                                 line (event-lines (path/join agents b "engine"))
                                                                 :let [end (hide-ends/scan-line line)]
                                                                 :when end]
                                                             end))))
                       0)
       "spot-returns" (do (println (pr-str (spot-returns/count-in agents))) 0)
       "field-passes" (do (println (pr-str (field-passes/count-in agents probe?))) 0)
       "arrow-shots" (let [rows (mapcat #(arrow-shots/meets (read-agent (path/join agents % "engine") since-ms))
                                        (filter #(fs/existsSync (path/join agents % "engine")) (array-seq (fs/readdirSync agents))))]
                       (println (pr-str (assoc (settings/with-settings {::threat/difficulty difficulty} #(arrow-shots/report rows))
                                               :difficulty difficulty :since-ms since-ms)))
                       0)
       "drowned-meets" (do (println (pr-str (assoc (drowned/report (since (:drowned-exposure folded)) (since (:drowned-meets folded))) :since-ms since-ms))) 0)
       "caught-flights" (let [recs (map #(read-agent (path/join agents % "engine") since-ms)
                                        (filter #(and (not (probe? %)) (fs/existsSync (path/join agents % "engine"))) (array-seq (fs/readdirSync agents))))]
                          (println (pr-str (assoc (settings/with-settings {::threat/difficulty difficulty}
                                                    #(caught-flights/report (mapcat caught-flights/episodes recs) (mapcat (fn [rs] (caught-flights/episodes rs :fight)) recs)))
                                                  :difficulty difficulty :since-ms since-ms)))
                          0)
       "vein-seen" (do (println (pr-str (vein-seen (mapcat #(read-agent (path/join agents % "engine") since-ms)
                                                       (filter #(fs/existsSync (path/join agents % "engine")) (array-seq (fs/readdirSync agents)))))))
                       0)
       "way-on" (let [per (for [b (array-seq (fs/readdirSync agents)) :when (fs/existsSync (path/join agents b "engine"))]
                            (way-prior/places (way-events (path/join agents b "engine") since-ms)))]
                  (println (pr-str (assoc (way-prior/report (mapcat :places per) (reduce + (map :evicted per))) :since-ms since-ms)))
                  0)
       "flee-pace" (do (println (pr-str (assoc (pace-report (since (:flight-steps folded))) :since-ms since-ms))) 0)
       "closing-rate" (do (println (pr-str (assoc (closing-report (since (:flight-steps folded))) :since-ms since-ms))) 0)
       "walk-pace" (do (println (pr-str (assoc (walk-report (since (:walk-steers folded))) :since-ms since-ms))) 0)
       "walk-detour" (do (println (pr-str (assoc (walk-detour (since (:walk-steers folded))) :since-ms since-ms))) 0)
       "gap-miss" (do (println (pr-str {:buckets (gap-miss/report (since (:gap-jumps folded))) :since-ms since-ms})) 0)
       "notice-rate" (do (println (pr-str (assoc (notice-report (since (:notices folded))) :since-ms since-ms))) 0)
       (do (js/console.error usage) 2)))))
