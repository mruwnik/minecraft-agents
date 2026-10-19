(ns world-test.lease
  "Plot leases for parallel world-test runners (pure parts). A lease is one file per plot index holding the owner's
  PID, created exclusively; a lease whose PID is dead is reclaimed. The file and process access is in the runner."
  (:require [clojure.string :as str]))

(def empty-tries
  "How often a lease file with no PID yet is re-read (settle! between) before it counts as abandoned."
  40)

(defn acquire
  "Takes the first free plot index from `first` below `total`, returning it. dir: {:create! (i pid -> true when this
  call made the lease) :holder (i -> pid, :empty for a file without a PID yet, nil for none) :reclaim! (i holder ->
  removes that lease only if it still holds `holder`) :alive? (pid -> bool) :settle! (optional, a short pause)}."
  [{:keys [create! holder reclaim! alive? settle!]} {:keys [pid first total]}]
  (loop [i first, empties 0]
    (when (>= i total) (throw (js/Error. (str "no free plot from " first " below " total))))
    (if (create! i pid)
      i
      (let [h (holder i)]
        (cond
          (nil? h) (recur i 0)
          (= :empty h) (if (< empties empty-tries)
                         (do (when settle! (settle!)) (recur i (inc empties)))
                         (do (reclaim! i :empty) (recur i 0)))
          (not (alive? h)) (do (reclaim! i h) (recur i 0))
          :else (recur (inc i) 0))))))

(defn- pad [n width] (let [s (str n)] (str (apply str (repeat (max 0 (- width (count s))) "0")) s)))

(defn local-iso
  "[y mo d h mi s ms] (local fields) and the offset east of UTC in minutes -> \"2026-10-05T15:04:09.007+01:00\"."
  [[y mo d h mi s ms] offset-min]
  (let [a (js/Math.abs offset-min)]
    (str (pad y 4) "-" (pad mo 2) "-" (pad d 2) "T" (pad h 2) ":" (pad mi 2) ":" (pad s 2) "." (pad ms 3)
         (if (neg? offset-min) "-" "+") (pad (quot a 60) 2) ":" (pad (mod a 60) 2))))

(defn leased-bodies
  "{body-name pid} (the body leases) and alive? -> the set of body names whose runner is still alive."
  [holders alive?]
  (set (keep (fn [[name pid]] (when (alive? pid) name)) holders)))

(defn parse-body-procs
  "The output of `pgrep -af out/body` -> [{:pid :name}] of the body processes (the --agent name)."
  [out]
  (vec (keep (fn [line]
               (when-let [[_ pid name] (re-find #"^\s*(\d+)\s+(?:\S*/)?node\s.*out/body(?:-release)?\.cjs --agent (\S+)" line)]
                 {:pid (js/parseInt pid 10) :name name}))
             (str/split-lines (or out "")))))

(defn stale-body-procs
  "The body processes named prefix + one capital letter (a pool's bodies) whose name holds no live lease (`leased`)."
  [procs prefix leased]
  (let [own (re-pattern (str "^" prefix "[A-Z]$"))]
    (filterv #(and (re-matches own (:name %)) (not (contains? leased (:name %)))) procs)))

(defn parse-online
  "The reply of the server's `list` command -> the player names."
  [reply]
  (let [after (second (re-find #"online:\s*(.*)$" (or reply "")))]
    (vec (remove empty? (map #(.trim %) (.split (or after "") ","))))))

(defn probe-body?
  "True for a name in the probe-body scheme (Probe...): a test body of ours, never another player."
  [name]
  (boolean (re-find #"^Probe" name)))

(defn own-bodies
  "The online names that are not strangers: this runner's body, bodies holding a live lease, and probe-scheme bodies."
  [online self leased]
  (into (conj (set leased) self) (filter probe-body?) online))

(defn strangers
  "Online names that are neither this runner's body, a body holding a live lease, nor a probe-scheme body."
  [online self leased]
  (let [own (own-bodies online self leased)]
    (vec (remove own online))))

(defn near-command
  "The command that succeeds when a player other than self and the leased bodies is within 500 blocks of [x y z]."
  [self leased [x y z]]
  (str "execute if entity @a[" (apply str (map #(str "name=!" % ",") (cons self (sort leased))))
       "x=" x ",y=" y ",z=" z ",distance=..500]"))

(defn phase-of-ticks
  "Time of day in ticks -> :night (13000..22999 of the day) or :day."
  [ticks]
  (let [t (mod ticks 24000)] (if (and (>= t 13000) (< t 23000)) :night :day)))

(defn effective-phase
  "The phase a request takes: its own, or for :any (a case that does not depend on the time of day) the phase the
  holders hold, else world-phase."
  [entries pid phase world-phase]
  (if-not (= :any phase)
    phase
    (or (some (fn [[p e]] (when (and (not= p pid) (= :hold (:state e))) (:phase e))) entries)
        world-phase)))

(defn decide
  "entries: {pid {:phase :state (:hold or :want) :seq :batch}} of live processes. -> {:held true :first? bool} when pid may
  hold phase now (every holder shares the phase and no waiter of the other phase is older), else {:waiting-on pids}.
  :night-x (a sleeping case) shares with nobody but the :night-x entries of its own batch (nil batch: nobody): every other
  holder and every older waiter blocks it, and it blocks all."
  [entries pid phase seq batch]
  (let [others (dissoc entries pid)
        holders (filter (fn [[_ e]] (= :hold (:state e))) others)
        solo? (fn [p] (contains? #{:night-x :weather-x} p))
        same-batch? (fn [e] (and (some? batch) (= :night-x (:phase e)) (= batch (:batch e))))
        differs? (fn [e] (or (not= phase (:phase e)) (and (solo? phase) (not (same-batch? e)))))
        blockers (concat (filter (fn [[_ e]] (differs? e)) holders)
                         (filter (fn [[_ e]] (and (= :want (:state e)) (differs? e) (< (:seq e) seq))) others))]
    (if (empty? blockers)
      {:held true :first? (empty? holders)}
      {:waiting-on (vec (sort (map first blockers)))})))

(defn sweep-file
  "The file a running world sweep keeps its PID in."
  [shared-dir]
  (str shared-dir "/mc-world-sweep.pid"))

(defn sweep-block
  "Why a run may not take the time lock for phase, or nil. While a sweep lives (marker-pid alive), only its own
  children (sweep-env-pid = marker-pid) take any time lock, day included; time-independent (:any) runs take none."
  [phase marker-pid sweep-env-pid alive?]
  (when (and (number? marker-pid) (alive? marker-pid)
             (not= :any phase)
             (not= marker-pid sweep-env-pid))
    (str "a world sweep is running (pid " marker-pid "); time-locked cases (" (name phase) ") wait for the lead")))

(defn try-share
  "One attempt at the time lock shared by phase: any number of processes may hold the same phase together; the other
  phase waits for all of them, and once it waits, new holders of the current phase queue behind it. A joiner also waits
  ({:waiting-on [setter] :setting true}) while the first holder has not yet called mark-set. Dead processes'
  entries are dropped. dir: {:guard (thunk -> its result, run exclusively) :entries (-> {pid entry}) :put! (pid entry)
  :remove! (pid) :alive? (pid -> bool)}. seq: when this process began waiting (keeps its place in the queue). phase :any
  (a time-independent case) joins the holders' phase, else world-phase (:day or :night), and waits like a holder of it."
  ([dir pid phase seq] (try-share dir pid phase seq :day nil))
  ([dir pid phase seq world-phase] (try-share dir pid phase seq world-phase nil))
  ([{:keys [guard entries put! remove! alive?]} pid phase seq world-phase batch]
   (guard
    (fn []
      (let [live (into {} (filter (fn [[p _]] (or (= p pid) (alive? p)))) (entries))
            _ (run! remove! (remove live (keys (entries))))
            phase (effective-phase live pid phase world-phase)
            r (decide live pid phase seq batch)
            entry (cond-> {:phase phase :seq seq} batch (assoc :batch batch))
            pending (when (:held r) (vec (sort (keep (fn [[p e]] (when (and (not= p pid) (= :hold (:state e)) (:setting e)) p)) live))))]
        (cond
          (not-empty pending) (do (put! pid (assoc entry :state :want)) {:waiting-on pending :setting true})
          (:held r) (do (put! pid (cond-> (assoc entry :state :hold) (:first? r) (assoc :setting true))) r)
          :else (do (put! pid (assoc entry :state :want)) r)))))))

(defn mark-set
  "The first holder calls this once the phase is really in place (its `time set` returned and `time query` confirmed):
  joiners of the phase wait until then. Same dir as try-share. A holder that dies before this leaves no entry, so the
  next joiner is first again."
  [{:keys [guard entries put!]} pid]
  (guard (fn [] (when-let [e (get (entries) pid)] (put! pid (dissoc e :setting))))))

(defn weather-x?
  "Whether case c sets rain or thunder (server-wide, so it runs alone: phase :weather-x)."
  [c]
  (contains? #{:rain :thunder} (:weather c)))

(defn dusk-set?
  "Whether case c has a :time-set step to dusk (11500) or later in the day: it brings hostiles, so it may not share a pass."
  [c]
  (boolean (some (fn [[op ticks]] (and (= :time-set op) (>= (mod ticks 24000) 11500))) (:act c))))

(defn underground?
  "Whether case c runs in a sealed underground cell (:plot :light :lit or :dark): sky light is 0 there whatever the clock says, so it takes no time lock."
  [c]
  (contains? #{:lit :dark} (get-in c [:plot :light])))

(defn time-phase
  "The phase (:day, :night, :night-x or :weather-x, a case with its own :weather) whose time lock case c holds: its :time, else the phase of its first :time-set step (a day one with a :time-set to dusk or later, or a :night one with any :time-set: :night-x, solo); nil
  when it does not depend on the time of day."
  [c]
  (if (weather-x? c)
    :weather-x
    (let [phase (case (:time c)
                  :day :day
                  :night :night
                  :night-exclusive :night-x
                  (some (fn [[op ticks]] (when (= :time-set op) (phase-of-ticks ticks))) (:act c)))]
      (cond
        (and (= :day phase) (dusk-set? c)) :night-x
        (and (= :night (:time c)) (some (fn [[op]] (= :time-set op)) (:act c))) :night-x
        :else phase))))

(defn barrier-ticks
  "The time an exclusive night case sets with its one :time-set step, else nil. Cases that start alike with the same
  barrier-ticks wait for each other at that step, so one body sets it for all."
  [c]
  (let [sets (filter #(= :time-set (first %)) (:act c))]
    (when (and (= 1 (count sets)) (not (weather-x? c)))
      (second (first sets)))))

(defn night-class
  "How a :night-exclusive case may share a night: :barrier (one :time-set step: batches with the cases that start alike and
  set the same time, meeting at the step), :solo (several :time-set steps flip the time for everyone, so it runs alone),
  :skip (:sleep-skip true: needs the vanilla night skip, so it runs in a batch where every body sleeps), else :natural
  (needs only night, batches with other natural cases)."
  [c]
  (cond
    (weather-x? c) :solo
    (barrier-ticks c) :barrier
    (some #(= :time-set (first %)) (:act c)) :solo
    (:sleep-skip c) :skip
    :else :natural))
