(ns world-test.lease
  "Plot leases for parallel world-test runners (pure parts). A lease is one file per plot index holding the owner's
  PID, created exclusively; a lease whose PID is dead is reclaimed. The file and process access is in the runner.")

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

(defn parse-online
  "The reply of the server's `list` command -> the player names."
  [reply]
  (let [after (second (re-find #"online:\s*(.*)$" (or reply "")))]
    (vec (remove empty? (map #(.trim %) (.split (or after "") ","))))))

(defn strangers
  "Online names that are neither this runner's body nor a body holding a live lease."
  [online self leased]
  (vec (remove #(or (= % self) (contains? leased %)) online)))

(defn near-command
  "The command that succeeds when a player other than self and the leased bodies is within 500 blocks of [x y z]."
  [self leased [x y z]]
  (str "execute if entity @a[" (apply str (map #(str "name=!" % ",") (cons self (sort leased))))
       "x=" x ",y=" y ",z=" z ",distance=..500]"))

(defn phase-of-ticks
  "Time of day in ticks -> :night (13000..22999 of the day) or :day."
  [ticks]
  (let [t (mod ticks 24000)] (if (and (>= t 13000) (< t 23000)) :night :day)))

(defn decide
  "entries: {pid {:phase :state (:hold or :want) :seq}} of live processes. -> {:held true :first? bool} when pid may
  hold phase now (every holder shares the phase and no waiter of the other phase is older), else {:waiting-on pids}.
  :night-x (a sleeping case) shares with nobody: every holder and every older waiter blocks it, and it blocks all."
  [entries pid phase seq]
  (let [others (dissoc entries pid)
        holders (filter (fn [[_ e]] (= :hold (:state e))) others)
        solo? (fn [p] (= :night-x p))
        differs? (fn [e] (or (not= phase (:phase e)) (solo? phase)))
        blockers (concat (filter (fn [[_ e]] (differs? e)) holders)
                         (filter (fn [[_ e]] (and (= :want (:state e)) (differs? e) (< (:seq e) seq))) others))]
    (if (empty? blockers)
      {:held true :first? (empty? holders)}
      {:waiting-on (vec (sort (map first blockers)))})))

(defn try-share
  "One attempt at the time lock shared by phase: any number of processes may hold the same phase together; the other
  phase waits for all of them, and once it waits, new holders of the current phase queue behind it. Dead processes'
  entries are dropped. dir: {:guard (thunk -> its result, run exclusively) :entries (-> {pid entry}) :put! (pid entry)
  :remove! (pid) :alive? (pid -> bool)}. seq: when this process began waiting (keeps its place in the queue)."
  [{:keys [guard entries put! remove! alive?]} pid phase seq]
  (guard
   (fn []
     (let [live (into {} (filter (fn [[p _]] (or (= p pid) (alive? p)))) (entries))
           _ (run! remove! (remove live (keys (entries))))
           r (decide live pid phase seq)]
       (put! pid {:phase phase :state (if (:held r) :hold :want) :seq seq})
       r))))

(defn time-phase
  "The phase (:day, :night or :night-x) whose time lock case c holds: its :time, else the phase of its first :time-set step; nil
  when it does not depend on the time of day."
  [c]
  (case (:time c)
    :day :day
    :night :night
    :night-exclusive :night-x
    (some (fn [[op ticks]] (when (= :time-set op) (phase-of-ticks ticks))) (:act c))))
