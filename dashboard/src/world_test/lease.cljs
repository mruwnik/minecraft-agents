(ns world-test.lease
  "Plot leases for parallel world-test runners (pure parts). A lease is one file per plot index holding the owner's
  PID, created exclusively; a lease whose PID is dead is reclaimed. The file and process access is in the runner.")

(defn acquire
  "Takes the first free plot index from `first` below `total`, returning it. dir: {:create! (i pid -> true when this
  call made the lease) :holder (i -> pid or nil) :reclaim! (i -> removes the lease) :alive? (pid -> bool)}."
  [{:keys [create! holder reclaim! alive?]} {:keys [pid first total]}]
  (loop [i first]
    (when (>= i total) (throw (js/Error. (str "no free plot from " first " below " total))))
    (cond
      (create! i pid) i
      (let [h (holder i)] (and h (not (alive? h)))) (do (reclaim! i) (recur i))
      (nil? (holder i)) (recur i)
      :else (recur (inc i)))))

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
