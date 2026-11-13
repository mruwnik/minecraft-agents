(ns dashboard.takeover
  "Emergency takeover (POST /api/takeover/<world>/<name>): SIGKILL every process of one body, so its owner can log in.
  A process belongs to the body when its argv holds `--agent <name>` and `--world <world>` as exact token pairs (the
  body, its res-slot/flock/timeout/npm wrappers), or, for a shell, the words of its `-c` script do (a restart loop).
  Ancestors are taken while they belong to the body or supervise it (`--body <name>`: the world-test runner), and
  every descendant of those; killing top-down leaves no supervisor alive to respawn a child. Nothing here starts a
  body. This process and its own ancestors are never killed; pids are checked by start time before each kill."
  (:require [clojure.string :as str]
            ["fs" :as fs]))

(def shells #{"sh" "bash" "dash" "zsh"})

(defn base-name [s] (last (str/split (or s "") #"/")))

(defn words
  "The tokens a process was started with: argv, plus the words of a shell's -c script."
  [argv]
  (if (and (shells (base-name (first argv))) (some #{"-c"} argv))
    (let [[before [_ script & after]] (split-with #(not= "-c" %) argv)]
      (concat before (str/split (or script "") #"[\s;&|()'\"]+") after))
    argv))

(defn pair? [tokens flag value]
  (boolean (some #(= [flag value] %) (partition 2 1 tokens))))

(defn body-process? [argv name world]
  (let [ws (words argv)] (and (pair? ws "--agent" name) (pair? ws "--world" world))))

(defn supervisor? [argv name world]
  (let [ws (words argv)]
    (and (pair? ws "--body" name)
         (or (not (some #{"--world"} ws)) (pair? ws "--world" world)))))

(defn ancestors-of [table pid]
  (rest (take-while some? (iterate #(get-in table [% :ppid]) pid))))

(defn depth [table pid] (count (ancestors-of table pid)))

(defn plan
  "The pids to kill for body name in world, parents before children: [{:pid :start :argv :role}]. table = {pid {:ppid
  :start :argv}}; self = this process's pid, which with its ancestors is spared."
  [table name world self]
  (let [spared (into #{self 0 1} (ancestors-of table self))
        bodies (filter #(body-process? (:argv (table %)) name world) (keys table))
        owner? #(let [argv (:argv (table %))] (or (body-process? argv name world) (supervisor? argv name world)))
        chain (mapcat (fn [pid] (cons pid (take-while owner? (ancestors-of table pid)))) bodies)
        roots (set chain)
        children (group-by #(:ppid (table %)) (keys table))
        below (fn below [pid] (mapcat #(cons % (below %)) (children pid)))
        pids (->> (concat chain (mapcat below roots)) distinct (remove spared))]
    (->> pids
         (sort-by #(depth table %))
         (mapv (fn [pid]
                 (let [{:keys [start argv]} (table pid)]
                   {:pid pid :start start :argv argv
                    :role (cond (body-process? argv name world) :body
                                (supervisor? argv name world) :supervisor
                                :else :child)}))))))

(defn launcher
  "The first ancestor of the plan's top that is not killed (the shell or service the body was started from), or nil."
  [table targets]
  (let [killed (set (map :pid targets))
        top (first targets)]
    (when-let [pid (and top (first (remove killed (ancestors-of table (:pid top)))))]
      (when (> pid 1) {:pid pid :argv (:argv (table pid))}))))

;; ---------------------------------------------------------------- /proc

(defn parse-stat
  "{:state :ppid :start} of a /proc/<pid>/stat line; the comm may hold spaces and parens."
  [text]
  (let [fields (str/split (str/trim (subs text (inc (str/last-index-of text ")")))) #" ")]
    {:state (nth fields 0) :ppid (js/parseInt (nth fields 1) 10) :start (nth fields 19)}))

(defn read-proc
  "{:ppid :start :state :argv} of a live pid, nil when it is gone or a zombie."
  [pid]
  (when-let [stat (try (parse-stat (fs/readFileSync (str "/proc/" pid "/stat") "utf8")) (catch :default _ nil))]
    (when-not (#{"Z" "X"} (:state stat))
      (when-let [cmd (try (fs/readFileSync (str "/proc/" pid "/cmdline") "utf8") (catch :default _ nil))]
        (assoc stat :argv (vec (remove empty? (str/split cmd #"\u0000"))))))))

(defn read-table []
  (into {} (keep (fn [entry]
                   (when (re-matches #"\d+" entry)
                     (let [pid (js/parseInt entry 10)]
                       (some->> (read-proc pid) (vector pid))))))
        (fs/readdirSync "/proc")))

(defn kill! [pid] (.kill js/process pid "SIGKILL"))

(def real-io
  {:read-table read-table :read-proc read-proc :kill! kill! :self (.-pid js/process)
   :sleep #(js/Promise. (fn [ok] (js/setTimeout ok %)))})

;; ---------------------------------------------------------------- kill and verify

(def poll-ms 50)
(def polls
  "How many times a pass looks for its pids to be gone (SIGKILL cannot be caught; only an uninterruptible wait delays it)."
  40)

(defn alive? [read-proc {:keys [pid start]}] (= start (:start (read-proc pid))))

(defn kill-all!
  "Kills each target still the same process; returns the targets with :error when the kill threw."
  [{:keys [read-proc kill!]} targets]
  (mapv (fn [t]
          (if-not (alive? read-proc t)
            (assoc t :gone-before true)
            (try (kill! (:pid t)) t
                 (catch :default e (assoc t :error (ex-message e))))))
        targets))

(defn await-gone!
  "Resolves to the targets still alive after up to `polls` checks."
  [{:keys [read-proc sleep]} targets]
  (letfn [(check [n]
            (let [left (filterv #(alive? read-proc %) targets)]
              (if (or (empty? left) (>= n polls))
                (js/Promise.resolve left)
                (.then (sleep poll-ms) #(check (inc n))))))]
    (check 0)))

(defn pass!
  "One scan, kill and wait: {:killed :survivors :launcher}."
  [io name world]
  (let [table ((:read-table io))
        targets (plan table name world (:self io))
        killed (kill-all! io targets)]
    (.then (await-gone! io targets)
           (fn [survivors] {:killed killed :survivors survivors :launcher (launcher table targets)}))))

(defn takeover!
  "Kills every process of body name in world and checks they are gone; one rescan catches a respawn raced in between.
  Resolves to {:killed [...] :survivors [...] :respawned [...] :launcher {:pid :argv}|nil :ok?}."
  ([name world] (takeover! real-io name world))
  ([io name world]
   (-> (pass! io name world)
       (.then (fn [first-pass]
                (.then (pass! io name world)
                       (fn [second-pass]
                         (let [seen (set (map (juxt :pid :start) (:killed first-pass)))
                               respawned (vec (remove #(seen ((juxt :pid :start) %)) (:killed second-pass)))
                               survivors (vec (distinct (concat (:survivors first-pass) (:survivors second-pass))))]
                           {:body name :world world
                            :killed (:killed first-pass)
                            :respawned respawned
                            :survivors survivors
                            :launcher (or (:launcher first-pass) (:launcher second-pass))
                            :ok? (and (empty? survivors) (empty? respawned))}))))))))
