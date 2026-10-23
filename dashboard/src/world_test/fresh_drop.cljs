(ns world-test.fresh-drop
  "tools/fresh-drop.mjs: the fresh-drop soak. `drop` sets a body down in a far, unclaimed place with nothing; `report` reads the
  body's event logs and gives one row per run: time to base, farm and kit, deaths, kit losses, lead teleports, jobs the agent queued.
  Commands: plan BODY... --build DIR [--seed N] [--together] | drop BODY... --build DIR [--seed N] [--together] [--go] | report BODY... [--world W] [--from-ms MS] [--tokens N] [--md]
  Pure parts: pick-spots, steps, milestones, report, row."
  (:require ["child_process" :as cp]
            ["fs" :as fs]
            ["path" :as path]
            ["zlib" :as zlib]
            [cljs.reader :as reader]
            [clojure.string :as str]))

(def usage
  (str "usage: node tools/fresh-drop.mjs plan BODY... --build DIR [--seed N] [--together] | drop BODY... --build DIR [--seed N] [--together] [--go] | report BODY... [--world test] [--from-ms MS] [--tokens N] [--md]\n"
       "several BODYs need --together: one spot for all, a few blocks apart.\ndrop prints the steps (like plan) and runs them only with --go: whitelist, workspace, a --fresh body start (engine state and memory wiped), a spot >= 5000 blocks from every known base, empty inventory, spawn point at the spot.\n"
       "report reads worlds/<world>/agents/BODY/engine events (default start: the body's last drop record, else its first event) and prints EDN per body, or markdown rows with --md."))

(def ms-per-day 1200000)
(def min-base-distance 5000)
(def wheat-goal 20)
(def kit-pieces
  #{"iron_pickaxe" "iron_sword" "iron_axe" "iron_shovel" "iron_helmet" "iron_chestplate" "iron_leggings" "iron_boots"})
(def pos-kinds
  "Kinds whose :pos is the body's own; other kinds carry targets, mobs or other players there."
  #{:queued :waiting :completed :hurt :cut :stopped :died :respawned :spawned})
(def jump-blocks 64)
(def jump-gap-ms 5000)
(def respawn-quiet-ms 10000)

;; ---- the drop

(defn- dist [[ax az] [bx bz]] (Math/hypot (- ax bx) (- az bz)))

(defn pick-spots
  "rnd (a fn -> [0,1)), the known base positions [[x z] ...] and n -> n spots [x z] at 5000-20000 blocks from the origin and at least
  min-base-distance from every base. Draws that qualify; the last draws if too few do."
  [rnd bases n]
  (let [draw (fn []
               (let [a (* 2 Math/PI (rnd)) r (+ 5000 (* 15000 (rnd)))]
                 [(Math/round (* r (Math/cos a))) (Math/round (* r (Math/sin a)))]))
        draws (repeatedly 200 draw)]
    (vec (take n (concat (filter (fn [p] (every? #(>= (dist p %) min-base-distance) bases)) draws) (reverse draws))))))

(defn seeded-rnd
  "A deterministic [0,1) generator (mulberry32) from an integer seed."
  [seed]
  (let [s (atom (bit-or seed 0))]
    (fn []
      (let [t (swap! s #(bit-or (+ % 0x6D2B79F5) 0))
            t1 (js/Math.imul (bit-xor t (unsigned-bit-shift-right t 15)) (bit-or t 1))
            t2 (bit-xor t1 (+ t1 (js/Math.imul (bit-xor t1 (unsigned-bit-shift-right t1 7)) (bit-or t1 61))))]
        (/ (unsigned-bit-shift-right (bit-xor t2 (unsigned-bit-shift-right t2 14)) 0) 4294967296)))))

(defn place-commands
  "bodies, spot [x z] -> the rcon argument vectors that put all bodies within a few blocks of the spot: spreadplayers takes one target,
  so the first body is spread around the spot and each other body around the first."
  [bodies [x z]]
  (let [[b & more] bodies]
    (if more
      (into [["spreadplayers" (str x) (str z) "4" "8" "false" b]]
            (map (fn [o] ["execute" "at" b "run" "spreadplayers" "~" "~" "2" "4" "false" o])) more)
      [["spreadplayers" (str x) (str z) "0" "100" "false" b]])))

(defn placed-ok?
  "The rcon answer of a spreadplayers: only the server's success line counts, anything else (an error, 'Could not') is a failure."
  [out]
  (boolean (re-find #"Spread \d+ entit" (str out))))

(defn steps
  "The drop as data, in order: per body [:whitelist b] [:file path text] [:body-stop b world] [:workspace b world] [:body-start b {:world :build}], then
  [:wait-online b], [:check-build b world build] and the wipe for each, one [:place bodies spots], per body the spawn point and [:record path m].
  The --fresh start wipes engine state and memory; [:place bodies spots] puts all bodies on the surface within a few blocks of each other at the
  first of the candidate spots that has dry ground (the rest are for oceans); slow falling then covers a placement in the air while the spot's chunks load."
  [{:keys [bodies world build spots now] :or {world "test"}}]
  (let [each (fn [f] (mapcat f bodies))]
    (vec
     (concat
      (each (fn [body]
              [[:whitelist body]
               [:file (str "worlds/" world "/agents/" body "/config.json")
                (js/JSON.stringify (clj->js {:username body :character {:name body :note "fresh-drop soak body"}}) nil 2)]
               [:body-stop body world]
               [:workspace body world]
               [:body-start body {:world world :build build}]]))
      (each (fn [body] [[:wait-online body]
                        [:check-build body world build]
                        [:rcon (str "gamemode survival " body)]
                        [:rcon (str "clear " body)]]))
      [[:place bodies spots]]
      (each (fn [body]
              [[:rcon (str "effect give " body " minecraft:slow_falling 60 0 true")]
               [:rcon (str "execute as " body " at " body " run spawnpoint " body " ~ ~ ~")]
               [:rcon (str "effect give " body " minecraft:instant_health 1 5 true")]
               [:record (str "worlds/" world "/fresh-drops/" body "-" now ".edn") {:body body :world world :spots spots :build build :started-ms now}]]))))))

;; ---- the report

(defn- ms [e] (:time-ms e))
(defn- xz [e] (let [{:keys [x z]} (get-in e [:data :pos])] (when (and x z) [x z])))

(defn- iron-in-text [text]
  (set (filter kit-pieces (re-seq #"iron_[a-z]+" (str text)))))

(defn- first-ms-where [events pred] (some #(when (pred %) (ms %)) events))

(defn- reaching
  "The time of the first event after which (acc-fn state event) satisfies done?, folding events from init; nil if never."
  [events init step done?]
  (reduce (fn [st e]
            (let [st (step st e)]
              (if (done? st) (reduced (assoc st ::at (ms e))) st)))
          init events))

(defn milestones
  "events (time ordered) -> {milestone ms}: :bed (first spawn set), :home / :chest (first place of that name), :base (bed and home),
  :wheat (20 cut and 20 replanted by farm.harvest), :bred (first baby), :farm (wheat and bred), :kit (all 8 iron pieces reported by kit.progress or crafted).
  A milestone that never happened is absent. The door, the walls and the chest contents leave no event: :base is the bed and the home place."
  [events]
  (let [place? (fn [n] (fn [e] (and (= :place.set (:kind e)) (= n (get-in e [:data :name])))))
        bed (first-ms-where events #(= :spawn-set (:kind %)))
        home (first-ms-where events (place? :home))
        chest (first-ms-where events (place? :chest))
        done? (fn [e n] (and (= :completed (:kind e)) (= n (get-in e [:data :name]))))
        wheat (::at (reaching events {:cut 0 :replanted 0}
                              (fn [st e] (if (done? e "jobs.farm.harvest")
                                           (-> st (update :cut + (get-in e [:data :cut] 0)) (update :replanted + (get-in e [:data :replanted] 0)))
                                           st))
                              #(and (>= (:cut %) wheat-goal) (>= (:replanted %) wheat-goal))))
        bred (first-ms-where events #(and (done? % "jobs.animals.breed") (pos? (get-in % [:data :babies] 0))))
        kit (::at (reaching events {:have #{}}
                            (fn [st e] (case (:kind e)
                                         :kit.progress (update st :have into (iron-in-text (:message e)))
                                         :completed (update st :have into (iron-in-text (get-in e [:data :item])))
                                         st))
                            #(= kit-pieces (:have %))))
        all (fn [& ts] (when (every? some? ts) (apply max ts)))]
    (into {} (filter val {:bed bed :home home :chest chest :base (all bed home)
                          :wheat wheat :bred bred :farm (all wheat bred) :kit kit}))))

(defn deaths
  "[{:at-ms :cause :kit-pieces n}]: n = iron kit pieces in the inventory the death event lists."
  [events]
  (vec (for [e events :when (= :died (:kind e))]
         {:at-ms (ms e) :cause (get-in e [:data :cause])
          :kit-pieces (count (filter #(kit-pieces (:name %)) (get-in e [:data :inventory])))})))

(defn teleports
  "Position jumps over jump-blocks between two events at most jump-gap-ms apart, after which the body stays there (a lone odd position is not one), that no respawn or body start explains: [{:at-ms :blocks}]."
  [events]
  (let [spawn-ms (keep #(when (#{:spawned :respawned :started :start} (:kind %)) (ms %)) events)
        explained? (fn [t] (some #(<= 0 (- t %) respawn-quiet-ms) spawn-ms))
        with-pos (filter #(and (pos-kinds (:kind %)) (xz %)) events)]
    (vec (for [[a b c] (partition 3 1 with-pos)
               :let [d (dist (xz a) (xz b))]
               :when (and (> d jump-blocks) (<= (- (ms b) (ms a)) jump-gap-ms) (<= (dist (xz b) (xz c)) jump-blocks)
                          (not (explained? (ms b))))]
           {:at-ms (ms b) :blocks (Math/round d)}))))

(defn agent-jobs
  "Jobs the agent queued at top level."
  [events]
  (count (filter #(and (= :queued (:kind %)) (= :agent (get-in % [:data :by])) (= 1 (count (get-in % [:context :chain])))) events)))

(defn report
  "body, events (time ordered), start-ms, tokens (or nil) -> the run's report map; times are ms after start-ms."
  [body events start-ms tokens]
  (let [events (filter #(>= (ms %) start-ms) events)
        rel #(- % start-ms)
        ds (deaths events)]
    {:body body :start-ms start-ms
     :end-ms (some-> (last events) ms)
     :milestones (into (sorted-map) (map (fn [[k t]] [k (rel t)])) (milestones events))
     :deaths (count ds)
     :kit-losses (count (filter (comp pos? :kit-pieces) ds))
     :teleports (count (teleports events))
     :agent-jobs (agent-jobs events)
     :tokens tokens}))

(defn- hm [t] (when t (let [m (Math/round (/ t 60000))] (str (quot m 60) "h" (let [r (mod m 60)] (str (when (< r 10) "0") r))))))

(def row-header
  "| body | start (UTC) | base | farm | kit | deaths | kit losses | lead tps | agent jobs | tokens |\n|---|---|---|---|---|---|---|---|---|---|")

(defn row
  "A report -> one markdown row; a goal that was not reached shows `-`."
  [{:keys [body start-ms milestones deaths kit-losses teleports agent-jobs tokens]}]
  (str "| " (str/join " | " [body (.slice (.toISOString (js/Date. start-ms)) 0 16)
                             (or (hm (:base milestones)) "-") (or (hm (:farm milestones)) "-") (or (hm (:kit milestones)) "-")
                             deaths kit-losses teleports agent-jobs (or tokens "-")]) " |"))

;; ---- node edge

(defn- repo [] (fs/realpathSync (or js/process.env.WORLD_TEST_REPO ".")))

(defn- parse-args [argv]
  (loop [[a & more :as args] argv o {:pos []}]
    (cond
      (empty? args) o
      (= a "--build") (recur (rest more) (assoc o :build (first more)))
      (= a "--world") (recur (rest more) (assoc o :world (first more)))
      (= a "--seed") (recur (rest more) (assoc o :seed (js/parseInt (first more))))
      (= a "--from-ms") (recur (rest more) (assoc o :from-ms (js/parseInt (first more))))
      (= a "--tokens") (recur (rest more) (assoc o :tokens (js/parseInt (first more))))
      (= a "--together") (recur more (assoc o :together true))
      (= a "--go") (recur more (assoc o :go true))
      (= a "--md") (recur more (assoc o :md true))
      :else (recur more (update o :pos conj a)))))

(defn- read-text [dir f]
  (let [p (path/join dir f)]
    (if (str/ends-with? f ".gz") (str (.toString (zlib/gunzipSync (fs/readFileSync p)) "utf8")) (str (fs/readFileSync p "utf8")))))

(defn log-files
  "The event log files of a directory listing, oldest first: events.edn.N and events.edn.N.gz (the plain file wins) by N descending, then events.edn."
  [names]
  (let [n-of #(some-> (re-find #"^events\.edn\.(\d+)(\.gz)?$" %) second js/parseInt)
        rotated (->> names (keep (fn [f] (when-let [n (n-of f)] [n f])))
                     (group-by first)
                     (map (fn [[n fs]] [n (or (some (fn [[_ f]] (when-not (str/ends-with? f ".gz") f)) fs) (second (first fs)))]))
                     (sort-by first >) (map second))]
    (vec (concat rotated (filter #{"events.edn"} names)))))

(def full-kinds
  "Kinds the report reads in full; every other line is only scanned for time, kind and body position."
  #{:spawn-set :place.set :completed :kit.progress :died :queued :spawned :respawned :started :start})

(defn read-line
  "One log line -> an event map, or nil. Lines of the full-kinds are read whole; the rest give :time-ms :kind :seq :generation-id
  and, for pos-kinds, the first :pos."
  [line]
  (let [kind (some-> (re-find #":kind :([^\s,]+)" line) second keyword)]
    (cond
      (nil? kind) nil
      (full-kinds kind) (try (reader/read-string line) (catch :default _ nil))
      (#{:player-joined :player-left} kind) nil
      :else (when-let [t (some-> (re-find #":time-ms (\d+)" line) second js/parseInt)]
              (let [[_ x z] (re-find #":pos \{:x (-?[\d.]+), :y -?[\d.]+, :z (-?[\d.]+)" line)]
                (cond-> {:time-ms t :kind kind
                         :seq (some-> (re-find #":seq (\d+)" line) second js/parseInt)
                         :generation-id (some-> (re-find #":generation-id \"([^\"]*)\"" line) second)}
                  (and x (pos-kinds kind)) (assoc-in [:data :pos] {:x (js/parseFloat x) :z (js/parseFloat z)})))))))

(defn- read-events [world body]
  (let [dir (path/join (repo) "worlds" world "agents" body "engine")
        files (log-files (array-seq (fs/readdirSync dir)))]
    (->> files
         (mapcat #(str/split-lines (read-text dir %)))
         (keep read-line)
         (filter :time-ms)
         (sort-by :time-ms)
         (reduce (fn [[seen out] e] (let [k [(:generation-id e) (:seq e)]] (if (seen k) [seen out] [(conj seen k) (conj out e)]))) [#{} []])
         second)))

(defn- last-drop-ms [world body]
  (let [dir (path/join (repo) "worlds" world "fresh-drops")
        mine (when (fs/existsSync dir)
               (filter #(str/starts-with? % (str body "-")) (array-seq (fs/readdirSync dir))))]
    (->> mine (keep #(:started-ms (reader/read-string (str (fs/readFileSync (path/join dir %) "utf8"))))) sort last)))

(defn- known-bases
  "[x z] of every place or spawn another body recorded, from the current event files."
  [world]
  (let [dir (path/join (repo) "worlds" world "agents")]
    (vec (for [b (array-seq (fs/readdirSync dir))
               :let [f (path/join dir b "engine" "events.edn")]
               :when (fs/existsSync f)
               line (str/split-lines (str (fs/readFileSync f "utf8")))
               :when (re-find #":kind :(place\.set|spawn-set)," line)
               :let [m (re-find #":pos \{:x (-?\d+)(?:\.\d+)?, :y -?[\d.]+, :z (-?\d+)" line)]
               :when m]
           [(js/parseInt (nth m 1)) (js/parseInt (nth m 2))]))))

(defn reset-workspace!
  "Makes the body's workspace a working one on this repo: the old notes/ move aside to notes.old-<date>-<ms> (unique per call, so a
  second drop never leaves old notes in place) and the stale context.edn goes, so the rebind and the wrapper refresh below are accepted."
  [dir now-ms]
  (let [notes (path/join dir "notes")
        aside (str notes ".old-" (.slice (.toISOString (js/Date. now-ms)) 0 10) "-" now-ms)]
    (when (fs/existsSync notes) (fs/renameSync notes aside))
    (fs/rmSync (path/join dir "context.edn") #js {:force true})))

(defn body-process?
  "True for the argv (a vector) of the node process running body's engine: not the res-slot shell that restarts it, not a pgrep."
  [argv body world]
  (let [s (str/join " " argv)]
    (and (not (re-find #"(^| )(sh|pgrep)( |$)" (first argv)))
         (str/includes? s "body-release.cjs")
         (str/includes? s (str "--agent " body " --world " world)))))

(defn status-commit
  "The build commit in an `observe status` answer (EDN text), or nil."
  [out]
  (second (re-find #":build \{[^}]*:commit \"([^\"]+)\"" (str out))))

(defn build-commit
  "The commit tools/compile wrote beside the build's engine bundle, or nil."
  [build]
  (let [f (path/join build "engine/out/build-id.edn")]
    (when (fs/existsSync f) (:commit (reader/read-string (fs/readFileSync f "utf8"))))))

(defn- sleep! [s] (cp/spawnSync "sleep" #js [(str s)]))

(defn- body-pids [body world]
  (let [out (str (.-stdout (cp/spawnSync "pgrep" #js ["-f" "--" (str "--agent " body " --world " world)] #js {:encoding "utf8"})))]
    (filterv (fn [pid]
               (try (let [argv (vec (remove empty? (str/split (fs/readFileSync (str "/proc/" pid "/cmdline") "utf8") "\u0000")))]
                      (body-process? argv body world))
                    (catch :default _ false)))
             (remove empty? (str/split-lines out)))))

(defn- body-online? [body]
  (str/includes? (str (.-stdout (cp/spawnSync js/process.execPath #js [(path/join (repo) "tools/rcon-raw.mjs") "list"] #js {:cwd (repo) :encoding "utf8"}))) body))

(defn- stop-body!
  "Ends the running engine for body (a second one for the same name exits on the duplicate login and --fresh never takes);
  throws when it is still there after 30 s."
  [body world]
  (doseq [pid (body-pids body world)] (js/process.kill (js/parseInt pid) "SIGTERM"))
  (loop [n 0]
    (when (or (seq (body-pids body world)) (body-online? body))
      (when (>= n 30) (throw (js/Error. (str body " is still running or online 30 s after it was told to stop"))))
      (sleep! 1)
      (recur (inc n)))))

(defn- check-build!
  "Waits for the body's status to report the drop's build commit; throws when it never does."
  [body world build]
  (let [want (build-commit build)
        status #(str (.-stdout (cp/spawnSync js/process.execPath #js [(path/join (repo) "worlds" world "agents" body "bin/observe") "status"] #js {:encoding "utf8"})))]
    (loop [n 0]
      (let [got (status-commit (status))]
        (cond
          (and want (= want got)) nil
          (>= n 60) (throw (js/Error. (str body " reports build " (pr-str got) ", the drop's build is " (pr-str want))))
          :else (do (sleep! 2) (recur (inc n))))))))

(defn- run-node! [script args]
  (cp/spawnSync js/process.execPath (clj->js (into [(path/join (repo) script)] args)) #js {:stdio "inherit" :cwd (repo)}))

(def placed (atom nil))

(defn- exec-step!
  "Run one step of `steps`. Not run by the tests: a live server and a built body are needed."
  [[kind & a]]
  (case kind
    :whitelist (run-node! "tools/rcon.mjs" [(first a)])
    :file (let [[f text] a, p (path/join (repo) f)]
            (fs/mkdirSync (path/dirname p) #js {:recursive true})
            (fs/writeFileSync p text))
    :body-stop (stop-body! (first a) (second a))
    :check-build (check-build! (first a) (second a) (nth a 2))
    :workspace (let [[body world] a
                     worlds (fs/realpathSync (path/join (repo) "worlds"))]
                 (when (body-online? body)
                   (throw (js/Error. (str body " is online; stop it before its workspace is reset"))))
                 (reset-workspace! (path/join worlds world "agents" body) (js/Date.now))
                 (run-node! "engine/tools/workspace.mjs" [(path/join worlds world "agents" body) "--body" body "--world" world "--worlds" worlds "--adopt-existing" "--update-tools"]))
    :body-start (let [[body {:keys [world build]}] a
                      child (cp/spawn "sh"
                                      #js ["-c" "while :; do \"$@\"; [ $? -eq 75 ] || break; sleep 20; done" "sh" (path/join (repo) "tools/res-slot")
                                           "body" "--" js/process.execPath "--max-old-space-size=192" "--heap-growing-percent=100" "out/body-release.cjs" "--agent" body "--world" world
                                           "--scenario" (path/join build "engine/scenarios/survival.edn")
                                           "--worlds" (path/join (repo) "worlds") "--blueprints" (path/join (repo) "blueprints") "--fresh"]
                                      #js {:cwd (path/join build "engine") :detached true :stdio "ignore"
                                           :env (js/Object.assign #js {} js/process.env #js {:NODE_COMPILE_CACHE (path/join (repo) "engine/out/compile-cache")})})]
                  (.unref child))
    :wait-online (let [body (first a)]
                   (loop [n 0]
                     (when-not (or (body-online? body) (>= n 300))
                       (cp/spawnSync "sleep" #js ["2"])
                       (recur (inc n)))))
    :place (let [[bodies spots] a
                 try-spot (fn [spot]
                            (every? (fn [args]
                                      (let [out (str (.-stdout (cp/spawnSync js/process.execPath (clj->js (into [(path/join (repo) "tools/rcon-raw.mjs")] args))
                                                                            #js {:cwd (repo) :encoding "utf8"})))]
                                        (js/console.log (str/trim out))
                                        (placed-ok? out)))
                                    (place-commands bodies spot)))]
             (reset! placed (first (filter try-spot spots))))
    :rcon (run-node! "tools/rcon-raw.mjs" (str/split (first a) #" "))
    :record (let [[f m] a, p (path/join (repo) f)]
              (fs/mkdirSync (path/dirname p) #js {:recursive true})
              (fs/writeFileSync p (pr-str (cond-> m @placed (assoc :spot @placed)))))))

(defn main
  "argv (array) -> promise of the exit code."
  [argv]
  (let [{[cmd & pos] :pos :keys [go build world seed from-ms tokens md together] :or {world "test"}} (parse-args (vec (array-seq argv)))]
    (js/Promise.resolve
     (case cmd
       ("plan" "drop")
       (if-not (and (first pos) build (or together (nil? (next pos))))
         (do (js/console.error usage) 2)
         (let [rnd (seeded-rnd (or seed (js/Date.now)))
               ss (steps {:bodies (if together pos [(first pos)]) :world world :build build :spots (pick-spots rnd (known-bases world) 8) :now (js/Date.now)})]
           (doseq [s ss] (js/console.log (pr-str s)))
           (if-not (and go (= "drop" cmd))
             0
             (loop [[s & more] ss]
               (if-not s
                 0
                 (do (exec-step! s)
                     (if (and (= :place (first s)) (nil? @placed))
                       (do (js/console.error "no candidate spot placed every body; stopping before the spawn points") 1)
                       (recur more))))))))
       "report"
       (if (empty? pos)
         (do (js/console.error usage) 2)
         (let [reps (for [b pos
                          :let [evs (read-events world b)
                                start (or from-ms (last-drop-ms world b) (some-> (first evs) ms) 0)]]
                      (report b evs start tokens))]
           (if md
             (do (js/console.log row-header) (doseq [r reps] (js/console.log (row r))))
             (doseq [r reps] (js/console.log (pr-str r))))
           0))
       (do (js/console.error usage) 2)))))
