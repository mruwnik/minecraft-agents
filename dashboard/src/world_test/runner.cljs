(ns world-test.runner
  "tools/world-test.mjs: runs world fixtures (engine/fixtures/world/*.edn, format in its README) against the live
  test server with one probe body. Per case and run: a fresh plot of the reserved grid is cleared and built, the body
  is put at its start, the act steps run, the body's event log is read until every expectation is decided, the
  :after checks ask the server, and the plot is cleaned up. The body is started once per distinct :register (the
  scenario) and restarted before every case with its engine/memory.edn, seen.bin and notes file and its leftover test plans deleted and the plot it was last left on cleared (a case with :keep-memory true keeps
  the memory: the body goes on from the case before it). Stopped at the end. Pure parts: world-test.fixture and world-test.expect."
  (:require ["child_process" :as cp]
            ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            [cljs.reader :as reader]
            [clojure.string :as str]
            [dashboard.rcon :as rcon]
            [agent-tools.world-data :as wd]
            [dashboard.goal :as goal]
            [world-test.build :as build]
            [world-test.changed :as chg]
            [world-test.events :as ev]
            [world-test.trace :as trace]
            [world-test.expect :as x]
            [world-test.fixture :as f]
            [world-test.lease :as lease]))

(def usage
  (str "usage: node tools/world-test.mjs [fixture.edn|dir ...] [--tag T] [--match TEXT] [--match-id ID ...] [--repeat N] [--body NAME]\n"
       "         [--world test] [--first-plot I] [--card ID] [--allow-time --time-log FILE] [--results FILE] [--trace-dir DIR [--trace-attempt N]] [--phase day|night|night-exclusive] [--night-batch ID] [--night-barrier N] [--night-batch-end ID] [--night-classes] [--day-limit TICKS] [--retry-failed N] [--changed-since-pass] [--stop-on-fail] [--list] [--check] [--stop-stale-bodies PREFIX] [--refresh-build]\n"
       "--phase runs only the cases of that time class (case level; :any and untimed cases, and :day, count as day; a case whose first :time-set step is\n"
       "night counts as night): run day, then night, then night-exclusive so the time lock never flips mid-pass.\n"
       "--changed-since-pass skips a fixture file that passed fully at a recorded revision (every run passes record it, in .claude/world-test-passes.edn) when no file of the jobs and triggers it uses (and what their source names) changed since, tracked or untracked; any engine, js, trigger-default or runner change, or a change to the fixture file, runs it.\n"
       "--day-limit TICKS (default 9000) is the world time at which a held day phase is reset to 1000: well before dusk (12000), when natural hostiles start to spawn.\n"
       "--retry-failed N reruns each failed case (not errors or inconclusive) up to N times after the batch, on the same body; a pass on a retry is reported :flaky (counted apart, never :pass, exit 0, also the live outcome) with the first failure kept under :first-failure; not combinable with --stop-on-fail.\n"
       "--trace-dir DIR writes, for each failed or errored case, DIR/<fixture>__<case>__r<run>-a<attempt>.edn (its body events from the setup on, capped, and the tail of body.log) and names it under :trace in the result; --trace-attempt N (default 0) numbers the attempt in the file name.\n"
       "--check only loads and validates the fixtures (no body, no server): one result per case, exit 1 on a parse error or problem.\n"
       "A job case (no :register, no :wait-full true, only :event expectations pending) ends :idle-grace-s (default 3) s after every job it submitted has ended, failed or parked (\"no job active: ...\", :ended-early-s and :idle-grace-s in the result).\n"
       "Runs world fixtures (default dir engine/fixtures/world) on the reserved plot grid x/z 20000..20640, y 150 (large plots: lanes south of it, to z 22240).\n"
       "--allow-time lets a case that needs night or day set the time (each set appended to --time-log); without it\n"
       "such a case is skipped. A case that depends on the time of day (with --allow-time, every case) holds a time lock shared by phase (day cases together,\n"
       "night cases together; the other phase waits; the first holder sets the time) for its whole run; a manual `time set` takes it too: node tools/time-set.mjs <ticks|day|noon|night|midnight>. A failed case whose day time jumped meanwhile (a foreign `time set`) is INCONCLUSIVE. "
       "Exit code 0 when every run passed, 1 when one failed, 2 on a usage or setup error."))

(defn parse-args [argv]
  (when (and (some #{"--allow-time"} argv) (not (some #{"--time-log"} argv)))
    (throw (js/Error. "--allow-time needs --time-log FILE (every time set is logged there)")))
  (when (and (some #{"--retry-failed"} argv) (some #{"--stop-on-fail"} argv))
    (throw (js/Error. "--retry-failed cannot be combined with --stop-on-fail (the batch ends at the first failure, so nothing is retried)")))
  (loop [[a b & more :as all] (vec argv) opts {:paths [] :repeat 1 :body "ProbeFixture" :world "test" :first-plot 0}]
    (cond
      (empty? all) opts
      (= a "--tag") (recur more (assoc opts :tag b))
      (= a "--match") (recur more (assoc opts :match b))
      (= a "--night-batch") (recur more (assoc opts :night-batch b))
      (= a "--night-barrier") (recur more (assoc opts :night-barrier (js/Number b)))
      (= a "--night-batch-end") (recur more (assoc opts :night-batch-end b))
      (= a "--sleep-rule-restore") (recur (rest all) (assoc opts :sleep-rule-restore :now))
      (= a "--sleep-rule-restore-dead") (recur (rest all) (assoc opts :sleep-rule-restore :dead))
      (= a "--stop-stale-bodies") (recur more (assoc opts :stop-stale-bodies b))
      (= a "--refresh-build") (recur (rest all) (assoc opts :refresh-build true))
      (= a "--night-classes") (recur (rest all) (assoc opts :night-classes true))
      (= a "--match-id") (recur more (update opts :match-ids (fnil conj #{}) b))
      (= a "--repeat") (recur more (assoc opts :repeat (js/Number b)))
      (= a "--body") (recur more (assoc opts :body b))
      (= a "--world") (recur more (assoc opts :world b))
      (= a "--first-plot") (recur more (assoc opts :first-plot (js/Number b)))
      (= a "--time-log") (recur more (assoc opts :time-log b))
      (= a "--phase") (if (#{"day" "night" "night-exclusive"} b)
                        (recur more (assoc opts :phase b))
                        (throw (js/Error. (str "--phase must be day, night or night-exclusive, not " b))))
      (= a "--day-limit") (if (re-matches #"\d+" (str b))
                            (recur more (assoc opts :day-limit (js/Number b)))
                            (throw (js/Error. (str "--day-limit needs a tick count, not " b))))
      (= a "--retry-failed") (if (re-matches #"\d+" (str b))
                               (recur more (assoc opts :retry-failed (js/Number b)))
                               (throw (js/Error. (str "--retry-failed needs a count, not " b))))
      (= a "--changed-since-pass") (recur (rest all) (assoc opts :changed-since-pass true))
      (= a "--card") (recur more (assoc opts :card b))
      (= a "--results") (recur more (assoc opts :results b))
      (= a "--trace-dir") (recur more (assoc opts :trace-dir b))
      (= a "--trace-attempt") (recur more (assoc opts :trace-attempt (js/Number b)))
      (= a "--stop-on-fail") (recur (rest all) (assoc opts :stop-on-fail true))
      (= a "--allow-time") (recur (rest all) (assoc opts :allow-time true))
      (= a "--list") (recur (rest all) (assoc opts :list true))
      (= a "--check") (recur (rest all) (assoc opts :check true))
      (str/starts-with? a "--") (throw (js/Error. (str "unknown option " a)))
      :else (recur (rest all) (update opts :paths conj a)))))

;; ------------------------------------------------------------------ io helpers

(defn repo
  "The repository root: WORLD_TEST_REPO (set by tools/world-test.mjs), else the working directory."
  []
  (or (.. js/process -env -WORLD_TEST_REPO) (js/process.cwd)))
(defn repo-path [& parts] (apply path/join (repo) parts))

(defn sleep [ms] (js/Promise. (fn [resolve] (js/setTimeout resolve ms))))

(defn log! [& parts] (js/console.log (apply str parts)))

(defn rcon!
  "Sends the commands in order on one connection; resolves to their replies."
  [commands]
  (if (empty? commands) (js/Promise.resolve []) (rcon/send-commands! {} (vec commands))))

(defn rcon-step!
  "One :rcon act step: sends text and logs the reply, so a fixture run shows what the server said."
  [{:keys [send log]} text]
  (.then (send [text]) (fn [[reply]] (log (str "rcon: " text " -> " (str/trim (str reply)))))))

(defn exec-file
  "Runs node with args in the repo; resolves to {:code :out}."
  [args]
  (js/Promise. (fn [resolve]
                 (cp/execFile "node" (clj->js args) #js {:cwd (repo) :maxBuffer (* 4 1024 1024)}
                              (fn [err stdout stderr]
                                (resolve {:code (if err (or (.-code err) 1) 0) :out (str stdout stderr)}))))))

(defn fixture-files [paths]
  (let [paths (if (seq paths) paths [(repo-path "engine" "fixtures" "world")])]
    (vec (mapcat (fn [p]
                   (if (.isDirectory (fs/statSync p))
                     (->> (fs/readdirSync p) (filter #(str/ends-with? % ".edn")) sort (map #(path/join p %)))
                     [p]))
                 paths))))

(defn select-phase
  "The cases of time class phase (\"day\", \"night\" or \"night-exclusive\"); all of them when phase is nil. A case with no
  time-bound lock phase (:any, untimed) is day."
  [cases phase]
  (if-not phase
    cases
    (filterv #(= phase (case (lease/time-phase %) :night "night" :night-x "night-exclusive" :weather-x "night-exclusive" "day")) cases)))

(def pass-record-path ".claude/world-test-passes.edn")

(defn git-out
  "Output lines of git args in the repo; nil when git fails."
  [args]
  (try (->> (str/split-lines (str (cp/execFileSync "git" (clj->js args) #js {:cwd (repo) :encoding "utf8" :maxBuffer (* 64 1024 1024)})))
            (remove str/blank?) vec)
       (catch :default _ nil)))

(defn head-rev [] (first (git-out ["rev-parse" "HEAD"])))

(defn changed-since
  "Repo paths changed in the tree since rev (committed, uncommitted, untracked); nil when git cannot say."
  [rev]
  (let [tracked (git-out ["diff" "--name-only" rev]) untracked (git-out ["ls-files" "--others" "--exclude-standard"])]
    (when (and tracked untracked) (into tracked untracked))))

(defn read-pass-record []
  (let [file (repo-path pass-record-path)]
    (try (if (fs/existsSync file) (reader/read-string (fs/readFileSync file "utf8")) {}) (catch :default _ {}))))

(defn source-texts
  "{repo-relative path text} of every job and trigger source."
  []
  (let [walk (fn walk [dir]
               (mapcat (fn [e] (let [p (path/join dir (.-name e))]
                                 (cond (.isDirectory e) (walk p) (str/ends-with? (.-name e) ".cljs") [p] :else [])))
                       (fs/readdirSync (repo-path dir) #js {:withFileTypes true})))]
    (into {} (map (fn [p] [p (fs/readFileSync (repo-path p) "utf8")])) (concat (walk "engine/src/jobs") (walk "engine/src/triggers")))))

(defn fixture-dir-texts
  "{repo-relative path text} of the fixture files under paths."
  [paths]
  (into {} (map (fn [f] [(path/relative (repo) (path/resolve f)) (fs/readFileSync f "utf8")])) (fixture-files paths)))

(defn stem-path [p] (path/basename p ".edn"))

(defn select-changed
  "Cases of the fixture files that are stale since their recorded pass (see world-test.changed)."
  [cases paths]
  (let [stale (into #{} (map stem-path)
                    (chg/stale-stems {:fixtures (fixture-dir-texts paths) :record (read-pass-record) :src (source-texts)
                                      :changed-since changed-since}))
        kept (filterv #(contains? stale (:file %)) cases)]
    (log! "world-test: --changed-since-pass skips " (- (count (distinct (map :file cases))) (count (distinct (map :file kept))))
          " unchanged fixture file(s)")
    kept))

(defn run-state
  "HEAD and the changed paths now; taken at the body build and again after the run."
  []
  (let [rev (head-rev)] {:rev rev :changed (when rev (changed-since rev))}))

(defn record-passes!
  "Records the start revision for every fixture file of all-cases whose every run passed, when HEAD and the changes held since start (the body build's state) and its code is not changed since HEAD. A file that ran and did not pass loses its record."
  [opts all-cases paths results start]
  (let [expected (into {} (map (fn [[k n]] [k (* n (:repeat opts))])) (frequencies (map :file all-cases)))
        passed (chg/passed-stems expected results)
        texts (fixture-dir-texts paths)
        by-stem (group-by stem-path (keys texts))
        ran (set (mapcat by-stem (map :file results)))
        clean (chg/recordable {:passed (set (mapcat by-stem passed)) :fixtures texts :src (source-texts) :start start :end (run-state)})
        record (read-pass-record)
        next (chg/next-record record (:rev start) {:clean clean :ran ran})]
    (when (not= record next)
      (let [file (repo-path pass-record-path)]
        (fs/mkdirSync (path/dirname file) #js {:recursive true})
        (fs/writeFileSync file (pr-str next))))))

(defn load-cases [paths]
  (vec (mapcat #(f/file-cases (fs/readFileSync % "utf8") (path/basename % ".edn")) (fixture-files paths))))

(defn emitted-kinds
  "Every name in the engine's and dashboard's source that an event kind can be (see f/kinds-in-text)."
  []
  (let [walk (fn walk [dir]
               (mapcat (fn [e] (let [p (path/join dir (.-name e))]
                                 (cond (.isDirectory e) (if (= "node_modules" (.-name e)) [] (walk p))
                                       (re-find #"\.(cljs|cljc|clj|js|mjs)$" (.-name e)) [p]
                                       :else [])))
                       (fs/readdirSync (repo-path dir) #js {:withFileTypes true})))]
    (into #{} (mapcat #(f/kinds-in-text (fs/readFileSync (repo-path %) "utf8")))
          (mapcat walk ["engine/src" "engine/js" "dashboard/src"]))))

(defn check-fixtures
  "Loads every fixture file without a server: one {:id :run :status :why} per case (a file that does not parse is one
  failed result named by its stem). An entity UUID literal used by more than one case fails them all."
  [paths]
  (let [loaded (mapv (fn [file]
                       (let [stem (path/basename file ".edn")]
                         (try {:cases (f/file-cases (fs/readFileSync file "utf8") stem)}
                              (catch :default e {:error {:id stem :run 1 :status :fail :why (str file ": " (.-message e))}}))))
                     (fixture-files paths))
        all (mapcat :cases loaded)
        reuse (f/uuid-reuse all)
        emitted (emitted-kinds)
        stale-kinds (fn [c] (when-let [ks (seq (f/unknown-kinds c emitted))] (str "event kind " (str/join ", " (map pr-str ks)) " is emitted by no source")))
        shared-by (fn [c] (for [[u ids] reuse :when (some #{(:id c)} ids)] (str "UUID " u " also in " (str/join ", " (remove #{(:id c)} ids)))))]
    (vec (mapcat (fn [{:keys [cases error]}]
                   (if error
                     [error]
                     (let [repeated (set (keep (fn [[id n]] (when (< 1 n) id)) (frequencies (map :id cases))))]
                       (map (fn [c] (let [ps (cond-> (into (vec (:problems c)) (shared-by c)) (stale-kinds c) (conj (stale-kinds c)) (repeated (:id c)) (conj "case name repeated in the file"))]
                                      (cond-> {:id (:id c) :run 1 :status (if (seq ps) :fail :pass)}
                                        (seq ps) (assoc :why (str/join "; " ps)))))
                            cases))))
                 loaded))))

(defn check-exit-code [results] (if (every? #(= :pass (:status %)) results) 0 1))

;; ------------------------------------------------------------------ the event log

(defn body-dir [{:keys [world body]}] (repo-path "worlds" world "agents" body))
(defn events-file [opts] (path/join (body-dir opts) "engine" "events.edn"))

(defn file-size [file] (if (fs/existsSync file) (.-size (fs/statSync file)) 0))

(defn log-cursor
  "Where the log stands now: {:ino :pos}. The inode lets a reader find the file again after the engine rotates it."
  [file]
  (if (fs/existsSync file)
    (let [st (fs/statSync file)] {:ino (.-ino st) :pos (.-size st)})
    {:ino nil :pos 0}))

(defn read-from
  "The parsed events of file from byte start on; unreadable lines skipped."
  [file start]
  (let [size (file-size file)]
    (if (<= size start)
      []
      (let [fd (fs/openSync file "r")
            buf (js/Buffer.alloc (- size start))]
        (fs/readSync fd buf 0 (- size start) start)
        (fs/closeSync fd)
        (->> (str/split-lines (.toString buf "utf8"))
             (keep #(try (reader/read-string {:default (fn [_ v] v)} %) (catch :default _ nil)))
             (filter map?)
             vec)))))

(defn read-events-from
  "The events logged after cursor, oldest first. When the engine rotated the log since (events.edn.N), the tail of the
  segment that was the active file at cursor, the newer segments, then the active file. When that segment is gone, every
  kept segment (all newer) then the active file."
  [file {:keys [ino pos]}]
  (let [segments (mapv #(str file "." %) (range 3 0 -1))
        cur-ino (when (fs/existsSync file) (.-ino (fs/statSync file)))]
    (if (or (nil? ino) (= ino cur-ino))
      (read-from file pos)
      (let [live (filterv fs/existsSync segments)
            from (first (keep-indexed #(when (= ino (.-ino (fs/statSync %2))) %1) live))]
        (if from
          (vec (concat (read-from (nth live from) pos)
                       (mapcat #(read-from % 0) (subvec live (inc from)))
                       (read-from file 0)))
          (vec (concat (mapcat #(read-from % 0) live) (read-from file 0))))))))

(defn await-since
  "The time an :await counts events from: t0, but a :world.reloaded await after a :cli/:http step (cli-ms, when that step
  began) counts from the step, so a reload from before the tool wrote (the body's start, an earlier edit) cannot satisfy it."
  [pattern t0 cli-ms]
  (if (and cli-ms (= :world.reloaded (:kind pattern))) (max t0 cli-ms) t0))

(defn await-event
  "Polls the log from offset until an event matching pattern with :time-ms >= since-ms comes; resolves to it or nil
  after limit-ms. A pattern with :of-job true matches only events of job-ids (the jobs submitted so far)."
  [opts offset pattern since-ms limit-ms job-ids]
  (let [until (+ (js/Date.now) limit-ms)
        own? (:of-job pattern)
        pattern (dissoc pattern :of-job)]
    (letfn [(poll []
              (let [hit (some #(when (and (>= (:time-ms % 0) since-ms) (x/matches? pattern %) (or (not own?) (x/of-jobs? job-ids %))) %)
                              (read-events-from (events-file opts) offset))]
                (cond
                  hit (js/Promise.resolve hit)
                  (> (js/Date.now) until) (js/Promise.resolve nil)
                  :else (.then (sleep 500) poll))))]
      (poll))))

;; ------------------------------------------------------------------ the body

(defn ensure-body!
  "A probe body's config (created when missing) and its whitelist entry."
  [{:keys [body] :as opts}]
  (let [dir (body-dir opts)
        config (path/join dir "config.json")]
    (when-not (fs/existsSync config)
      (fs/mkdirSync dir #js {:recursive true})
      (fs/writeFileSync config (js/JSON.stringify
                                (clj->js {:username body :harness "claude-code" :auth "offline"
                                          :character {:name body :source "the world-fixture runner's test body"
                                                      :note "temporary test body"}})
                                nil 1)))
    (rcon! [(str "whitelist add " body)])))

(defn source-mtimes
  "mtimes (ms) of every file under the dirs."
  [dirs]
  (letfn [(walk [dir]
            (mapcat (fn [e]
                      (let [p (path/join dir (.-name e))]
                        (cond (.isDirectory e) (walk p)
                              (.isFile e) [(.-mtimeMs (fs/statSync p))]
                              :else [])))
                    (array-seq (fs/readdirSync dir #js {:withFileTypes true}))))]
    (mapcat #(when (fs/existsSync %) (walk %)) dirs)))

(defn refresh-body-build!
  "Rebuilds engine/out/body-release.cjs (tools/compile engine body-release) when it is older than a file under engine/src or engine/js.
  Returns true when the build is current (or was refreshed), false when the compile failed."
  []
  (let [bundle (repo-path "engine" "out" "body-release.cjs")
        built (when (fs/existsSync bundle) (.-mtimeMs (fs/statSync bundle)))]
    (if-not (build/stale? built (source-mtimes [(repo-path "engine" "src") (repo-path "engine" "js")]))
      true
      (let [r (cp/spawnSync (repo-path "tools" "compile") #js ["engine" "body-release"] #js {:encoding "utf8"})]
        (if (zero? (.-status r))
          (do (log! "engine/out/body-release.cjs was stale: rebuilt with tools/compile engine body-release") true)
          (do (log! "engine/out/body-release.cjs is stale and tools/compile engine body-release failed:\n" (.-stdout r) (.-stderr r)) false))))))

(def bodies (atom {}))

(defn running? [{:keys [body]}]
  (str/includes? (or (.-stdout (cp/spawnSync "pgrep" #js ["-af" (str "out/body(-release)?\\.cjs --agent " body " ")] #js {:encoding "utf8"})) "")
                 (str "--agent " body " ")))

(defn run-dir [opts] (path/join (os/tmpdir) (str "world-test-" (:body opts))))

(defn last-plot-file [opts] (path/join (run-dir opts) "last-origin.edn"))

(defn note-last-plot!
  "Records the plot the body is about to be put on, so a later start can clear it first (a run that broke may have left the body there)."
  [opts origin grid]
  (fs/mkdirSync (path/dirname (last-plot-file opts)) #js {:recursive true})
  (fs/writeFileSync (last-plot-file opts) (pr-str {:origin origin :dims (f/dims grid)})))

(defn reset-last-plot!
  "Resolves once the plot the body was last left on is cleared (its traps gone), before the body starts."
  [opts]
  (let [file (last-plot-file opts)
        {:keys [origin dims]} (when (fs/existsSync file)
                                (try (let [v (reader/read-string (fs/readFileSync file "utf8"))] (if (vector? v) {:origin v} v))
                                     (catch :default _ nil)))]
    (if (vector? origin)
      (rcon! (f/reset-plot-commands (let [[sx sz] dims] (if sx (assoc f/default-grid :size-x sx :size-z sz) f/default-grid)) origin))
      (js/Promise.resolve nil))))

(defn finish-run!
  "End of a run: clears the plot the body was left on, then removes the run's temp dir, unless results is nil (the run
  broke) or holds a failed or errored case: then the dir stays with its body.log and the log says where. When the
  clearing fails the dir stays too (it holds the plot to clear on the next start) and the rejection goes on."
  [opts results]
  (.then (reset-last-plot! opts)
         (fn []
           (if (or (nil? results) (some #(#{:fail :error} (:status %)) results))
             (log! "world-test: kept " (run-dir opts) " (body.log) for the post-mortem")
             (fs/rmSync (run-dir opts) #js {:recursive true :force true})))))

(defn body-argv
  "node argv for a body: V8 flags must be on the command line (heap cap: RSS 335 -> ~255 MB). --fresh drops engine.edn
  (the queue and register), unless restore?."
  ([opts scenario] (body-argv opts scenario false))
  ([opts scenario restore?]
   (into-array (concat ["--max-old-space-size=128" "--max-semi-space-size=8" "out/body-release.cjs" "--agent" (:body opts) "--world" (:world opts) "--scenario" scenario]
                       (when-not restore? ["--fresh"])))))

(defn await-online!
  "Polls the server's player list (every 250 ms, up to 10 s) until the body is on it (the start check polls the rest)."
  [opts]
  (let [until (+ (js/Date.now) 10000)]
    (letfn [(poll []
              (.then (rcon! ["list"])
                     (fn [[reply]]
                       (if (or (str/includes? (or reply "") (:body opts)) (> (js/Date.now) until))
                         nil
                         (.then (sleep 250) poll)))))]
      (poll))))

(declare clear-stale-plans!)

(defn clear-body-notes!
  "Deletes the notes file of the body (worlds/<world>/notes/<body>.edn): what an earlier case saw or searched (24 h)
  would change where a later case looks."
  [opts]
  (fs/rmSync (path/join (repo-path "worlds" (:world opts) "notes") (str (:body opts) ".edn")) #js {:force true}))

(defn reset-body-log!
  "Drops the body.log an earlier run kept, so this run's log holds only this run (start-body! appends across restarts)."
  [opts]
  (fs/rmSync (path/join (run-dir opts) "body.log") #js {:force true}))

(defn start-body!
  "Starts the body with a scenario holding register, --fresh; resolves once it logged :system :started. The body's own
  engine/memory.edn is deleted first unless keep-memory? (--fresh only drops engine.edn), then written from
  memory-data when given (the case's :memory seed; the body reads the file as it starts). restore?: starts without
  --fresh, so the body restores its engine.edn."
  [opts register keep-memory? memory-data restore?]
  (when-not keep-memory?
    (clear-stale-plans! opts)
    (clear-body-notes! opts)
    (doseq [name f/clean-start-files
            :let [file (path/join (body-dir opts) "engine" name)]
            :when (fs/existsSync file)]
      (fs/unlinkSync file))
    (when memory-data
      (fs/mkdirSync (path/join (body-dir opts) "engine") #js {:recursive true})
      (fs/writeFileSync (path/join (body-dir opts) "engine" "memory.edn") (pr-str memory-data))))
  (let [dir (run-dir opts)
        scenario (path/join dir "scenario.edn")
        out (path/join dir "body.log")
        since (js/Date.now)
        offset (log-cursor (events-file opts))]
    (fs/mkdirSync dir #js {:recursive true})
    (fs/writeFileSync scenario (pr-str {:register [] :queue []}))
    (let [fd (fs/openSync out "a")
          child (cp/spawn "node" (body-argv opts scenario restore?)
                          #js {:cwd (repo-path "engine") :stdio #js ["ignore" fd fd]})]
      (swap! bodies assoc (:body opts) child)
      (.then (await-event opts offset {:source :system :kind :started} since 90000 #{})
             (fn [ev]
               (when-not ev (throw (js/Error. (str "the body did not start within 90 s (see " out ")"))))
               (await-online! opts))))))

(defn write-body-settings!
  "Writes the case's :settings as the body's settings.edn, or removes the file when it sets none."
  [opts c]
  (let [file (path/join (body-dir opts) "settings.edn")]
    (if-let [text (f/settings-text c)]
      (do (fs/mkdirSync (body-dir opts) #js {:recursive true})
          (fs/writeFileSync file text))
      (fs/rmSync file #js {:force true}))))

(defn stop-body!
  "Stops the body this runner started (its own child, by PID) and waits for it to exit."
  [opts]
  (if-let [child (get @bodies (:body opts))]
    (js/Promise. (fn [resolve]
                   (swap! bodies dissoc (:body opts))
                   (if (some? (.-exitCode child))
                     (resolve nil)
                     (let [timer (js/setTimeout #(do (.kill child "SIGKILL") (resolve nil)) 20000)]
                       (.once child "exit" (fn [] (js/clearTimeout timer) (resolve nil)))
                       (.kill child "SIGTERM")))))
    (js/Promise.resolve nil)))

;; ------------------------------------------------------------------ time

(defn daytime [reply]
  (when-let [[_ n] (re-find #"is at (\d+) tick" reply)] (mod (js/Number n) 24000)))

(defn night? [t] (and t (<= 13000 t 23000)))

(def time-jump-tolerance-ticks 200)

(def weather-commands
  "Vanilla has no weather query: two inline predicates, thundering first."
  ["execute if predicate {condition:\"minecraft:weather_check\",thundering:true}"
   "execute if predicate {condition:\"minecraft:weather_check\",raining:true}"])

(defn weather-of
  "The weather (:clear :rain :thunder) from the replies to weather-commands; nil when a reply is not a test result."
  [[thunder rain]]
  (let [passed? #(re-find #"Test passed" (str %))
        failed? #(re-find #"Test failed" (str %))]
    (cond (passed? thunder) :thunder
          (passed? rain) :rain
          (and (failed? thunder) (failed? rain)) :clear)))

(defn read-weather!
  "Resolves to the weather now (read only), nil when the server will not say."
  [send]
  (-> (send weather-commands) (.then weather-of) (.catch (fn [_] nil))))

(defn tick-rate-of
  "The target tick rate in a `tick query` reply (20 when it has none)."
  [reply]
  (if-let [[_ n] (re-find #"Target tick rate: ([\d.]+)" (or reply ""))] (js/Number n) 20))

(defn in-phase?
  "Whether day time t is in the phase the case runs in (its :time; :any and a case with none accept day or night)."
  [c t]
  (case (:time c)
    :night (night? t)
    :night-exclusive (night? t)
    :any true
    (not (night? t))))

(defn time-disturbed?
  "Whether the day time was disturbed between two readings (ticks, nil when unread): it jumped forward by more than the
  elapsed real time allows at the server's tick rate (20 unless given) plus a margin, or backward, AND the world left
  the case's phase (day/night changed between the readings, or the later one is outside the case's :time). A jump that
  keeps the phase (the runner's own refresh) is none. False for a case with a :time-set step, and when the body woke
  from sleep (all online players asleep skips the night)."
  ([c t0 t1 elapsed-ms woke?] (time-disturbed? c t0 t1 elapsed-ms woke? 20))
  ([c t0 t1 elapsed-ms woke? tick-rate]
   (boolean
    (and t0 t1 (not woke?)
         (not-any? #(= :time-set (first %)) (:act c))
         (let [moved (mod (- t1 t0) 24000)]
           (and (> moved (+ (* elapsed-ms (/ tick-rate 1000)) time-jump-tolerance-ticks))
                (< moved (- 24000 time-jump-tolerance-ticks))
                (or (not= (boolean (night? t0)) (boolean (night? t1)))
                    (not (in-phase? c t1)))))))))

(defn woke? [events] (boolean (some #(and (= :body (:source %)) (= :woke (:kind %))) events)))

(defn mark-time-disturbed
  "A failed result becomes :inconclusive when the time was disturbed during the case."
  [r disturbed?]
  (if (and disturbed? (= :fail (:status r)))
    (assoc r :status :inconclusive :why "world time jumped during the case (someone ran `time set`)")
    r))

(defn night-overran?
  "Whether a night batch case ran into dawn: opts has a --night-batch, the case wants the night (and sets no time itself)
  and the day time t1 is outside it."
  [opts c t1]
  (boolean (and (:night-batch opts) t1 (= :night-exclusive (:time c))
                (not-any? #(= :time-set (first %)) (:act c))
                (not (night? t1)))))

(defn mark-night-overran
  "A failed result becomes :inconclusive when its night batch case ended after dawn (the pool reruns it)."
  [r overran?]
  (if (and overran? (= :fail (:status r)))
    (assoc r :status :inconclusive :overran true :first-why (:why r) :why "the night ended during the batch case (dawn); rerun it")
    r))

;; ------------------------------------------------------------------ plot leases (parallel runners)

(defn pause-sync [ms] (js/Atomics.wait (js/Int32Array. (js/SharedArrayBuffer. 4)) 0 0 ms))

(def lease-dir (path/join (os/tmpdir) "world-test-plot-leases"))

(defn lease-file [i] (path/join lease-dir (str "plot-" i ".lease")))

(defn pid-alive? [pid]
  (try (.kill js/process pid 0) true
       (catch :default e (= "EPERM" (.-code e)))))

(defn proc-start
  "The start time (clock ticks since boot, /proc/PID/stat field 22) of a process, as a string; nil when it is gone or unreadable.
  A reused PID has another one."
  [pid]
  (try (let [stat (str (fs/readFileSync (str "/proc/" pid "/stat") "utf8"))]
         (nth (str/split (subs stat (inc (.lastIndexOf stat ")"))) #"\s+") 20 nil))
       (catch :default _ nil)))

(defn owner-pid
  "The pool that runs this process (WORLD_TEST_POOL_PID), else the parent process."
  []
  (let [n (js/parseInt (or (.. js/process -env -WORLD_TEST_POOL_PID) "") 10)]
    (if (js/isNaN n) (.-ppid js/process) n)))

(defn read-pid-file
  "The PID in file; :empty when it exists without a PID (a writer between create and write, or a crashed one); nil when absent."
  [file]
  (try (let [n (js/parseInt (str/trim (fs/readFileSync file "utf8")) 10)] (if (js/isNaN n) :empty n))
       (catch :default _ nil)))

(defn create-file-exclusive!
  "Creates file holding `content` only when it does not exist, returning true when this call made it. The content is
  written to a temp file first and hard-linked in, so the file is never visible empty (a reclaimer of a crashed empty
  file can never take a fresh creator's)."
  [file content]
  (let [tmp (str file "." (.-pid js/process) ".tmp")]
    (fs/writeFileSync tmp content)
    (try (fs/linkSync tmp file) true
         (catch :default e (if (= "EEXIST" (.-code e)) false (throw e)))
         (finally (fs/rmSync tmp #js {:force true})))))

(defn read-holder [i] (read-pid-file (lease-file i)))

(def unlink-if-script
  "sh script ($1 file, $2 expected content): removes the file only while it holds exactly that."
  "cur=$(cat \"$1\" 2>/dev/null) || exit 1; [ \"$cur\" = \"$2\" ] && rm -f \"$1\"")

(defn unlink-if!
  "Removes file only while it still holds `content` (\"\" = empty). The check and the removal run under a flock
  guard beside the file that every reclaim and release takes, so a fresh lock taken meanwhile is never removed
  (a creator needs the file absent, and only a guarded removal makes it so)."
  [file content]
  (zero? (.-status (cp/spawnSync "flock" #js [(str file ".guard") "sh" "-c" unlink-if-script "sh" file content]))))

(defn reclaim-file!
  "Removes file only while it still holds `holder` (a dead PID or :empty)."
  [file holder]
  (unlink-if! file (if (= :empty holder) "" (str holder))))

(defn acquire-plot!
  "Leases the first free plot index from `first` (exclusive lease file holding this PID; leases of dead PIDs are
  reclaimed), so runners started together never share a plot."
  [first total]
  (fs/mkdirSync lease-dir #js {:recursive true})
  (lease/acquire
   {:create! (fn [i pid] (create-file-exclusive! (lease-file i) (str pid)))
    :holder read-holder
    :reclaim! (fn [i holder] (reclaim-file! (lease-file i) holder))
    :alive? pid-alive?
    :settle! #(pause-sync 5)}
   {:pid (.-pid js/process) :first first :total total}))

(defn release-plot! [i]
  (unlink-if! (lease-file i) (str (.-pid js/process))))

;; ------------------------------------------------------------------ time lock

(def time-lock-dir (path/join (os/tmpdir) "mc-time-lock"))
(def time-guard-file (path/join (os/tmpdir) "mc-time-lock.guard"))

(defn with-guard
  "Runs thunk holding a short-lived exclusive guard file (a dead holder's guard is reclaimed)."
  [thunk]
  (let [pid (.-pid js/process)]
    (loop [tries 0]
      (let [made (create-file-exclusive! time-guard-file (str pid))]
        (if made
          (try (thunk) (finally (unlink-if! time-guard-file (str pid))))
          (let [h (read-pid-file time-guard-file)
                age-ms (try (- (js/Date.now) (.-mtimeMs (fs/statSync time-guard-file))) (catch :default _ 0))]
            (when (or (and (number? h) (not (pid-alive? h))) (and (= :empty h) (> age-ms 2000)))
              (reclaim-file! time-guard-file h))
            (pause-sync 5)
            (recur (inc tries))))))))

(defn time-entry-file [pid] (path/join time-lock-dir (str pid)))

(defn time-lock-dir-ops []
  (fs/mkdirSync time-lock-dir #js {:recursive true})
  {:guard with-guard
   :entries (fn [] (into {} (keep (fn [f]
                                    (let [pid (js/parseInt f 10)]
                                      (when-let [e (try (reader/read-string (fs/readFileSync (path/join time-lock-dir f) "utf8"))
                                                        (catch :default _ nil))]
                                        (when-not (js/isNaN pid) [pid e]))))
                                  (fs/readdirSync time-lock-dir))))
   :put! (fn [pid e] (fs/writeFileSync (time-entry-file pid) (pr-str e)))
   :remove! (fn [pid] (try (fs/unlinkSync (time-entry-file pid)) (catch :default _ nil)))
   :alive? pid-alive?})

(defn release-time-lock! []
  (try (fs/unlinkSync (time-entry-file (.-pid js/process))) (catch :default _ nil)))

(defn lock-phase
  "The time-lock phase of case rc: its own when it depends on the time of day; else :day when the run may set the time
  (it then waits out night, and the first holder sets day), else :any (joins the current phase)."
  [opts rc]
  (or (lease/time-phase rc) (if (:allow-time opts) :day :any)))

(defn starts-night?
  "Whether case rc starts in a night (a :night-x case that only sets dusk itself starts at day)."
  [rc]
  (or (contains? #{:night :night-exclusive} (:time rc)) (= :night (lease/time-phase rc))))

(defn lock-batch
  "The batch id a case locks with: only an exclusive night case (:night-x) shares, with the cases of the same --night-batch."
  [opts rc]
  (when (= :night-x (lease/time-phase rc)) (:night-batch opts)))

(defn batch-start-ticks
  "The time the first case of a night batch sets: night, or morning for a batch that starts at day (and sets dusk itself)."
  [rc]
  (if (starts-night? rc) 14000 1000))

(defn joiner-refresh?
  "Whether a joiner of phase re-sets a drifted time of day: not for :any, without --allow-time, for a case that sets the time itself,
  nor in a night batch (the pool sizes a batch to one night, so a reset would only break the cases running)."
  [opts phase rc]
  (not (or (#{:any :weather-x} phase) (not (:allow-time opts)) (some #(= :time-set (first %)) (:act rc))
           (and (= :night-x phase) (:night-batch opts)))))

(defn night-class-line
  "The --list --night-classes line of an exclusive night case: its id and lease/night-class (a barrier case adds :ticks,
  :day-ticks when it starts at day: only cases that start alike share a barrier)."
  [c]
  (str (:id c) "  " (name (lease/night-class c))
       (when-let [t (lease/barrier-ticks c)] (str ":" (when-not (starts-night? c) "day-") t))))

;; Machine-wide and outside every checkout: sweeps run in throwaway worktrees, and /tmp does not survive a reboot.
(def machine-state-dir (path/join (os/homedir) ".local" "state" "minecraft-claude"))

(def night-batch-marker-dir (path/join machine-state-dir "night-batches"))

(defn night-batch-set?
  "Whether a case of the batch already put the night in place (the marker lives until `--night-batch-end`)."
  [batch & [dir]]
  (fs/existsSync (path/join (or dir night-batch-marker-dir) batch)))

(declare local-now)

(def sleep-rule "players_sleeping_percentage")

(defn parse-gamerule
  "The reply of `gamerule NAME` -> its number, or nil."
  [reply]
  (some-> (re-find #"(?:set to|is):?\s*(-?\d+)" (or reply "")) second js/parseInt))

(defn log-command!
  "Appends a line for a world change (time, weather, gamerule) to the --time-log file, if any."
  [opts what why]
  (when-let [file (:time-log opts)]
    (fs/appendFileSync file (str (local-now) " " what " by world-test " (:body opts) " for card " (or (:card opts) "world-fixtures") " (" why ")\n"))))

(defn set-sleep-rule!
  [send opts value why]
  (.then (send [(str "gamerule " sleep-rule " " value)])
         (fn [_] (log-command! opts (str "gamerule " sleep-rule " " value) why))))

;; The server value to restore lives in the machine-wide state dir, written BEFORE the set: a killed run leaves it for the next one.
(defn sleep-state-file [opts]
  (path/join (or (:sleep-state-dir opts) machine-state-dir) "world-test-sleep-rule.edn"))

(defn read-sleep-state
  "{:old n :batch id :pid p} of a recorded set, or nil (none, or unreadable)."
  [opts]
  (let [file (sleep-state-file opts)]
    (when (fs/existsSync file)
      (try (reader/read-string (str (fs/readFileSync file "utf8"))) (catch :default _ nil)))))

(defn restore-sleep-rule!
  "Restores the recorded players_sleeping_percentage (logged) and removes the state file; a recorded 101 (our own leftover) restores the vanilla 100.
  :only-dead? restores only when the recording pool is gone (same PID and start time), not a batch still running;
  :batch only a state recorded for that batch; :pool only a state recorded by that pool PID."
  [send opts why & [{:keys [only-dead? batch pool]}]]
  (let [{:keys [old pid start] :as st} (read-sleep-state opts)
        file (sleep-state-file opts)]
    (cond
      (nil? st) (do (fs/rmSync file #js {:force true}) (js/Promise.resolve nil))
      (and batch (not= batch (:batch st))) (js/Promise.resolve nil)
      (and pool (not= pool pid)) (js/Promise.resolve nil)
      (and only-dead? pid (pid-alive? pid) (or (nil? start) (= start (proc-start pid)))) (js/Promise.resolve nil)
      :else (.then (if old (set-sleep-rule! send (update opts :time-log #(or % (:time-log st))) (if (= 101 old) 100 old) why) (js/Promise.resolve nil))
                   (fn [_] (fs/rmSync file #js {:force true}) old)))))

(defn reset-stray-sleep-rule!
  "A 101 on the server that no recorded batch owns (a killed run, an old bug) goes back to the vanilla 100, logged.
  Only with time changes allowed; any other value is left alone."
  [send opts why]
  (if (or (not (:allow-time opts)) (read-sleep-state opts))
    (js/Promise.resolve nil)
    (.then (send [(str "gamerule " sleep-rule)])
           (fn [[reply]]
             (if (= 101 (parse-gamerule reply))
               (set-sleep-rule! send opts 100 why)
               nil)))))

(defn claim-night-batch!
  "Atomically writes the batch marker; true for the one starter that wins it."
  [batch & [dir]]
  (let [dir (or dir night-batch-marker-dir)]
    (fs/mkdirSync dir #js {:recursive true})
    (try (fs/writeFileSync (path/join dir batch) (pr-str {:at (js/Date.now)}) #js {:flag "wx"}) true
         (catch :default _ false))))

(defn start-sleep-rule!
  "First setter of a night batch: claims the batch marker (a second starter gets nil and sets nothing). Unless the batch needs
  the vanilla night skip (:sleep-skip) or time changes are not allowed, reads players_sleeping_percentage; only when that read
  parses (101 counts as the vanilla 100) it records the old value in the state file, then sets 101 (sleepers never skip the night early).
  Resolves to the old value (nil when nothing was changed)."
  [send opts rc batch dir why]
  (cond
    (not (claim-night-batch! batch dir)) (js/Promise.resolve nil)
    (or (not (:allow-time opts)) (= :skip (lease/night-class rc))) (js/Promise.resolve nil)
    (read-sleep-state opts) (js/Promise.resolve nil)
    :else (.then (send [(str "gamerule " sleep-rule)])
                 (fn [[reply]]
                   (let [old (some-> (parse-gamerule reply) (#(if (= 101 %) 100 %)))]
                     (when (nil? old)
                       (log-command! opts (str "gamerule " sleep-rule " left alone (read " (pr-str reply) ")") why))
                     (if (nil? old)
                       nil
                       (do (fs/mkdirSync (path/dirname (sleep-state-file opts)) #js {:recursive true})
                           (fs/writeFileSync (sleep-state-file opts) (pr-str {:old old :batch batch :pid (or (:owner-pid opts) (owner-pid)) :start (proc-start (or (:owner-pid opts) (owner-pid))) :time-log (:time-log opts) :at (js/Date.now)}))
                           (.then (set-sleep-rule! send opts 101 why) (fn [_] old)))))))))

(defn end-night-batch!
  "Ends a night batch: restores the recorded gamerule (logged), removes the marker and the barrier files."
  [send opts batch dir]
  (let [dir (or dir night-batch-marker-dir)]
    (fs/rmSync (path/join dir (str batch ".barrier")) #js {:recursive true :force true})
    (.then (restore-sleep-rule! send opts (str "night batch " batch " ended") {:batch batch})
           (fn [_] (fs/rmSync (path/join dir batch) #js {:force true}) true))))

(defn barrier-open?
  "states: {pid :arrived|:gone} of a barrier batch. Open when n bodies arrived or left."
  [states n]
  (>= (count states) n))

(def barrier-timeout-ms
  "How long a body waits at the barrier for the others (they may be failing or slow) before it sets the time itself."
  300000)

(defn- barrier-dir [dir batch] (path/join (or dir night-batch-marker-dir) (str batch ".barrier")))

(defn- barrier-write! [dir batch pid state]
  (fs/mkdirSync (barrier-dir dir batch) #js {:recursive true})
  (fs/writeFileSync (path/join (barrier-dir dir batch) (str pid)) (name state)))

(defn barrier-leave!
  "A body that ends without reaching the barrier counts as arrived, so the others do not wait for it."
  [dir batch pid]
  (let [f (path/join (barrier-dir dir batch) (str pid))]
    (when-not (fs/existsSync f) (barrier-write! dir batch pid :gone))))

(defn- barrier-states [dir batch]
  (let [d (barrier-dir dir batch)]
    (into {} (comp (remove #{"set" "done"}) (map (fn [f] [f (keyword (str/trim (str (fs/readFileSync (path/join d f) "utf8"))))])))
          (if (fs/existsSync d) (array-seq (fs/readdirSync d)) []))))

(defn barrier-wait!
  "Waits until n bodies of batch reached the barrier (or left it), then exactly one of them runs do-set! (-> promise) and the rest
  wait for it to finish. A wait past timeout-ms ends the waiting (note is called): one body still wins the set flag and runs do-set!,
  the others wait for its done flag for up to timeout-ms more, never setting twice (a stuck body must not hang the batch).
  env: {:dir :batch :n :pid :sleep :timeout-ms :note}."
  [{:keys [dir batch n pid sleep timeout-ms note]} do-set!]
  (barrier-write! dir batch pid :arrived)
  (let [until (+ (js/Date.now) timeout-ms)
        d (barrier-dir dir batch)
        flag (fn [name] (path/join d name))]
    (letfn [(wait-done []
              (cond
                (fs/existsSync (flag "done")) (js/Promise.resolve true)
                (> (js/Date.now) (+ until timeout-ms)) (do (when note (note "the barrier setter never finished; going on without waiting")) (js/Promise.resolve true))
                :else (.then (sleep 200) wait-done)))
            (wait-open []
              (when (and note (> (js/Date.now) until) (not (fs/existsSync (flag "set"))))
                (note (str "barrier timed out after " timeout-ms " ms with " (count (barrier-states dir batch)) " of " n " bodies; setting the time anyway")))
              (cond
                (or (barrier-open? (barrier-states dir batch) n) (> (js/Date.now) until))
                (if (try (fs/writeFileSync (flag "set") (str pid) #js {:flag "wx"}) true (catch :default _ false))
                  (.then (do-set!) (fn [r] (fs/writeFileSync (flag "done") "") r))
                  (wait-done))
                :else (.then (sleep 200) wait-open)))]
      (wait-open))))

(defn acquire-shared!
  "Resolves to {:first? bool} when this process holds the time lock for phase (:day, :night or :any), logging who it waits
  for (once per holder set). Holders of the same phase share it; :any joins the phase that holds, else the world's.
  env: {:ops lock dir ops, :read-phase (-> promise of :day or :night), :sleep, :log}."
  [{:keys [ops read-phase sleep log batch]} phase what]
  (let [pid (.-pid js/process)
        seq (js/Date.now)]
    (letfn [(attempt [told]
              (.then (if (= :any phase) (read-phase) (js/Promise.resolve :day))
                     (fn [wp]
                       (let [r (lease/try-share ops pid phase seq wp batch)]
                         (if (:held r)
                           {:first? (:first? r)}
                           (do (when-not (= told (:waiting-on r))
                                 (log "waiting for time lock (" (name phase) ") held by " (str/join "," (:waiting-on r)) " (" what ")"))
                               (.then (sleep 2000) #(attempt (:waiting-on r)))))))))]
      (attempt nil))))

(defn acquire-time-lock!
  [phase what & [batch]]
  (acquire-shared! {:ops (time-lock-dir-ops) :batch batch
                    :read-phase #(.then (rcon! ["time query day"]) (fn [[reply]] (if (night? (daytime (or reply ""))) :night :day)))
                    :sleep sleep :log log!}
                   phase what))

(defn with-time-lock!
  "Runs thunk (a promise-returning fn) holding the time lock for phase; always releases it."
  [phase what thunk]
  (.then (acquire-time-lock! phase what)
         (fn [_] (.finally (thunk) release-time-lock!))))

(defn confirm-phase!
  "Resolves once the world reports phase (:day or :night), polling `time query` a few times."
  [phase]
  (letfn [(poll [n]
            (.then (rcon! ["time query day"])
                   (fn [[reply]]
                     (if (or (zero? n) (= phase (if (night? (daytime (or reply ""))) :night :day)))
                       true
                       (.then (sleep 300) #(poll (dec n)))))))]
    (poll 10)))

(defn mark-phase-set!
  "Called by the first holder once its phase is in place; joiners wait for it."
  []
  (lease/mark-set (time-lock-dir-ops) (.-pid js/process)))

(defn time-set-main
  "argv: ticks or day|noon|night|midnight -> promise of the exit code; one `time set` under the time lock."
  [argv]
  (let [t (first (array-seq argv))]
    (if-not (and t (re-matches #"day|noon|night|midnight|\d+" t))
      (do (js/console.error "usage: node tools/time-set.mjs <ticks|day|noon|night|midnight>") (js/Promise.resolve 2))
      (.then (restore-sleep-rule! rcon! {:body "time-set" :card "time-set"} "leftover of a crashed run" {:only-dead? true})
             (fn [_] (with-time-lock! (lease/phase-of-ticks (case t "day" 1000 "noon" 6000 "night" 14000 "midnight" 18000 (js/parseInt t 10)))
        (str "time set " t)
        #(.then (rcon! [(str "time set " t)]) (fn [[reply]] (mark-phase-set!) (log! reply) 0))))))))

(defn local-now
  "Now as local ISO with the UTC offset, the shared time log's format."
  []
  (let [d (js/Date.)]
    (lease/local-iso [(.getFullYear d) (inc (.getMonth d)) (.getDate d) (.getHours d) (.getMinutes d) (.getSeconds d)
                      (.getMilliseconds d)]
                     (- (.getTimezoneOffset d)))))

(defn set-time!
  "Sets the time when allowed (and logs it); resolves to true, or false when not allowed."
  [opts ticks why]
  (if-not (:allow-time opts)
    (js/Promise.resolve false)
    (.then (rcon! [(str "time set " ticks)])
           (fn [_]
             (when-let [file (:time-log opts)]
               (fs/appendFileSync file (str (local-now) " time set " ticks " by world-test " (:body opts)
                                            " for card " (or (:card opts) "world-fixtures") " (" why ")\n")))
             true))))

(defn weather-wanted
  "The weather case rc runs under: its own :weather, else clear."
  [rc]
  (or (:weather rc) :clear))

(defn apply-weather!
  "Sets the weather when allowed (and logs it like a time set); resolves to true, or false when not allowed. send: rcon fn."
  [send opts weather why]
  (if-not (:allow-time opts)
    (js/Promise.resolve false)
    (.then (send [(str "weather " (name weather))])
           (fn [_]
             (when-let [file (:time-log opts)]
               (fs/appendFileSync file (str (local-now) " weather " (name weather) " by world-test " (:body opts)
                                            " for card " (or (:card opts) "world-fixtures") " (" why ")\n")))
             true))))

(defn set-weather! [opts weather why] (apply-weather! rcon! opts weather why))

(defn time-ok!
  "Whether the server's time suits case c, setting it first when allowed."
  [opts c]
  (if (= :any (:time c))
    (js/Promise.resolve true)
    (.then (rcon! ["time query day"])
           (fn [[reply]]
             (let [want-night (boolean (#{:night :night-exclusive} (:time c)))]
               (if (= want-night (boolean (night? (daytime reply))))
                 true
                 (set-time! opts (if want-night 14000 1000) (str (:id c) " needs " (name (:time c))))))))))

;; ------------------------------------------------------------------ one run

(defn plan-prefix [opts] (str "test-" (str/lower-case (:body opts)) "-"))
(defn plans-dir [opts] (repo-path "worlds" (:world opts) "plans"))

(defn clear-stale-plans!
  "Deletes the plan files this body's earlier cases wrote (test-<body>-*.edn, or <name>-<shared tag>.edn added by a CLI
  step) and a killed or failed run left behind, so a case sees only the plans it writes itself."
  [opts]
  (when (fs/existsSync (plans-dir opts))
    (doseq [name (array-seq (fs/readdirSync (plans-dir opts)))
            :when (or (and (str/starts-with? name (plan-prefix opts)) (str/ends-with? name ".edn"))
                      (str/ends-with? name (str "-" (f/shared-tag (:body opts)) ".edn")))]
      (fs/rmSync (path/join (plans-dir opts) name) #js {:force true}))))

(defn write-plans! [opts c]
  (doall (for [plan (:plans c)]
           (let [file (path/join (plans-dir opts) (str (plan-prefix opts) (:id plan) ".edn"))]
             (fs/writeFileSync file (f/plan-file-text (f/resolve-body-refs plan (:body opts)) (plan-prefix opts)))
             file))))

(defn world-file [opts name] (repo-path "worlds" (:world opts) name))

(def shared-settle-ms
  "How long seed-shared! waits for the body to re-read an edited file (it stats them lazily, at most every 3 s, and only
  while it ticks); a body that has not by then fails the case."
  8000)

(defn await-reload!
  "Polls the body's log from cursor until every file in names (file names, e.g. \"zones.edn\") appears in a :world.reloaded
  event newer than since-ms; resolves true then, false after limit-ms."
  [opts cursor since-ms names limit-ms]
  (let [until (+ (js/Date.now) limit-ms)
        reloaded (fn []
                   (->> (read-events-from (events-file opts) cursor)
                        (filter #(and (= :world.reloaded (:kind %)) (>= (:time-ms % 0) since-ms)))
                        (mapcat #(get-in % [:data :files]))))
        done? (fn [] (let [files (reloaded)] (every? (fn [n] (some #(= n (path/basename %)) files)) names)))]
    (letfn [(poll []
              (cond
                (done?) (js/Promise.resolve true)
                (> (js/Date.now) until) (js/Promise.resolve false)
                :else (.then (sleep 250) poll)))]
      (poll))))

(defn write-plans-settled!
  "Writes the case's plans, then resolves to their files once the body logged world.reloaded for every one; throws after
  shared-settle-ms without. A body that just started may have read the files silently at its first load, so after half
  the time the plans are written again (a new stamp) and the wait goes on."
  [opts c]
  (if (empty? (:plans c))
    (js/Promise.resolve [])
    (let [cursor (log-cursor (events-file opts))
          since (js/Date.now)
          limit (:shared-settle-ms opts shared-settle-ms)
          files (write-plans! opts c)
          names (mapv path/basename files)]
      (-> (await-reload! opts cursor since names (quot limit 2))
          (.then (fn [ok] (or ok (do (write-plans! opts c) (await-reload! opts cursor since names limit)))))
          (.then (fn [ok]
                   (when-not ok
                     (throw (js/Error. (str "the body logged no world.reloaded for the plans within " limit " ms"))))
                   files))))))

(defn with-shared-files!
  "Returns a thunk that runs thunk (-> promise) while holding the agent tools' own lock (<file>.lock) on each of files
  in turn, so an edit of zones.edn or places.json cannot lose a tool's write or be lost to it."
  [files thunk]
  (reduce (fn [run file] #(wd/with-file-lock file run {:timeout-ms 15000})) thunk (reverse files)))

(defn edit-shared-file!
  "Rewrites file with (f text) atomically (tmp file, rename); a missing file reads as empty text."
  [file f]
  (let [text (if (fs/existsSync file) (fs/readFileSync file "utf8") "")
        out (f text)]
    (when (not= out text)
      (fs/writeFileSync (str file ".wt-tmp") out)
      (fs/renameSync (str file ".wt-tmp") file))))

(defn drop-shared!
  "Removes the body's tagged zones and markers (the case's, or an earlier run's leftovers)."
  [opts]
  (let [tag (f/shared-tag (:body opts))]
    ((with-shared-files! [(world-file opts "zones.edn") (world-file opts "places.json")]
       (fn []
         (edit-shared-file! (world-file opts "zones.edn") #(f/zones-without % tag))
         (when (fs/existsSync (world-file opts "places.json"))
           (edit-shared-file! (world-file opts "places.json") #(f/markers-without % tag))))))))

(defn run-cleanup!
  "Runs the thunks (-> promise or nil) one after another; one that fails does not stop the rest."
  [steps]
  (reduce (fn [p step] (.then p #(-> (js/Promise.resolve) (.then step) (.catch (fn [_] nil))))) (js/Promise.resolve) steps))

(defn abs-entries [origin entries ks]
  (mapv (fn [e] (reduce #(update %1 %2 (partial f/abs-pos origin)) e (filter #(contains? e %) ks))) entries))

(defn seed-shared!
  "Adds the case's :zones and :places (plot-relative in c, resolved against origin) to the world's zones.edn and
  places.json, tagged with the body; resolves once the body logged world.reloaded for them, throws after shared-settle-ms
  without one. Nothing to do when the case has neither. Only a reload logged after the lock is held counts: one logged
  while waiting for the lock (a fresh body's first load) predates the edit."
  [opts origin c]
  (if (and (empty? (:zones c)) (empty? (:places c)))
    (js/Promise.resolve nil)
    (let [zones (f/zone-entries (:body opts) (abs-entries origin (:zones c) [:min :max]))
          markers (f/marker-entries (:body opts) (f/resolve-body-refs (abs-entries origin (:places c) [:pos]) (:body opts)))]
      (let [cursor (log-cursor (events-file opts))]
        (-> ((with-shared-files! [(world-file opts "zones.edn") (world-file opts "places.json")]
               (fn []
                 (let [since (js/Date.now)]
                   (when (seq zones) (edit-shared-file! (world-file opts "zones.edn") #(f/zones-with % zones)))
                   (when (seq markers) (edit-shared-file! (world-file opts "places.json") #(f/markers-with % markers)))
                   since))))
            (.then #(await-reload! opts cursor % (cond-> [] (seq zones) (conj "zones.edn") (seq markers) (conj "places.json"))
                                   (:shared-settle-ms opts shared-settle-ms)))
            (.then #(when-not %
                      (throw (js/Error. (str "the body logged no world.reloaded for the seeded zones/places within "
                                             (:shared-settle-ms opts shared-settle-ms) " ms"))))))))))

(defn submit-job!
  "Submits spec with tools/jobs.mjs under one request id; resolves to the job id or throws with the tool's answer. A
  {:reason :timeout} answer (busy body) repeats the same request id (the body returns the first result) every second, up
  to :submit-retries (default 8) times."
  [opts spec flags]
  (let [exec (:exec opts exec-file)
        nap (:sleep opts sleep)
        argv (into ["engine/tools/jobs.mjs" (:body opts) "--world" (:world opts) "submit" (pr-str spec)
                    "--request-id" (str "wt-" (random-uuid))]
                   flags)]
    (letfn [(go [left]
              (.then (exec argv)
                     (fn [{:keys [out]}]
                       (if-let [[_ id] (re-find #":id \"(j\d+)\"" out)]
                         id
                         (if (and (pos? left) (re-find #":reason :?\"?timeout" out))
                           (.then (nap 1000) #(go (dec left)))
                           (throw (js/Error. (str "submit refused: " (str/trim out)))))))))]
      (go (:submit-retries opts 8)))))

(defn entity-id!
  "Resolves to the numeric id of the nearest mob of type the body has seen (entities.mjs), polling up to 10 s; throws when none."
  [opts type]
  (let [until (+ (js/Date.now) 10000)]
    (letfn [(poll []
              (.then (exec-file ["engine/tools/entities.mjs" (:body opts) "--world" (:world opts) "--type" type "--limit" "1"])
                     (fn [{:keys [out]}]
                       (let [id (some-> (try (f/read-edn out) (catch :default _ nil)) :items first :id)]
                         (cond
                           id id
                           (> (js/Date.now) until) (throw (js/Error. (str "$entity-id:" type ": the body sees no " type)))
                           :else (.then (sleep 500) poll))))))]
      (poll))))

(defn resolve-entity-ids!
  "Resolves to spec with each \"$entity-id:<type>\" string replaced by that mob's numeric id."
  [opts spec]
  (.then (reduce (fn [p type] (.then p (fn [ids] (.then (entity-id! opts type) #(assoc ids type %)))))
                 (js/Promise.resolve {})
                 (f/entity-id-types spec))
         #(f/resolve-entity-refs spec %)))

(defn rcon-until!
  "Sends cmd every every-s seconds until an event matching pattern (since t0) is logged or limit-s passes; resolves to true
  when the event came. For shoves that must outlast a job whose pace is not fixed."
  [opts origin box offset t0 cmd {:keys [until every-s limit-s]}]
  (let [stop (+ (js/Date.now) (* 1000 limit-s))
        text (f/substitute cmd (:body opts) origin box)]
    (letfn [(tick []
              (if (some #(and (>= (:time-ms % 0) t0) (x/matches? until %)) (read-events-from (events-file opts) offset))
                (js/Promise.resolve true)
                (if (> (js/Date.now) stop)
                  (js/Promise.resolve false)
                  (.then (rcon! [text]) #(.then (sleep (* 1000 every-s)) tick)))))]
      (tick))))

(declare put-register!)

(defn restart-body!
  "Stops this runner's body and starts it again keeping engine/memory.edn, seen.bin, the world position and the queue
  and register of engine.edn (a restart as a player's is)."
  [opts]
  (-> (stop-body! opts)
      (.then #(start-body! opts nil true nil true))))

(defn settling-reply?
  "True when a tool's answer is the lease's refusal of a body that has just started: {:ok false :reason \"settling\"}."
  [out]
  (boolean (re-find #"reason\W+settling" (str out))))

(defn busy-reply?
  "True when a tool's answer says the body cannot answer yet: settling, or {:reason :timeout} (a busy body)."
  [out]
  (boolean (or (settling-reply? out) (re-find #":reason :?\"?timeout" (str out)))))

(defn exec-settled
  "Runs argv with exec (-> promise of {:code :out}) again every every-ms, up to limit-ms, while the answer is busy-reply?.
  Resolves to the last result, unchanged."
  [{:keys [exec sleep limit-ms every-ms] :or {exec exec-file sleep sleep limit-ms 10000 every-ms 500}} argv]
  (letfn [(go [waited]
            (.then (exec argv)
                   (fn [res]
                     (if (and (busy-reply? (:out res)) (< waited limit-ms))
                       (.then (sleep every-ms) #(go (+ waited every-ms)))
                       res))))]
    (go 0)))

(defn tool-step!
  "Runs a :cli or :http step: the tool's answer must pass f/judge-reply or the step throws. Resolves to the tool's output.
  A step waits (up to 10 s) while the body answers settling or timeout (a mutating tool carries its request id, so the repeat is safe)."
  [opts step last-job event-job]
  (let [argv (f/step-argv (:body opts) (:world opts) step last-job event-job)]
    (if-let [why (f/argv-gap argv step last-job event-job)]
      (js/Promise.reject (js/Error. (str (pr-str (vec (take 2 step))) " step: " why)))
      (.then (exec-settled {} argv)
             (fn [{:keys [code out]}]
               (let [{:keys [pass? evidence]} (f/judge-reply (some-> (f/step-pattern step) (f/fill-pattern last-job event-job)) code out)]
                 (when-not pass? (throw (js/Error. (str (pr-str (vec (take 2 step))) " step: " evidence))))
                 out))))))

(declare judge-file-check)

(defn until-reply!
  "Sends cmd every 500 ms until (match? reply); throws what + the last reply after limit-s seconds.
  io: {:send :sleep :now} (:now defaults to the clock)."
  [{:keys [send sleep now] :or {now #(js/Date.now)}} cmd match? what limit-s]
  (let [until (+ (now) (* 1000 limit-s))]
    (letfn [(poll []
              (.then (send [cmd])
                     (fn [[reply]]
                       (cond
                         (match? reply) nil
                         (> (now) until) (throw (js/Error. (str what " not met after " limit-s " s: last reply " (str/trim (str reply)))))
                         :else (.then (sleep 500) poll)))))]
      (poll))))

(defn until-rcon-step!
  "Polls cmd until its reply matches the regex pattern (see until-reply!)."
  [io cmd pattern limit-s]
  (until-reply! io cmd #(f/rcon-reply-matches? % pattern) (str ":until-rcon " (pr-str cmd) " " (pr-str pattern)) limit-s))

(defn until-pos-step!
  "Polls the Pos reply of cmd until it lies inside bounds (plot-relative, see f/pos-in-bounds?)."
  [io cmd origin bounds limit-s]
  (until-reply! io cmd #(f/pos-in-bounds? % origin bounds) (str ":until-pos " (pr-str bounds)) limit-s))

(defn until-step!
  "Polls check (a :memory / :file after check or a :cli step) every 500 ms until it passes; throws with its last evidence
  after limit-s seconds."
  [opts check limit-s last-job event-job]
  (let [until (+ (js/Date.now) (* 1000 limit-s))
        run (fn [] (if (f/after-file check)
                     (js/Promise.resolve (let [r (judge-file-check opts check)] (when-not (:pass? r) (:evidence r))))
                     (-> (tool-step! opts check last-job event-job)
                         (.then (constantly nil))
                         (.catch #(.-message %)))))]
    (letfn [(poll []
              (.then (run)
                     (fn [why]
                       (cond
                         (nil? why) nil
                         (> (js/Date.now) until) (throw (js/Error. (str ":until " (pr-str (vec (take 2 check))) " not met after " limit-s " s: " why)))
                         :else (.then (sleep 500) poll)))))]
      (poll))))

(defn run-steps!
  "Runs the act steps in order; resolves to the set of submitted job ids. :await and :rcon-until see events from
  offset/since (the watch window: a trigger firing inside the settle counts, as it does for :expect)."
  [opts origin c offset t0]
  (let [box (f/box-selector (f/case-grid c) origin (get-in c [:plot :height]))
        last-job (atom nil)
        event-job (atom nil)
        cli-ms (atom nil)]
   (reduce (fn [p [op a b :as step]]
            (.then p (fn [ids]
                       (case op
                         :summon (.then (rcon! [(f/summon-command origin step)]) (constantly ids))
                         :rcon (.then (rcon-step! {:send rcon! :log log!} (f/substitute a (:body opts) origin box)) (constantly ids))
                         :kill-body (.then (rcon! [(str "kill " (:body opts))]) (constantly ids))
                         :rcon-until (.then (rcon-until! opts origin box offset t0 a b) (constantly ids))
                         :wait-s (.then (sleep (* 1000 a)) (constantly ids))
                         :time-set (.then (let [do-set #(set-time! opts a (str (:id c) " step"))]
                                            (if (and (:night-barrier opts) (:night-batch opts) (= a (lease/barrier-ticks c)))
                                              (barrier-wait! {:dir nil :batch (:night-batch opts) :n (:night-barrier opts) :pid (.-pid js/process)
                                                              :sleep sleep :timeout-ms barrier-timeout-ms
                                                              :note #(log-command! opts "time set barrier" %)}
                                                             do-set)
                                              (do-set)))
                                          (fn [ok] (if ok ids (throw (js/Error. "a :time-set step needs --allow-time")))))
                         :await (.then (await-event opts offset a (await-since a t0 @cli-ms) (* 1000 b) ids)
                                       (fn [ev] (if ev (do (reset! event-job (get-in ev [:context :job-id])) ids) (throw (js/Error. (str ":await " (pr-str a) " timed out after " b " s"))))))
                         :restart-body (.then (restart-body! opts) (constantly ids))
                         :until (.then (until-step! opts a b @last-job @event-job) (constantly ids))
                         :until-seen (.then (until-step! opts (f/seen-check a) b @last-job @event-job) (constantly ids))
                         :until-rcon (.then (until-rcon-step! {:send rcon! :sleep sleep} (f/substitute a (:body opts) origin box) b (nth step 3)) (constantly ids))
                         :until-pos (.then (until-pos-step! {:send rcon! :sleep sleep} (f/substitute "data get entity $BODY Pos" (:body opts) origin box) origin a b) (constantly ids))
                         (:cli :http) (.then (do (reset! cli-ms (js/Date.now)) (tool-step! opts step @last-job @event-job))
                                             (fn [out] (if-let [id (and (= [:http :submit] (take 2 step)) (f/submitted-id out))]
                                                         (do (reset! last-job id) (conj ids id))
                                                         ids)))
                         :job (.then (resolve-entity-ids! opts (f/resolve-body-refs (f/resolve-plan-refs a (plan-prefix opts)) (:body opts)))
                                     (fn [spec] (.then (submit-job! opts spec (vec (map #(str "--" (name %)) b)))
                                     (fn [id] (reset! last-job id) (conj ids id)))))))))
          (js/Promise.resolve #{})
          (:act c))))

(defn watch-window
  "Where watch! starts counting events: {:offset :from-ms}. A case with a register counts from just before the register
  was put (its triggers may fire inside the settle); one without counts from t0, after the settle."
  [register? before-register settled]
  (if register? before-register settled))

(def stall-gap-ms
  "A gap between two polls (every 500 ms) this long means the runner was not running (host suspended, process stopped)."
  60000)

(defn stalled? [last-ms now-ms] (> (- now-ms last-ms) stall-gap-ms))

(defn stall-clock
  "A clock for one case: {:last ms of the latest tick, :gap the longest gap between two ticks, :gap-start when it began}."
  [now-ms]
  (atom {:last now-ms :gap 0 :gap-start now-ms}))

(defn tick!
  "Records a tick at now-ms in the stall clock."
  [clock now-ms]
  (swap! clock (fn [{:keys [last gap gap-start]}]
                 (let [g (- now-ms last)]
                   (if (> g gap)
                     {:last now-ms :gap g :gap-start last}
                     {:last now-ms :gap gap :gap-start gap-start})))))

(defn clock-stalled? [clock] (> (:gap @clock) stall-gap-ms))

(defn start-stall-monitor!
  "Ticks the clock every 500 ms until the returned stop fn is called, so a gap anywhere in the case (build, lock wait,
  steps), not only inside watch!, shows."
  [clock]
  (let [timer (js/setInterval #(tick! clock (js/Date.now)) 500)]
    (.unref timer)
    #(js/clearInterval timer)))

(defn start-natural-sweep!
  "Clears naturally spawned hostiles around the plot (f/natural-hostiles-commands) now and every 5 s until the
  returned stop fn is called; a no-op with :mobs :keep."
  [cmds]
  (if (empty? cmds)
    (fn [])
    (let [sweep #(.catch (rcon! cmds) (fn [_]))
          timer (js/setInterval sweep 5000)]
      (.unref timer)
      (sweep)
      #(js/clearInterval timer))))

(defn stall-results
  "Expectation results of a stalled run: the undecided ones become :stalled (the run did not watch them for gap-ms)."
  [results gap-ms]
  (mapv #(if (= :pending (:status %))
           (assoc % :status :stalled :evidence (str "runner stalled " (js/Math.round (/ gap-ms 1000)) " s while watching"))
           %)
        results))

(defn mark-stalled
  "A failed result with a :stalled expectation becomes :inconclusive: the stall, not the job, ended the watch. A real
  :fail among the expectations keeps the case failed."
  [r]
  (if (and (= :fail (:status r))
           (some #(= :stalled (:status %)) (:expects r))
           (not-any? #(= :fail (:status %)) (:expects r)))
    (assoc r :status :inconclusive :why "runner stalled during the case (host suspended or process stopped)")
    r))

(defn idle-grace-ms
  "How long a job case waits after its last job ended, for trailing events, before it is judged: its :idle-grace-s (default 3)."
  [c]
  (* 1000 (:idle-grace-s c 3)))

(defn early-end?
  "Whether a case may end before its limit once its jobs are idle: it submitted jobs, has no register, does not set
  :wait-full, and every pending expectation is an :event one about the case's own jobs (:of-job, or a pattern with
  :source :job); a :no-event / :count-event may still pass by waiting, a reflex or trigger event may still come."
  [c ids results]
  (and (seq ids) (empty? (:register c)) (not (:wait-full c))
       (every? #(or (not= :pending (:status %))
                    (let [e (:expect %)]
                      (and (:event e) (or (:of-job e) (= :job (:source (:event e)))))))
               results)))

(defn near-miss
  "Text for the case's own job event closest to the pending expectation: the last one of its :kind, else the last
  non-lifecycle one (nil when there is none), with its message or data."
  [events ids expect]
  (let [own (filter #(and (= :job (:source %)) (some ids (get-in % [:context :chain]))) events)
        kind (get-in expect [:event :kind])
        e (or (last (filter #(= kind (:kind %)) own))
              (last (remove #(x/job-lifecycle (:kind %)) own)))]
    (when e (str (x/evidence e) (when-let [d (:data e)] (str " " (pr-str d)))))))

(defn end-early
  "results with the pending ones failed: no job was active; saved-s seconds before the case's limit. The evidence ends
  with the job's near miss (see near-miss) from events of the case's job ids."
  [results {:keys [state why]} saved-s events ids]
  (mapv #(if (= :pending (:status %))
           (assoc % :status :fail :ended-early-s saved-s
                  :evidence (str "no job active: " (name state) (when why (str " " why))
                                 (when-let [m (near-miss events ids (:expect %))] (str "; nearest: " m))))
           %)
        results))

(defn watch-with!
  "Polls until every expectation is decided or the case's limit; resolves to the judged results. io: {:now :sleep
  :events (events from the window start)}. clock: the case's stall clock; a gap over stall-gap-ms in it ends the watch
  with the undecided expectations :stalled. A job case whose jobs have all ended for idle-grace-ms ends early (see
  early-end?); the pending expectations fail with :ended-early-s. io :left-plot (optional, -> promise of a reason or nil)
  is asked every 2 s; a reason fails the pending expectations with it."
  [{:keys [now sleep events left-plot]} c t0 ids clock]
  (let [polls (atom 0)
        limit (+ t0 (* 1000 (max (:limit-s c) (+ 2 (x/deadline-s (:expect c))))))
        idle-since (atom nil)]
    (letfn [(check-left-plot [results]
              (if (and left-plot (zero? (mod (swap! polls inc) 4)))
                (.then (left-plot)
                       (fn [why] (when why (mapv #(if (= :pending (:status %)) (assoc % :status :fail :evidence why) %) results))))
                (js/Promise.resolve nil)))
            (poll []
              (let [t (now)
                    _ (tick! clock t)
                    stalled? (clock-stalled? clock)
                    evs (events)
                    ;; a stall must not run out the clock of an expectation: judge as of when the gap began
                    results (x/judge-all (:expect c) evs {:t0-ms t0 :now-ms (if stalled? (:gap-start @clock) t) :job-ids ids})
                    idle (when (and (not stalled?) (early-end? c ids results)) (x/jobs-idle evs (into ids (x/job-ids-in evs))))
                    _ (reset! idle-since (when idle (or @idle-since t)))]
                (cond
                  stalled? (js/Promise.resolve (stall-results results (:gap @clock)))
                  (x/failed? results) (js/Promise.resolve (x/stop-early results))
                  (or (x/decided? results) (> t limit))
                  (js/Promise.resolve (mapv #(if (= :pending (:status %)) (assoc % :status :fail :evidence "undecided at the case's limit") %) results))
                  (and idle (>= (- t @idle-since) (idle-grace-ms c)))
                  (js/Promise.resolve (end-early results idle (js/Math.round (/ (- limit t) 1000)) evs ids))
                  :else
                  (.then (check-left-plot results) (fn [failed] (or failed (.then (sleep 500) poll)))))))]
      (poll))))

(defn watch!
  "watch-with! on the body's event log. window: see watch-window; events before t0 count when the window starts earlier.
  The body's position is read to fail a case whose body left its plot (origin; raw-case c: its #at tags are unresolved)."
  [opts c raw origin {offset :offset from :from-ms} t0 ids clock]
  (watch-with! {:now js/Date.now :sleep sleep
                :left-plot #(.then (rcon! [(f/start-check-command (:body opts))])
                                   (fn [[reply]] (f/left-plot-why origin raw (or reply ""))))
                :events #(filterv (fn [e] (>= (:time-ms e 0) from)) (read-events-from (events-file opts) offset))}
               c t0 ids clock))

(defn judge-file-check
  "Result of a :memory / :file check: reads the file under the body's directory."
  [opts check]
  (let [file (path/join (body-dir opts) (f/after-file check))]
    (f/judge-file-after check (when (fs/existsSync file) (fs/readFileSync file "utf8")))))

(defn after-checks! [opts origin c]
  (let [checks (:after c)
        file? (comp some? f/after-file)
        rcon-checks (vec (remove file? checks))
        cmds (mapv #(f/after-commands origin (:body opts) (f/case-grid c) c %) rcon-checks)]
    (.then (rcon! (vec (apply concat cmds)))
           (fn [replies]
             (let [joined (map f/join-after-replies (f/group-replies (map count cmds) replies))
                   judged (map vector rcon-checks (map #(f/judge-after origin %1 %2) rcon-checks joined))
                   by-check (into {} (map (fn [[chk r]] [chk r])) judged)
                   results (mapv (fn [chk] (if (file? chk) (judge-file-check opts chk) (by-check chk))) checks)
                   probes (mapv #(when (:stray? %) (f/entity-probe-command origin (:check %))) results)]
               (.then (rcon! (keep identity probes))
                      (fn [dumps]
                        (let [named (zipmap (keep-indexed #(when %2 %1) probes) dumps)]
                          (vec (map-indexed (fn [i r]
                                              (if-let [d (named i)]
                                                (update r :evidence str "; " (f/describe-entity-reply origin d))
                                                r))
                                            results))))))))))

(def register-put-tries "A put to a busy body can time out (its tool waits 3 s); it is asked again this many times." 4)

(defn put-until-answered!
  "Runs (run) -> promise of {:code :out} until it exits 0, or answers other than a :timeout, or tries run out; resolves to the last answer."
  [run tries pause-ms]
  (.then (run)
         (fn [{:keys [code out] :as answer}]
           (if (and (not (zero? code)) (str/includes? out ":timeout") (> tries 1))
             (.then (sleep pause-ms) #(put-until-answered! run (dec tries) pause-ms))
             answer))))

(defn put-register!
  "Puts the case's register entries on the running body (one triggers.mjs put each); throws when one is refused."
  [opts register origin]
  (reduce (fn [p argv]
            (.then p (fn []
                       (.then (put-until-answered! #(exec-file (into ["engine/tools/triggers.mjs"] argv)) register-put-tries 1500)
                              (fn [{:keys [code out]}]
                                (when-not (zero? code)
                                  (throw (js/Error. (str "register put failed: " (str/trim out))))))))))
          (js/Promise.resolve nil)
          (f/register-put-argvs (:body opts) (:world opts) register origin)))

(defn park!
  "Sends park-commands until the server finds the body (a body still logging in is not found yet, stays survival and
  suffocates in the plot built round its saved spot; a body already a spectator gets an empty reply), 16 tries 300 ms apart. Resolves to nil
  when parked, else the failure message. io: {:send cmds->promise of replies, :sleep ms->promise}."
  [{:keys [send sleep]} body]
  (letfn [(attempt [retries]
            (.then (send (f/park-commands body))
                   (fn [replies]
                     (cond
                       (not-any? #(re-find #"(?i)no (player|entity)" (or % "")) replies) nil
                       (zero? retries) (str "the body could not be parked as a spectator: " (str/join "; " replies))
                       :else (.then (sleep 300) #(attempt (dec retries)))))))]
    (attempt 16)))

(defn ensure-at-start!
  "Checks the body stands at the case's start after body-commands; when it does not (a tp that did not take), repeats
  the tp and polls up to 16 times, 300 ms apart (about 5 s). Once it stands there it becomes survival (a spectator
  switched where the tp did not take suffocates in the blocks built there) and this resolves to nil, else to the
  failure message. io: {:send cmds->promise of replies, :sleep ms->promise}."
  [{:keys [send sleep]} origin body c]
  (let [{:keys [at yaw]} (:body c)
        tp (str "tp " body " " (f/xyz-str (f/abs-pos origin at)) " " (or yaw 0) " 0")]
    (letfn [(check [retries]
              (.then (send [(f/start-check-command body)])
                     (fn [[reply]]
                       (let [{:keys [pass? why]} (f/judge-start origin c (or reply ""))]
                         (cond
                           pass? (.then (send (f/survival-commands body)) (constantly nil))
                           (zero? retries) (str why " (tp repeated 16 times over 5 s)")
                           :else (.then (send [tp]) (fn [_] (.then (sleep 300) #(check (dec retries))))))))))]
      (check 16))))

(defn build-plot!
  "Sends the plot's setup and block commands (idempotent), then checks the replies: a plot whose chunks were not loaded
  yet is built again after a pause, up to 3 times. Resolves to nil when built, else to the failure message (a setup
  error, not a job failure). io: {:send cmds->promise of replies, :sleep ms->promise}."
  [{:keys [send sleep]} grid origin c]
  (letfn [(attempt [retries]
            (-> (send (f/setup-commands grid origin c))
                (.then (fn [setup] (.then (send (f/block-commands origin c)) #(into setup %))))
                (.then (fn [replies]
                         (let [bad (f/build-failure replies)]
                           (cond
                             (nil? bad) nil
                             (zero? retries) (str "plot not built: " bad " (tried 4 times)")
                             :else (.then (sleep 1000) #(attempt (dec retries)))))))))]
    (attempt 3)))

(def default-day-limit
  "The world time at which a held day phase is reset: natural hostiles spawn from dusk (12000) and stay."
  9000)

(defn phase-time-fix
  "The ticks to `time set` when the world time left the window of phase (:day, :night, :night-x), else nil. A held phase
  drifts with the clock (a day is 24000 ticks, ~10 real min at the sweep tick rate): day ends at day-limit, night spans
  13000-22000."
  [phase ticks day-limit]
  (case phase
    :day (when (and ticks (<= day-limit ticks 22999)) 1000)
    (:night :night-x) (when (and ticks (not (<= 13000 ticks 22000))) 14000)
    nil))

(defn refresh-phase-time!
  "Keeps the phase's time of day for the whole phase: re-sets the time (env :set-ticks!, which logs) when it left the window.
  Resolves to true. env: {:read-ticks (-> promise of ticks or nil), :set-ticks! (ticks why -> promise), :day-limit ticks}."
  [{:keys [read-ticks set-ticks! day-limit]} phase what]
  (.then (read-ticks)
         (fn [ticks]
           (if-let [fix (phase-time-fix phase ticks day-limit)]
             (.then (set-ticks! fix (str what " found the " (name phase) " phase at " ticks)) (fn [_] true))
             true))))

(defn hold-phase!
  "Resolves to ok? once stop! (the previous body goes offline, before the wait so it cannot sleep or act meanwhile),
  this process holds the time lock, the first holder has put the world in the phase (first-set!, resolving to ok?; a
  joiner runs refresh!, which re-sets the time when it drifted out of the phase) and
  start! (the body start) has run. The body starts under the lock, so it never comes up while another runner's body
  sleeps (its sleep-status would make a night job log out for the whole case)."
  [{:keys [stop! acquire! first-set! refresh! start!]}]
  (.then (stop!)
         (fn [_] (.then (acquire!) (fn [held]
           (.then (if (:first? held) (js/Promise.resolve (first-set!)) (if refresh! (refresh!) (js/Promise.resolve true)))
                  (fn [ok] (.then (start!) (fn [_] ok)))))))))

(defn write-trace!
  "A failed or errored result r with its trace file named under :trace, when --trace-dir is given: the body events from
  cursor on and the tail of body.log. A trace that cannot be written is logged and r stays as it is."
  [opts r cursor]
  (if-not (and (:trace-dir opts) (#{:fail :error} (:status r)))
    r
    (try
      (let [attempt (+ (:trace-attempt opts 0) (or (:retry opts) 0))
            log-file (path/join (run-dir opts) "body.log")
            doc (trace/trace-doc (assoc (select-keys r [:id :run :status :why])
                                        :attempt attempt
                                        :events (read-events-from (events-file opts) cursor)
                                        :log (when (fs/existsSync log-file) (trace/tail-text (str (fs/readFileSync log-file "utf8")) trace/max-log-bytes))))]
        (assoc r :trace (trace/write! (:trace-dir opts) (:id r) (:run r) attempt doc)))
      (catch :default e (log! "world-test: trace not written: " (.-message e)) r))))

(defn run-case!
  "One run of case c on plot i (leased by the caller); stop! (a thunk to a promise) stops the previous body before the
  time lock is waited for, start! starts or keeps the body once the lock is held. Resolves to a result map. register: the entries to put on the body once it stands in
  the built plot (the body was just started with none), nil when it keeps the register it has."
  [opts c i run register stop! start!]
  (let [grid (f/case-grid c)
        origin (f/plot-origin grid i)
        rc (-> (f/resolve-tags c origin) (f/resolve-expect-refs (plan-prefix opts)))
        started (js/Date.now)
        plan-files (atom [])
        t-start (atom nil)
        clock (stall-clock (js/Date.now))
        stop-monitor (start-stall-monitor! clock)
        natural (f/natural-hostiles-commands grid origin rc)
        stop-sweep (atom (fn []))
        pre-register (atom nil)
        setup-cursor (atom nil)
        parked-at (atom nil)
        start-cursor (log-cursor (events-file opts))
        weather (atom nil)
        result (fn [m] (cond-> (merge {:id (:id c) :run run :plot i :origin origin :elapsed-s (/ (- (js/Date.now) started) 1000)} m)
                         @weather (assoc :weather @weather)))]
    (-> (let [phase (lock-phase opts rc)
              batch (lock-batch opts rc)]
          (hold-phase!
           {:stop! stop!
            :acquire! #(acquire-time-lock! phase (str (:id c) (if (= :any phase) " runs under the current time" " depends on the time of day")) batch)
            :first-set! (fn []
                          (-> (if (and (:sleep-skip rc) (not= :any phase))
                                (reset-stray-sleep-rule! rcon! opts (str (:id c) " needs the vanilla night skip"))
                                (js/Promise.resolve nil))
                              (.then (fn [_] (cond
                                (= :any phase) true
                                (and batch (night-batch-set? batch)) (js/Promise.resolve true)
                                batch (.then (set-time! opts (batch-start-ticks rc) (str (:id c) " starts night batch " batch))
                                             (fn [ok] (if ok
                                                        (.then (start-sleep-rule! rcon! opts rc batch nil (str (:id c) " starts night batch " batch)) (constantly ok))
                                                        ok)))
                                :else (time-ok! opts (if (and (lease/time-phase rc) (not (and (= :night-x phase) (= :any (:time rc))))) rc (assoc rc :time :day))))))
                              (.then (fn [ok]
                                       (if (or (not ok) (= :any phase))
                                         ok
                                         (.then (confirm-phase! (if (starts-night? rc) :night :day))
                                                (fn [_] ok)))))
                              (.then (fn [ok]
                                       (if (or (not ok) (= :any phase))
                                         ok
                                         (.then (set-weather! opts (weather-wanted rc) (str (:id c) " starts the " (name phase) " phase"))
                                                (fn [set?] (or set? (not= :weather-x phase)))))))
                              (.then (fn [ok] (mark-phase-set!) ok))))
            :refresh! (fn []
                        (if-not (joiner-refresh? opts phase rc)
                          (js/Promise.resolve true)
                          (refresh-phase-time!
                           {:read-ticks #(.then (rcon! ["time query day"]) (fn [[reply]] (daytime (or reply ""))))
                            :set-ticks! (fn [ticks why] (set-time! opts ticks why))
                            :day-limit (:day-limit opts default-day-limit)}
                           phase (:id c))))
            :start! start!}))
        (.then (fn [ok] (note-last-plot! opts origin grid) ok))
        (.then (fn [ok]
                 (if-not ok
                   (result {:status :skipped :why (str "needs " (if (lease/weather-x? rc) (str "weather " (name (:weather rc))) (name (:time rc))) " (no --allow-time)")})
                   (-> (.then (rcon! ["time query day"])
                              (fn [[reply]] (reset! t-start {:ticks (daytime (or reply "")) :ms (js/Date.now)})))
                       (.then #(read-weather! rcon!))
                       (.then #(reset! weather %))
                       (.then (fn [] (ev/emit! (ev/phase :setup)) (park! {:send rcon! :sleep sleep} (:body opts))))
                       (.then (fn [why] (when why (throw (js/Error. why)))))
                       (.then #(reset! parked-at (js/Date.now)))
                       (.then (fn [] (build-plot! {:send rcon! :sleep sleep} grid origin rc)))
                       (.then (fn [why] (when why (throw (js/Error. why)))))
                       (.then #(write-plans-settled! opts rc))
                       (.then #(reset! plan-files %))
                       (.then #(seed-shared! opts origin c))
                       (.then #(reset! setup-cursor (log-cursor (events-file opts))))
                       (.then #(rcon! (f/body-commands origin (:body opts) rc)))
                       (.then #(ensure-at-start! {:send rcon! :sleep sleep} origin (:body opts) rc))
                       (.then (fn [why] (when why (throw (js/Error. why)))))
                       (.then #(rcon! (f/clear-hostiles-commands grid origin rc)))
                       (.then #(reset! stop-sweep (start-natural-sweep! natural)))
                       (.then #(reset! pre-register {:offset (log-cursor (events-file opts)) :from-ms (js/Date.now)}))
                       (.then #(when register (ev/emit! (ev/phase :register)) (put-register! opts register origin)))
                       (.then #(sleep (* 1000 (get-in rc [:body :settle-s]))))
                       (.then (fn [] (when-let [why (f/start-hurt @parked-at (read-events-from (events-file opts) @setup-cursor))] (throw (js/Error. why)))))
                       (.then #(rcon! (f/clear-hostiles-commands grid origin rc)))
                       (.then (fn []
                                (let [t0 (js/Date.now)
                                      _ (ev/emit! (ev/phase :run))
                                      window (watch-window (some? register) @pre-register {:offset (log-cursor (events-file opts)) :from-ms t0})]
                                  (-> (run-steps! opts origin rc (:offset window) (:from-ms window))
                                      (.then (fn [ids]
                                               (.then (watch! opts rc c origin window t0 ids clock)
                                                      (fn [expects]
                                                        (.then (after-checks! opts origin rc)
                                                               (fn [afters]
                                                                 (mark-stalled
                                                                  (result (cond-> (ev/with-why {:status (if (and (x/passed? expects) (every? :pass? afters)) :pass :fail)
                                                                                                :expects expects :afters afters})
                                                                            (some :ended-early-s expects) (assoc :ended-early-s (some :ended-early-s expects)
                                                                                                          :idle-grace-s (:idle-grace-s rc 3)))))))))))))))))))
        (.catch (fn [e] (result {:status :error :why (.-message e)})))
        (.then (fn [r]
                 (if (and @t-start (= :fail (:status r)))
                   (.then (rcon! ["time query day" "tick query"])
                          (fn [[reply rate-reply]]
                            (-> r
                                (mark-time-disturbed (time-disturbed? rc (:ticks @t-start) (daytime (or reply ""))
                                                                      (- (js/Date.now) (:ms @t-start))
                                                                      (some-> @pre-register :offset (as-> off (woke? (read-events-from (events-file opts) off))))
                                                                      (tick-rate-of rate-reply)))
                                (mark-night-overran (night-overran? opts rc (daytime (or reply "")))))))
                   r)))
        (.then (fn [r] (write-trace! opts r (or @setup-cursor start-cursor))))
        (.then (fn [r]
                 (-> (run-cleanup! [#(when (some (fn [s] (#{:cli :http} (first s))) (:act c))
                                       (exec-file ["engine/tools/drive.mjs" (:body opts) "release" "--force" "--world" (:world opts)]))
                                    #(exec-file ["engine/tools/jobs.mjs" (:body opts) "--world" (:world opts) "cancel-all"])
                                    #(rcon! (f/cleanup-commands grid origin (:body opts) rc))
                                    #(when (lease/weather-x? rc) (set-weather! opts :clear (str (:id c) " ended")))
                                    #(when (and (:night-barrier opts) (:night-batch opts)) (barrier-leave! nil (:night-batch opts) (.-pid js/process)))
                                    #(when (f/writes-shared? c) (drop-shared! opts))
                                    #(run! (fn [f] (when (fs/existsSync f) (fs/unlinkSync f))) @plan-files)])
                     (.then (constantly r)))))
        (.finally (fn [] (stop-monitor) (@stop-sweep) (release-time-lock!))))))

;; ------------------------------------------------------------------ reporting

(defn expect-line [{:keys [expect status evidence at-s]}]
  (str "    " (if (= :pass status) "ok  " "FAIL") " "
       (cond
         (:event expect) (str "event " (pr-str (:event expect)) " within " (:within-s expect) " s")
         (:count-event expect) (str "count of " (pr-str (:count-event expect)) " in " (:for-s expect) " s, "
                                    (pr-str (select-keys expect [:at-least :at-most])))
         :else (str "no event " (pr-str (:no-event expect)) " for " (:for-s expect) " s"))
       (when evidence (str " -> " evidence)) (when at-s (str " at " (.toFixed at-s 1) " s"))))

(defn report! [r]
  (ev/emit! (ev/result r))
  (log! (str/upper-case (name (:status r))) " " (:id r) " #" (:run r) " (plot " (:plot r) ", " (.toFixed (:elapsed-s r) 1) " s)"
        (when (:weather r) (str " [" (name (:weather r)) "]"))
        (when (:why r) (str ": " (:why r))))
  (when-not (= :pass (:status r))
    (run! #(log! (expect-line %)) (:expects r))
    (run! #(log! "    " (if (:pass? %) "ok  " "FAIL") " after " (pr-str (:check %)) " -> " (:evidence %)) (:afters r))))

;; ------------------------------------------------------------------ main

(defn body-lease-file [name] (path/join lease-dir (str "body-" name ".lease")))

(defn read-body-leases
  "{body-name pid} of the body lease files in the lease dir."
  []
  (if-not (fs/existsSync lease-dir)
    {}
    (into {} (keep (fn [f]
                     (when-let [[_ name] (re-matches #"body-(.+)\.lease" f)]
                       (let [n (js/parseInt (str/trim (fs/readFileSync (path/join lease-dir f) "utf8")) 10)]
                         (when-not (js/isNaN n) [name n]))))
                   (array-seq (fs/readdirSync lease-dir))))))

(defn lease-body!
  "Records that this runner's body is a test body (the other runners' players check ignores it while this PID lives)."
  [{:keys [body]}]
  (fs/mkdirSync lease-dir #js {:recursive true})
  (fs/writeFileSync (body-lease-file body) (str (.-pid js/process))))

(defn release-body! [{:keys [body]}]
  (try (fs/unlinkSync (body-lease-file body)) (catch :default _ nil)))

(def precheck-exit
  "Exit code of a refusal that holds for every case (a stranger near the plot grid); the pool stops its pass on it."
  5)

(defn stop-stale-bodies!
  "Stops (SIGTERM, by PID) the running bodies named prefix + a capital letter whose runner lease is dead or missing."
  [prefix]
  (let [out (.-stdout (cp/spawnSync "pgrep" #js ["-af" "out/body(-release)?\\.cjs --agent "] #js {:encoding "utf8"}))
        stale (lease/stale-body-procs (lease/parse-body-procs out) prefix (lease/leased-bodies (read-body-leases) pid-alive?))]
    (doseq [{:keys [pid name]} stale]
      (log! "world-test: stopping the leftover body " name " (pid " pid ", no live runner)")
      (try (.kill js/process pid "SIGTERM") (catch :default _ nil)))
    (count stale)))

(defn players-near
  "Resolves to nil, or to the refusal text, when a player other than this body and the bodies of live runners
  (body leases) is within 500 blocks of the grid's centre; it names who is online besides those."
  [opts]
  (let [leased (lease/leased-bodies (read-body-leases) pid-alive?)]
    (.then (rcon! [(lease/near-command (:body opts) leased (f/grid-centre f/default-grid)) "list"])
           (fn [[reply listing]]
             (when (str/includes? reply "Test passed")
               (str "another player is within 500 blocks of the plot grid (not a leased test body): "
                    (str/join ", " (lease/strangers (lease/parse-online listing) (:body opts) leased))))))))

(def boot-exit
  "Exit code of a batch stopped because the body died at boot boot-limit times in a row; the pool and the sweep stop on it."
  6)

(def boot-limit 2)

(defn boot-dead?
  "Whether the last boot-limit results are all errors of a body that did not start."
  [results]
  (let [tail (take-last boot-limit results)]
    (and (= boot-limit (count tail))
         (every? #(and (= :error (:status %)) (str/includes? (str (:why %)) "did not start within")) tail))))

(defn stop-batch?
  "Whether the batch ends: the body died at boot repeatedly, or --stop-on-fail and some result so far failed or errored."
  [{:keys [stop-on-fail]} results]
  (boolean (or (boot-dead? results)
               (and stop-on-fail (some #(#{:fail :error} (:status %)) results)))))

(defn exit-code
  "boot-exit when the body died at boot repeatedly; else 0 when there is at least one result and all passed or were flaky, else 1."
  [results]
  (cond (boot-dead? results) boot-exit
        (and (seq results) (every? #(#{:pass :flaky} (:status %)) results)) 0
        :else 1))

(defn retry-targets
  "The results worth a retry: plain failures (an error or an inconclusive run is not a flake candidate)."
  [results]
  (filterv #(= :fail (:status %)) results))

(defn retry-view
  "A retry result as reported live: a pass is :flaky (the case already reported its failure), a failure stays as it is."
  [r]
  (if (= :pass (:status r))
    (assoc r :status :flaky :why "passed on a retry after an earlier failure")
    r))

(defn retry-groups
  "The failed results as register groups of [case run] pairs for run-groups!, in failure order within each register."
  [by-id failed]
  (for [[register rs] (group-by #(:register (by-id (:id %))) failed)]
    [register (for [r rs] [(by-id (:id r)) (:run r)])]))

(defn align-retries
  "The retry results ({[id run] result}) in the order of failed; a case that did not run again keeps its failure."
  [failed again]
  (mapv #(or (again [(:id %) (:run %)]) %) failed))

(defn settle-retries
  "The first failed result r1 after its retry results: the first passing retry makes it :flaky (that retry's data, the
  attempt number under :retry, r1 under :first-failure); else r1 stays failed with :retries counted."
  [r1 retries]
  (if-let [[n pass] (first (keep-indexed (fn [i x] (when (= :pass (:status x)) [(inc i) x])) retries))]
    (assoc pass :status :flaky :retry n :first-failure r1)
    (assoc r1 :retries (count retries) :last-retry (last retries))))

(defn retry-failed!
  "Up to n rounds: rerun! (failed results -> promise of new results in the same order) the cases still failing.
  Resolves to results with each first failure settled (see settle-retries)."
  [n results rerun!]
  (let [attempts (fn [tried r] (get tried ((juxt :id :run) r) []))
        passed? (fn [tried r] (some #(= :pass (:status %)) (attempts tried r)))
        round (fn round [tried left]
                (let [open (filterv #(not (passed? tried %)) (retry-targets results))]
                  (if (or (zero? left) (empty? open))
                    (js/Promise.resolve tried)
                    (.then (rerun! open)
                           (fn [new-results]
                             (round (reduce (fn [t [r nr]] (update t ((juxt :id :run) r) (fnil conj []) nr)) tried (map vector open new-results))
                                    (dec left)))))))]
    (.then (round {} n)
           (fn [tried] (mapv (fn [r] (if (seq (attempts tried r)) (settle-retries r (attempts tried r)) r)) results)))))

(defn write-results!
  "Writes results to the --results file, when there is one."
  [opts results]
  (when-let [file (:results opts)] (fs/writeFileSync file (pr-str results))))

;; ------------------------------------------------------------------ the body goal (shown on the dashboard)

(defn pattern-brief [p]
  (let [n #(if (keyword? %) (name %) (str %))]
    (if (and (:source p) (:kind p)) (str (n (:source p)) "." (n (:kind p))) (pr-str p))))

(defn expect-brief [e]
  (cond (:event e) (pattern-brief (:event e))
        (:count-event e) (str "count " (pattern-brief (:count-event e)))
        :else (str "no " (pattern-brief (:no-event e)))))

(defn case-goal
  "The goal text of run `run` (retry `retry`) of case c: \"test <file> › <case>\", plus \" (run N, retry M)\" naming
  only the counters above the first."
  [c run retry]
  (let [id (str (:id c))
        k (str/last-index-of id "/")
        name (if k (str (subs id 0 k) " › " (subs id (inc k))) id)
        counts (cond-> []
                 (> run 1) (conj (str "run " run))
                 (> retry 0) (conj (str "retry " retry)))]
    (str "test " name (when (seq counts) (str " (" (str/join ", " counts) ")")))))

(defn case-wait
  "What the case waits for, \"waiting: <expectations>\" cut to fit goal/max-text; nil without expectations."
  [c]
  (when (seq (:expect c))
    (let [t (str "waiting: " (str/join ", " (map expect-brief (:expect c))))]
      (if (> (count t) goal/max-text) (str (subs t 0 (dec goal/max-text)) "…") t))))

(def goal-dirs
  "Body dirs whose goal this process set and has not yet cleared."
  (atom #{}))

(defn clear-goals!
  "Clears the goal of every body dir in goal-dirs (synchronously, so an exit or signal handler can call it)."
  []
  (doseq [dir @goal-dirs]
    (try (goal/clear-goal! dir) (catch :default _ nil)))
  (reset! goal-dirs #{}))

(defonce goal-cleanup-installed? (atom false))

(defn install-goal-cleanup!
  "Once per process: clear-goals! on exit, and on SIGINT / SIGTERM (then exit with 130 / 143), so a killed runner does not
  leave its goal on an idle body."
  []
  (when-not @goal-cleanup-installed?
    (reset! goal-cleanup-installed? true)
    (.on js/process "exit" clear-goals!)
    (doseq [[sig code] [["SIGINT" 130] ["SIGTERM" 143]]]
      (.on js/process sig (fn [] (clear-goals!) (.exit js/process code))))))

(defn with-goal!
  "Sets the body goal in body-dir to text (and the optional dim wait part), runs thunk (-> promise) and clears the goal when it settles, also on an error
  (and on exit or a signal, see install-goal-cleanup!). A goal that cannot be written is logged; the run goes on."
  ([body-dir text thunk] (with-goal! body-dir text nil thunk))
  ([body-dir text wait thunk]
   (try (goal/write-goal! body-dir text "world-test" (js/Date.now) wait)
        (swap! goal-dirs conj body-dir)
        (catch :default e (log! "world-test: goal not set: " (.-message e))))
   (-> (js/Promise.resolve nil)
       (.then thunk)
       (.finally (fn [] (swap! goal-dirs disj body-dir) (goal/clear-goal! body-dir))))))

(defn run-groups!
  "Runs the [case run] pairs of each register group in order on the body, on-result! (result -> any) after each; resolves
  once every group ran. A batch stops early under --stop-on-fail (stop?: -> bool)."
  [opts groups stop? on-result!]
  (-> (reduce (fn [p [register pairs]]
                (.then p (fn []
                           (-> (reduce (fn [p2 [n [c run]]]
                                         (.then p2 (fn []
                                                    (when-not (stop?)
                                                     (let [plan (f/body-start-plan c (zero? n))
                                                           [from end] (f/plot-range (f/case-grid c))
                                                           i (acquire-plot! (max from (:first-plot opts)) end)
                                                           memory (when (seq (f/memory-entries c))
                                                                    (f/memory-seed (f/resolve-tags (f/memory-entries c) (f/plot-origin (f/case-grid c) i)) (js/Date.now)))]
                                                       (-> (with-goal! (body-dir opts) (case-goal c run (or (:retry opts) 0)) (case-wait c)
                                                             #(run-case! opts c i run (when-not (= :keep plan) register)
                                                                         (fn [] (if (= :keep plan) (js/Promise.resolve nil) (stop-body! opts)))
                                                                         (fn [] (if (= :keep plan)
                                                                                  (js/Promise.resolve nil)
                                                                                  (-> (if (= :restart-keep plan) (js/Promise.resolve nil) (reset-last-plot! opts))
                                                                                      (.then (fn [] (write-body-settings! opts c) (start-body! opts register (= :restart-keep plan) memory false))))))))
                                                           (.then (fn [r] (on-result! c r)))
                                                           (.finally #(release-plot! i))))))))
                                       (js/Promise.resolve nil)
                                       (map-indexed vector pairs))
                               (.finally #(.then (stop-body! opts) (fn [] (write-body-settings! opts {}))))))))
              (js/Promise.resolve nil)
              groups)))

(defn run-all! [opts cases]
  (let [results (atom [])
        expected (frequencies (map :file cases))
        expected (into {} (map (fn [[k n]] [k (* n (:repeat opts))]) expected))
        done (atom 0)
        report-fixtures! (fn []
                           (let [n (count (ev/fixtures-done expected @results))]
                             (when (> n @done) (reset! done n) (ev/emit! (ev/progress n (count expected))))))
        groups (for [[register group] (group-by :register cases)]
                 [register (for [run (range 1 (inc (:repeat opts))) c group] [c run])])
        first-pass (fn [c r] (report! r) (swap! results conj (assoc r :file (:file c))) (write-results! opts @results) (report-fixtures!))
        by-id (into {} (map (juxt :id identity)) cases)
        retry (atom 0)
        rerun! (fn [failed]
                 (let [again (atom {})
                       groups (retry-groups by-id failed)]
                   (log! "world-test: retrying " (count failed) " failed: " (str/join ", " (map #(str (:id %) " #" (:run %)) failed)))
                   (-> (run-groups! (assoc opts :retry (swap! retry inc)) groups (constantly false)
                                    (fn [c r] (report! (retry-view r)) (swap! again assoc [(:id c) (:run r)] (assoc r :file (:file c)))))
                       (.then (fn [] (align-retries failed @again))))))]
    (reset-body-log! opts)
    (ev/emit! (ev/plan cases (:repeat opts)))
    (-> (run-groups! opts groups #(stop-batch? opts @results) first-pass)
        (.then (fn []
                 (if (:retry-failed opts)
                   (-> (retry-failed! (:retry-failed opts) @results rerun!)
                       (.then (fn [settled] (reset! results settled) (write-results! opts settled) settled)))
                   @results))))))

(defn main
  "argv (array) -> promise of the exit code."
  [argv]
  (-> (js/Promise.resolve nil)
      (.then (fn []
               (install-goal-cleanup!)
               (let [opts (parse-args (array-seq argv))
                     final-results (atom nil)
                     run-start (atom nil)
                     all-cases (if (or (:check opts) (:night-batch-end opts) (:sleep-rule-restore opts)) [] (load-cases (:paths opts)))
                     cases (if (or (:check opts) (:night-batch-end opts) (:sleep-rule-restore opts)) [] (select-phase (f/select-cases all-cases opts) (:phase opts)))
                     cases (if (:changed-since-pass opts) (select-changed cases (:paths opts)) cases)
                     bad (filter :problems cases)]
                 (cond
                   (:check opts) (let [res (check-fixtures (:paths opts))]
                                   (ev/emit! (ev/plan res 1))
                                   (run! (fn [r] (ev/emit! (ev/result r))
                                           (when-not (= :pass (:status r)) (log! (:id r) ": " (:why r))))
                                         res)
                                   (log! "world-test --check: " (count res) " cases, " (count (remove #(= :pass (:status %)) res)) " failed")
                                   (check-exit-code res))
                   (:refresh-build opts) (if (refresh-body-build!) 0 2)
                   (:stop-stale-bodies opts) (do (stop-stale-bodies! (:stop-stale-bodies opts)) 0)
                   (:sleep-rule-restore opts) (-> (restore-sleep-rule! rcon! opts "world-test restores a recorded gamerule" (if (= :dead (:sleep-rule-restore opts)) {:only-dead? true} {:pool (owner-pid)}))
                                                  (.then (fn [_] (when (= :dead (:sleep-rule-restore opts)) (reset-stray-sleep-rule! rcon! opts "pool start"))))
                                                  (.then (constantly 0)))
                   (:night-batch-end opts) (.then (end-night-batch! rcon! opts (:night-batch-end opts) nil) (constantly 0))
                   (:list opts) (do (run! (if (:night-classes opts)
                                           #(log! (night-class-line %))
                                           #(log! (:id %) "  " (pr-str (:tags %)) (when (:problems %) (str "  PROBLEMS " (:problems %)))))
                                         cases)
                                   0)
                   (empty? cases) (do (log! "no cases selected") 2)
                   (seq bad) (do (run! #(log! (:id %) ": " (str/join "; " (:problems %))) bad) 2)
                   (running? opts) (do (log! "the body " (:body opts) " already runs; stop it first") 2)
                   (not (do (reset! run-start (run-state)) (refresh-body-build!))) 2
                   :else
                   (.then (players-near opts)
                          (fn [refusal]
                            (if refusal
                              (do (log! refusal) precheck-exit)
                              (-> (ensure-body! opts)
                                  (.then #(lease-body! opts))
                                  (.then #(drop-shared! opts))
                                  (.then #(run-all! opts cases))
                                  (.then (fn [results]
                                           (reset! final-results results)
                                           (write-results! opts results)
                                           (record-passes! opts all-cases (:paths opts) results @run-start)
                                           (let [n (frequencies (map :status results))]
                                             (log! "world-test: " (count results) " runs, " (n :pass 0) " passed, " (n :fail 0) " failed, "
                                                   (n :error 0) " errors, " (n :flaky 0) " flaky, " (n :skipped 0) " skipped, " (n :inconclusive 0) " inconclusive")
                                             (exit-code results))))
                                  (.finally #(-> (stop-body! opts)
                                                                 (.then (fn [] (finish-run! opts @final-results)))
                                                                 (.catch (fn [e] (log! "world-test: temp dir kept: " (.-message e))))
                                                                 (.then (fn [] (release-body! opts)))))))))))))
      (.catch (fn [e] (js/console.error (.-message e)) (js/console.error usage) 2))))
