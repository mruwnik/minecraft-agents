(ns world-test.runner
  "tools/world-test.mjs: runs world fixtures (engine/fixtures/world/*.edn, format in its README) against the live
  test server with one probe body. Per case and run: a fresh plot of the reserved grid is cleared and built, the body
  is put at its start, the act steps run, the body's event log is read until every expectation is decided, the
  :after checks ask the server, and the plot is cleaned up. The body is started once per distinct :register (the
  scenario) and restarted before every case with its engine/memory.edn and seen.bin deleted and the plot it was last left on cleared (a case with :keep-memory true keeps
  the memory: the body goes on from the case before it). Stopped at the end. Pure parts: world-test.fixture and world-test.expect."
  (:require ["child_process" :as cp]
            ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            [cljs.reader :as reader]
            [clojure.string :as str]
            [dashboard.rcon :as rcon]
            [world-test.expect :as x]
            [world-test.fixture :as f]
            [world-test.lease :as lease]))

(def usage
  (str "usage: node tools/world-test.mjs [fixture.edn|dir ...] [--tag T] [--match TEXT] [--repeat N] [--body NAME]\n"
       "         [--world claude] [--first-plot I] [--card ID] [--allow-time --time-log FILE] [--results FILE] [--list]\n"
       "Runs world fixtures (default dir engine/fixtures/world) on the reserved plot grid x/z 20000..20640, y 150.\n"
       "--allow-time lets a case that needs night or day set the time (each set appended to --time-log); without it\n"
       "such a case is skipped. A case that depends on the time of day holds a time lock shared by phase (day cases together,\n"
       "night cases together; the other phase waits; the first holder sets the time) for its whole run; a manual `time set` takes it too: node tools/time-set.mjs <ticks|day|noon|night|midnight>. Exit code 0 when every run passed, 1 when one failed, 2 on a usage or setup error."))

(defn parse-args [argv]
  (loop [[a b & more :as all] (vec argv) opts {:paths [] :repeat 1 :body "ProbeFixture" :world "claude" :first-plot 0}]
    (cond
      (empty? all) opts
      (= a "--tag") (recur more (assoc opts :tag b))
      (= a "--match") (recur more (assoc opts :match b))
      (= a "--repeat") (recur more (assoc opts :repeat (js/Number b)))
      (= a "--body") (recur more (assoc opts :body b))
      (= a "--world") (recur more (assoc opts :world b))
      (= a "--first-plot") (recur more (assoc opts :first-plot (js/Number b)))
      (= a "--time-log") (recur more (assoc opts :time-log b))
      (= a "--card") (recur more (assoc opts :card b))
      (= a "--results") (recur more (assoc opts :results b))
      (= a "--allow-time") (recur (rest all) (assoc opts :allow-time true))
      (= a "--list") (recur (rest all) (assoc opts :list true))
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

(defn load-cases [paths]
  (vec (mapcat #(f/file-cases (fs/readFileSync % "utf8") (path/basename % ".edn")) (fixture-files paths))))

;; ------------------------------------------------------------------ the event log

(defn body-dir [{:keys [world body]}] (repo-path "worlds" world "agents" body))
(defn events-file [opts] (path/join (body-dir opts) "engine" "events.edn"))

(defn file-size [file] (if (fs/existsSync file) (.-size (fs/statSync file)) 0))

(defn read-events-from
  "The events logged in file from byte offset on (the whole file when it shrank), parsed; unreadable lines skipped."
  [file offset]
  (let [size (file-size file)
        start (if (< size offset) 0 offset)]
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

(defn await-event
  "Polls the log from offset until an event matching pattern with :time-ms >= since-ms comes; resolves to it or nil
  after limit-ms."
  [opts offset pattern since-ms limit-ms]
  (let [until (+ (js/Date.now) limit-ms)]
    (letfn [(poll []
              (let [hit (some #(when (and (>= (:time-ms % 0) since-ms) (x/matches? pattern %)) %)
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

(def bodies (atom {}))

(defn running? [{:keys [body]}]
  (str/includes? (or (.-stdout (cp/spawnSync "pgrep" #js ["-af" (str "out/body.cjs --agent " body " ")] #js {:encoding "utf8"})) "")
                 (str "--agent " body " ")))

(defn last-plot-file [opts] (path/join (os/tmpdir) (str "world-test-" (:body opts)) "last-origin.edn"))

(defn note-last-plot!
  "Records the plot the body is about to be put on, so a later start can clear it first (the body is saved there)."
  [opts origin]
  (fs/mkdirSync (path/dirname (last-plot-file opts)) #js {:recursive true})
  (fs/writeFileSync (last-plot-file opts) (pr-str origin)))

(defn reset-last-plot!
  "Resolves once the plot the body was last left on is cleared (its traps gone), before the body starts."
  [opts]
  (let [file (last-plot-file opts)
        origin (when (fs/existsSync file)
                 (try (reader/read-string (fs/readFileSync file "utf8")) (catch :default _ nil)))]
    (if (vector? origin)
      (rcon! (f/reset-plot-commands f/default-grid origin))
      (js/Promise.resolve nil))))

(defn start-body!
  "Starts the body with a scenario holding register, --fresh; resolves once it logged :system :started. The body's own
  engine/memory.edn is deleted first unless keep-memory? (--fresh only drops engine.edn)."
  [opts register keep-memory?]
  (when-not keep-memory?
    (doseq [name f/clean-start-files
            :let [file (path/join (body-dir opts) "engine" name)]
            :when (fs/existsSync file)]
      (fs/unlinkSync file)))
  (let [dir (path/join (os/tmpdir) (str "world-test-" (:body opts)))
        scenario (path/join dir "scenario.edn")
        out (path/join dir "body.log")
        since (js/Date.now)
        offset (file-size (events-file opts))]
    (fs/mkdirSync dir #js {:recursive true})
    (fs/writeFileSync scenario (pr-str {:register register :queue []}))
    (let [fd (fs/openSync out "a")
          child (cp/spawn "node" #js ["out/body.cjs" "--agent" (:body opts) "--world" (:world opts) "--scenario" scenario "--fresh"]
                          #js {:cwd (repo-path "engine") :stdio #js ["ignore" fd fd]})]
      (swap! bodies assoc (:body opts) child)
      (.then (await-event opts offset {:source :system :kind :started} since 90000)
             (fn [ev]
               (when-not ev (throw (js/Error. (str "the body did not start within 90 s (see " out ")"))))
               (sleep 2000))))))

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

;; ------------------------------------------------------------------ plot leases (parallel runners)

(def lease-dir (path/join (os/tmpdir) "world-test-plot-leases"))

(defn lease-file [i] (path/join lease-dir (str "plot-" i ".lease")))

(defn pid-alive? [pid]
  (try (.kill js/process pid 0) true
       (catch :default e (= "EPERM" (.-code e)))))

(defn read-holder [i]
  (try (let [n (js/parseInt (str/trim (fs/readFileSync (lease-file i) "utf8")) 10)] (when-not (js/isNaN n) n))
       (catch :default _ nil)))

(defn acquire-plot!
  "Leases the first free plot index from `first` (exclusive lease file holding this PID; leases of dead PIDs are
  reclaimed), so runners started together never share a plot."
  [first]
  (fs/mkdirSync lease-dir #js {:recursive true})
  (lease/acquire
   {:create! (fn [i pid] (try (fs/writeFileSync (lease-file i) (str pid) #js {:flag "wx"}) true
                              (catch :default e (if (= "EEXIST" (.-code e)) false (throw e)))))
    :holder read-holder
    :reclaim! (fn [i] (try (fs/unlinkSync (lease-file i)) (catch :default _ nil)))
    :alive? pid-alive?}
   {:pid (.-pid js/process) :first first :total (* (:cols f/default-grid) (:rows f/default-grid))}))

(defn release-plot! [i]
  (when (= (.-pid js/process) (read-holder i))
    (try (fs/unlinkSync (lease-file i)) (catch :default _ nil))))

;; ------------------------------------------------------------------ time lock

(def time-lock-dir (path/join (os/tmpdir) "mc-time-lock"))
(def time-guard-file (path/join (os/tmpdir) "mc-time-lock.guard"))

(defn pause-sync [ms] (js/Atomics.wait (js/Int32Array. (js/SharedArrayBuffer. 4)) 0 0 ms))

(defn with-guard
  "Runs thunk holding a short-lived exclusive guard file (a dead holder's guard is reclaimed)."
  [thunk]
  (let [pid (.-pid js/process)]
    (loop [tries 0]
      (let [made (try (fs/writeFileSync time-guard-file (str pid) #js {:flag "wx"}) true
                      (catch :default e (if (= "EEXIST" (.-code e)) false (throw e))))]
        (if made
          (try (thunk) (finally (try (fs/unlinkSync time-guard-file) (catch :default _ nil))))
          (let [h (try (js/parseInt (str/trim (fs/readFileSync time-guard-file "utf8")) 10) (catch :default _ nil))]
            (when (and h (not (js/isNaN h)) (not (pid-alive? h)))
              (try (fs/unlinkSync time-guard-file) (catch :default _ nil)))
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

(defn acquire-time-lock!
  "Resolves to {:first? bool} when this process holds the time lock for phase (:day or :night), logging who it waits for
  (once per holder set). Holders of the same phase share it."
  [phase what]
  (let [pid (.-pid js/process)
        ops (time-lock-dir-ops)
        seq (js/Date.now)]
    (letfn [(attempt [told]
              (let [r (lease/try-share ops pid phase seq)]
                (if (:held r)
                  (js/Promise.resolve {:first? (:first? r)})
                  (do (when-not (= told (:waiting-on r))
                        (log! "waiting for time lock (" (name phase) ") held by " (str/join "," (:waiting-on r)) " (" what ")"))
                      (.then (sleep 2000) #(attempt (:waiting-on r)))))))]
      (attempt nil))))

(defn with-time-lock!
  "Runs thunk (a promise-returning fn) holding the time lock for phase; always releases it."
  [phase what thunk]
  (.then (acquire-time-lock! phase what)
         (fn [_] (.finally (thunk) release-time-lock!))))

(defn time-set-main
  "argv: ticks or day|noon|night|midnight -> promise of the exit code; one `time set` under the time lock."
  [argv]
  (let [t (first (array-seq argv))]
    (if-not (and t (re-matches #"day|noon|night|midnight|\d+" t))
      (do (js/console.error "usage: node tools/time-set.mjs <ticks|day|noon|night|midnight>") (js/Promise.resolve 2))
      (with-time-lock! (lease/phase-of-ticks (case t "day" 1000 "noon" 6000 "night" 14000 "midnight" 18000 (js/parseInt t 10)))
        (str "time set " t)
        #(.then (rcon! [(str "time set " t)]) (fn [[reply]] (log! reply) 0))))))

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

(defn write-plans! [opts c]
  (doall (for [plan (:plans c)]
           (let [file (path/join (plans-dir opts) (str (plan-prefix opts) (:id plan) ".edn"))]
             (fs/writeFileSync file (f/plan-file-text plan (plan-prefix opts)))
             file))))

(defn submit-job!
  "Submits spec with tools/jobs.mjs; resolves to the job id or throws with the tool's answer."
  [opts spec flags]
  (.then (exec-file (into ["engine/tools/jobs.mjs" (:body opts) "--world" (:world opts) "submit" (pr-str spec)] flags))
         (fn [{:keys [out]}]
           (if-let [[_ id] (re-find #":id \"(j\d+)\"" out)]
             id
             (throw (js/Error. (str "submit refused: " (str/trim out))))))))

(defn run-steps!
  "Runs the act steps in order; resolves to the set of submitted job ids."
  [opts origin c offset t0]
  (reduce (fn [p [op a b :as step]]
            (.then p (fn [ids]
                       (case op
                         :summon (.then (rcon! [(f/summon-command origin step)]) (constantly ids))
                         :rcon (.then (rcon! [(f/substitute a (:body opts) origin)]) (constantly ids))
                         :kill-body (.then (rcon! [(str "kill " (:body opts))]) (constantly ids))
                         :wait-s (.then (sleep (* 1000 a)) (constantly ids))
                         :time-set (.then (set-time! opts a (str (:id c) " step"))
                                          (fn [ok] (if ok ids (throw (js/Error. "a :time-set step needs --allow-time")))))
                         :await (.then (await-event opts offset a t0 (* 1000 b))
                                       (fn [ev] (if ev ids (throw (js/Error. (str ":await " (pr-str a) " timed out after " b " s"))))))
                         :job (.then (submit-job! opts a (vec (map #(str "--" (name %)) b))) #(conj ids %))))))
          (js/Promise.resolve #{})
          (:act c)))

(defn watch!
  "Polls the log until every expectation is decided or the case's limit; resolves to the judged results."
  [opts c offset t0 ids]
  (let [limit (+ t0 (* 1000 (max (:limit-s c) (+ 2 (x/deadline-s (:expect c))))))]
    (letfn [(poll []
              (let [now (js/Date.now)
                    events (filterv #(>= (:time-ms % 0) t0) (read-events-from (events-file opts) offset))
                    results (x/judge-all (:expect c) events {:t0-ms t0 :now-ms now :job-ids ids})]
                (if (or (x/decided? results) (> now limit))
                  (js/Promise.resolve (mapv #(if (= :pending (:status %)) (assoc % :status :fail :evidence "undecided at the case's limit") %) results))
                  (.then (sleep 500) poll))))]
      (poll))))

(defn after-checks! [opts origin c]
  (let [cmds (mapv #(f/after-command origin (:body opts) f/default-grid c %) (:after c))]
    (.then (rcon! cmds) (fn [replies] (mapv #(f/judge-after origin %1 %2) (:after c) replies)))))

(defn run-case!
  "One run of case c on plot i; resolves to a result map."
  [opts c i run]
  (let [origin (f/plot-origin f/default-grid i)
        rc (f/resolve-tags c origin)
        started (js/Date.now)
        plan-files (atom [])
        _ (note-last-plot! opts origin)
        result (fn [m] (merge {:id (:id c) :run run :plot i :origin origin :elapsed-s (/ (- (js/Date.now) started) 1000)} m))]
    (-> (if-let [phase (lease/time-phase rc)]
          (acquire-time-lock! phase (str (:id c) " depends on the time of day"))
          (js/Promise.resolve nil))
        (.then (fn [held] (if (or (nil? held) (:first? held)) (time-ok! opts rc) true)))
        (.then (fn [ok]
                 (if-not ok
                   (result {:status :skipped :why (str "needs " (name (:time rc)) " (no --allow-time)")})
                   (-> (rcon! (f/setup-commands f/default-grid origin rc))
                       (.then #(sleep 1000))
                       (.then #(rcon! (f/block-commands origin rc)))
                       (.then #(reset! plan-files (write-plans! opts rc)))
                       (.then #(rcon! (f/body-commands origin (:body opts) rc)))
                       (.then #(sleep (* 1000 (get-in rc [:body :settle-s]))))
                       (.then (fn []
                                (let [offset (file-size (events-file opts))
                                      t0 (js/Date.now)]
                                  (-> (run-steps! opts origin rc offset t0)
                                      (.then (fn [ids]
                                               (.then (watch! opts rc offset t0 ids)
                                                      (fn [expects]
                                                        (.then (after-checks! opts origin rc)
                                                               (fn [afters]
                                                                 (result {:status (if (and (x/passed? expects) (every? :pass? afters)) :pass :fail)
                                                                          :expects expects :afters afters})))))))))))))))
        (.catch (fn [e] (result {:status :error :why (.-message e)})))
        (.then (fn [r]
                 (-> (exec-file ["engine/tools/jobs.mjs" (:body opts) "--world" (:world opts) "cancel-all"])
                     (.then #(rcon! (f/cleanup-commands f/default-grid origin (:body opts) rc)))
                     (.then (fn [_] (run! #(when (fs/existsSync %) (fs/unlinkSync %)) @plan-files) r))
                     (.catch (fn [_] (run! #(when (fs/existsSync %) (fs/unlinkSync %)) @plan-files) r)))))
        (.finally release-time-lock!))))

;; ------------------------------------------------------------------ reporting

(defn expect-line [{:keys [expect status evidence at-s]}]
  (str "    " (if (= :pass status) "ok  " "FAIL") " "
       (if (:event expect) (str "event " (pr-str (:event expect)) " within " (:within-s expect) " s")
           (str "no event " (pr-str (:no-event expect)) " for " (:for-s expect) " s"))
       (when evidence (str " -> " evidence)) (when at-s (str " at " (.toFixed at-s 1) " s"))))

(defn report! [r]
  (log! (str/upper-case (name (:status r))) " " (:id r) " #" (:run r) " (plot " (:plot r) ", " (.toFixed (:elapsed-s r) 1) " s)"
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

(defn run-all! [opts cases]
  (let [groups (group-by :register cases)
        results (atom [])]
    (-> (reduce (fn [p [register group]]
                  (.then p (fn []
                             (-> (reduce (fn [p2 [n [c run]]]
                                           (.then p2 (fn []
                                                       (let [plan (f/body-start-plan c (zero? n))]
                                                         (-> (if (= :keep plan)
                                                               (js/Promise.resolve nil)
                                                               (-> (stop-body! opts)
                                                                   (.then #(when-not (= :restart-keep plan) (reset-last-plot! opts)))
                                                                   (.then #(start-body! opts register (= :restart-keep plan)))))
                                                             (.then (fn []
                                                                      (let [i (acquire-plot! (:first-plot opts))]
                                                                        (-> (run-case! opts c i run)
                                                                            (.then (fn [r] (report! r) (swap! results conj r)))
                                                                            (.finally #(release-plot! i)))))))))))
                                         (js/Promise.resolve nil)
                                         (map-indexed vector (for [run (range 1 (inc (:repeat opts))) c group] [c run])))
                                 (.finally #(stop-body! opts))))))
                (js/Promise.resolve nil)
                groups)
        (.then (fn [] @results)))))

(defn main
  "argv (array) -> promise of the exit code."
  [argv]
  (-> (js/Promise.resolve nil)
      (.then (fn []
               (let [opts (parse-args (array-seq argv))
                     cases (f/select-cases (load-cases (:paths opts)) opts)
                     bad (filter :problems cases)]
                 (cond
                   (:list opts) (do (run! #(log! (:id %) "  " (pr-str (:tags %)) (when (:problems %) (str "  PROBLEMS " (:problems %)))) cases) 0)
                   (empty? cases) (do (log! "no cases selected") 2)
                   (seq bad) (do (run! #(log! (:id %) ": " (str/join "; " (:problems %))) bad) 2)
                   (running? opts) (do (log! "the body " (:body opts) " already runs; stop it first") 2)
                   :else
                   (.then (players-near opts)
                          (fn [refusal]
                            (if refusal
                              (do (log! refusal) 2)
                              (-> (ensure-body! opts)
                                  (.then #(lease-body! opts))
                                  (.then #(run-all! opts cases))
                                  (.then (fn [results]
                                           (when-let [file (:results opts)] (fs/writeFileSync file (pr-str results)))
                                           (let [n (frequencies (map :status results))]
                                             (log! "world-test: " (count results) " runs, " (n :pass 0) " passed, " (n :fail 0) " failed, "
                                                   (n :error 0) " errors, " (n :skipped 0) " skipped")
                                             (if (= (count results) (n :pass 0)) 0 1))))
                                  (.finally #(release-body! opts))))))))))
      (.catch (fn [e] (js/console.error (.-message e)) (js/console.error usage) 2))))
