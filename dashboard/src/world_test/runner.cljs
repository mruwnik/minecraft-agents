(ns world-test.runner
  "tools/world-test.mjs: runs world fixtures (engine/fixtures/world/*.edn, format in its README) against the live
  test server with one probe body. Per case and run: a fresh plot of the reserved grid is cleared and built, the body
  is put at its start, the act steps run, the body's event log is read until every expectation is decided, the
  :after checks ask the server, and the plot is cleaned up. The body is started once per distinct :register (the
  scenario) and stopped at the end. Pure parts: world-test.fixture and world-test.expect."
  (:require ["child_process" :as cp]
            ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            [cljs.reader :as reader]
            [clojure.string :as str]
            [dashboard.rcon :as rcon]
            [world-test.expect :as x]
            [world-test.fixture :as f]))

(def usage
  (str "usage: node tools/world-test.mjs [fixture.edn|dir ...] [--tag T] [--match TEXT] [--repeat N] [--body NAME]\n"
       "         [--world claude] [--first-plot I] [--allow-time --time-log FILE] [--results FILE] [--list]\n"
       "Runs world fixtures (default dir engine/fixtures/world) on the reserved plot grid x/z 20000..20640, y 150.\n"
       "--allow-time lets a case that needs night or day set the time (each set appended to --time-log); without it\n"
       "such a case is skipped. Exit code 0 when every run passed, 1 when one failed, 2 on a usage or setup error."))

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

(defn start-body!
  "Starts the body with a scenario holding register, --fresh; resolves once it logged :system :started."
  [opts register]
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

(defn set-time!
  "Sets the time when allowed (and logs it); resolves to true, or false when not allowed."
  [opts ticks why]
  (if-not (:allow-time opts)
    (js/Promise.resolve false)
    (.then (rcon! [(str "time set " ticks)])
           (fn [_]
             (when-let [file (:time-log opts)]
               (fs/appendFileSync file (str (.toISOString (js/Date.)) " time set " ticks " by world-test " (:body opts)
                                            " for card 17342482 (" why ")\n")))
             true))))

(defn time-ok!
  "Whether the server's time suits case c, setting it first when allowed."
  [opts c]
  (if (= :any (:time c))
    (js/Promise.resolve true)
    (.then (rcon! ["time query day"])
           (fn [[reply]]
             (let [want-night (= :night (:time c))]
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
        result (fn [m] (merge {:id (:id c) :run run :plot i :origin origin :elapsed-s (/ (- (js/Date.now) started) 1000)} m))]
    (-> (time-ok! opts rc)
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
                     (.catch (fn [_] (run! #(when (fs/existsSync %) (fs/unlinkSync %)) @plan-files) r))))))))

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

(defn players-near
  "Resolves to the reply of the check for other players within 500 blocks of the grid's centre."
  [opts]
  (let [[x y z] (f/grid-centre f/default-grid)]
    (.then (rcon! [(str "execute if entity @a[name=!" (:body opts) ",x=" x ",y=" y ",z=" z ",distance=..500]")]) first)))

(defn run-all! [opts cases]
  (let [groups (group-by :register cases)
        plot (atom (:first-plot opts))
        results (atom [])]
    (-> (reduce (fn [p [register group]]
                  (.then p (fn []
                             (-> (start-body! opts register)
                                 (.then (fn []
                                          (reduce (fn [p2 [c run]]
                                                    (.then p2 (fn []
                                                                (let [i @plot]
                                                                  (swap! plot inc)
                                                                  (.then (run-case! opts c i run)
                                                                         (fn [r] (report! r) (swap! results conj r)))))))
                                                  (js/Promise.resolve nil)
                                                  (for [run (range 1 (inc (:repeat opts))) c group] [c run]))))
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
                          (fn [reply]
                            (if (str/includes? reply "Test passed")
                              (do (log! "another player is within 500 blocks of the plot grid: " reply) 2)
                              (-> (ensure-body! opts)
                                  (.then #(run-all! opts cases))
                                  (.then (fn [results]
                                           (when-let [file (:results opts)] (fs/writeFileSync file (pr-str results)))
                                           (let [n (frequencies (map :status results))]
                                             (log! "world-test: " (count results) " runs, " (n :pass 0) " passed, " (n :fail 0) " failed, "
                                                   (n :error 0) " errors, " (n :skipped 0) " skipped")
                                             (if (= (count results) (n :pass 0)) 0 1))))))))))))
      (.catch (fn [e] (js/console.error (.-message e)) (js/console.error usage) 2))))
