(ns world-test.sweep
  "tools/world-sweep.mjs: one world sweep in one command: the passes day, night and night-exclusive of tools/world-test.mjs (the pool),
  then `tools/time-set.mjs day`, and one summary EDN. A pass that exits 6 (a body died at boot repeatedly), 5 (players near the plot grid) or 4 (--min-bodies timeout) stops the
  sweep; the final time-set (which also restores a leftover sleep gamerule of a dead run) runs anyway. Pure parts: parse-args, passes, counts, run-sweep."
  (:require ["child_process" :as cp]
            ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            [cljs.reader :as reader]
            [clojure.string :as str]
            [engine.test-events :as ev]))

(def usage
  (str "usage: node tools/world-sweep.mjs [--dir DIR] [--bodies 16] [--min-bodies 8] [--no-night-batches] [--durations RESULTS.edn ...] [--match-ids FILE] [--run-known] [--unskip-ids FILE] [--unskip-id ID] [--retry-failed 1] [fixture paths ...]\n"
       "a case whose last result in --durations was a failure is skipped (:skipped-known) unless named by --match-ids; --run-known runs those too; --unskip-ids FILE (one case id per line) runs just those known failures without limiting the run.\n"
       "runs --phase day, night, night-exclusive (--allow-time, --time-log DIR/time-log-PASS.txt, --results DIR/sweep-PASS.edn), then time-set day; writes DIR/sweep-summary.edn.\n"
       "INT/TERM go to the running pass, then time-set day runs and the exit is 130/143.\n"
       "exit: 6 / 5 / 4 when a pass stopped the sweep with it, else 1 when any pass had failures or errors, else 0."))

(defn parse-args
  "argv (seq of strings) -> options map."
  [argv]
  (loop [[a & more :as args] argv, o {:bodies "16" :min-bodies "8" :retry-failed "1" :night-batches true :durations [] :paths []}]
    (cond
      (empty? args) o
      (= a "--dir") (recur (rest more) (assoc o :dir (first more)))
      (= a "--bodies") (recur (rest more) (assoc o :bodies (first more)))
      (= a "--min-bodies") (recur (rest more) (assoc o :min-bodies (first more)))
      (= a "--retry-failed") (recur (rest more) (assoc o :retry-failed (first more)))
      (= a "--match-ids") (recur (rest more) (assoc o :match-ids (first more)))
      (= a "--night-batches") (recur more (assoc o :night-batches true))
      (= a "--unskip-ids") (recur (rest more) (assoc o :unskip-ids (first more)))
      (= a "--unskip-id") (recur (rest more) (update o :unskip-id (fnil conj []) (first more)))
      (= a "--run-known") (recur more (assoc o :run-known true))
      (= a "--no-night-batches") (recur more (assoc o :night-batches false))
      (= a "--durations") (let [[files rest-args] (split-with #(not (str/starts-with? % "--")) more)]
                            (recur rest-args (update o :durations into files)))
      :else (recur more (update o :paths conj a)))))

(def default-path "engine/fixtures/world")

(defn passes
  "Options -> the three passes [{:name :argv :results :time-log}] in order; argv is for tools/world-test.mjs. The fixture paths (default engine/fixtures/world) come before the flags: the pool reads every word after --durations as a durations file."
  [{:keys [dir bodies min-bodies retry-failed night-batches durations match-ids unskip-ids unskip-id run-known paths] :or {dir "."}}]
  (for [pass [:day :night :night-exclusive]
        :let [n (name pass)
              results (str dir "/sweep-" n ".edn")
              pool (cond
                     (= pass :night-exclusive) (if night-batches ["--bodies" bodies "--night-batches"] ["--bodies" "1"])
                     (= pass :day) ["--bodies" bodies "--min-bodies" min-bodies]
                     :else ["--bodies" bodies])]]
    {:name pass :results results
     :argv (vec (concat ["--phase" n] pool (or (seq paths) [default-path])
                        ["--allow-time" "--time-log" (str dir "/time-log-" n ".txt") "--results" results
                         "--retry-failed" retry-failed]
                        (when (seq durations) (cons "--durations" durations))
                        (when match-ids ["--match-ids" match-ids])
                        (when unskip-ids ["--unskip-ids" unskip-ids])
                        (mapcat #(vector "--unskip-id" %) unskip-id)
                        (when run-known ["--run-known"])))}))

(defn counts
  "Results vector text (or nil) -> {:pass :fail :flaky :error ... :total}."
  [text]
  (let [rs (try (some-> text reader/read-string) (catch :default _ nil))
        by (frequencies (map :status rs))]
    (cond-> {:total (count rs)}
      (seq rs) (merge {:pass 0 :fail 0 :flaky 0 :error 0} by))))

(def stop-codes #{4 5 6})

(def signal-codes {"SIGINT" 130 "SIGTERM" 143})

(defn signal-exit [sig] (get signal-codes sig 1))

(defn- stop-why [name code]
  (str "pass " (clojure.core/name name) " exited " code
       (case code 6 " (a body died at boot repeatedly)" 5 " (players near the plot grid, precheck)" 4 " (--min-bodies wait timed out)" "")))

(defn failing-ids
  "Results vector text (or nil) -> ids of the :fail and :error cases."
  [text]
  (let [rs (try (some-> text reader/read-string) (catch :default _ nil))]
    (vec (keep #(when (#{:fail :error} (:status %)) (:id %)) rs))))

(defn- safe-promise
  "(f) as a promise that rejects instead of throwing."
  [f]
  (try (js/Promise.resolve (f)) (catch :default e (js/Promise.reject e))))

(defn run-sweep
  "deps: :run-pass! (pass -> promise of {:code :results}), :time-set! (-> promise of code), :now (ms), :emit! (@@test event map),
  :interrupted (-> signal name or nil; optional; stops the remaining passes). A pass that throws or rejects counts as code 1 with its :error;
  the time-set runs in every case.
  -> promise of {:passes [{:name :code :wall-min :counts :results-file}] :exit :stopped :interrupted :time-set}."
  [ps {:keys [run-pass! time-set! now emit! interrupted] :or {interrupted (constantly nil)}}]
  (emit! {:event "plan" :total (count ps)})
  (letfn [(finish [r] (js/Promise.resolve r))
          (go [[p & more] done]
            (if (or (not p) (interrupted))
              (finish done)
              (let [t0 (now)]
                (.then (.catch (safe-promise #(run-pass! p))
                               (fn [e] {:code 1 :error (str "pass " (name (:name p)) ": " (or (.-message e) e))}))
                       (fn [{:keys [code results error]}]
                         (let [r (cond-> {:name (:name p) :code code :wall-min (/ (- (now) t0) 60000.0)
                                          :counts (counts results) :results-file (:results p)}
                                   error (assoc :error error))
                               done (conj done r)
                               failing (failing-ids results)]
                           (emit! (cond-> {:event "result" :name (str "sweep:" (name (:name p))) :outcome (if (zero? code) "passed" "failed")}
                                    (pos? code) (assoc :message (str (cond error error
                                                                         (stop-codes code) (stop-why (:name p) code)
                                                                         :else (str "exit " code))
                                                                     ", " (pr-str (:counts r))
                                                                     (when (seq failing) (str ", failing: " (str/join " " failing)))))))
                           (if (stop-codes code)
                             (finish done)
                             (go more done))))))))]
    (.then (.catch (go ps []) (fn [e] (js/console.error (str "world-sweep: " (or (.-message e) e))) []))
           (fn [done]
             (let [stop (first (filter (comp stop-codes :code) done))
                   sig (interrupted)]
               (.then (.catch (safe-promise time-set!) (fn [e] (js/console.error (str "world-sweep: time-set: " (or (.-message e) e))) 1))
                      (fn [ts]
                        {:passes done
                         :exit (cond sig (signal-exit sig) stop (:code stop) (some (comp pos? :code) done) 1 (pos? ts) ts :else 0)
                         :stopped (when stop (stop-why (:name stop) (:code stop)))
                         :interrupted sig
                         :time-set ts})))))))

(defn forward-signals!
  "On INT/TERM of `proc`: record the signal in the `interrupted` atom and pass it to the child in the `current` atom (if any); a child still
  there after grace-ms gets SIGKILL, so the final time-set is not held up for ever."
  [proc current interrupted grace-ms]
  (doseq [sig ["SIGINT" "SIGTERM"]]
    (.on proc sig
         (fn []
           (reset! interrupted sig)
           (when-let [child @current]
             (.kill child sig)
             (let [timer (js/setTimeout #(when (identical? child @current) (.kill child "SIGKILL")) grace-ms)]
               (when (.-unref timer) (.unref timer))))))))

;; ---- node edge

(defn- repo [] (or js/process.env.WORLD_TEST_REPO "."))

(def event-prefix "@@test ")

(defn pass-line
  "One stdout line of a pass's child -> the line to print. Its @@test plan and phase lines pass (plans add up in live-tests), a result is named
  `pass:name`, a progress line is dropped (its total is the pass's own); other lines pass unchanged."
  [pass line]
  (let [at (str/index-of line event-prefix)
        e (when at (try (js->clj (js/JSON.parse (subs line (+ at (count event-prefix)))) :keywordize-keys true) (catch :default _ nil)))]
    (case (:event e)
      "progress" nil
      "result" (str (subs line 0 at) event-prefix (js/JSON.stringify (clj->js (update e :name #(str (name pass) ":" %)))))
      line)))

(defn- relay-lines!
  "Prints the complete lines of `stream` through (f line) as they arrive."
  [stream f]
  (let [buf (atom "")
        out! (fn [l] (when-let [x (f l)] (js/console.log x)))]
    (.setEncoding stream "utf8")
    (.on stream "data" (fn [chunk]
                         (let [lines (str/split (str @buf chunk) #"\n" -1)]
                           (reset! buf (peek lines))
                           (run! out! (pop lines)))))
    (.on stream "end" #(when (seq @buf) (out! @buf)))))

(defn- spawn-node!
  "Runs `node tools/SCRIPT args` with the output passed through. With `pass` the child keeps TEST_EVENTS and its @@test lines are relayed through `pass-line`
  (case progress over all passes); else none. `current` holds the running child. -> promise of the exit code."
  [script args current pass]
  (js/Promise.
   (fn [resolve _]
     (let [env (js/Object.assign #js {} js/process.env)
           _ (when-not pass (js-delete env "TEST_EVENTS"))
           child (cp/spawn js/process.execPath (clj->js (into [(path/join (repo) "tools" script)] args))
                           #js {:stdio (if pass #js ["inherit" "pipe" "inherit"] "inherit") :env env})]
       (when pass (relay-lines! (.-stdout child) #(pass-line pass %)))
       (reset! current child)
       (.on child "error" (fn [e] (reset! current nil) (js/console.error (str script ": " (.-message e))) (resolve 1)))
       (.on child "close" (fn [code] (reset! current nil) (resolve (or code 1))))))))

(defn- read-results [file]
  (try (str (fs/readFileSync file "utf8")) (catch :default _ nil)))

(defn main
  "argv (array) -> promise of the exit code."
  [argv]
  (let [args (vec (array-seq argv))]
    (if (some #{"--help" "-h"} args)
      (do (js/console.log usage) (js/Promise.resolve 0))
      (let [o (parse-args args)
            dir (or (:dir o) (fs/mkdtempSync (path/join (os/tmpdir) "world-sweep-")))
            ps (passes (assoc o :dir dir))
            _ (fs/mkdirSync dir #js {:recursive true})
            emit! (fn [m] (when ev/enabled? (ev/emit! m)))
            current (atom nil)
            interrupted (atom nil)
            _ (forward-signals! js/process current interrupted 60000)]
        (js/console.error (str "world-sweep: results in " dir))
        (.then (run-sweep ps {:run-pass! (fn [p]
                                           (js/console.error (str "world-sweep: pass " (name (:name p))))
                                           (.then (spawn-node! "world-test.mjs" (:argv p) current (:name p))
                                                  (fn [code] {:code code :results (read-results (:results p))})))
                              :time-set! #(spawn-node! "time-set.mjs" ["day"] (atom nil) nil)
                              :interrupted #(deref interrupted)
                              :now #(js/Date.now)
                              :emit! emit!})
               (fn [r]
                 (let [file (str dir "/sweep-summary.edn")]
                   (fs/writeFileSync file (pr-str (assoc r :dir dir)))
                   (js/console.error (str "world-sweep: " (when (:stopped r) (str (:stopped r) "; "))
                                          (str/join ", " (map #(str (name (:name %)) " " (pr-str (:counts %)) " " (.toFixed (:wall-min %) 1) " min") (:passes r)))
                                          "; time-set day " (:time-set r) "; summary " file))
                   (:exit r))))))))
