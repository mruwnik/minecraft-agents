(ns world-test.sweep
  "tools/world-sweep.mjs: one world sweep in one command: the passes day, night and night-exclusive of tools/world-test.mjs (the pool),
  then `tools/time-set.mjs day`, and one summary EDN. A pass that exits 5 (players near the plot grid) or 4 (--min-bodies timeout) stops the
  sweep; the final time-set (which also restores a leftover sleep gamerule of a dead run) runs anyway. Pure parts: parse-args, passes, counts, run-sweep."
  (:require ["child_process" :as cp]
            ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            [cljs.reader :as reader]
            [clojure.string :as str]
            [engine.test-events :as ev]))

(def usage
  (str "usage: node tools/world-sweep.mjs [--dir DIR] [--bodies 16] [--min-bodies 8] [--no-night-batches] [--durations RESULTS.edn ...] [--match-ids FILE] [--retry-failed 1] [fixture paths ...]\n"
       "runs --phase day, night, night-exclusive (--allow-time, --time-log DIR/time-log-PASS.txt, --results DIR/sweep-PASS.edn), then time-set day; writes DIR/sweep-summary.edn.\n"
       "exit: 5 / 4 when a pass stopped the sweep with it, else 1 when any pass had failures or errors, else 0."))

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
      (= a "--no-night-batches") (recur more (assoc o :night-batches false))
      (= a "--durations") (let [[files rest-args] (split-with #(not (str/starts-with? % "--")) more)]
                            (recur rest-args (update o :durations into files)))
      :else (recur more (update o :paths conj a)))))

(defn passes
  "Options -> the three passes [{:name :argv :results :time-log}] in order; argv is for tools/world-test.mjs."
  [{:keys [dir bodies min-bodies retry-failed night-batches durations match-ids paths] :or {dir "."}}]
  (for [pass [:day :night :night-exclusive]
        :let [n (name pass)
              results (str dir "/sweep-" n ".edn")
              pool (cond
                     (= pass :night-exclusive) (if night-batches ["--bodies" bodies "--night-batches"] ["--bodies" "1"])
                     (= pass :day) ["--bodies" bodies "--min-bodies" min-bodies]
                     :else ["--bodies" bodies])]]
    {:name pass :results results
     :argv (vec (concat ["--phase" n] pool
                        ["--allow-time" "--time-log" (str dir "/time-log-" n ".txt") "--results" results
                         "--retry-failed" retry-failed]
                        (when (seq durations) (cons "--durations" durations))
                        (when match-ids ["--match-ids" match-ids])
                        paths))}))

(defn counts
  "Results vector text (or nil) -> {:pass :fail :flaky :error ... :total}."
  [text]
  (let [rs (try (some-> text reader/read-string) (catch :default _ nil))
        by (frequencies (map :status rs))]
    (cond-> {:total (count rs)}
      (seq rs) (merge {:pass 0 :fail 0 :flaky 0 :error 0} by))))

(def stop-codes #{4 5})

(defn- stop-why [name code]
  (str "pass " (clojure.core/name name) " exited " code
       (case code 5 " (players near the plot grid, precheck)" 4 " (--min-bodies wait timed out)" "")))

(defn run-sweep
  "deps: :run-pass! (pass -> promise of {:code :results}), :time-set! (-> promise of code), :now (ms), :emit! (@@test event map).
  -> promise of {:passes [{:name :code :wall-min :counts :results-file}] :exit :stopped :time-set}."
  [ps {:keys [run-pass! time-set! now emit!]}]
  (emit! {:event "plan" :total (count ps)})
  (letfn [(go [[p & more] done]
            (if-not p
              (js/Promise.resolve done)
              (let [t0 (now)]
                (.then (run-pass! p)
                       (fn [{:keys [code results]}]
                         (let [r {:name (:name p) :code code :wall-min (/ (- (now) t0) 60000.0)
                                  :counts (counts results) :results-file (:results p)}
                               done (conj done r)]
                           (emit! (cond-> {:event "result" :name (str "sweep:" (name (:name p))) :outcome (if (zero? code) "passed" "failed")}
                                    (pos? code) (assoc :message (str (if (stop-codes code) (stop-why (:name p) code) (str "exit " code)) ", " (pr-str (:counts r))))))
                           (emit! {:event "progress" :done (count done) :total (count ps) :unit "passes"})
                           (if (stop-codes code)
                             (js/Promise.resolve done)
                             (go more done))))))))]
    (.then (go ps [])
           (fn [done]
             (let [stop (first (filter (comp stop-codes :code) done))]
               (.then (time-set!)
                      (fn [ts]
                        {:passes done
                         :exit (cond stop (:code stop) (some (comp pos? :code) done) 1 (pos? ts) ts :else 0)
                         :stopped (when stop (stop-why (:name stop) (:code stop)))
                         :time-set ts})))))))

;; ---- node edge

(defn- repo [] (or js/process.env.WORLD_TEST_REPO "."))

(defn- spawn-node!
  "Runs `node tools/SCRIPT args` with the output passed through; no TEST_EVENTS in the child (the sweep prints the events). -> promise of the exit code."
  [script args]
  (js/Promise.
   (fn [resolve _]
     (let [env (js/Object.assign #js {} js/process.env)
           _ (js-delete env "TEST_EVENTS")
           child (cp/spawn js/process.execPath (clj->js (into [(path/join (repo) "tools" script)] args))
                           #js {:stdio "inherit" :env env})]
       (.on child "error" (fn [e] (js/console.error (str script ": " (.-message e))) (resolve 1)))
       (.on child "close" (fn [code] (resolve (or code 1))))))))

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
            emit! (fn [m] (when ev/enabled? (ev/emit! m)))]
        (js/console.error (str "world-sweep: results in " dir))
        (.then (run-sweep ps {:run-pass! (fn [p]
                                           (js/console.error (str "world-sweep: pass " (name (:name p))))
                                           (.then (spawn-node! "world-test.mjs" (:argv p))
                                                  (fn [code] {:code code :results (read-results (:results p))})))
                              :time-set! #(spawn-node! "time-set.mjs" ["day"])
                              :now #(js/Date.now)
                              :emit! emit!})
               (fn [r]
                 (let [file (str dir "/sweep-summary.edn")]
                   (fs/writeFileSync file (pr-str (assoc r :dir dir)))
                   (js/console.error (str "world-sweep: " (when (:stopped r) (str (:stopped r) "; "))
                                          (str/join ", " (map #(str (name (:name %)) " " (pr-str (:counts %)) " " (.toFixed (:wall-min %) 1) " min") (:passes r)))
                                          "; time-set day " (:time-set r) "; summary " file))
                   (:exit r))))))))
