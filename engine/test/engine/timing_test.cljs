(ns engine.timing-test
  "Not a test: test-run hooks. When MC_TEST_TIMINGS names a file, appends a {:started} line at load, one JSON line per test var ({:var :ms :cpu-ms}: wall and the process's CPU, which load does not inflate) and the process peak RSS on exit (tools/test-shards.mjs reads them).
  When TEST_EVENTS=1, prints the live-tests @@test lines: plan (var count), one result per var, progress per namespace.
  After every test var the server rules are unselected, so a test that never selects a fake's rules prices as unknown instead of inheriting the previous test's difficulty, and the per-body stores (jobs.lib.per-body/store!) are emptied, so a finished test's world is not kept alive by them."
  (:require [engine.game :as game]
            [jobs.lib.per-body :as per-body]
            [engine.test-events :as ev]))

(def out-file (some-> js/process .-env (aget "MC_TEST_TIMINGS")))
(def fs (js/require "fs"))
(def started (atom {}))
(defn cpu-ms [] (let [u (.cpuUsage js/process)] (/ (+ (.-user u) (.-system u)) 1000)))

(defn- line! [m] (.appendFileSync fs out-file (str (js/JSON.stringify (clj->js m)) "\n")))

(when out-file
  (line! {:started (js/Date.now)}) ; tells tools/test-run.mjs the stall watchdog may start counting
  (ev/hook! :begin-test-var (fn [m] (swap! started assoc (str (:var m)) [(js/performance.now) (cpu-ms)])))
  (ev/hook! :end-test-var (fn [m]
                         (let [k (str (:var m)) [t0 c0] (get @started k)]
                           (line! {:var k :ms (js/Math.round (- (js/performance.now) t0)) :cpu-ms (js/Math.round (- (cpu-ms) c0))
                                 :heap-kb (js/Math.round (/ (.-heapUsed (.memoryUsage js/process)) 1024))}))))
  (.on js/process "exit" (fn [] (line! {:peak-rss-kb (.-maxRSS (.resourceUsage js/process))}))))

(ev/hook! :end-test-var (fn [_] (reset! game/rules-source (constantly nil)) (per-body/clear-stores!)))

(ev/install!)
