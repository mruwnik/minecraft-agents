(ns engine.timing-test
  "Not a test: test-run hooks. When MC_TEST_TIMINGS names a file, appends a {:started} line at load, one JSON line per test var ({:var :ms}) and the process peak RSS on exit (tools/test-shards.mjs reads them).
  When TEST_EVENTS=1, prints the live-tests @@test lines: plan (var count), one result per var, progress per namespace.
  After every test var the server rules are unselected, so a test that never selects a fake's rules prices as unknown instead of inheriting the previous test's difficulty."
  (:require [engine.game :as game]
            [engine.test-events :as ev]))

(def out-file (some-> js/process .-env (aget "MC_TEST_TIMINGS")))
(def fs (js/require "fs"))
(def started (atom {}))

(defn- line! [m] (.appendFileSync fs out-file (str (js/JSON.stringify (clj->js m)) "\n")))

(when out-file
  (line! {:started (js/Date.now)}) ; tells tools/test-run.mjs the stall watchdog may start counting
  (ev/hook! :begin-test-var (fn [m] (swap! started assoc (str (:var m)) (js/performance.now))))
  (ev/hook! :end-test-var (fn [m]
                         (let [k (str (:var m))]
                           (line! {:var k :ms (js/Math.round (- (js/performance.now) (get @started k)))
                                 :heap-kb (js/Math.round (/ (.-heapUsed (.memoryUsage js/process)) 1024))}))))
  (.on js/process "exit" (fn [] (line! {:peak-rss-kb (.-maxRSS (.resourceUsage js/process))}))))

(ev/hook! :end-test-var (fn [_] (reset! game/rules-source (constantly nil))))

(ev/install!)
