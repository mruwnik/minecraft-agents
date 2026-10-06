(ns engine.timing-test
  "Not a test: test-run hooks. When MC_TEST_TIMINGS names a file, appends one JSON line per test var ({:var :ms}) and the process peak RSS on exit (tools/test-shards.mjs reads them).
  When TEST_EVENTS=1, prints the live-tests @@test lines: plan (var count), one result per var, progress per namespace."
  (:require [engine.test-events :as ev]))

(def out-file (some-> js/process .-env (aget "MC_TEST_TIMINGS")))
(def fs (js/require "fs"))
(def started (atom {}))

(defn- line! [m] (.appendFileSync fs out-file (str (js/JSON.stringify (clj->js m)) "\n")))

(when out-file
  (ev/hook! :begin-test-var (fn [m] (swap! started assoc (str (:var m)) (js/performance.now))))
  (ev/hook! :end-test-var (fn [m]
                         (let [k (str (:var m))]
                           (line! {:var k :ms (js/Math.round (- (js/performance.now) (get @started k)))}))))
  (.on js/process "exit" (fn [] (line! {:peak-rss-kb (.-maxRSS (.resourceUsage js/process))}))))

(ev/install!)
