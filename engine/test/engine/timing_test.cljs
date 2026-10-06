(ns engine.timing-test
  "Not a test: when MC_TEST_TIMINGS names a file, appends one JSON line per test var ({:var :ms}) and the process peak RSS on exit (tools/test-shards.mjs reads them)."
  (:require [cljs.test :as t]))

(def out-file (some-> js/process .-env (aget "MC_TEST_TIMINGS")))
(def fs (js/require "fs"))
(def started (atom {}))

(defn- line! [m] (.appendFileSync fs out-file (str (js/JSON.stringify (clj->js m)) "\n")))

(when out-file
  (defmethod t/report [:cljs.test/default :begin-test-var] [m]
    (swap! started assoc (str (:var m)) (js/performance.now)))
  (defmethod t/report [:cljs.test/default :end-test-var] [m]
    (let [k (str (:var m))]
      (line! {:var k :ms (js/Math.round (- (js/performance.now) (get @started k)))})))
  (.on js/process "exit" (fn [] (line! {:peak-rss-kb (.-maxRSS (.resourceUsage js/process))}))))
