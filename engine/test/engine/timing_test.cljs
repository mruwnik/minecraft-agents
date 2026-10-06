(ns engine.timing-test
  "Not a test: test-run hooks. When MC_TEST_TIMINGS names a file, appends one JSON line per test var ({:var :ms}) and the process peak RSS on exit (tools/test-shards.mjs reads them).
  When TEST_EVENTS=1, prints the live-tests @@test lines: plan (var count), one result per var, progress per namespace."
  (:require [cljs.test :as t]
            [engine.test-events :as ev]))

(def out-file (some-> js/process .-env (aget "MC_TEST_TIMINGS")))
(def events? (= "1" (some-> js/process .-env (aget "TEST_EVENTS"))))
(def fs (js/require "fs"))
(def started (atom {}))
(def reports (atom []))
(def run (atom {:ns-count 0 :ns-done 0}))

(defn- line! [m] (.appendFileSync fs out-file (str (js/JSON.stringify (clj->js m)) "\n")))
(defn- emit! [m] (println (ev/line m)))

;; Adds a step to a report type without dropping the default method (it prints the failures).
(defn- hook! [type f]
  (let [k [:cljs.test/default type]
        prior (get-method t/report k)]
    (defmethod t/report k [m] (f m) (when prior (prior m)))))

(when out-file
  (hook! :begin-test-var (fn [m] (swap! started assoc (str (:var m)) (js/performance.now))))
  (hook! :end-test-var (fn [m]
                         (let [k (str (:var m))]
                           (line! {:var k :ms (js/Math.round (- (js/performance.now) (get @started k)))}))))
  (.on js/process "exit" (fn [] (line! {:peak-rss-kb (.-maxRSS (.resourceUsage js/process))}))))

(when events?
  (hook! :begin-run-tests (fn [m] (reset! run {:ns-count (:ns-count m) :ns-done 0}) (emit! {:event "plan" :total (:var-count m)})))
  (hook! :begin-test-var (fn [_] (reset! reports [])))
  (hook! :fail (fn [m] (swap! reports conj m)))
  (hook! :error (fn [m] (swap! reports conj m)))
  (hook! :end-test-var (fn [m] (emit! (ev/result (:var m) @reports))))
  (hook! :end-test-ns (fn [_]
                        (emit! {:event "progress" :done (:ns-done (swap! run update :ns-done inc)) :total (:ns-count @run) :unit "namespaces"}))))
