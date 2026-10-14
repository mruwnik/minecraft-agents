(ns engine.test-events
  "The live-tests @@test protocol: pure line builders, and install! (cljs.test report hooks that print plan, one result per test var, progress per namespace). Shared by the engine (engine.timing-test) and dashboard test builds; install! prints @@test lines only when TEST_EVENTS=1, but always fails a run that exits before its end."
  (:require [cljs.test :as t]))

(def enabled? (= "1" (some-> js/process .-env (aget "TEST_EVENTS"))))

(defn line [m] (str "@@test " (js/JSON.stringify (clj->js m))))

(defn var-name [v] (.replace (str v) #"^#'" ""))

(defn- failure-text [{:keys [message expected actual]}]
  (str (when message (str message ": ")) "expected " (pr-str expected) ", got " (if (instance? js/Error actual) (.-stack actual) (pr-str actual))))

(defn result [v reports]
  (let [bad (filter #(#{:fail :error} (:type %)) reports)
        text (apply str (interpose "\n" (map failure-text bad)))]
    (cond-> {:event "result" :name (var-name v)
             :outcome (cond (some #(= :error (:type %)) bad) "error" (seq bad) "failed" :else "passed")}
      (seq bad) (assoc :message (subs text 0 (min 2000 (count text)))))))

(def ^:private installed (atom false))
(def reports (atom []))
(def run (atom {:ns-count 0 :ns-done 0}))

(defn installed? [] @installed)

(defn emit! [m] (println (line m)))

;; Adds a step to a report type without dropping the default method (it prints the failures).
(defn hook! [type f]
  (let [k [:cljs.test/default type]
        prior (get-method t/report k)]
    (defmethod t/report k [m] (f m) (when prior (prior m)))))

(def last-started (atom nil))

(defn incomplete-exit!
  "Fails the run in proc and returns why: the test last started never finished."
  [proc last]
  (set! (.-exitCode proc) 1)
  (str "test run stopped before its end: " (or last "no test") " never finished (a promise that never settles?)"))

;; shadow's node runner ends a run with process.exit, which skips beforeExit: beforeExit means the event loop drained with a test pending.
(defn- guard! []
  (hook! :begin-test-var (fn [m] (reset! last-started (var-name (:var m)))))
  (.on js/process "beforeExit" (fn []
                                 (let [msg (incomplete-exit! js/process @last-started)]
                                   (.error js/console msg)
                                   (when enabled? (emit! {:event "result" :name @last-started :outcome "error" :message msg}))))))

(def ^:private guarded (atom false))

(defn install! []
  (when (compare-and-set! guarded false true)
    (guard!))
  (when (and enabled? (compare-and-set! installed false true))
    (hook! :begin-run-tests (fn [m] (reset! run {:ns-count (:ns-count m) :ns-done 0}) (emit! {:event "plan" :total (:var-count m)})))
    (hook! :begin-test-var (fn [_] (reset! reports [])))
    (hook! :fail (fn [m] (swap! reports conj m)))
    (hook! :error (fn [m] (swap! reports conj m)))
    (hook! :end-test-var (fn [m] (emit! (result (:var m) @reports))))
    (hook! :end-test-ns (fn [_]
                          (emit! {:event "progress" :done (:ns-done (swap! run update :ns-done inc)) :total (:ns-count @run) :unit "namespaces"})))))
