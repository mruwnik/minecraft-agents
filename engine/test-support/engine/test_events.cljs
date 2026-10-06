(ns engine.test-events
  "The live-tests @@test protocol: pure line builders, and install! (cljs.test report hooks that print plan, one result per test var, progress per namespace). Shared by the engine (engine.timing-test) and dashboard test builds; install! does nothing unless TEST_EVENTS=1."
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

(defn install! []
  (when (and enabled? (compare-and-set! installed false true))
    (hook! :begin-run-tests (fn [m] (reset! run {:ns-count (:ns-count m) :ns-done 0}) (emit! {:event "plan" :total (:var-count m)})))
    (hook! :begin-test-var (fn [_] (reset! reports [])))
    (hook! :fail (fn [m] (swap! reports conj m)))
    (hook! :error (fn [m] (swap! reports conj m)))
    (hook! :end-test-var (fn [m] (emit! (result (:var m) @reports))))
    (hook! :end-test-ns (fn [_]
                          (emit! {:event "progress" :done (:ns-done (swap! run update :ns-done inc)) :total (:ns-count @run) :unit "namespaces"})))))
