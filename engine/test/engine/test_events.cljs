(ns engine.test-events
  "Pure builders for the live-tests @@test protocol lines (plan, result, progress); engine.timing-test prints them when TEST_EVENTS=1.")

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
