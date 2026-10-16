(ns world-test.sweep-test
  (:require [cljs.test :refer [deftest is async]]
            [world-test.sweep :as s]))

(def opts (s/parse-args ["--dir" "/d" "--durations" "a.edn" "b.edn"]))

(defn names [passes] (mapv :name passes))

(deftest passes-in-order-with-the-per-pass-options
  (let [ps (s/passes opts)]
    (is (= [:day :night :night-exclusive] (names ps)))
    (is (= ["--phase" "day"] (take 2 (:argv (first ps)))))
    (is (= ["--phase" "day" "--bodies" "16" "--min-bodies" "8"] (take 6 (:argv (first ps)))))
    (is (every? #(some #{"--allow-time"} (:argv %)) ps))
    (is (= ["--time-log" "/d/time-log-night.txt" "--results" "/d/sweep-night.edn"]
           (let [a (:argv (second ps)) i (.indexOf (to-array a) "--time-log")] (subvec (vec a) i (+ i 4)))))
    (is (every? #(some #{"a.edn" "b.edn"} (:argv %)) ps))
    (is (every? #(= ["--retry-failed" "1"] (let [a (vec (:argv %)) i (.indexOf (to-array a) "--retry-failed")] (subvec a i (+ i 2)))) ps))))

(deftest night-exclusive-uses-batches-or-one-body
  (let [argv #(vec (:argv (last (s/passes (s/parse-args %)))))]
    (is (some #{"--night-batches"} (argv [])))
    (is (not-any? #{"--night-batches"} (argv ["--no-night-batches"])))
    (is (= "1" (nth (argv ["--no-night-batches"]) (inc (.indexOf (to-array (argv ["--no-night-batches"])) "--bodies")))))))

(deftest match-ids-and-paths-reach-every-pass
  (let [ps (s/passes (s/parse-args ["--match-ids" "ids.txt" "engine/fixtures/world/x.edn"]))]
    (is (every? #(some #{"ids.txt"} (:argv %)) ps))
    (is (every? #(some #{"engine/fixtures/world/x.edn"} (:argv %)) ps))))

(defn- repo-root []
  (let [fs (js/require "fs") path (js/require "path")]
    (loop [d (js/process.cwd)]
      (cond
        (.existsSync fs (.join path d "tools" "world-test-pool.mjs")) d
        (= d (.dirname path d)) (throw (js/Error. "repo root not found"))
        :else (recur (.dirname path d))))))

(defn- pool-parse
  "argv -> the real tools/world-test-pool.mjs parsePoolArgs of it (run in a node child: the pool is an ES module)."
  [argv]
  (let [cp (js/require "child_process")
        code "import(process.argv[1]).then((m) => console.log(JSON.stringify(m.parsePoolArgs(JSON.parse(process.argv[2])))))"
        out (.execFileSync cp "node" #js ["-e" code (str "file://" (repo-root) "/tools/world-test-pool.mjs") (js/JSON.stringify (clj->js argv))]
                           #js {:encoding "utf8"})]
    (js->clj (js/JSON.parse out) :keywordize-keys true)))

(deftest argv-parses-with-the-real-pool-parser
  (doseq [[cli paths durs] [[["--durations" "a.edn" "b.edn"] ["engine/fixtures/world"] ["a.edn" "b.edn"]]
                            [["--durations" "a.edn" "--match-ids" "ids.txt" "x/y.edn"] ["x/y.edn"] ["a.edn"]]
                            [[] ["engine/fixtures/world"] []]]
          ps (s/passes (s/parse-args (into ["--dir" "/d"] cli)))
          :let [p (pool-parse (:argv ps))]]
    (is (= paths (:paths p)) (str (:name ps) " paths"))
    (is (= durs (:durations p)) (str (:name ps) " durations"))
    (is (= (:results ps) (:results p)))))

(deftest counts-from-a-results-vector
  (is (= {:pass 2 :fail 1 :flaky 1 :error 0 :total 4}
         (s/counts "[{:id \"a\" :status :pass} {:id \"b\" :status :pass} {:id \"c\" :status :fail} {:id \"d\" :status :flaky}]")))
  (is (= {:total 0} (s/counts nil))))

(defn fake [codes]
  (let [calls (atom [])]
    {:calls calls
     :deps {:run-pass! (fn [p] (swap! calls conj (:name p))
                         (js/Promise.resolve {:code (get codes (:name p) 0)
                                              :results "[{:id \"a\" :status :pass}]"}))
            :time-set! (fn [] (swap! calls conj :time-set) (js/Promise.resolve 0))
            :now (let [t (atom 0)] #(swap! t + 60000))
            :emit! (fn [_])}}))

(deftest all-green-runs-three-passes-then-time-set
  (async done
    (let [{:keys [calls deps]} (fake {})]
      (.then (s/run-sweep (s/passes opts) deps)
             (fn [r]
               (is (= [:day :night :night-exclusive :time-set] @calls))
               (is (= 0 (:exit r)))
               (is (= [1.0 1.0 1.0] (mapv :wall-min (:passes r))))
               (done))))))

(deftest a-failed-pass-does-not-stop-the-sweep
  (async done
    (let [{:keys [calls deps]} (fake {:day 1})]
      (.then (s/run-sweep (s/passes opts) deps)
             (fn [r]
               (is (= [:day :night :night-exclusive :time-set] @calls))
               (is (= 1 (:exit r)))
               (done))))))

(deftest exit-5-and-4-stop-the-sweep-but-time-set-still-runs
  (async done
    (let [{:keys [calls deps]} (fake {:night 5})]
      (.then (s/run-sweep (s/passes opts) deps)
             (fn [r]
               (is (= [:day :night :time-set] @calls))
               (is (= 5 (:exit r)))
               (is (re-find #"night" (:stopped r)))
               (is (= [:day :night] (mapv :name (:passes r))))
               (done))))))

(deftest exit-6-boot-death-stops-the-sweep-with-a-reason
  (async done
    (let [{:keys [calls deps]} (fake {:night 6})]
      (.then (s/run-sweep (s/passes opts) deps)
             (fn [r]
               (is (= [:day :night :time-set] @calls))
               (is (= 6 (:exit r)))
               (is (re-find #"died at boot" (:stopped r)))
               (done))))))

(deftest a-rejecting-or-throwing-pass-is-reported-and-time-set-still-runs
  (doseq [boom [(fn [_] (js/Promise.reject (js/Error. "pool spawn failed")))
                (fn [_] (throw (js/Error. "pool spawn failed")))]]
    (async done
      (let [{:keys [calls deps]} (fake {})
            evs (atom [])]
        (.then (s/run-sweep (s/passes opts) (assoc deps :run-pass! boom :emit! #(swap! evs conj %)))
               (fn [r]
                 (is (= [:time-set] @calls))
                 (is (= 1 (:exit r)))
                 (is (re-find #"pool spawn failed" (:error (first (:passes r)))))
                 (is (re-find #"pool spawn failed" (:message (second @evs))))
                 (done)))))))

(deftest a-failed-pass-names-its-failing-fixtures
  (async done
    (let [evs (atom [])
          {:keys [deps]} (fake {:day 1})]
      (.then (s/run-sweep (s/passes opts)
                          (assoc deps :run-pass! (fn [_] (js/Promise.resolve {:code 1 :results "[{:id \"a\" :status :pass} {:id \"b/x\" :status :fail} {:id \"c\" :status :error}]"}))
                                      :emit! #(swap! evs conj %)))
             (fn [_]
               (is (re-find #"b/x" (:message (second @evs))))
               (is (re-find #"c" (:message (second @evs))))
               (is (not (re-find #"\"a\"|a," (:message (second @evs)))))
               (done))))))

(deftest an-interrupt-stops-further-passes-and-time-set-still-runs
  (async done
    (let [sig (atom nil)
          {:keys [calls deps]} (fake {})]
      (.then (s/run-sweep (s/passes opts)
                          (assoc deps
                                 :run-pass! (fn [p] (swap! calls conj (:name p)) (reset! sig "SIGTERM")
                                              (js/Promise.resolve {:code 143 :results nil}))
                                 :interrupted #(deref sig)))
             (fn [r]
               (is (= [:day :time-set] @calls))
               (is (= 143 (:exit r)))
               (is (= "SIGTERM" (:interrupted r)))
               (done))))))

(defn fake-proc []
  (let [handlers (atom {})]
    #js {:on (fn [sig f] (swap! handlers assoc sig f))
         :fire (fn [sig] ((@handlers sig)))}))

(defn fake-child []
  (let [kills (atom [])]
    #js {:kills kills :kill (fn [sig] (swap! kills conj sig))}))

(deftest signals-are-forwarded-to-the-running-child
  (let [proc (fake-proc) child (fake-child) current (atom child) sig (atom nil)]
    (s/forward-signals! proc current sig 60000)
    (.fire proc "SIGINT")
    (is (= ["SIGINT"] @(.-kills child)))
    (is (= "SIGINT" @sig))
    (is (= 130 (s/signal-exit "SIGINT")))
    (is (= 143 (s/signal-exit "SIGTERM")))))

(deftest a-signal-without-a-child-is-just-recorded
  (let [proc (fake-proc) sig (atom nil)]
    (s/forward-signals! proc (atom nil) sig 60000)
    (.fire proc "SIGTERM")
    (is (= "SIGTERM" @sig))))

(deftest a-child-that-ignores-the-signal-is-killed-after-the-grace
  (async done
    (let [proc (fake-proc) child (fake-child) current (atom child)]
      (s/forward-signals! proc current (atom nil) 10)
      (.fire proc "SIGTERM")
      (js/setTimeout (fn []
                       (is (= ["SIGTERM" "SIGKILL"] @(.-kills child)))
                       (done))
                     80))))

(deftest events-plan-results-and-exit
  (async done
    (let [evs (atom [])
          {:keys [deps]} (fake {:night 4})]
      (.then (s/run-sweep (s/passes opts) (assoc deps :emit! #(swap! evs conj %)))
             (fn [_]
               (is (= {:event "plan" :total 3} (first @evs)))
               (is (= [{:event "result" :name "sweep:day" :outcome "passed"}
                       {:event "result" :name "sweep:night" :outcome "failed"}]
                      (mapv #(select-keys % [:event :name :outcome]) (filter #(= "result" (:event %)) @evs))))
               (done))))))

(deftest pass-lines-relay-case-events
  (let [r (fn [l] (s/pass-line :day l))
        ev (fn [l] (js->clj (js/JSON.parse (subs l 7)) :keywordize-keys true))]
    (is (= {:event "plan" :total 5} (ev (r "@@test {\"event\":\"plan\",\"total\":5}"))))
    (is (= {:event "result" :name "day:a/x#1" :outcome "failed"} (ev (r "@@test {\"event\":\"result\",\"name\":\"a/x#1\",\"outcome\":\"failed\"}"))))
    (is (nil? (r "@@test {\"event\":\"progress\",\"done\":1,\"total\":5,\"unit\":\"fixtures\"}")))
    (is (= "PASS a/x" (r "PASS a/x")))
    (is (= "@@test {bad" (r "@@test {bad")))))

(deftest run-known-reaches-every-pass
  (is (every? #(some #{"--run-known"} (:argv %)) (s/passes (s/parse-args ["--run-known"]))))
  (is (not-any? #(some #{"--run-known"} (:argv %)) (s/passes (s/parse-args [])))))

(deftest unskip-ids-reach-every-pass
  (let [ps (s/passes (s/parse-args ["--unskip-ids" "u.txt"]))]
    (is (every? #(= ["--unskip-ids" "u.txt"] (take-last 2 (:argv %))) ps)))
  (is (every? #(= ["--unskip-id" "a/x" "--unskip-id" "b/y"] (take-last 4 (:argv %))) (s/passes (s/parse-args ["--unskip-id" "a/x" "--unskip-id" "b/y"])))))
