(ns world-test.sweep-test
  (:require [cljs.test :refer [deftest is async]]
            [world-test.sweep :as s]))

(def opts (s/parse-args ["--dir" "/d" "--durations" "a.edn" "b.edn"]))

(defn names [passes] (mapv :name passes))

(deftest passes-in-order-with-the-per-pass-options
  (let [ps (s/passes opts)]
    (is (= [:day :night :night-exclusive] (names ps)))
    (let [day (first (:argv (first ps)))]
      (is (some? day)))
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
