(ns engine.noop-test
  "jobs.time.noop: an intentional pause, waited out in :wait acts inside the round, so reflexes still cut it and the
  time counts on the caller's report."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]))

(def pauser
  {:target []
   :round (fn ^:async pauser-round [c]
            (let [e (await (ctx/call c :pause 'jobs.time.noop {:seconds (or (:seconds (:args c)) 25) :why :testing}))]
              (ctx/end c :done nil {:pause (:kind e)})))})

(defn setup
  "An engine whose clock moves by each :wait act's ms (not at all when frozen?); cut-at: the wait act (1-based) that is cut."
  ([cut-at] (setup cut-at false))
  ([cut-at frozen?]
  (let [clock (atom 1000000)
        waits (atom [])
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake {})
        eng (core/create {:primitives p :dir (tu/tmp-dir) :now #(deref clock)
                          :jobs (assoc registry/jobs 'pauser pauser) :triggers {}
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (.override (.-world p) "wait"
               (fn [token a impl]
                 (swap! waits conj (.-ms a))
                 (when-not frozen? (swap! clock + (.-ms a)))
                 (if (= cut-at (count @waits))
                   (do (.setOwner p "other") (js/Promise.reject (core/cut-error)))
                   (impl token a))))
    {:eng eng :p p :seen seen :waits waits})))

(defn of-kind [seen k] (filter #(= k (:kind %)) @seen))

(deftest a-pause-waits-its-seconds-holding-and-counts-on-the-callers-report
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen waits]} (setup nil)]
          (core/submit! eng '(pauser) {})
          (await (core/tick! eng))
          (is (= [10000 10000 5000] @waits) "in waits of at most prim/wait-max-ms")
          (is (= [:testing] (distinct (map #(:reason %) (of-kind seen :holding)))) "holding with its reason")
          (let [completed (first (of-kind seen :completed))]
            (is (= :done (:pause completed)))
            (is (= {:job-time 25 :wall-time 25} (select-keys (get-in completed [:report :uses]) [:job-time :wall-time])) "the pause counts as both job time and wall time")))))))

(deftest a-cut-pause-waits-only-what-is-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen waits]} (setup 2)
              id (core/submit! eng '(pauser) {})]
          (await (core/tick! eng))
          (is (= [id] (:list (core/state eng))) "cut, still listed")
          (await (tu/run-until-empty eng 5))
          (is (= [10000 10000 5000] @waits) "the cut wait elapsed, so only what is left of the deadline is waited")
          (is (= :done (:pause (first (of-kind seen :completed))))))))))

(deftest a-long-pause-writes-a-constant-number-of-events
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen waits]} (setup nil)]
          (core/submit! eng '(pauser {:seconds 600}) {})
          (await (core/tick! eng))
          (is (= 60 (count @waits)) "still one wait act per prim/wait-max-ms")
          (is (< (count @seen) 30) (str "events: " (frequencies (map (juxt :source :kind) @seen)))))))))

(deftest a-pause-ends-on-a-frozen-clock
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen waits]} (setup nil true)]
          (core/submit! eng '(pauser) {})
          (await (core/tick! eng))
          (is (= [10000 10000 5000] @waits))
          (is (= :done (:pause (first (of-kind seen :completed))))))))))
