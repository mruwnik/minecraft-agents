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
  {:conditions [] :target []
   :round (fn ^:async pauser-round [c]
            (let [e (await (ctx/call c :pause 'jobs.time.noop {:seconds 25 :why :testing}))]
              (ctx/end c :done nil {:pause (:kind e)})))})

(defn setup
  "An engine whose clock moves by each :wait act's ms; cut-at: the wait act (1-based) that is cut."
  [cut-at]
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
                 (swap! clock + (.-ms a))
                 (if (= cut-at (count @waits))
                   (do (.setOwner p "other") (js/Promise.reject (core/cut-error)))
                   (impl token a))))
    {:eng eng :p p :seen seen :waits waits}))

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
            (is (= 25 (get-in completed [:report :uses :time])) "the pause is the caller's time")))))))

(deftest a-cut-pause-waits-only-what-is-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen waits]} (setup 2)
              id (core/submit! eng '(pauser) {})]
          (await (core/tick! eng))
          (is (= [id] (:list (core/state eng))) "cut, still listed")
          (await (tu/run-until-empty eng 5))
          (is (= [10000 10000 10000 5000] @waits) "the cut wait is not counted, the first is")
          (is (= :done (:pause (first (of-kind seen :completed))))))))))
