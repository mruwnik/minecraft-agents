(ns jobs.debug.ready-probe-test
  "jobs.debug.ready-probe: the world fixtures' stand-in for a job that binds a readiness checker."
  (:require [cljs.test :refer [deftest are is]]
            [engine.ready-test :as rt]
            [engine.registry :as registry]
            [jobs.debug.ready-probe :as probe]))

(defn kind [args world]
  (first ((:safe? (get registry/jobs 'jobs.debug.ready-probe)) (assoc (rt/ctx-of world) :args args))))

(deftest safe-binds-the-checker-its-args-choose
  (rt/lit 0 15
    #(are [args world expected] (= expected (kind args world))
       {} rt/night-hut :ok
       {:at [15 64 0]} rt/night-hut :later
       {:at [5 64 0]} rt/night-hut :ok
       {:lit? true} rt/open-night :later
       {:lit? true} rt/night-hut :ok)))

(deftest the-report-carries-the-status-of-the-checker-run-before-it-without-running-it-again
  (let [c (assoc (rt/ctx-of rt/night-hut) :args {})
        runs (atom 0)
        checker probe/checker]
    (with-redefs [probe/checker (fn [c] (let [f (checker c)] (fn [c] (swap! runs inc) (f c))))]
      (let [s (probe/safe? c)]
        (is (= s (:status (probe/report c))))
        (is (= 1 @runs))))))
