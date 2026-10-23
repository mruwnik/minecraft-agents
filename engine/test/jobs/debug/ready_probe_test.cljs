(ns jobs.debug.ready-probe-test
  "jobs.debug.ready-probe: the world fixtures' stand-in for a job that binds a readiness checker."
  (:require [cljs.test :refer [deftest are]]
            [engine.ready-test :as rt]
            [engine.registry :as registry]))

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
