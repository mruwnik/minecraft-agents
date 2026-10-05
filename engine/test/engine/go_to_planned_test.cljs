(ns engine.go-to-planned-test
  "go-to's per-round planning numbers: an info :planned event with :ms, :status and, once a search is over, :nodes."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [floor]]
            [engine.triggers :as triggers]
            [jobs.movement.go-to :as go-to]))

(deftest go-to-emits-a-planned-event-per-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              [seen sink] (tu/legacy-capture-sink)
              p (tu/fake {:self {:pos {:x 0 :y 64 :z 0}} :blocks (floor -2 -3 40 3)})
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (core/submit! eng '(jobs.movement.go-to {:pos [10 64 0] :range 0}) {})
          (dotimes [_ 10] (await (core/tick! eng)))
          (let [planned (filter #(= :planned (:kind %)) @seen)
                data (map #(or (:data %) %) planned)]
            (is (seq planned) "a planned event was emitted")
            (is (every? #(number? (:ms %)) data))
            (is (every? :status data))
            (is (some :nodes data) "a finished search reports its nodes")))))))
