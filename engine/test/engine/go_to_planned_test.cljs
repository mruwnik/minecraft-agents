(ns engine.go-to-planned-test
  "go-to's per-round planning numbers: an info :planned event with :ms, :status and, once a search is over, :nodes."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [floor box]]
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

(deftest the-planned-event-of-a-body-under-water-says-its-oxygen-and-the-swim-steps
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              [seen sink] (tu/legacy-capture-sink)
              p (tu/fake {:self {:pos [1.5 58 0.5] :inWater true :oxygen 10}
                          :blocks (merge (box -2 55 -4 40 66 4 "stone")
                                         (box 1 58 0 1 64 0 "water") (box 1 65 0 1 66 0 "air")
                                         (box 2 58 0 31 59 0 "water") (box 32 58 0 32 59 0 "air"))})
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (core/submit! eng '(jobs.movement.go-to {:pos [32 58 0] :range 0 :ways #{:walk :clear-seal}}) {})
          (dotimes [_ 6] (await (core/tick! eng)))
          (let [data (map #(or (:data %) %) (filter #(= :planned (:kind %)) @seen))]
            (is (= 10 (:oxygen (first data))) (pr-str (first data)))
            (is (some #(number? (:swim-steps %)) data) "a found plan counts its swim steps")))))))
