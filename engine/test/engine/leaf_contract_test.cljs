(ns engine.leaf-contract-test
  "The leaf jobs of wave 1.1 (memory, time, debug.notify, explore.look, movement.look-around) as contract jobs: they
  export the contract, quote validly, and a refused call ends with a kind and a cause instead of a done result."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]))

(def leaves
  '[jobs.memory.forget-place jobs.memory.remember jobs.memory.set-place jobs.memory.reserve jobs.memory.unreserve jobs.time.wait-for-day
    jobs.time.wait-for-dusk jobs.time.noop jobs.debug.notify jobs.explore.look jobs.movement.look-around])

(deftest every-leaf-exports-the-contract-and-quotes-validly
  (let [p (tu/fake {:time 18000})]
    (doseq [sym leaves
            :let [job (get registry/jobs sym)]]
      (is (fn? (:round job)) (str sym " has a round"))
      (is (vector? (:target job)) (str sym " has a target"))
      (is (nil? (contract/quote-problem ((:quote-of job) {:primitives p :args {:every-ms 2000}}))) (str sym)))))

(deftest waiting-for-the-day-is-quoted-by-the-ticks-left-of-the-night
  (let [quote-of (fn [time] (get-in ((:quote-of (get registry/jobs 'jobs.time.wait-for-day)) {:primitives (tu/fake {:time time})})
                                    [:uses :job-time]))]
    (is (= [0 0 0] (quote-of 1000)) "day: nothing to wait for")
    (is (< 0 (second (quote-of 13000)) (nth (quote-of 13000) 2)))
    (is (> (second (quote-of 13000)) (second (quote-of 20000))) "later in the night, less to wait")))

(defn ^:async stopped-data [spec]
  (let [[seen sink] (tu/capture-sink)
        eng (core/create {:primitives (tu/seeing-all (tu/fake {})) :jobs registry/jobs :dir (tu/tmp-dir)
                          :events (events/make {:sinks [sink]})})]
    (core/submit! eng spec {})
    (await (core/tick! eng))
    (let [e (first (filter #(#{:completed :stopped} (:kind %)) @seen))]
      (core/shutdown! eng)
      (assoc (select-keys (:data e) [:kind :reason]) :event (:kind e)))))

(defn ^:async waits-with [spec]
  (let [[seen sink] (tu/capture-sink)
        eng (core/create {:primitives (tu/seeing-all (tu/fake {})) :jobs registry/jobs :dir (tu/tmp-dir)
                          :events (events/make {:sinks [sink]})})]
    (core/submit! eng spec {})
    (await (core/tick! eng))
    (await (core/tick! eng))
    (let [r {:listed (count (:list (core/state eng)))
             :causes (mapv #(get-in % [:data :reason]) (filter #(= :waiting (:kind %)) @seen))
             :started (count (filter #(= :round_started (:kind %)) @seen))}]
      (core/shutdown! eng)
      r)))

(deftest a-leaf-call-with-bad-args-is-impossible-instead-of-starting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[spec reason] [['(jobs.memory.remember {:kind :hurt}) :reserved-kind]
                               ['(jobs.memory.remember {:kind :reserve}) :reserved-kind]
                               ['(jobs.memory.reserve {}) :bad-args]
                               ['(jobs.memory.unreserve {}) :bad-args]
                               ['(jobs.memory.remember {}) :bad-kind]
                               ['(jobs.memory.set-place {:name :home :pos [2 999 3]}) :bad-pos]
                               ['(jobs.memory.forget-place {:name "Bad Name"}) :bad-name]
                               ['(jobs.explore.look {:block-names ["no_such_block"]}) :bad-args]]]
          (is (= {:event :stopped :kind :impossible :reason reason} (await (stopped-data spec))) (pr-str spec)))))))

(deftest an-accepted-leaf-call-ends-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [spec ['(jobs.memory.remember {:kind :bred-cows}) '(jobs.debug.notify {}) '(jobs.explore.look {})
                    '(jobs.memory.forget-place {:name :nowhere}) '(jobs.memory.unreserve {:id "nowhere"})
                    '(jobs.memory.reserve {:item "diamond"})]]
          (is (= {:event :completed :kind :done} (await (stopped-data spec))) (pr-str spec)))))))
