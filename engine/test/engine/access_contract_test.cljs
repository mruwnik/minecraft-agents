(ns engine.access-contract-test
  "access.bridge, toggle and leave-tunnel as contract jobs: they export the contract, quote validly, and end with a
  kind and a cause."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.lib.world-files :as world]))

(def quote-args
  {'jobs.access.bridge {:heading :east :length 5}
   'jobs.access.toggle {:pos [3 64 0] :state :open}
   'jobs.access.leave-tunnel {:tunnel {:torches [{:cell [1 64 0] :site 1} {:cell [2 64 0] :site 2}] :dug [{:cell [1 64 0]}]}}})

(deftest each-job-exports-the-contract-and-quotes-validly
  (let [p (tu/fake {})]
    (doseq [[sym args] quote-args
            :let [job (get registry/jobs sym)]]
      (is (some? (:conditions job)) (str sym " has conditions"))
      (is (nil? (:check job)) (str sym " has no old check"))
      (is (vector? (:target job)) (str sym " has a target"))
      (is (nil? (contract/quote-problem ((:quote-of job) {:primitives p :args args}))) (str sym)))))

(deftest a-longer-bridge-quotes-more-time
  (let [quote-of (:quote-of (get registry/jobs 'jobs.access.bridge))
        least (fn [n] (get-in (quote-of {:primitives (tu/fake {}) :args {:heading :east :length n}}) [:uses :time 0]))]
    (is (< (least 2) (least 20)))))

(defn ^:async ending-of
  "Submit spec over a fake world and tick until the job ends: {:event :completed|:stopped :kind :reason}."
  [spec world]
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        eng (core/create {:primitives (tu/fake world) :jobs registry/jobs :dir (tu/tmp-dir) :now #(deref clock)
                          :world (world/of-data {} {} [])
                          :events (events/make {:sinks [sink] :now #(deref clock)})})]
    (core/submit! eng spec {})
    (loop [i 0]
      (when (and (< i 40) (not-any? #(#{:completed :stopped} (:kind %)) @seen))
        (swap! clock + 500)
        (await (core/tick! eng))
        (recur (inc i))))
    (let [e (first (filter #(#{:completed :stopped} (:kind %)) @seen))]
      (core/shutdown! eng)
      (assoc (select-keys (:data e) [:kind :reason]) :event (:kind e)))))

(def ground (into {} (for [x (range -5 20) z (range -5 20)] [(str x ",63," z) "stone"])))
(defn gate-world [name state] {:self {:pos {:x 3 :y 64 :z 2}} :blocks (assoc ground "3,64,0" name) :states {"3,64,0" state}})

(deftest a-refused-toggle-is-a-failed-condition
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[block state args reason] [["stone" nil {:pos [3 64 0] :state :open} :not-toggleable]
                                           ["iron_door" {:open false} {:pos [3 64 0] :state :open} :needs-redstone]
                                           ["lever" {:powered false} {:pos [3 64 0] :state :open} :bad-state]
                                           ["oak_door" {:open false} {:state :open} :bad-args]]]
          (is (= {:event :stopped :kind :condition-failed :reason reason}
                 (await (ending-of (list 'jobs.access.toggle args) (gate-world block state))))
              (str block " " args)))))))

(deftest a-click-that-changes-nothing-is-nothing-possible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :stopped :kind :nothing-possible :reason :unchanged}
               (await (ending-of '(jobs.access.toggle {:pos [3 64 0] :state :open}) (gate-world "oak_door" {:open false :locked true})))))))))

(deftest a-toggle-already-in-state-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed}
               (select-keys (await (ending-of '(jobs.access.toggle {:pos [3 64 0] :state :open}) (gate-world "oak_fence_gate" {:open true})))
                            [:event])))))))

(deftest a-bridge-without-blocks-and-without-fetch-is-a-failed-condition
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :stopped :kind :condition-failed :reason :too-few-blocks}
               (await (ending-of '(jobs.access.bridge {:heading :east :length 3 :fetch false :ignore-zones? true})
                                 {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks {"0,63,0" "stone"} :inventory []}))))))))

(deftest a-bridge-over-a-gap-ends-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed}
               (select-keys (await (ending-of '(jobs.access.bridge {:heading :east :length 2 :ignore-zones? true})
                                              {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks {"0,63,0" "stone"}
                                               :inventory [{:name "dirt" :count 8}]}))
                            [:event])))))))

(deftest a-tunnel-result-without-a-line-is-a-failed-condition
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :stopped :reason :bad-args}
               (select-keys (await (ending-of '(jobs.access.leave-tunnel {:tunnel {} :ignore-zones? true}) {})) [:event :reason])))))))
