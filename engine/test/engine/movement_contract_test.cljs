(ns engine.movement-contract-test
  "movement.go-to, follow, linger-near, mount, leave-vehicle and boat-drive as contract jobs: they export the contract,
  quote validly, and end with a kind and a cause."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.lib.world-files :as world]
            [jobs.movement.boat-drive :as boat-drive]
            [jobs.movement.follow :as follow]
            [jobs.movement.go-to :as go-to]))

(defn job [sym] (get registry/jobs sym))

(deftest each-job-exports-the-contract-and-quotes-validly
  (doseq [[sym args] [['jobs.movement.follow {:player "Steve"}]
                      ['jobs.movement.linger-near {:pos [0 64 0] :wait-s 5}]
                      ['jobs.movement.mount {:id 9}]
                      ['jobs.movement.leave-vehicle {}]
                      ['jobs.movement.boat-drive {:pos [10 63 2]}]]]
    (is (some? (:conditions (job sym))) (str sym " has conditions"))
    (is (nil? (:check (job sym))) (str sym " has no old check"))
    (is (some? (:done? (job sym))) (str sym " has done?"))
    (is (nil? (contract/quote-problem ((:quote-of (job sym)) {:primitives (tu/fake {}) :args args :quote-call {:spend {:time 60}}}))) (str sym))))

(deftest targets-and-moves-to
  (is (= [:player] (:target (job 'jobs.movement.follow))))
  (is (= [:pos] (:target (job 'jobs.movement.linger-near))))
  (is (= [:pos] (:target (job 'jobs.movement.boat-drive))))
  (is (= [10.5 63 2.5] (:moves-to ((:quote-of (job 'jobs.movement.boat-drive))
                                   {:primitives (tu/fake {}) :args {:pos [10 63 2]} :quote-call {}})))))

(deftest old-time-limits-become-the-spend
  (is (= {:player "S" :spend {:time 9}} (follow/legacy-args {:player "S" :timeout-s 9})))
  (is (= {:pos [1 63 1] :spend {:time 9}} (boat-drive/legacy-args {:pos [1 63 1] :max-s 9}))))

(defn ^:async ending-of
  "Submit spec over a fake world and tick until the job ends: {:event :completed|:stopped :kind :reason}, plus the
  event's data keys ks."
  ([world spec] (ending-of world spec []))
  ([world spec ks]
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        p (tu/seeing-all (tu/fake-on-floor world))
        now (tu/act-clock clock p 20)
        eng (core/create {:primitives p :jobs registry/jobs :dir (tu/tmp-dir) :now now
                          :world (world/of-data {} {} [])
                          :events (events/make {:sinks [sink] :now now})})]
    (core/submit! eng spec {})
    (loop [i 0]
      (when (and (< i 80) (not-any? #(#{:completed :stopped} (:kind %)) @seen))
        (swap! clock + 700)
        (await (core/tick! eng))
        (recur (inc i))))
    (let [e (first (filter #(#{:completed :stopped} (:kind %)) @seen))]
      (core/shutdown! eng)
      (assoc (select-keys (:data e) (into [:kind :reason] ks)) :event (:kind e))))))

(def steve {:id 5 :name "Steve" :kind "player" :pos {:x 1 :y 64 :z 0}})
(def boat {:id 9 :name "oak_boat" :uuid "u-9" :kind "other" :pos {:x 1.5 :y 64 :z 0.5}})

(deftest follow-ends-on-its-time-and-when-the-player-never-came
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :stopped :kind :limit-reached :reason :timeout}
               (await (ending-of {:entities [steve]} '(jobs.movement.follow {:player "Steve" :timeout-s 2})))))
        (is (= {:event :stopped :kind :condition-failed :reason :absent}
               (await (ending-of {} '(jobs.movement.follow {:player "Steve"}))))))))) 

(deftest linger-near-waits-its-time-then-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed}
               (select-keys (await (ending-of {} '(jobs.movement.linger-near {:pos {:x 0 :y 64 :z 0} :wait-s 1}))) [:event])))))))

(deftest mount-is-done-or-a-failed-condition
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed}
               (select-keys (await (ending-of {:entities [boat]} '(jobs.movement.mount {:id 9}))) [:event])))
        (is (= {:event :stopped :kind :condition-failed :reason :not-mountable}
               (await (ending-of {:entities [(assoc boat :name "cow")]} '(jobs.movement.mount {:id 9})))))
        (is (= {:event :stopped :kind :nothing-possible :reason :timeout}
               (await (ending-of {:entities [boat] :mountFails true} '(jobs.movement.mount {:id 9})))))))))

(deftest leave-vehicle-ends-done-or-nothing-possible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed}
               (select-keys (await (ending-of {:self {:vehicle 9} :entities [boat]} '(jobs.movement.leave-vehicle {}))) [:event])))
        (is (= {:event :stopped :kind :nothing-possible :reason :dismount-failed}
               (await (ending-of {:self {:vehicle 9} :entities [(assoc boat :dismountFails true)] :dismountFails true}
                                 '(jobs.movement.leave-vehicle {:max-tries 2 :wait-ms 1})))))))))

(def lake (into {} (for [x (range 0 21) z (range 0 21)] [(str x ",63," z) "water"])))

(deftest boat-drive-endings
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label world args kind reason]
                [["not a boat" {:blocks lake :entities [(assoc boat :name "minecart" :pos {:x 2.5 :y 63.5 :z 2.5})] :self {:vehicle 9}} {:pos [10 63 2]} :condition-failed :not-a-boat]
                 ["not water" {:blocks lake :entities [(assoc boat :pos {:x 2.5 :y 63.5 :z 2.5})] :self {:vehicle 9}} {:pos [10 64 2]} :condition-failed :not-water]
                 ["out of strokes" {:blocks lake :entities [(assoc boat :pos {:x 2.5 :y 63.5 :z 2.5})] :self {:vehicle 9}} {:pos [18 63 18] :max-strokes 2} :nothing-possible :strokes]]]
          (is (= {:event :stopped :kind kind :reason reason}
                 (await (ending-of world (list 'jobs.movement.boat-drive args)))) label))))))

(deftest go-to-exports-the-contract
  (let [j (job 'jobs.movement.go-to)]
    (is (some? (:conditions j)))
    (is (nil? (:check j)) "no old check")
    (is (= [:pos :place] (:target j)) "a go-to may call go-to to another place")
    (is (= [:walk :clear-path :pillar :stair :dig-out] (:ways j)))))

(deftest go-to-old-escalate-becomes-the-ways
  (is (= {:pos [1 64 1] :ways #{:walk}} (go-to/legacy-args {:pos [1 64 1] :escalate false})))
  (is (= {:pos [1 64 1]} (go-to/legacy-args {:pos [1 64 1] :escalate true})))
  (is (= {:pos [1 64 1] :ways #{:walk :stair}} (go-to/legacy-args {:pos [1 64 1] :escalate false :ways #{:walk :stair}}))
      "ways given win"))

(deftest go-to-endings
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed}
               (select-keys (await (ending-of {} '(jobs.movement.go-to {:pos [5 64 0]}))) [:event])))
        (is (= {:event :stopped :kind :condition-failed :reason :unknown-place}
               (await (ending-of {} '(jobs.movement.go-to {:place :nowhere})))))
        (is (= {:event :stopped :kind :nothing-possible :reason :no-way}
               (await (ending-of {} '(jobs.movement.go-to {:pos [5 64 0] :ways #{:stair}}))))
            "a call that may not walk")
        (let [e (await (ending-of {:unreachable ["5,64,0"]} '(jobs.movement.go-to {:pos [5 64 0] :range 0}) [:cause]))]
          (is (= {:event :stopped :kind :nothing-possible :reason :unreachable} (dissoc e :cause)))
          (is (= :unreachable (:reason (:cause e))) (pr-str e))
          (is (keyword? (:why (:cause e))) "the planner's reason"))))))
