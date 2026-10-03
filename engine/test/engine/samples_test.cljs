(ns engine.samples-test
  "The sample jobs and trigger end to end against the fake, from a scenario."
  (:require [cljs.test :refer [deftest is async]]
            [engine.catalog :as catalog]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.scenario :as scenario]
            [engine.test-util :as tu]))

(def scenario-text
  "{:register [{:trigger :health-low}]
    :queue [{:job :go-to :args {:pos {:x 30 :y 64 :z 0}}}
            {:job :wait-for-day}]}")

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        p (tu/fake world)
        eng (core/create {:primitives p :catalog catalog/catalog :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock}))

(deftest scenario-reads-and-validates
  (let [s (scenario/parse scenario-text)]
    (is (= [] (scenario/problems catalog/catalog s)))
    (is (= ["unknown trigger :nope" "unknown job :fly" "queue entry 0 has no :job"]
           (scenario/problems catalog/catalog {:register [{:trigger :nope} {:trigger :health-low :job :fly}]
                                               :queue [{:args {}}]})))))

(deftest go-to-wait-for-day-and-health-low-end-to-end
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time 14000 :inventory [{:name "bread" :count 1}]})
              world (.-world p)]
          (core/load-scenario! eng (scenario/parse scenario-text))
          (await (core/tick! eng))
          (is (= {:x 30 :y 64 :z 0} (core/self-pos p)) "go-to walked")
          (await (core/tick! eng))
          (is (= ["j2"] (:list (core/state eng))) "go-to is done; wait-for-day yielded")
          (is (nil? (core/tick! eng)) "night: nothing ready")
          (set! (.. world -state -self -health) 6)
          (set! (.. world -state -self -food) 10)
          (await (core/tick! eng))
          (is (= 15 (.-food (.self p))) "health-low ate")
          (is (nil? (core/tick! eng)) "cooling down, and still night")
          (.setTime world 1000)
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))))
          (is (= [:go-to :wait-for-day :eat :wait-for-day]
                 (->> @seen (filter #(= :round_started (:kind %))) (mapv :name)))))))))

(deftest go-to-gives-up-after-three-blocked-walks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:unreachable ["9,64,9"]})]
          (core/submit! eng :go-to {:pos {:x 9 :y 64 :z 9}} {})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= [] (:list (core/state eng))))
          (is (some #(= :unreachable (:kind %)) @seen)))))))

(deftest go-to-continues-on-partial
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {})]
          (core/submit! eng :go-to {:pos {:x 100 :y 64 :z 0}} {})
          (await (core/tick! eng))
          (is (= ["j1"] (:list (core/state eng))))
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng)))))))))
