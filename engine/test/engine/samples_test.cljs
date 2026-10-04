(ns engine.samples-test
  "The sample jobs and trigger end to end against the fake, from a scenario."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.scenario :as scenario]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(def scenario-text
  "{:register [{:trigger :health-low}]
    :queue [(jobs.movement.go-to {:pos {:x 30 :y 64 :z 0}})
            (jobs.time.wait-for-day)]}")

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock}))

(deftest scenario-reads-and-validates
  (let [s (scenario/parse scenario-text)]
    (is (= [] (scenario/problems registry/jobs triggers/all s)))
    (let [ps (scenario/problems registry/jobs triggers/all
                                '{:register [{:trigger :nope}
                                             {:trigger :health-low :job (jobs.fly)}
                                             {:trigger :health-low :job (hold (jobs.survival.eat))}]
                                  :queue [{:job :go-to} (jobs.fly) (seq)]})]
      (is (= 6 (count ps)))
      (is (= "unknown trigger :nope" (first ps)))
      (is (re-find #"unknown job or combinator jobs.fly" (nth ps 1)))
      (is (re-find #"hold is not allowed in a register entry" (nth ps 2)))
      (is (re-find #"a job spec is a list" (nth ps 3)) "the old map form is refused")
      (is (re-find #"unknown job or combinator jobs.fly" (nth ps 4)))
      (is (re-find #"seq takes at least one" (nth ps 5))))))

(deftest go-to-wait-for-day-and-health-low-end-to-end
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:floor tu/walk-floor :time 14000 :inventory [{:name "bread" :count 2}]})
              world (.-world p)]
          (core/load-scenario! eng (scenario/parse scenario-text))
          (await (core/tick! eng))
          (is (= {:x 29 :y 64 :z 0} (core/self-pos p)) "go-to walked, to range 1 of the target")
          (await (core/tick! eng))
          (is (= ["j2"] (:list (core/state eng))) "go-to is done; wait-for-day declines at night")
          (is (nil? (core/tick! eng)) "night: nothing ready")
          (set! (.. world -state -self -health) 6)
          (set! (.. world -state -self -food) 10)
          (await (core/tick! eng))
          (is (= 15 (.-food (.self p))) "health-low ran recover, which ate")
          (set! (.. world -state -self -health) 18)
          (await (core/tick! eng))
          (is (= ["j2"] (:list (core/state eng))) "healed: recover is done")
          (is (nil? (core/tick! eng)) "cooling down, and still night")
          (.setTime world 1000)
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))))
          (is (= ["jobs.movement.go-to" "jobs.survival.recover" "jobs.survival.recover" "jobs.time.wait-for-day"]
                 (->> @seen (filter #(= :round_started (:kind %))) (mapv :name)))))))))

(deftest go-to-gives-up-after-three-blocked-walks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:floor tu/walk-floor :unreachable ["9,64,9"]})]
          (core/submit! eng (list 'jobs.movement.go-to {:pos {:x 9 :y 64 :z 9}}) {})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= [] (:list (core/state eng))))
          (is (some #(= :unreachable (:kind %)) @seen)))))))

(deftest go-to-continues-on-partial
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the floor ends at x 40 and the goal's chunk is not loaded: the first round walks to the edge and is not done
        (let [{:keys [eng p]} (setup {:floor [-30 -10 40 10]})
              blocks (.. p -world -state -blocks)]
          (core/submit! eng (list 'jobs.movement.go-to {:pos {:x 60 :y 64 :z 0}}) {})
          (await (core/tick! eng))
          (is (= ["j1"] (:list (core/state eng))))
          (doseq [[k v] (tu/floor 41 -10 60 10)] (.set blocks k v))
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng)))))))))
