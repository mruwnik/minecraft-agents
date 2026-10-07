(ns engine.linger-near-test
  "jobs.movement.linger-near against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(def job 'jobs.movement.linger-near)

(defn setup
  "An engine over the fake world; a recording parent runs the job as its child and puts the child's result in :out."
  [world args]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor world)
        out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid job args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'recording-parent parent)
                          :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng '(recording-parent) {})
    {:eng eng :p p :clock clock :seen seen :out out}))

(defn ^:async run-ticks [{:keys [eng clock]} n step]
  (loop [i 0]
    (when (and (< i n) (seq (:list (core/state eng))))
      (swap! clock + step)
      (await (core/tick! eng))
      (recur (inc i)))))

(defn ^:async linger [world args n step]
  (let [s (setup world args)]
    (await (run-ticks s n step))
    s))

(defn self-x [p] (.-x (.-pos (.self p))))

(deftest check-wants-a-pos-and-a-positive-wait
  (are [args ok] (= ok ((:check (get registry/jobs job)) {:args args}))
    {:pos {:x 1 :y 64 :z 0} :wait-s 5} true
    {:pos {:x 1 :y 64 :z 0} :wait-s 0} false
    {:pos {:x 1 :y 64 :z 0}} false
    {:wait-s 5} false))

(deftest within-range-it-holds-still-and-ends-after-the-wait
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out eng]} (await (linger {} {:pos {:x 2 :y 64 :z 0} :range 3 :wait-s 3} 12 500))]
          (is (empty? (tu/walk-calls p)))
          (is (= {:lingered true :reason "waited"} @out))
          (is (empty? (tu/walked-to eng))))))))

(deftest a-far-target-is-walked-to-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out eng]} (await (linger {} {:pos {:x 10 :y 64 :z 0} :range 3 :wait-s 30} 4 500))]
          (is (= [{:x 10 :y 64 :z 0}] (tu/walked-to eng)))
          (is (<= (Math/abs (- 10 (self-x p))) 3))
          (is (= :not-done @out) "still waiting"))))))

(deftest an-unreachable-target-ends-stopped-with-a-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (linger {:unreachable ["10,64,0"]} {:pos {:x 10 :y 64 :z 0} :range 0 :wait-s 30} 12 500))]
          (is (= :stopped (:status @out)))
          (is (= :unreachable (:reason @out)))
          (is (false? (:lingered @out))))))))
