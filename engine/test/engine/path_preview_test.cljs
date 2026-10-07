(ns engine.path-preview-test
  "jobs.movement.path-preview against the fake world: plans like go-to, walks nowhere."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.test-util :as tu :refer [box floor]]
            [engine.triggers :as triggers]))

(def job 'jobs.movement.path-preview)

(defn ^:async preview
  "Run the job as a child of a recording parent over the fake world (body at 0 64 1); {:out the result :p primitives}."
  [blocks args]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        p (tu/fake {:blocks blocks :self {:pos {:x 0 :y 64 :z 1}}})
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
    (loop [i 0]
      (when (and (< i 200) (seq (:list (core/state eng))))
        (swap! clock + 500)
        (await (core/tick! eng))
        (recur (inc i))))
    {:out @out :p p}))

(defn pos-of [p] (let [q (.-pos (.self p))] [(.-x q) (.-y q) (.-z q)]))

(deftest a-found-route-is-reported-and-not-walked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (preview (floor 0 0 11 2) {:pos [10 64 1] :range 0}))]
          (is (= :completed (:status out)))
          (is (true? (:found out)))
          (is (= 10 (js/Math.round (:length out))))
          (is (pos? (:seconds out)))
          (is (string? (:summary out)))
          (is (pos? (:steps out)))
          (is (= [10 64 1] (last (:waypoints out))))
          (is (= [0 64 1] (pos-of p))))))))

(deftest a-goal-with-no-way-is-stopped-with-a-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (preview (merge (floor 0 0 3 2) (floor 8 0 11 2)) {:pos [10 64 1] :range 0}))]
          (is (= :stopped (:status out)))
          (is (false? (:found out)))
          (is (some? (:reason out))))))))

(deftest drop-cost-false-takes-no-two-block-drop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (merge (floor 0 0 3 2) (floor 61 4 0 6 2))
              free (:out (await (preview world {:pos [5 62 1] :range 0})))
              none (:out (await (preview world {:pos [5 62 1] :range 0 :drop-cost false})))]
          (is (true? (:found free)))
          (is (= 1 (get-in free [:moves :drop])))
          (is (false? (:found none))))))))

(deftest tolls-send-the-route-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (floor 0 0 11 2)
              cheap (:out (await (preview world {:pos [10 64 1] :range 0})))
              dear (:out (await (preview world {:pos [10 64 1] :range 0
                                                :tolls (vec (for [x (range 2 9)] {:x x :y 64 :z 1 :factor 20}))})))]
          (is (= 1 (count (:waypoints cheap))))
          (is (> (count (:waypoints dear)) 1)))))))

(deftest a-bad-drop-cost-is-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (preview (floor 0 0 3 2) {:pos [2 64 1] :drop-cost -1}))]
          (is (= :stopped (:status out)))
          (is (= :bad-drop-cost (:reason out))))))))
