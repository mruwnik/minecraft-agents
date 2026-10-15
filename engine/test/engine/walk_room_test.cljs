(ns engine.walk-room-test
  "A walk's steer asks no more time than its call chain has left, so limits never refuse it."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [floor]]
            [engine.triggers :as triggers]))

(defn setup
  "The fake world over a flat floor, its clock 1 ms on at every read (time passes while a walk plans); jobs as well
  as the registry's."
  [clock jobs]
  (let [[seen sink] (tu/capture-sink)
        p (tu/fake {:blocks (floor -2 -3 40 3) :self {:pos {:x 0 :y 64 :z 0}}})
        eng (core/create {:primitives p :jobs (merge registry/jobs jobs) :triggers triggers/all :dir (tu/tmp-dir) :now #(swap! clock inc)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn parent
  "Runs go-to with args as its child, out its ending; before the first call, the clock moves on ahead-ms."
  [out clock ahead-ms args]
  (let [begun (atom false)]
    {:round (fn ^:async parent-round [c]
              (when-not @begun
                (swap! clock + ahead-ms)
                (reset! begun true))
              (let [e (await (ctx/call c :kid 'jobs.movement.go-to args))]
                (if (= :continue e) :continue (do (reset! out e) (ctx/end c :done nil)))))}))

(defn ^:async go!
  "Run go-to with args under a parent (defaults defs) whose clock moves on ahead-ms first; {:ending :p :seen}."
  [defs ahead-ms args]
  (let [clock (atom 1000000)
        out (atom nil)
        {:keys [eng] :as s} (setup clock {'parent (assoc (parent out clock ahead-ms args) :defaults defs)})]
    (core/submit! eng '(parent) {})
    (loop [i 0]
      (when (and (< i 30) (seq (:list (core/state eng))))
        (await (core/tick! eng))
        (recur (inc i))))
    (assoc s :ending @out)))

(defn steer-timeouts [p] (keep #(when (= "steer" (.-name %)) (.-timeoutS (.-args %))) (.-calls (.-world p))))

(defn stopped [seen] (keep #(when (= [:job :stopped] [(:source %) (:kind %)]) (select-keys (:data %) [:kind :cause])) @seen))

(deftest a-leg-steers-what-is-left-of-the-leg
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] e :ending} (await (go! nil 0 {:pos [10 64 0] :range 0 :spend {:job-time 5}}))]
          (is (= :done (:kind e)) (pr-str (:cause e) (stopped seen)))
          (is (seq (steer-timeouts p)))
          (is (every? #(< % 5) (steer-timeouts p)) (pr-str (steer-timeouts p))))))))

(deftest a-walk-with-no-time-line-steers-what-its-caller-has-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] e :ending} (await (go! {:spend {:job-time 10}} 4000 {:pos [10 64 0] :range 0}))]
          (is (= :done (:kind e)) (pr-str (:cause e) (stopped seen)))
          (is (seq (steer-timeouts p)))
          (is (every? #(< % 6) (steer-timeouts p)) (pr-str (steer-timeouts p))))))))
