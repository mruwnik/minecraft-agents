(ns engine.clear-path-test
  "jobs.access.clear-path against the fake world: how a dig child's wait or a refilled cell ends the door."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [box]]
            [engine.triggers :as triggers]))

(def job 'jobs.access.clear-path)

(def wall
  "A stone wall at x 1..2 (y 64..65, z -1..1) on a stone floor, the body at the origin facing east."
  {:self {:pos {:x 0 :y 64 :z 0}}
   :blocks (merge (box -2 63 -2 8 63 2 "stone") (box 1 64 -1 2 65 1 "stone"))})

(defn ^:async run-door
  "Run clear-path east over world (stub: a replacement round for jobs.blocks.dig, or nil) as a child; its result."
  [world stub]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        p (tu/fake world)
        out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid job {:heading :east :max-thick 3}))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        jobs (cond-> (assoc registry/jobs 'recording-parent parent)
               stub (assoc 'jobs.blocks.dig (assoc (get registry/jobs 'jobs.blocks.dig) :round stub)))
        eng (core/create {:primitives p :jobs jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng '(recording-parent) {})
    (loop [i 0]
      (when (and (< i 60) (seq (:list (core/state eng))))
        (swap! clock + 700)
        (await (core/tick! eng))
        (recur (inc i))))
    @out))

(deftest a-dig-child-that-waits-ends-the-door-dig-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run-door wall nil))]
          (is (= [:stopped :dig-waits :no-tool] [(:status r) (:reason r) (:reason (:wait r))]) (pr-str r))
          (is (= [1 65 0] (:cell r)) "the top layer first"))))))

(deftest a-cell-that-is-dug-but-never-clears-ends-refills
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [digs (atom 0)
              stub (fn ^:async refilling-dig [c]
                     (swap! digs inc)
                     (ctx/result! c {:dug true})
                     :done)
              r (await (run-door (assoc wall :inventory [{:name "wooden_pickaxe" :count 1}]) stub))]
          (is (= [:stopped :refills] [(:status r) (:reason r)]) (pr-str r))
          (is (= 8 @digs) "a cell is dug max-cell-digs times"))))))
