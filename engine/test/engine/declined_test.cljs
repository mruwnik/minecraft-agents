(ns engine.declined-test
  "jobs.lib.declined: a parent whose round returns a declined child's :declined is parked by its check until the
  child's check passes."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [jobs.lib.declined :as declined]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as world]))

(defn setup []
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        need (atom true)
        use-kid (atom true)
        kid {:check (fn [c] (if @need (ctx/wait c {:reason :need :what "thing"}) true))
             :round (fn ^:async kid-round [_] :done)}
        parent {:check (fn [c] (declined/check c))
                :round (fn ^:async parent-round [c]
                         (declined/begin! c)
                         (if-not @use-kid
                           :continue
                           (let [r (await (declined/call-child! c :kid 'kid {}))]
                             (if (= :done r) :done r))))}
        p (tu/fake {:self {:pos {:x 0 :y 65 :z 0}}})
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'kid kid 'parent parent)
                          :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock) :world (world/of-data {} {} [])
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng '(parent) {})
    {:eng eng :clock clock :seen seen :need need :use-kid use-kid}))

(defn ^:async tick-n! [{:keys [eng clock]} n]
  (dotimes [_ n] (swap! clock + 500) (await (core/tick! eng))))

(defn rounds [{:keys [seen]}] (count (filter #(= :round_started (:kind %)) @seen)))

(deftest a-parent-of-a-declining-child-runs-one-round-then-none-and-resumes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup)]
          (await (tick-n! s 12))
          (is (= 1 (rounds s)) "one round meets the decline, then parked")
          (is (= 1 (count (filter #(= :waiting (:kind %)) @(:seen s)))) "the wait is told once")
          (reset! (:need s) false)
          (await (tick-n! s 3))
          (is (empty? (:list (core/state (:eng s)))) "done once the child's check passes"))))))

(deftest a-round-that-calls-no-child-drops-the-booking
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup)]
          (await (tick-n! s 4))
          (reset! (:use-kid s) false)
          (reset! (:need s) false)
          (await (tick-n! s 2))
          (let [before (rounds s)]
            (reset! (:need s) true)
            (await (tick-n! s 4))
            (is (< before (rounds s)) "no stale booking parks the parent on the child's check")))))))
