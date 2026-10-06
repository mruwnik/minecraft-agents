(ns engine.stopped-text-test
  "The :stopped job event's :text: the result's own :text (clipped), else the reason."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as world]))

(defn ^:async stopped-text
  "Run a job that ends with result; the :text of its :stopped event."
  [result]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        job {:check (constantly true)
             :round (fn ^:async r [c] (ctx/result! c result) :done)}
        eng (core/create {:primitives (tu/fake {:self {:pos {:x 0 :y 65 :z 0}}})
                          :jobs (assoc registry/jobs 'stopper job)
                          :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock) :world (world/of-data {} {} [])
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng '(stopper) {})
    (dotimes [_ 4] (swap! clock + 500) (await (core/tick! eng)))
    (:text (first (filter #(= :stopped (:kind %)) @seen)))))

(deftest stopped-event-carries-the-result-text
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= "dug down 5 blocks" (await (stopped-text {:status :stopped :reason :no-stone-found :text "dug down 5 blocks"}))))))))

(deftest stopped-event-without-text-names-the-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= "stopped: unreachable" (await (stopped-text {:status :stopped :reason :unreachable}))))))))

(deftest stopped-event-text-is-clipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [text (await (stopped-text {:status :stopped :reason :x :text (apply str (repeat 500 "a"))}))]
          (is (= 200 (count text)))
          (is (= "…" (subs text 199))))))))
