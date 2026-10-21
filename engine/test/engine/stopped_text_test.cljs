(ns engine.stopped-text-test
  "The :stopped job event's :text: the result's own :text (whole), else the reason."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as world]))

(defn ^:async stopped-run
  "Run a job that ends with result; [the :text of its :stopped event, the engine's recorded outcome of it]."
  [result]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        job {:round (fn ^:async r [c] (ctx/end c :nothing-possible (:reason result) (dissoc result :status :reason)))}
        eng (core/create {:primitives (tu/fake {:self {:pos {:x 0 :y 65 :z 0}}})
                          :jobs (assoc registry/jobs 'stopper job)
                          :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock) :world (world/of-data {} {} [])
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng '(stopper) {})
    (await (tu/run-ticks eng clock 4 500))
    [(:text (first (filter #(= :stopped (:kind %)) @seen))) (first (vals (core/recent-results eng)))]))

(defn ^:async stopped-text [result] (first (await (stopped-run result))))

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

(deftest stopped-event-text-is-whole
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [long (apply str (repeat 500 "a"))]
          (is (= long (await (stopped-text {:status :stopped :reason :x :text long})))))))))

(deftest recorded-outcome-of-a-stop-keeps-its-whole-cause-even-when-the-data-is-big
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [why (apply str (repeat 60 "gathering was tried and ended :no-torches; "))
              [_ entry] (await (stopped-run {:status :stopped :reason :x :why why :text why :filler (apply str (repeat 3000 "f"))}))]
          (is (= why (:text entry)))
          (is (= why (get-in entry [:result :why])))
          (is (true? (:result-clipped entry)))
          (is (not (contains? (:result entry) :filler))))))))
