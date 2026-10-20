(ns engine.bad-args-impossible-test
  "A call with missing or wrong args can never start: the job ends :impossible with the cause (end-statuses rule 2)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]))

(defn ^:async ended [spec]
  (let [[seen sink] (tu/capture-sink)
        eng (core/create {:primitives (tu/seeing-all (tu/fake {})) :jobs registry/jobs :dir (tu/tmp-dir)
                          :events (events/make {:sinks [sink]})})]
    (core/submit! eng spec {})
    (await (core/tick! eng))
    (let [e (first (filter #(#{:completed :stopped} (:kind %)) @seen))]
      (core/shutdown! eng)
      [(:kind (:data e)) (:reason (:data e))])))

(deftest a-call-missing-its-required-arg-ends-impossible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[spec reason] [['(jobs.items.obtain {}) :bad-args]
                               ['(jobs.items.craft {}) :bad-args]
                               ['(jobs.items.give {}) :bad-args]
                               ['(jobs.items.interact {}) :bad-args]
                               ['(jobs.items.enchant {}) :bad-args]
                               ['(jobs.items.get-tool {:block "no_such_block"}) :bad-args]
                               ['(jobs.farm.plant {}) :bad-args]
                               ['(jobs.farm.till {}) :bad-args]
                               ['(jobs.forestry.fell-from-pillar {}) :bad-args]
                               ['(jobs.strip.seal {}) :bad-args]
                               ['(jobs.gather.mine {}) :no-block]]]
          (is (= [:impossible reason] (await (ended spec))) (pr-str spec)))))))
