(ns engine.need-test
  "jobs.lib.need: the fetch jobs as contract children; their endings read as causes of the caller."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.test-util :as tu]
            [jobs.lib.need :as need]))

(def got (atom nil))

(defn stopper
  "An old fetch job that stops with reason, or finishes when reason is nil."
  [seen-args reason]
  {:check (fn [_] true)
   :round (fn ^:async r [c]
            (reset! seen-args (:args c))
            (when reason (ctx/result! c {:status :stopped :reason reason}))
            :done)})

(defn run! [call-fn obtain-reason tool-reason]
  (reset! got nil)
  (let [obtain-args (atom nil)
        tool-args (atom nil)
        [_ sink] (tu/capture-sink)
        jobs {'jobs.items.obtain (stopper obtain-args obtain-reason)
              'jobs.items.get-tool (stopper tool-args tool-reason)
              'caller {:conditions [] :args {}
                       :round (fn ^:async r [c]
                                (reset! got (await (call-fn c)))
                                (ctx/end c :done nil))}}
        eng (core/create {:primitives (tu/fake {}) :dir (tu/tmp-dir) :now (constantly 1000000) :jobs jobs :triggers {}
                          :events (events/make {:body "Fake" :sinks [sink] :now (constantly 1000000)})})]
    (core/submit! eng '(caller {}) {})
    (core/tick! eng)
    {:obtain obtain-args :tool tool-args}))

(defn ^:async run-wait! [& args]
  (let [r (apply run! args)]
    (await (js/Promise. (fn [res] (js/setTimeout res 50))))
    r))

(deftest item!-passes-a-done-child
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run-wait! #(need/item! % "iron_ingot" 3) nil nil))]
          (is (= :done (:kind @got)))
          (is (= {:item "iron_ingot" :count 3 :explore false} (select-keys @(:obtain r) [:item :count :explore]))))))))

(deftest item!-failure-reads-as-the-need-with-the-childs-ending-as-because
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (run-wait! #(need/item! % "iron_ingot" 3) :no-source nil))
        (is (= :condition-failed (:kind @got)) "no-source is a missing fact")
        (is (= {:reason :need :why :no-source :item "iron_ingot" :count 3} (:cause @got)))
        (is (= :no-source (contract/cause-key (:because @got))))))))

(deftest a-full-bag-is-free-slots-and-a-limit-stays-a-limit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (run-wait! #(need/blocks! % ["cobblestone" "dirt"] 8) :inventory-full nil))
        (is (= :free-slots (contract/cause-key @got)))
        (is (= ["cobblestone" "dirt"] (get-in @got [:cause :any-of])))
        (await (run-wait! #(need/item! % "coal" 1) :timeout nil))
        (is (= :limit-reached (:kind @got)) "obtain's :timeout is a time limit")
        (is (= :need (contract/cause-key @got)))))))

(deftest tool!-names-the-tool-wanted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run-wait! #(need/tool! % {:block "stone"} {:explore true}) nil :no-source))]
          (is (= {:reason :tool :why :no-source :block "stone"} (:cause @got)))
          (is (= {:block "stone" :explore true} (select-keys @(:tool r) [:block :explore]))))))))
