(ns engine.contract-done-test
  "done? as the engine asks it (docs/contract-conditions-design.md 2.2): before every round and between steps; an :ok
  ends the job :done, and every ending the engine makes carries its :result and count."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.core.ending :as ending]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.test-util :as tu]
            [jobs.lib.pace :as pace]))

(def world (atom {}))

(defn setup [jobs]
  (reset! world {})
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        eng (core/create {:primitives (tu/fake {}) :dir (tu/tmp-dir) :now #(deref clock) :jobs jobs :triggers {}
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :seen seen :clock clock}))

(defn job-ev [seen kind] (filterv #(= [:job kind] [(:source %) (:kind %)]) @seen))

(defn goal
  "done? reading world: :ok once :placed reaches 3, else :later; :result says how many."
  [_]
  (let [n (:placed @world 0)
        result {:placed n}]
    (if (>= n 3)
      (contract/ok :built "wall built: 3 placed" :units :blocks :done n :left 0 :result result)
      (contract/later :owed (str (- 3 n) " blocks owed") :units :blocks :done n :left (- 3 n) :result result))))

(def builder
  "Places one block a round and yields; done? alone ends it."
  {:can-run? (fn [_] (if (:paused @world) (contract/later :paused "paused") (contract/ok :go "go")))
   :defaults {:least :all :spend {:wall-time 60}}
   :done? goal
   :round (fn [_] (swap! world update :placed (fnil inc 0)) :continue)})

(deftest a-goal-that-holds-before-the-start-ends-done-already-with-its-result
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'builder builder})]
          (swap! world assoc :placed 3)
          (core/submit! eng '(builder) {})
          (await (core/tick! eng))
          (is (= {:kind :done :cause :already :placed 3}
                 (select-keys (:data (first (job-ev seen :completed))) [:kind :cause :placed])))
          (is (= {:units :blocks :done 3 :left 0}
                 (select-keys (:report (:data (first (job-ev seen :completed)))) [:units :done :left]))))))))

(deftest done-is-asked-before-every-round-and-its-ok-ends-the-job-with-text-and-result
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'builder builder})]
          (core/submit! eng '(builder) {})
          (dotimes [_ 5] (await (core/tick! eng)))
          (is (= 3 (:placed @world)) "no round once the goal holds")
          (is (= {:kind :done :placed 3} (select-keys (:data (first (job-ev seen :completed))) [:kind :cause :placed])))
          (is (= "wall built: 3 placed" (:message (first (job-ev seen :completed)))))
          (is (= {:units :blocks :done 3 :left 0}
                 (select-keys (:report (:data (first (job-ev seen :completed)))) [:units :done :left]))))))))

(deftest a-job-waiting-on-can-run-ends-done-when-its-goal-comes-to-hold
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'builder builder})]
          (core/submit! eng '(builder) {})
          (await (core/tick! eng))
          (swap! world assoc :paused true)
          (await (core/tick! eng))
          (swap! world assoc :placed 3)
          (await (core/tick! eng))
          (is (= {:kind :done :placed 3} (select-keys (:data (first (job-ev seen :completed))) [:kind :cause :placed]))))))))

(def stepper
  "One block a step; done? is asked between steps."
  {:can-run? (fn [_] (contract/ok :go "go"))
   :done? goal
   :round (fn ^:async stepper [c]
            (await (pace/steps! c (fn []
                                    (swap! world update :placed (fnil inc 0))
                                    (if (> (:placed @world) 5) (ctx/end c :nothing-possible :ran-on) :again)))))})

(deftest done-is-asked-between-steps
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'stepper stepper})]
          (core/submit! eng '(stepper) {})
          (await (core/tick! eng))
          (is (= 3 (:placed @world)) "the steps stop once the goal holds")
          (is (= {:kind :done :placed 3} (select-keys (:data (first (job-ev seen :completed))) [:kind :cause :placed]))))))))

(deftest a-limit-ending-carries-dones-result-and-count
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (setup {'builder builder})]
          (core/submit! eng '(builder) {})
          (await (core/tick! eng))
          (swap! world assoc :paused true)
          (swap! clock + 61000)
          (await (core/tick! eng))
          (is (= {:kind :limit-reached :placed 1} (select-keys (:data (first (job-ev seen :stopped))) [:kind :placed])))
          (is (= {:units :blocks :done 1 :left 2}
                 (select-keys (:report (:data (first (job-ev seen :stopped)))) [:units :done :left]))))))))

(deftest a-cancel-carries-the-last-asked-dones-result-and-count
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'builder builder})
              id (core/submit! eng '(builder) {})]
          (await (core/tick! eng))
          (core/cancel! eng id)
          (is (= {:kind :cut-off :cause :agent :placed 0} (select-keys (:data (first (job-ev seen :cancelled))) [:kind :cause :placed])))
          (is (= {:units :blocks :done 0 :left 3}
                 (select-keys (:report (:data (first (job-ev seen :cancelled)))) [:units :done :left]))))))))

(def fragile
  "A builder whose done? throws once :boom is set."
  (assoc builder :done? (fn [c] (when (:boom @world) (throw (js/Error. "boom"))) (goal c))))

(deftest a-cancel-uses-the-last-done-status-and-asks-nothing-more
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'fragile fragile})
              id (core/submit! eng '(fragile) {})]
          (await (core/tick! eng))
          (swap! world assoc :boom true)
          (core/cancel! eng id)
          (is (= {:kind :cut-off :cause :agent :placed 0} (select-keys (:data (first (job-ev seen :cancelled))) [:kind :cause :placed])))
          (is (empty? (:list (core/state eng))) "the job is gone"))))))

(deftest a-cancel-goes-ahead-when-the-progress-step-throws
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'builder builder})
              id (core/submit! eng '(builder) {})]
          (await (core/tick! eng))
          (with-redefs [ending/progress-of (fn [& _] (throw (js/Error. "boom")))]
            (core/cancel! eng id :death))
          (is (= {:kind :cut-off :cause :death} (select-keys (:data (first (job-ev seen :cancelled))) [:kind :cause])))
          (is (= 1 (count (filterv #(= :warn (:level %)) @seen)))))))))

(deftest done-is-asked-once-per-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [asked (atom 0)
              {:keys [eng]} (setup {'counted (assoc builder :done? (fn [c] (swap! asked inc) (goal c)))})]
          (core/submit! eng '(counted) {})
          (await (core/tick! eng))
          (await (core/tick! eng))
          (is (= 2 @asked) "one ask per round"))))))

(deftest a-waiting-job-asks-done-once-per-tick
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [asked (atom 0)
              {:keys [eng]} (setup {'counted (assoc builder :done? (fn [c] (swap! asked inc) (goal c)))})]
          (swap! world assoc :paused true)
          (core/submit! eng '(counted) {})
          (await (core/tick! eng))
          (let [n @asked]
            (await (core/tick! eng))
            (is (= 1 (- @asked n)))))))))
