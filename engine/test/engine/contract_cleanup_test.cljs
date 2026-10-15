(ns engine.contract-cleanup-test
  "A job's cleanup export (contract-conditions design 2.4): run once however the job ends, before the caller sees the
  ending, on its own budget; a cancel or a parent's end runs it innermost frame first; a death runs none."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.test-util :as tu]))

(def world (atom {}))
(def clock (atom 0))

(defn setup [jobs]
  (reset! world {})
  (reset! clock 1000000)
  (let [[seen sink] (tu/capture-sink)
        p (tu/fake {})
        eng (core/create {:primitives p :dir (tu/tmp-dir) :now #(deref clock) :jobs jobs :triggers {}
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :seen seen :p p}))

(defn job-ev [seen kind] (filterv #(= [:job kind] [(:source %) (:kind %)]) @seen))
(defn log! [x] (swap! world update :log (fnil conj []) x))
(defn looks [p] (count (filter #(= "look" (.-name %)) (.-calls (.-world p)))))
(defn look! [c] (ctx/act c :look #js {:yaw 0 :pitch 0}))

(def tidy
  {:conditions []
   :round (fn [c] (log! :round) (ctx/end c :done nil {:a 1}))
   :cleanup (fn [c e]
              (log! [:cleanup (:kind e)])
              (swap! clock + 2000)
              (ctx/end c :done nil {:still-led false}))})

(deftest cleanup-runs-once-after-a-done-ending-and-its-data-and-time-join-the-jobs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'tidy tidy})]
          (core/submit! eng '(tidy) {})
          (await (core/tick! eng))
          (await (core/tick! eng))
          (is (= [:round [:cleanup :done]] (:log @world)))
          (let [d (:data (first (job-ev seen :completed)))]
            (is (= {:a 1 :still-led false :kind :done} (select-keys d [:a :still-led :kind])))
            (is (= 2 (get-in (first (job-ev seen :completed)) [:data :report :uses :job-time]))
                "cleanup's time is in the job's report")))))))

(def stuck
  {:conditions []
   :round (fn [c] (ctx/end c :nothing-possible :stuck))
   :cleanup (fn [c e]
              (log! [:cleanup (:kind e) (:cause e)])
              (if (< (:n (swap! world update :n (fnil inc 0))) 3) :continue (ctx/end c :done nil {:let-go true})))})

(def parent
  {:conditions []
   :round (fn ^:async parent [c]
            (let [e (await (ctx/call c :kid 'stuck {}))]
              (if (= :continue e)
                (do (log! :parent-continue) :continue)
                (do (swap! world assoc :kid e) (ctx/end c :done nil)))))})

(deftest a-childs-cleanup-runs-before-its-parent-sees-the-ending-and-may-continue
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {'stuck stuck 'parent parent})]
          (core/submit! eng '(parent) {})
          (dotimes [_ 4] (await (core/tick! eng)))
          (is (= [[:cleanup :nothing-possible :stuck] :parent-continue
                  [:cleanup :nothing-possible :stuck] :parent-continue
                  [:cleanup :nothing-possible :stuck]]
                 (:log @world)))
          (is (= {:kind :nothing-possible :cause :stuck :data {:let-go true}}
                 (select-keys (:kid @world) [:kind :cause :data]))))))))

(def fails
  {:conditions []
   :round (fn [c] (ctx/end c :done nil))
   :cleanup (fn [c _] (ctx/end c :nothing-possible :gate-stuck {:gate 1}))})

(deftest a-failed-cleanup-keeps-the-ending-and-is-told
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'fails fails})]
          (core/submit! eng '(fails) {})
          (await (core/tick! eng))
          (is (= {:kind :done :cleanup {:kind :nothing-possible :cause :gate-stuck}}
                 (select-keys (:data (first (job-ev seen :completed))) [:kind :cleanup :gate])))
          (is (= [{:kind :nothing-possible :cause :gate-stuck}]
                 (mapv #(select-keys (:data %) [:kind :cause]) (job-ev seen :cleanup_failed)))))))))

(def overruns
  {:conditions []
   :cleanup-defaults {:spend {:job-time 1}}
   :round (fn [c] (ctx/end c :nothing-possible :stuck))
   :cleanup (fn [_ _] (log! :cleanup) (swap! clock + 2000) :continue)})

(deftest a-cleanup-past-its-own-budget-ends-and-the-ending-stays
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'overruns overruns})]
          (core/submit! eng '(overruns) {})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= [:cleanup] (:log @world)))
          (is (= {:kind :nothing-possible :cause :stuck :cleanup {:kind :limit-reached :cause {:line :job-time :spend 1}}}
                 (select-keys (:data (first (job-ev seen :stopped))) [:kind :cause :cleanup])))
          (is (= 1 (count (job-ev seen :cleanup_failed)))))))))

(def slow
  {:conditions [] :defaults {:spend {:job-time 5}}
   :round (fn [_] (swap! clock + 6000) :continue)
   :cleanup (fn ^:async cleanup [c e]
              (await (look! c))
              (log! [:slow-cleanup (:kind e) (:cause e)])
              (ctx/end c :done nil))})

(deftest the-jobs-own-spent-budget-does-not-stop-its-cleanup
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup {'slow slow})]
          (core/submit! eng '(slow) {})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= [[:slow-cleanup :limit-reached {:line :job-time :spend 5}]] (:log @world)))
          (is (= 1 (looks p)) "cleanup acts though the job's time is spent")
          (is (= :limit-reached (:kind (:data (first (job-ev seen :stopped)))))))))))

(def boss
  {:conditions [] :defaults {:spend {:job-time 5}}
   :round (fn ^:async boss [c]
            (let [e (await (ctx/call c :kid 'runner {}))]
              (if (= :continue e) :continue (ctx/end c :done nil))))
   :cleanup (fn [c e] (log! [:boss-cleanup (:kind e)]) (ctx/end c :done nil))})

(def runner
  {:conditions []
   :round (fn [_] (swap! clock + 6000) :continue)
   :cleanup (fn [c e] (log! [:runner-cleanup (:kind e)]) (ctx/end c :done nil))})

(deftest a-parents-end-runs-the-cleanups-innermost-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'boss boss 'runner runner})]
          (core/submit! eng '(boss) {})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= [[:runner-cleanup :cut-off] [:boss-cleanup :limit-reached]] (:log @world)))
          (is (= :limit-reached (:kind (:data (first (job-ev seen :stopped)))))))))))

(def endless
  {:conditions []
   :round (fn [_] (log! :round) :continue)
   :cleanup (fn [c e]
              (log! [:cleanup (:kind e) (:cause e)])
              (if (= 2 (count (filter vector? (:log @world)))) (ctx/end c :done nil) :continue))})

(deftest a-cancel-runs-cleanup-before-the-job-goes-and-a-second-cancel-does-not-cut-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'endless endless})
              id (core/submit! eng '(endless) {})]
          (await (core/tick! eng))
          (core/cancel! eng id)
          (is (= [] (job-ev seen :cancelled)) "not gone before its cleanup")
          (await (core/tick! eng))
          (core/cancel! eng id)
          (await (core/tick! eng))
          (await (core/tick! eng))
          (is (= [:round [:cleanup :cut-off :agent] [:cleanup :cut-off :agent]] (:log @world)))
          (is (= 1 (count (job-ev seen :cancelled))))
          (is (= [] (:list (core/state eng)))))))))

(deftest a-death-runs-no-cleanup
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {'endless endless})
              id (core/submit! eng '(endless) {})]
          (await (core/tick! eng))
          (core/cancel! eng id :death)
          (await (core/tick! eng))
          (is (= [:round] (:log @world)))
          (is (= [] (:list (core/state eng)))))))))
