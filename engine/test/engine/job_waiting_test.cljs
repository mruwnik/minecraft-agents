(ns engine.job-waiting-test
  "A listed job whose condition fails is never silent: one job.waiting event with its cause, again only when
  the cause changes, and the cause shown in jobs show and observe."
  (:require [engine.contract :as contract]
            [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.event-api :as event-api]
            [engine.fake :as fake]
            [engine.job-api :as job-api]
            [engine.primitive :as prim]
            [engine.test-util :as tu]
            [jobs.lib.cond :as cond]
            [jobs.lib.waits :as waits]))

(def gate
  "What the gated job's check answers: true (passes), or the :later status it waits with."
  (atom true))

(defn gate-condition [_]
  (let [g @gate] (if (true? g) (contract/ok :open "open") g)))

(defn ^:async count-round [c]
  (ctx/update-mem! c update :n (fnil inc 0))
  :continue)

(def asked (atom 0))

(def dark-later (contract/later :dark "dark" :recheck-after-ms 60000))

(defn ^:async child-waits-round
  "A round whose called child ended condition-failed :dark: it waits that out (:continue)."
  [c]
  (swap! asked inc)
  (waits/waiting-out c (contract/ending :condition-failed (contract/status-cause dark-later) nil)))

(defn torch-check
  "Passes once a torch is carried, else waits :no-torches naming the torch, rechecked in 10 min."
  [c]
  (swap! asked inc)
  (if (pos? (prim/carried (:primitives c) "torch"))
    (contract/ok :torches "torches")
    (contract/later :no-torches "no torches" :item "torch" :recheck-after-ms 600000)))

(def registry
  {'torch-wait (tu/job [torch-check] count-round)
   'gated (tu/job [gate-condition] count-round)
   'counted-dark (tu/job [(fn [_] (swap! asked inc) dark-later)] count-round)
   'child-dark (tu/job child-waits-round)
   'never (tu/job [(constantly (contract/later :not-ready "not ready"))] count-round)
   'torch-lit {:can-start? (cond/because :few-torches (cond/>= (cond/inventory "torch") 4)) :round count-round}})

(defn setup
  ([] (setup {}))
  ([spec]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake spec)
        eng (core/create {:primitives p :jobs registry :triggers {} :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :seen seen :clock clock :p p})))

(defn ^:async ticks [eng n]
  (dotimes [_ n] (await (core/tick! eng))))

(defn waits [seen] (filterv #(= :waiting (:kind %)) @seen))

(deftest a-failing-condition-emits-job-waiting-once-with-its-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (reset! gate (contract/later :no-furnace "no furnace"))
        (let [{:keys [eng seen]} (setup)
              id (core/submit! eng '(gated) {})]
          (await (ticks eng 5))
          (is (= 1 (count (waits seen))) "once, not every round")
          (is (= {:job id :reason :no-furnace} (select-keys (first (waits seen)) [:job :reason])))
          (is (= :no-furnace (get-in (job-api/summary eng id) [:waiting :reason])) "jobs list says why")
          (is (= :no-furnace (get-in (event-api/status eng nil) [:jobs :items 0 :waiting :reason])) "observe says why")
          (is (= :no-furnace (get-in (event-api/job-detail eng id 5) [:waiting :reason])) "jobs show says why"))))))

(deftest a-changed-reason-is-a-new-event-and-a-pass-clears-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (reset! gate (contract/later :cooking "cooking" :ready-at 5))
        (let [{:keys [eng seen]} (setup)
              id (core/submit! eng '(gated) {})]
          (await (ticks eng 3))
          (reset! gate (contract/later :cooking "cooking" :ready-at 9))
          (await (ticks eng 3))
          (is (= [5 9] (mapv :ready-at (waits seen))))
          (reset! gate true)
          (await (ticks eng 1))
          (is (nil? (:waiting (job-api/summary eng id))) "a passing condition clears the reason"))))))

(deftest a-composite-reports-its-childs-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (reset! gate (contract/later :no-tree "no tree" :near 3))
        (doseq [spec ['(seq (gated)) '(any (gated)) '(repeat (gated)) '(until (never) (gated))
                      '(seq (repeat (gated))) '(until (never) (any (seq (gated))))]]
          (let [{:keys [eng seen]} (setup)]
            (core/submit! eng spec {})
            (await (ticks eng 3))
            (is (= [[:no-tree 3]] (mapv (juxt :reason :near) (waits seen))) (str spec))))))))

(deftest wait-is-false-outside-a-scheduler-check
  (is (false? (ctx/wait {} :anything))))

(deftest a-failing-fact-condition-waits-showing-its-comparison
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (core/submit! eng '(torch-lit) {})
          (await (ticks eng 2))
          (is (= [[:few-torches "waiting: inventory torch 0 < 4"]] (mapv (juxt :reason :text) (waits seen)))))))))

(deftest a-job-behind-a-holding-reflex-says-so-and-the-reason-ends-with-the-reflex
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              release (atom nil)
              held (js/Promise. (fn [r] (reset! release r)))
              reg (assoc registry 'night-hold (tu/job [] (fn ^:async r [c] (await held) (ctx/end c :done nil nil))))
              [_ sink] (tu/legacy-capture-sink)
              eng (core/create {:primitives (tu/fake {}) :jobs reg
                                :triggers {:night {:name :night :job '(night-hold) :persistence :stop :when (constantly true)}}
                                :dir (tu/tmp-dir) :now #(deref clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})
              id (core/submit! eng '(gated) {})]
          (reset! gate true)
          (core/register-reflex! eng {:trigger :night})
          (let [night (core/tick! eng)]
            (core/tick! eng)
            (is (= :reflex-holds (get-in (job-api/summary eng id) [:waiting :reason])) "queued behind the reflex, and says so")
            (@release nil)
            (await night))
          (await (ticks eng 2))
          (is (nil? (:waiting (job-api/summary eng id))) "the reason goes with the reflex"))))))

(deftest a-wait-naming-its-recheck-time-is-not-asked-again-before-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [spec ['(counted-dark) '(child-dark)]]
          (reset! asked 0)
          (let [{:keys [eng clock]} (setup)
                id (core/submit! eng spec {})]
            (dotimes [_ 40] (swap! clock + 250) (await (core/tick! eng)))
            (is (= 1 @asked) (str spec ": 10 s of ticks inside a 60 s recheck, one check or round"))
            (is (= :dark (get-in (job-api/summary eng id) [:waiting :reason])) (str spec " says why"))
            (swap! clock + 60000)
            (await (core/tick! eng))
            (is (= 2 @asked) (str spec ": asked again once it lapsed"))))))))

(deftest a-resting-wait-wakes-the-tick-what-it-was-judged-on-changes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[what change asked?] [["torches arrive" #(fake/give % "torch" 4) #(< 1 %)]
                                      ["another item" #(fake/give % "dirt" 4) #{1}]
                                      ["the body moves" #(assoc-in % [:self :pos] [5.5 64 0.5]) #{2}]
                                      ["a torch lights its cell" #(assoc-in % [:light [0 64 0]] [15 14]) #{2}]
                                      ["the time is set to night" #(assoc % :time 18000) #{2}]]]
          (reset! asked 0)
          (let [{:keys [eng clock p]} (setup {:time 1000 :light {}})]
            (core/submit! eng '(seq (torch-wait)) {})
            (dotimes [_ 10] (swap! clock + 250) (await (core/tick! eng)))
            (is (= 1 @asked) (str what ": rests on its 10 min recheck"))
            (swap! (fake/state p) change)
            (swap! clock + 250)
            (await (core/tick! eng))
            (is (asked? @asked) (str what ": checked again in the next tick only when it was judged on it"))
            (dotimes [_ 10] (swap! clock + 250) (await (core/tick! eng)))
            (is (asked? @asked) (str what ": a wait told again rests again"))))))))
