(ns engine.job-waiting-test
  "A listed job whose check declines is never silent: one job.waiting event with the check's reason, again only when
  the reason changes, and the reason shown in jobs show and observe."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.event-api :as event-api]
            [engine.job-api :as job-api]
            [engine.test-util :as tu]))

(def gate
  "What the gated job's check answers: true, or a reason it waits for (nil: a plain false)."
  (atom true))

(defn gated-check [c]
  (let [g @gate]
    (cond (true? g) true
          (nil? g) false
          :else (ctx/wait c g))))

(defn ^:async count-round [c]
  (ctx/update-mem! c update :n (fnil inc 0))
  :continue)

(def registry
  {'gated {:check gated-check :round count-round}
   'parent {:check (fn [c] (ctx/check-child c :g 'gated {})) :round count-round}})

(defn setup []
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        eng (core/create {:primitives (tu/fake {}) :jobs registry :triggers {} :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :seen seen}))

(defn ^:async ticks [eng n]
  (dotimes [_ n] (await (core/tick! eng))))

(defn waits [seen] (filterv #(= :waiting (:kind %)) @seen))

(deftest a-declining-check-emits-job-waiting-once-with-its-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (reset! gate :no-furnace)
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
        (reset! gate {:reason :cooking :ready-at 5})
        (let [{:keys [eng seen]} (setup)
              id (core/submit! eng '(gated) {})]
          (await (ticks eng 3))
          (reset! gate {:reason :cooking :ready-at 9})
          (await (ticks eng 3))
          (is (= [{:reason :cooking :ready-at 5} {:reason :cooking :ready-at 9}]
                 (mapv #(select-keys % [:reason :ready-at]) (waits seen))))
          (reset! gate true)
          (await (ticks eng 1))
          (is (nil? (:waiting (job-api/summary eng id))) "a passing check clears the reason")
          (reset! gate {:reason :cooking :ready-at 9})
          (await (ticks eng 2))
          (is (= 3 (count (waits seen))) "a new wait after a run is told again"))))))

(deftest a-plain-false-check-still-says-it-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (reset! gate nil)
        (let [{:keys [eng seen]} (setup)]
          (core/submit! eng '(gated) {})
          (await (ticks eng 3))
          (is (= [:not-ready] (mapv :reason (waits seen)))))))))

(deftest a-parent-check-reports-its-childs-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (reset! gate :no-tree)
        (let [{:keys [eng seen]} (setup)]
          (core/submit! eng '(parent) {})
          (await (ticks eng 3))
          (is (= [:no-tree] (mapv :reason (waits seen)))))))))

(deftest wait-is-false-outside-a-scheduler-check
  (is (false? (ctx/wait {} :anything))))
