(ns engine.body-idle-test
  "body.idle: one agent-facing event per stretch with no useful act (only idle-ok jobs act, the rest wait), on the
  fake clock."
  (:require [engine.contract :as contract]
            [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]))

(def always (constantly true))

(defn ^:async filler-round [c]
  (await (ctx/act c :look #js {:pos #js {:x 0 :y 64 :z 3}}))
  (await (ctx/act c :wait #js {:ms 2000}))
  (ctx/end c :done nil))

(defn ^:async work-round [c]
  (await (ctx/act c :look #js {:pos #js {:x 3 :y 64 :z 0}}))
  :continue)

(defn no-source [_] (contract/later :no-source "no stone in sight"))

(def jobs
  {'filler {:round filler-round :idle-ok true}
   'work {:round work-round}
   'stuck (tu/job [no-source] work-round)})

(defn setup [opts]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        eng (core/create (merge {:primitives (tu/fake {}) :jobs jobs :triggers {} :dir (tu/tmp-dir)
                                 :now #(deref clock)
                                 :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})}
                                opts))]
    {:eng eng :seen seen :clock clock}))

(defn idles [seen] (filterv #(= [:body :idle] [(:source %) (:kind %)]) @seen))

(defn ^:async run-for
  "Tick every 500 ms of fake time for ms, letting each started round settle."
  [eng clock ms]
  (loop [left ms]
    (when (pos? left)
      (await (core/tick! eng))
      (swap! clock + 500)
      (recur (- left 500)))))

(deftest filler-only-for-the-idle-time-tells-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (setup {})]
          (core/submit! eng '(repeat (filler)) {})
          (await (run-for eng clock 9000))
          (is (= [] (idles seen)) "under 10 s: nothing yet")
          (await (run-for eng clock 2000))
          (is (= 1 (count (idles seen))) "10 s of filler only: one body.idle")
          (await (run-for eng clock 60000))
          (is (= 1 (count (idles seen))) "one per stretch")
          (is (= [{:job "j1" :name "(repeat filler)" :idle-ok true}] (:jobs (first (idles seen)))))
          (is (some? (mem/latest (mem/view (:store eng)) :body-idle)) "triggers see it: (since :body-idle)"))))))

(deftest a-waiting-job-and-filler-names-the-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (setup {})]
          (core/submit! eng '(stuck) {})
          (core/submit! eng '(repeat (filler)) {})
          (await (run-for eng clock 12000))
          (let [[e :as all] (idles seen)]
            (is (= 1 (count all)))
            (is (= [{:job "j1" :name "stuck" :waiting {:reason :no-source :text "no stone in sight"}}
                    {:job "j2" :name "(repeat filler)" :idle-ok true}]
                   (:jobs e)))
            (is (re-find #"j1 stuck waiting: no stone in sight" (:text e)) (:text e))))))))

(deftest a-real-job-acting-is-not-idle
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (setup {})]
          (core/submit! eng '(work) {})
          (core/submit! eng '(repeat (filler)) {})
          (await (run-for eng clock 60000))
          (is (= [] (idles seen))))))))

(deftest the-setting-sets-the-idle-time
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (setup {:body-idle-s 30})]
          (core/submit! eng '(repeat (filler)) {})
          (await (run-for eng clock 25000))
          (is (= [] (idles seen)) "the caller's 30 s, not the default 10")
          (await (run-for eng clock 6000))
          (is (= 1 (count (idles seen)))))))))

(deftest a-queue-change-or-a-real-act-re-arms
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (setup {})]
          (core/submit! eng '(repeat (filler)) {})
          (await (run-for eng clock 12000))
          (is (= 1 (count (idles seen))))
          (core/submit! eng '(stuck) {})
          (await (run-for eng clock 5000))
          (is (= 1 (count (idles seen))) "a queue change restarts the clock")
          (await (run-for eng clock 6000))
          (is (= 2 (count (idles seen))) "a new stretch tells again")
          (let [id (core/submit! eng '(work) {})]
            (await (run-for eng clock 3000))
            (core/cancel! eng id :agent))
          (await (run-for eng clock 9000))
          (is (= 2 (count (idles seen))) "the work's acts re-armed the clock")
          (await (run-for eng clock 2000))
          (is (= 3 (count (idles seen)))))))))

(deftest idle-ok-jobs-declare-it
  (doseq [sym '[jobs.movement.linger-near jobs.time.wait-for-light]]
    (is (true? (:idle-ok (get registry/jobs sym))) (str sym)))
  (is (nil? (:idle-ok (get registry/jobs 'jobs.items.get-tool)))))
