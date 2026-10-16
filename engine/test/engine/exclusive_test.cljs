(ns engine.exclusive-test
  "An exclusive run pauses the whole engine: no trigger, no other job, until the submitted job ends. It gets one round;
  any ending, a yield included, ends the run and returns to the agent."
  (:require [engine.contract :as contract]
            [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.job-api :as api]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as real-triggers]))

(def t0 1000000)
(def probes (atom 0))

(defn ^:async walk-round [c]
  (await (.moveTo (:primitives c) (:token c) #js {:pos #js {:x 5 :y 64 :z 0}}))
  (ctx/end c :done nil))

(def jobs
  (merge registry/jobs
         {'walk {:round walk-round}
          'spin {:round (fn [_] :continue)}
          'blocked (tu/job [(constantly (contract/later :no-tool "no axe" :need :axe))] nil)
          'boom {:round (fn [_] (throw (js/Error. "boom")))}
          'nop {:round #(ctx/end % :done nil)}}))

(def triggers
  (merge real-triggers/all
         {:probe {:name :probe :job '(nop) :when (fn [_ _ _] (swap! probes inc) false)}
          :always {:name :always :job '(walk) :when (fn [_ _ _] true)}}))

(defn make [dir clock]
  (let [[seen sink] (tu/legacy-capture-sink)
        p (tu/fake {})]
    {:eng (core/create {:primitives p :jobs jobs :triggers triggers :dir dir :now #(deref clock)
                        :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})
     :seen seen :p p :world (.-world p) :state (.-state (.-world p))}))

(defn setup [] (make (tu/tmp-dir) (atom t0)))

(defn ran [seen] (->> @seen (filter #(= :round_started (:kind %))) (mapv :job)))
(defn kinds [seen kind] (filterv #(= kind (:kind %)) @seen))
(defn settle-ms [] (js/Promise. #(js/setTimeout % 0)))

(defn request [eng op spec & [extra]]
  (api/mutate! eng (merge {:op op :spec spec :by "claude" :request-id (str (random-uuid))
                           :generation-id (:generation-id (core/state eng))}
                          extra)))
(defn exclusive [eng spec & [extra]] (request eng :exclusive spec extra))

(deftest an-exclusive-run-gets-the-body-before-queued-jobs-and-they-wait
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (core/submit! eng '(nop) {})
          (let [r (exclusive eng '(nop))]
            (is (true? (:ok r)) (pr-str r))
            (is (true? (:exclusive (:job r))))
            (is (= {:reason :exclusive-run :job "j2"} (:waiting (api/summary eng "j1")))))
          (await (core/tick! eng))
          (is (= ["j2"] (ran seen)) "only the exclusive job ran")
          (is (nil? (core/exclusive-job eng)))
          (is (= ["j1"] (:list (core/state eng))))
          (await (core/tick! eng))
          (is (= ["j2" "j1"] (ran seen)) "normal ticking resumes after the run"))))))

(deftest a-yield-ends-the-run-stopped-yielded
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (exclusive eng '(spin))
          (await (core/tick! eng))
          (is (empty? (:list (core/state eng))) "unlisted")
          (is (nil? (core/exclusive-job eng)))
          (is (= [:yielded] (mapv :reason (kinds seen :stopped))))
          (is (= ["spin" :stopped] ((juxt :name :status) (get (core/recent-results eng) "j1")))))))))

(deftest a-failing-check-ends-the-run-at-once-with-its-cause
  (let [{:keys [eng seen]} (setup)]
    (exclusive eng '(blocked))
    (is (nil? (core/tick! eng)) "no round starts")
    (is (nil? (core/exclusive-job eng)))
    (is (empty? (ran seen)))
    (let [[e] (kinds seen :stopped)]
      (is (= :no-tool (:reason e)))
      (is (= :axe (:need e))))))

(deftest a-throwing-job-ends-the-run-failed-and-unlisted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (exclusive eng '(boom))
          (await (core/tick! eng))
          (is (empty? (:list (core/state eng))))
          (is (empty? (:failed (core/state eng))) "not parked")
          (is (nil? (core/exclusive-job eng)))
          (is (= [:required] (mapv :attention (kinds seen :failed)))))))))

(deftest a-cut-round-ends-the-run-stopped-cut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen world]} (setup)]
          (.override world "moveTo" (fn [_ _ _] (js/Promise.reject (core/cut-error))))
          (exclusive eng '(walk))
          (await (core/tick! eng))
          (is (nil? (core/exclusive-job eng)))
          (is (= [:cut] (mapv :reason (kinds seen :stopped))))
          (is (empty? (:list (core/state eng)))))))))

(deftest no-trigger-is-evaluated-during-the-run-and-they-are-again-after
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup)]
          (core/register-reflex! eng {:trigger :probe})
          (exclusive eng '(spin))
          (reset! probes 0)
          (await (core/tick! eng))
          (is (= 0 @probes) "not during the run")
          (await (core/tick! eng))
          (is (pos? @probes) "after it"))))))

(deftest the-run-cuts-a-running-listed-job-which-continues-after
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen world]} (setup)
              release (.hold world "moveTo")]
          (core/submit! eng '(walk) {})
          (let [walking (core/tick! eng)]
            (await (settle-ms))
            (release)
            (exclusive eng '(nop))
            (await walking))
          (is (= "j1" (:resume (core/state eng))))
          (await (core/tick! eng))
          (is (= ["j1" "j2"] (ran seen)) "the exclusive job ran at once")
          (await (core/tick! eng))
          (is (= ["j1" "j2" "j1"] (ran seen)) "then the cut job continues"))))))

(deftest the-run-cuts-a-running-reflex-which-is-dropped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen world]} (setup)
              release (.hold world "moveTo")]
          (core/register-reflex! eng {:trigger :always})
          (let [walking (core/tick! eng)]
            (await (settle-ms))
            (is (:reflex (core/running eng)))
            (release)
            (exclusive eng '(nop))
            (await walking))
          (await (core/tick! eng))
          (is (= "j2" (last (ran seen))) "the exclusive job runs, not the reflex")
          (is (= [:dropped] (mapv :outcome (kinds seen :ended)))))))))

(deftest cancelling-the-run-cancels-the-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen world]} (setup)
              release (.hold world "moveTo")]
          (exclusive eng '(walk))
          (let [round (core/tick! eng)]
            (await (settle-ms))
            (core/cancel! eng "j1" "claude")
            (release)
            (await round))
          (is (= ["j1"] (mapv :job (kinds seen :cancelled))))
          (is (nil? (core/exclusive-job eng))))))))

(deftest death-cancels-the-run-and-normal-ticking-resumes
  (let [{:keys [eng seen]} (setup)]
    (exclusive eng '(spin))
    (core/drop-jobs-on-death! eng)
    (is (empty? (:list (core/state eng))))
    (is (nil? (core/exclusive-job eng)))
    (is (= [["j1" :death]] (mapv (juxt :job :by) (kinds seen :cancelled))))
    (core/register-reflex! eng {:trigger :probe})
    (reset! probes 0)
    (core/tick! eng)
    (is (pos? @probes) "triggers are evaluated again")))

(deftest a-second-run-and-a-run-while-offline-are-refused
  (let [{:keys [eng state]} (setup)]
    (is (true? (:ok (exclusive eng '(spin)))))
    (is (= :exclusive-running (:reason (exclusive eng '(spin)))))
    (is (= ["j1"] (:list (core/state eng))) "the second was not listed")
    (core/cancel! eng "j1" "claude")
    (swap! state assoc :offline true)
    (is (= :offline (:reason (exclusive eng '(spin)))))
    (is (empty? (:list (core/state eng))))))

(deftest a-run-takes-no-queue-position-or-hold-or-survive-death
  (let [{:keys [eng]} (setup)]
    (doseq [extra [{:next? true} {:hold? true} {:survive-death? true}]]
      (is (= :bad-field (:reason (exclusive eng '(spin) extra))) (pr-str extra)))
    (is (empty? (:list (core/state eng))))))

(deftest do-now-during-a-run-lists-the-job-without-cutting-the-run
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen world]} (setup)
              release (.hold world "moveTo")]
          (exclusive eng '(walk))
          (let [round (core/tick! eng)]
            (await (settle-ms))
            (core/do-now! eng '(nop))
            (is (= "j1" (:id (core/running eng))) "the run goes on")
            (release)
            (await round))
          (is (empty? (kinds seen :cut)))
          (is (= ["j1"] (mapv :job (kinds seen :completed))))
          (is (= ["j2"] (:list (core/state eng)))))))))

(deftest a-saved-run-is-ended-on-restart-and-not-listed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              clock (atom t0)
              first-run (make dir clock)]
          (exclusive (:eng first-run) '(spin))
          (await (settle-ms))
          (let [{:keys [eng seen]} (make dir clock)]
            (is (= [] (:list (core/state eng))))
            (is (empty? (:instances (core/state eng))))
            (is (= [[:restart :restart]] (mapv (juxt :by :cause) (filter #(= "j1" (:job %)) (kinds seen :cancelled)))))
            (core/tick! eng)
            (is (= [] (ran seen)))))))))
