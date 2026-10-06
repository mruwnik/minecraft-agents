(ns engine.manual-slot-test
  "Manual control runs one slot: the driver's job. No trigger, no loop; a new driver job replaces the slot's;
  others' jobs wait in the normal queue; release cancels the slot. The slot job gets one call and leaves the slot
  whatever it returns; only a round in flight beats the lease."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.job-api :as api]
            [engine.registry :as registry]
            [engine.takeover :as takeover]
            [engine.test-util :as tu]
            [engine.triggers :as real-triggers]))

(def t0 1000000)
(def probes (atom 0))

(defn ^:async walk-round [c]
  (await (.moveTo (:primitives c) (:token c) #js {:pos #js {:x 5 :y 64 :z 0}}))
  :done)

(defn ctx-wait [c] (reset! (:wait c) {:reason :no-tool :need :axe}))

(def jobs
  (merge registry/jobs
         {'walk {:check (constantly true) :round walk-round}
          'spin {:check (constantly true) :round (fn [_] :continue)}
          'blocked {:check (fn [c] (ctx-wait c) false) :round (fn [_] :done)}
          'boom {:check (constantly true) :round (fn [_] (throw (js/Error. "boom")))}
          'nop {:check (constantly true) :round (fn [_] :done)}}))

(def triggers
  (merge real-triggers/all
         {:probe {:name :probe :job '(nop) :when (fn [_ _ _] (swap! probes inc) false)}}))

(defn setup []
  (let [clock (atom t0)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake {})
        eng (core/create {:primitives p :jobs jobs :triggers triggers :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :clock clock :seen seen :world (.-world p) :state (.-state (.-world p))}))

(def me {:who "claude" :why "driving"})
(def opts {:idle-ms 15000})

(defn token [eng] (:token @(:manual eng)))
(defn owns? [{:keys [eng p]} tok] (.isOwner p tok))
(defn ran [seen] (->> @seen (filter #(= :round_started (:kind %))) (mapv :job)))
(defn kinds [seen kind] (filterv #(= kind (:kind %)) @seen))

(defn submit-as [eng by spec & [extra]]
  (api/mutate! eng (merge {:op :submit :spec spec :by by :request-id (str (random-uuid))
                           :generation-id (:generation-id (core/state eng))}
                          extra)))

(defn settle-ms [] (js/Promise. #(js/setTimeout % 0)))

(defn drive-set [eng]
  (let [r (takeover/handle eng opts "POST" "/drive" #js {:op "set" :who "claude" :controls #js {:forward true}} nil)]
    {:status (.-status r) :reason (some-> r .-json .-reason)}))

(deftest the-drivers-job-runs-while-manual-under-its-own-round-token
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen world] :as s} (setup)
              release (do (takeover/take! eng me) (.hold world "moveTo"))]
          (is (true? (:ok (submit-as eng "claude" '(walk)))))
          (let [round (core/tick! eng)]
            (await (settle-ms))
            (is (= ["j1"] (ran seen)) "a round starts for the driver's job")
            (is (false? (owns? s (token eng))) "the round's own token owns the body, not the lease token")
            (is (= {:status 409 :reason "job-running"} (drive-set eng)))
            (release)
            (await round))
          (is (true? (owns? s (token eng))) "after the job the lease token owns the body")
          (is (= 200 (:status (drive-set eng)))))))))

(deftest the-drivers-job-runs-and-the-lease-owns-the-body-between-jobs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as s} (setup)]
          (takeover/take! eng me)
          (submit-as eng "claude" '(nop))
          (await (core/tick! eng))
          (is (= ["j1"] (ran seen)))
          (is (empty? (:list (core/state eng))) "done: off the list")
          (is (true? (owns? s (token eng))) "between jobs the lease token owns the body")
          (is (nil? (core/tick! eng)) "then the body idles: no round starts")
          (is (= ["j1"] (ran seen))))))))

(deftest a-job-listed-before-the-take-and-a-job-from-another-wait-until-release
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (core/submit! eng '(nop) {:by "steward"})
          (takeover/take! eng me)
          (submit-as eng "other" '(nop))
          (is (nil? (core/tick! eng)))
          (is (nil? (core/tick! eng)))
          (is (empty? (ran seen)) "neither ran under manual control")
          (takeover/release! eng {:who "claude" :reason "released" :held-ms 1})
          (await (core/tick! eng))
          (await (core/tick! eng))
          (is (= ["j1" "j2"] (ran seen)) "both run in order once the lease ends"))))))

(deftest a-new-driver-job-replaces-the-listed-one
  (let [{:keys [eng seen]} (setup)]
    (takeover/take! eng me)
    (submit-as eng "claude" '(spin))
    (submit-as eng "claude" '(nop))
    (is (= ["j2"] (:list (core/state eng))) "the first job is cancelled, the second listed")
    (is (= [["j1" "claude"]] (mapv (juxt :job :by) (kinds seen :cancelled))))))

(deftest a-continuing-slot-job-ends-stopped-yielded-after-one-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as s} (setup)]
          (takeover/take! eng me)
          (submit-as eng "claude" '(spin))
          (await (core/tick! eng))
          (is (empty? (:list (core/state eng))) "left the slot")
          (is (nil? (core/manual-job eng)))
          (is (= [:yielded] (mapv #(:reason %) (kinds seen :stopped))))
          (is (nil? (core/tick! eng)) "then the body idles")
          (is (= ["j1"] (ran seen)))
          (is (true? (owns? s (token eng)))))))))

(deftest a-slot-job-whose-check-fails-ends-at-once-with-the-checks-reason
  (let [{:keys [eng seen]} (setup)]
    (takeover/take! eng me)
    (submit-as eng "claude" '(blocked))
    (is (nil? (core/tick! eng)) "no round starts")
    (is (empty? (:list (core/state eng))))
    (is (empty? (ran seen)))
    (let [[e] (kinds seen :stopped)]
      (is (= :no-tool (:reason e)))
      (is (= :axe (:need e))))))

(deftest no-trigger-is-evaluated-while-a-slot-job-runs-and-a-reflex-end-is-deferred
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup)]
          (core/register-reflex! eng {:trigger :probe})
          (takeover/take! eng me)
          (submit-as eng "claude" '(spin))
          (reset! probes 0)
          (await (core/tick! eng))
          (await (core/tick! eng))
          (is (= 0 @probes) "no trigger evaluated during the slot job's ticks"))))))

(deftest drive-stop-cancels-a-waiting-slot-job-and-set-works-after
  (let [{:keys [eng]} (setup)]
    (takeover/take! eng me)
    (submit-as eng "claude" '(nop))
    (is (some? (core/manual-job eng)))
    (is (= 200 (.-status (takeover/handle eng opts "POST" "/drive" #js {:op "stop" :who "claude"} nil))))
    (is (nil? (core/manual-job eng)) "the waiting slot job is cancelled")
    (is (= 200 (:status (drive-set eng))))))

(deftest release-cancels-the-running-slot-job-and-the-normal-queue-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen world] :as s} (setup)]
          (core/submit! eng '(nop) {:by "steward"})
          (takeover/take! eng me)
          (.hold world "moveTo")
          (submit-as eng "claude" '(walk))
          (let [round (core/tick! eng)]
            (await (settle-ms))
            (takeover/release! eng {:who "claude" :reason "released" :held-ms 1})
            (await round))
          (is (= ["j2"] (mapv :job (kinds seen :cancelled))) "the slot job is cancelled")
          (is (= ["j1"] (:list (core/state eng))) "the queued job is untouched")
          (await (core/tick! eng))
          (is (= ["j2" "j1"] (ran seen))))))))

(deftest a-running-slot-job-keeps-the-lease-alive
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng world clock]} (setup)]
          (takeover/handle eng opts "POST" "/drive" #js {:op "take" :who "claude" :why "x" :idleS 5} nil)
          (.hold world "moveTo")
          (submit-as eng "claude" '(walk))
          (let [round (core/tick! eng)]
            (await (settle-ms))
            (swap! clock + 20000)
            (takeover/tick! eng opts)
            (is (true? (core/manual? eng)) "the lease outlives its idle time while the job runs")
            (takeover/release! eng {:who "claude" :reason "released" :held-ms 1})
            (await round)))))))

(deftest an-idle-lease-with-no-slot-job-still-ends
  (let [{:keys [eng clock]} (setup)]
    (takeover/handle eng opts "POST" "/drive" #js {:op "take" :who "claude" :why "x" :idleS 5} nil)
    (swap! clock + 6000)
    (takeover/tick! eng opts)
    (is (false? (core/manual? eng)))))

(deftest front-and-interrupt-from-a-non-driver-are-refused-while-manual
  (let [{:keys [eng]} (setup)]
    (takeover/take! eng me)
    (is (= :manual-control (:reason (submit-as eng "other" '(nop) {:front? true}))))
    (is (= :manual-control (:reason (api/mutate! eng {:op :interrupt :spec '(nop) :by "other" :request-id "i1"
                                                      :generation-id (:generation-id (core/state eng))}))))
    (is (empty? (:list (core/state eng))) "nothing was listed")
    (is (true? (:ok (submit-as eng "other" '(nop)))) "a plain submit still queues")
    (is (nil? (core/manual-job eng)) "and is not the slot")))

(deftest the-tick-expires-changes-while-manual
  (let [{:keys [eng clock]} (setup)]
    (core/register-reflex! eng {:trigger :probe})
    (takeover/take! eng me)
    (swap! (:state eng) assoc-in [:changes :probe :mute] {:value true :until (+ t0 1000)})
    (swap! clock + 2000)
    (core/tick! eng)
    (is (empty? (:changes (core/state eng))))))

(deftest death-while-manual-cancels-the-slot-job-and-keeps-the-lease
  (let [{:keys [eng]} (setup)]
    (takeover/take! eng me)
    (submit-as eng "claude" '(spin))
    (core/drop-jobs-on-death! eng)
    (is (empty? (:list (core/state eng))))
    (is (true? (core/manual? eng)))
    (is (nil? (core/manual-job eng)))))

(deftest take-starts-with-no-slot-job
  (let [{:keys [eng]} (setup)]
    (takeover/take! eng me)
    (submit-as eng "claude" '(nop))
    (is (= "j1" (core/manual-job eng)))
    (takeover/release! eng {:who "claude" :reason "released" :held-ms 1})
    (is (nil? (core/manual-job eng)))
    (is (empty? (:list (core/state eng))) "release cancelled it")))

(deftest a-failed-slot-job-does-not-keep-the-lease-alive
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock]} (setup)]
          (takeover/handle eng opts "POST" "/drive" #js {:op "take" :who "claude" :why "x" :idleS 5} nil)
          (submit-as eng "claude" '(boom))
          (await (core/tick! eng))
          (is (contains? (:failed (core/state eng)) (core/manual-job eng)) "the job is listed and failed")
          (swap! clock + 6000)
          (takeover/tick! eng opts)
          (is (false? (core/manual? eng))))))))

(deftest a-listed-slot-job-not-yet-running-does-not-keep-the-lease-alive
  (let [{:keys [eng clock]} (setup)]
    (takeover/handle eng opts "POST" "/drive" #js {:op "take" :who "claude" :why "x" :idleS 5} nil)
    (submit-as eng "claude" '(spin))
    (swap! clock + 20000)
    (takeover/tick! eng opts)
    (is (false? (core/manual? eng)))))

(deftest drive-stop-cancels-the-running-slot-job-and-stops-the-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen world] :as s} (setup)]
          (takeover/take! eng me)
          (.hold world "moveTo")
          (submit-as eng "claude" '(walk))
          (let [round (core/tick! eng)]
            (await (settle-ms))
            (let [r (takeover/handle eng opts "POST" "/drive" #js {:op "stop" :who "claude"} nil)]
              (is (= 200 (.-status r)))
              (await round))
            (is (= ["j1"] (mapv :job (kinds seen :cancelled))) "the slot job is cancelled")
            (is (nil? (core/manual-job eng)))
            (is (true? (core/manual? eng)) "the lease stays")
            (is (owns? s (token eng)) "the lease token owns the body again")
            (is (= 200 (:status (drive-set eng))))))))))

(deftest drive-set-still-409s-while-the-slot-job-runs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng world] :as s} (setup)]
          (takeover/take! eng me)
          (.hold world "moveTo")
          (submit-as eng "claude" '(walk))
          (let [round (core/tick! eng)]
            (await (settle-ms))
            (is (= "job-running" (:reason (drive-set eng))))
            (takeover/release! eng {:who "claude" :reason "released" :held-ms 1})
            (await round)))))))
