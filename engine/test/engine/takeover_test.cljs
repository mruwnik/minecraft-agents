(ns engine.takeover-test
  "Manual takeover: take! cuts the holder like a reflex and pauses the scheduler until release!;
  the manual state is never persisted. handle and tick! apply engine.lease's rules; the wire contract in
  test/contract/drive-contract.json runs through them here."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.takeover :as takeover]
            [engine.test-util :as tu]
            [engine.triggers :as real-triggers]
            [cljs.reader :as reader]
            ["fs" :as fs]
            ["path" :as path]))

(def t0 1000000)
(def probes (atom 0))

(defn ^:async eat-round [c]
  (await (.eat (:primitives c) (:token c) #js {}))
  :done)

(defn ^:async walk-round [c]
  (await (.moveTo (:primitives c) (:token c) #js {:pos #js {:x 5 :y 64 :z 0}}))
  :done)

(def jobs
  (merge registry/jobs
         {'walk {:check (constantly true) :round walk-round}
          'eat-up {:check (constantly true) :round eat-round}
          'nop {:check (constantly true) :round (fn [_] :done)}}))

(def triggers
  (merge real-triggers/all
         {:hurt {:name :hurt :job '(eat-up) :persistence :cooldown :cooldown-s 30
                 :when (fn [w _ _] (< (.-health (.self w)) 10))}
          :probe {:name :probe :job '(nop) :when (fn [_ _ _] (swap! probes inc) false)}}))

(defn setup [spec]
  (let [clock (atom t0)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake spec)
        dir (tu/tmp-dir)
        eng (core/create {:primitives p :jobs jobs :triggers triggers :dir dir :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :clock clock :seen seen :dir dir :world (.-world p) :state (.-state (.-world p))}))

(defn kinds-of [seen kind] (filterv #(= kind (:kind %)) @seen))
(defn fired [seen] (count (kinds-of seen :fired)))
(defn ran [seen] (->> @seen (filter #(= :round_started (:kind %))) (mapv :job)))

(def me {:who "claude" :why "look around"})

(deftest take-cuts-a-running-job-which-resumes-after-release
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen world]} (setup {})]
          (core/submit! eng '(walk) {})
          (.hold world "moveTo")
          (let [walking (core/tick! eng)]
            (is (= {:ok true} (takeover/take! eng me)))
            (await walking))
          (is (= [:takeover] (mapv :by (kinds-of seen :cut))) "job.cut by :takeover")
          (is (true? (core/manual? eng)))
          (is (nil? (core/tick! eng)) "no round starts while manual")
          (takeover/release! eng {:who "claude" :reason "released" :held-ms 5})
          (is (false? (core/manual? eng)))
          (await (core/tick! eng))
          (is (= ["j1" "j1"] (ran seen)) "the cut job is the next round after release"))))))

(deftest take-drops-a-running-reflex-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen world state]} (setup {:inventory [{:name "bread" :count 3}]})]
          (core/register-reflex! eng {:trigger :hurt})
          (set! (.. state -self -health) 6)
          (.hold world "eat")
          (let [round (core/tick! eng)]
            (is (= 1 (fired seen)))
            (takeover/take! eng me)
            (await round))
          (is (= [:dropped] (mapv :outcome (kinds-of seen :ended))) "reflex.ended dropped"))))))

(deftest while-manual-no-trigger-is-evaluated-and-the-trigger-fires-after-release
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen state]} (setup {})]
          (core/register-reflex! eng {:trigger :probe})
          (core/register-reflex! eng {:trigger :hurt})
          (set! (.. state -self -health) 6)
          (takeover/take! eng me)
          (reset! probes 0)
          (await (core/tick! eng))
          (is (= 0 @probes) "no trigger evaluated")
          (is (= 0 (fired seen)) "a holding trigger does not fire")
          (takeover/release! eng {:who "claude" :reason "released" :held-ms 1})
          (await (core/tick! eng))
          (is (pos? @probes))
          (is (= 1 (fired seen)) "after release the trigger fires on the next tick"))))))

(deftest take-is-refused-when-offline-settling-or-held
  (let [{:keys [eng world state]} (setup {})]
    (set! (.-offline state) true)
    (is (= {:ok false :reason "offline"} (takeover/take! eng me)))
    (set! (.-offline state) false)
    (.settle world true)
    (is (= {:ok false :reason "settling"} (takeover/take! eng me)))
    (.settle world false)
    (is (= {:ok true} (takeover/take! eng me)))
    (is (= {:ok false :reason "held-by claude"} (takeover/take! eng {:who "view" :why "x"})))))

(deftest drive-moves-the-controls-and-release-clears-them
  (let [{:keys [eng state]} (setup {})]
    (takeover/take! eng me)
    (let [r (takeover/drive! eng #js {:controls #js {:forward true} :look #js {:yaw 90 :pitch 10}})]
      (is (= 90 (.-yaw r))))
    (is (= {:forward true} (js->clj (.-controls state) :keywordize-keys true)))
    (takeover/release! eng {:who "claude" :reason "idle" :held-ms 1})
    (is (= {} (js->clj (.-controls state))))))

(deftest events-carry-who-why-and-reason
  (let [{:keys [eng seen]} (setup {})]
    (takeover/take! eng me)
    (takeover/deadman! eng {:who "claude" :silent-ms 2100})
    (takeover/release! eng {:who "claude" :reason "idle" :held-ms 70})
    (let [[started] (kinds-of seen :takeover_started)
          [ended] (kinds-of seen :takeover_ended)
          [dead] (kinds-of seen :drive_deadman)]
      (is (= ["claude" "look around"] ((juxt :who :why) started)))
      (is (nil? (:level started)))
      (is (= "manual control by claude: look around; jobs and reflexes paused" (:text started)))
      (is (= ["claude" "idle" 70] ((juxt :who :reason :held-ms) ended)))
      (is (= "manual control by claude ended: idle; jobs and reflexes resume" (:text ended)))
      (is (nil? (:level dead)))
      (is (= "driver claude silent 2100 ms: controls released" (:text dead))))))

(deftest manual-state-is-not-persisted
  (let [{:keys [eng dir]} (setup {})]
    (takeover/take! eng me)
    (is (not (re-find #"manual|claude" (fs/readFileSync (path/join dir "engine.edn") "utf8"))))))

(def opts {:idle-ms 15000})

(defn call
  "takeover/handle with a clj body (nil for none), the reply as clj {:status :json}."
  [eng method path body]
  (let [r (takeover/handle eng opts method path (clj->js body) nil)]
    {:status (.-status r) :json (js->clj (.-json r) :keywordize-keys true)}))

(defn post [eng body] (call eng "POST" "/drive" body))
(defn world-call [eng body]
  (let [r (takeover/handle eng opts "POST" "/world" (pr-str body) "application/edn")]
    {:status (.-status r) :edn (reader/read-string (.-text r))}))

(deftest handle-takes-drives-and-releases-through-the-wire
  (let [{:keys [eng seen state]} (setup {})
        taken (post eng {:op "take" :who "claude" :why "look around"})]
    (is (= [200 true "claude"] [(:status taken) (get-in taken [:json :ok]) (get-in taken [:json :manual :who])]))
    (is (true? (core/manual? eng)))
    (is (= 1 (count (kinds-of seen :takeover_started))))
    (let [set-r (post eng {:op "set" :who "claude" :controls {:forward true} :look {:yaw 30}})]
      (is (= 30 (get-in set-r [:json :manual :yaw])))
      (is (= {:forward true} (js->clj (.-controls state) :keywordize-keys true))))
    (is (= {:ok true :manual nil} (:json (post eng {:op "release" :who "claude"}))))
    (is (false? (core/manual? eng)))
    (is (= {} (js->clj (.-controls state))))
    (is (= ["claude" "released"] ((juxt :who :reason) (first (kinds-of seen :takeover_ended)))))))

(deftest handle-refuses-a-take-the-engine-refuses
  (let [{:keys [eng state]} (setup {})]
    (set! (.-offline state) true)
    (is (= {:ok false :reason "offline"} (:json (post eng {:op "take" :who "claude" :why "x"}))))
    (is (false? (core/manual? eng)))))

(deftest tick-ends-an-idle-takeover-and-fires-the-dead-man
  (let [{:keys [eng seen clock]} (setup {})]
    (post eng {:op "take" :who "claude" :why "x"})
    (post eng {:op "set" :who "claude" :controls {:forward true}})
    (swap! clock + 1100)
    (takeover/tick! eng opts)
    (is (= "driver claude silent 1100 ms: controls released" (:text (first (kinds-of seen :drive_deadman)))))
    (is (true? (core/manual? eng)))
    (swap! clock + 13900)
    (takeover/tick! eng opts)
    (is (false? (core/manual? eng)))
    (is (= ["idle" 15000] ((juxt :reason :held-ms) (first (kinds-of seen :takeover_ended)))))))

(deftest close-ends-the-takeover-before-the-engine-stops
  (let [{:keys [eng seen]} (setup {})]
    (post eng {:op "take" :who "claude" :why "x"})
    (takeover/close! eng)
    (core/shutdown! eng)
    (let [ks (mapv :kind @seen)]
      (is (< (.indexOf ks :takeover_ended) (.indexOf ks :stopping)))
      (is (= "shutdown" (:reason (first (kinds-of seen :takeover_ended))))))
    (is (false? (.isOwner (:primitives eng) "m1")) "no owner token holds the body")
    (is (false? (core/manual? eng)))))

;; The wire contract: every scenario of the shared fixture through handle and tick!, exact equality.

(def contract (tu/read-json "test/contract/drive-contract.json"))

(defn run-contract-step [{:keys [eng clock world state]} name {:keys [at status tick req expect]}]
  (reset! clock (+ t0 at))
  (cond
    status (do (set! (.-offline state) (:offline status))
               (.settle world (:settling status)))
    tick (takeover/tick! eng opts)
    :else (is (= expect (call eng (:method req) (:path req) (:body req))) (str name " at " at " " (pr-str req)))))

(deftest every-drive-contract-scenario-matches-the-wire
  (doseq [{:keys [name steps]} contract
          :let [rig (setup {})]]
    ;; the fake's self has no pos while offline; the fixture's GET still reports the last known {0 64 0}
    (set! (.-self (:primitives (:eng rig))) (fn [] #js {:pos (tu/pos 0 64 0)}))
    (doseq [step steps] (run-contract-step rig name step))))

(deftest ping-moves-expires-at
  (let [{:keys [eng clock]} (setup {})]
    (post eng {:op "take" :who "claude" :why "test"})
    (reset! clock (+ t0 5000))
    (is (= {:expiresAt 1020000 :idleLeftS 15} (select-keys (get-in (post eng {:op "ping" :who "claude"}) [:json :manual])
                                                            [:expiresAt :idleLeftS])))
    (reset! clock (+ t0 5100))
    (is (= {:expiresAt 1020000 :idleLeftS 14.9} (select-keys (get-in (call eng "GET" "/drive" nil) [:json :manual])
                                                             [:expiresAt :idleLeftS])))))

(deftest world-submit-is-async-idempotent-single-flight-and-cancellable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng world seen]} (setup {})]
          (post eng {:op "take" :who "claude" :why "manual test" :idleS 30})
          (.hold world "moveTo")
          (let [request {:op :submit :who "claude" :request-id "move-1" :action :move-to
                         :args {:pos {:x 10 :y 64 :z 0}}}
                submitted (world-call eng request)
                duplicate (world-call eng request)
                overlap (world-call eng (assoc request :request-id "move-2" :args {:pos {:x 11 :y 64 :z 0}}))
                blocked-drive (post eng {:op "set" :who "claude" :controls {:forward true}})]
            (is (= [200 :running] [(:status submitted) (get-in submitted [:edn :operation :status])]))
            (is (true? (get-in duplicate [:edn :duplicate])))
            (is (= "action-running" (get-in overlap [:edn :reason])))
            (is (= "action-running" (get-in blocked-drive [:json :reason])))
            (let [cancelled (world-call eng {:op :cancel :who "claude" :request-id "move-1"})]
              (is (= :cancelled (get-in cancelled [:edn :operation :status])))
              (is (= :cancelled (get-in (world-call eng {:op :status :who "claude" :request-id "move-1"}) [:edn :operation :status]))))
            (await (js/Promise.resolve))
            (is (= 1 (count (filter #(and (= :action (:source %)) (= :done (:kind %))
                                          (= "move-1" (:action-id %))) @seen))))))))))

(deftest world-action-requires-owner-and-enough-bounded-lease-time
  (let [{:keys [eng]} (setup {})
        request {:op :submit :who "claude" :request-id "too-long" :action :move-to
                 :args {:pos {:x 1 :y 64 :z 0} :timeoutS 10}}]
    (is (= "not-taken" (get-in (world-call eng request) [:edn :reason])))
    (post eng {:op "take" :who "claude" :why "short" :idleS 5})
    (is (= "lease-too-short" (get-in (world-call eng request) [:edn :reason])))
    (is (= "bad-args" (get-in (world-call eng (assoc request :request-id "bad" :args {:pos {:x 1 :y 64 :z 0} :unexpected true})) [:edn :reason])))))

(deftest lease-expiry-cuts-an-inflight-world-action
  (let [{:keys [eng world clock]} (setup {})]
    (post eng {:op "take" :who "claude" :why "expiry" :idleS 2})
    (.hold world "moveTo")
    (world-call eng {:op :submit :who "claude" :request-id "expiring" :action :move-to
                     :args {:pos {:x 10 :y 64 :z 0} :timeoutS 1}})
    (swap! clock + 2000)
    (takeover/tick! eng opts)
    (is (false? (core/manual? eng)))
    (is (= :cancelled (get-in (world-call eng {:op :status :who "claude" :request-id "expiring"}) [:edn :operation :status])))))

(deftest world-submit-clears-prior-held-drive-controls
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng state world clock seen]} (setup {})]
          (post eng {:op "take" :who "claude" :why "clear controls" :idleS 30})
          (post eng {:op "set" :who "claude" :controls {:forward true}})
          (.hold world "moveTo")
          (world-call eng {:op :submit :who "claude" :request-id "after-drive" :action :move-to
                           :args {:pos {:x 10 :y 64 :z 0} :timeoutS 1}})
          (is (= {} (js->clj (.-controls state) :keywordize-keys true)))
          (swap! clock + 1100)
          (takeover/tick! eng opts)
          (is (empty? (kinds-of seen :drive_deadman)))
          (world-call eng {:op :cancel :who "claude" :request-id "after-drive"})
          (await (js/Promise.resolve)))))))

(deftest late-world-completion-cannot-overwrite-a-reused-evicted-request-id
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {})
              old-resolve (atom nil)
              new-resolve (atom nil)]
          (set! (.-moveTo p)
                (fn [_ args]
                  (case (.-x (.-pos args))
                    10 (js/Promise. (fn [resolve _] (reset! old-resolve resolve)))
                    20 (js/Promise. (fn [resolve _] (reset! new-resolve resolve)))
                    (js/Promise.resolve #js {:status "arrived"}))))
          (post eng {:op "take" :who "claude" :why "stale completion" :idleS 30})
          (world-call eng {:op :submit :who "claude" :request-id "reused" :action :move-to
                           :args {:pos {:x 10 :y 64 :z 0} :timeoutS 1}})
          (world-call eng {:op :cancel :who "claude" :request-id "reused"})
          (doseq [n (range 32)]
            (world-call eng {:op :submit :who "claude" :request-id (str "evict-" n) :action :move-to
                             :args {:pos {:x 1 :y 64 :z 0} :timeoutS 1}})
            (await (js/Promise.resolve))
            (await (js/Promise.resolve)))
          (world-call eng {:op :submit :who "claude" :request-id "reused" :action :move-to
                           :args {:pos {:x 20 :y 64 :z 0} :timeoutS 1}})
          (is (= :running (get-in (world-call eng {:op :status :who "claude" :request-id "reused"}) [:edn :operation :status])))
          (@old-resolve #js {:status "arrived"})
          (await (js/Promise.resolve))
          (await (js/Promise.resolve))
          (is (= :running (get-in (world-call eng {:op :status :who "claude" :request-id "reused"}) [:edn :operation :status])))
          (@new-resolve #js {:status "arrived"})
          (await (js/Promise.resolve))
          (await (js/Promise.resolve))
          (is (= :done (get-in (world-call eng {:op :status :who "claude" :request-id "reused"}) [:edn :operation :status]))))))))
