(ns engine.takeover-test
  "Manual takeover: take! cuts the holder like a reflex and pauses the scheduler until release!;
  the manual state is never persisted. handle and tick! apply engine.lease's rules; the wire contract in
  test/contract/drive-contract.json runs through them here."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.takeover :as takeover]
            [engine.test-util :as tu :refer [box floor]]
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
            ;; the fake records a call one microtask after it is made, so let the walk's moveTo reach its hold first
            (await (js/Promise. #(js/setTimeout % 0)))
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
          (swap! state assoc-in [:self :health] 6)
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
          (swap! state assoc-in [:self :health] 6)
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
    (swap! state assoc :offline true)
    (is (= {:ok false :reason "offline"} (takeover/take! eng me)))
    (swap! state assoc :offline false)
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
    (is (= {:forward true} (:controls @state)))
    (takeover/release! eng {:who "claude" :reason "idle" :held-ms 1})
    (is (= {} (:controls @state)))))

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
      (is (= {:forward true} (:controls @state))))
    (is (= {:ok true :manual nil} (:json (post eng {:op "release" :who "claude"}))))
    (is (false? (core/manual? eng)))
    (is (= {} (:controls @state)))
    (is (= ["claude" "released"] ((juxt :who :reason) (first (kinds-of seen :takeover_ended)))))))

(deftest handle-refuses-a-take-the-engine-refuses
  (let [{:keys [eng state]} (setup {})]
    (swap! state assoc :offline true)
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
    status (do (swap! state assoc :offline (:offline status))
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
            (is (= [:queued "move-1"] [(get-in overlap [:edn :operation :status]) (get-in overlap [:edn :behind])])
                "a second action while one runs is queued behind it")
            (is (= "action-running" (get-in blocked-drive [:json :reason])))
            (let [cancelled (world-call eng {:op :cancel :who "claude" :request-id "move-1"})]
              (is (= :cancelled (get-in cancelled [:edn :operation :status])))
              (is (= :cancelled (get-in (world-call eng {:op :status :who "claude" :request-id "move-1"}) [:edn :operation :status]))))
            (await (js/Promise.resolve))
            (is (= 1 (count (filter #(and (= :action (:source %)) (= :done (:kind %))
                                          (= "move-1" (:action-id %))) @seen))))))))))

(deftest drive-set-from-a-non-holder-does-not-reveal-the-running-action
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng world]} (setup {})]
          (post eng {:op "take" :who "claude" :why "manual test" :idleS 30})
          (.hold world "moveTo")
          (world-call eng {:op :submit :who "claude" :request-id "move-1" :action :move-to
                           :args {:pos {:x 10 :y 64 :z 0}}})
          (doseq [op ["set" "stop"]]
            (let [r (post eng {:op op :who "intruder" :controls {:forward true}})]
              (is (= "not-driver" (get-in r [:json :reason])))
              (is (nil? (get-in r [:json :requestId])))))
          (is (= "action-running" (get-in (post eng {:op "stop" :who "claude"}) [:json :reason]))))))))

(deftest world-request-checks-the-method-before-the-content-type
  (let [{:keys [eng]} (setup {})
        r #(.-status (takeover/handle eng opts %1 "/world" "{}" %2))]
    (is (= 405 (r "GET" "text/plain")))
    (is (= 415 (r "POST" "text/plain")))))

(deftest cancel-active-returns-nil-when-nothing-was-cancelled
  (let [{:keys [eng]} (setup {})]
    (swap! (:world-ops eng) assoc :active "ghost")
    (is (nil? (takeover/cancel-active! eng "x")) "an active id without a running record is not cancelled")))

;; ---------------------------------------------------------------- queued world actions

(defn held-moves!
  "Make the primitive moveTo (no path sensing) hold each call until resolved by x: an atom {x resolve}."
  [p]
  (let [pending (atom {})]
    (set! (.-pathWorld p) nil)
    (set! (.-moveTo p) (fn [_ args] (js/Promise. (fn [resolve _] (swap! pending assoc (.-x (.-pos args)) resolve)))))
    pending))

(defn move [id x] {:op :submit :who "claude" :request-id id :action :move-to :args {:pos {:x x :y 64 :z 0} :timeoutS 1}})
(defn op-status [eng id] (get-in (world-call eng {:op :status :who "claude" :request-id id}) [:edn :operation :status]))
(defn settle! [] (js/Promise. #(js/setTimeout % 0)))

(deftest world-actions-submitted-while-one-runs-run-in-order
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {})
              pending (held-moves! p)]
          (post eng {:op "take" :who "claude" :why "queue" :idleS 30})
          (world-call eng (move "a" 10))
          (let [b (world-call eng (move "b" 20))
                c (world-call eng (move "c" 30))]
            (is (= [200 :queued "a" 1] [(:status b) (get-in b [:edn :operation :status]) (get-in b [:edn :behind]) (get-in b [:edn :position])]))
            (is (= [:queued 2] [(get-in c [:edn :operation :status]) (get-in c [:edn :position])])))
          (await (settle!))
          (is (= [10] (keys @pending)) "only the first action has started")
          ((get @pending 10) #js {:status "arrived"})
          (await (settle!))
          (is (= [:done :running :queued] (mapv #(op-status eng %) ["a" "b" "c"])))
          ((get @pending 20) #js {:status "arrived"})
          (await (settle!))
          ((get @pending 30) #js {:status "arrived"})
          (await (settle!))
          (is (= [:done :done :done] (mapv #(op-status eng %) ["a" "b" "c"])))
          (is (= ["a" "b" "c"] (mapv :action-id (filter #(and (= :action (:source %)) (= :started (:kind %))) @seen))))
          (is (= ["b" "c"] (mapv :action-id (filter #(and (= :action (:source %)) (= :queued (:kind %))) @seen)))))))))

(deftest a-queued-world-action-can-be-cancelled-and-a-release-cancels-the-queue
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {})
              pending (held-moves! p)]
          (post eng {:op "take" :who "claude" :why "queue" :idleS 30})
          (doseq [[id x] [["a" 10] ["b" 20] ["c" 30]]] (world-call eng (move id x)))
          (is (= :cancelled (get-in (world-call eng {:op :cancel :who "claude" :request-id "b"}) [:edn :operation :status])))
          ((get @pending 10) #js {:status "arrived"})
          (await (settle!))
          (is (= [:done :cancelled :running] (mapv #(op-status eng %) ["a" "b" "c"])) "the cancelled one is skipped")
          (world-call eng (move "d" 40))
          (post eng {:op "release" :who "claude"})
          (is (= [:cancelled :cancelled] (mapv #(op-status eng %) ["c" "d"])) "a release cuts the running action and drops the queue")
          (is (= [10 30] (sort (keys @pending))) "the dropped action never started"))))))

(deftest the-world-action-queue-is-bounded
  (let [{:keys [eng p]} (setup {})]
    (held-moves! p)
    (post eng {:op "take" :who "claude" :why "queue" :idleS 30})
    (world-call eng (move "running" 1))
    (doseq [n (range takeover/max-queued-world-ops)] (world-call eng (move (str "q" n) 1)))
    (let [full (world-call eng (move "one-more" 1))]
      (is (= [409 "queue-full"] [(:status full) (get-in full [:edn :reason])])))))

(deftest world-action-requires-owner-and-enough-bounded-lease-time
  (let [{:keys [eng]} (setup {})
        request {:op :submit :who "claude" :request-id "too-long" :action :move-to
                 :args {:pos {:x 1 :y 64 :z 0} :timeoutS 10}}]
    (is (= "not-taken" (get-in (world-call eng request) [:edn :reason])))
    (post eng {:op "take" :who "claude" :why "short" :idleS 5})
    (is (= "lease-too-short" (get-in (world-call eng request) [:edn :reason])))
    (is (= "bad-args" (get-in (world-call eng (assoc request :request-id "bad" :args {:pos {:x 1 :y 64 :z 0} :unexpected true})) [:edn :reason])))))

(deftest world-ops-by-a-non-driver-name-the-holder
  (let [{:keys [eng]} (setup {})
        request {:op :submit :who "other" :request-id "r1" :action :move-to :args {:pos {:x 1 :y 64 :z 0}}}]
    (is (nil? (get-in (world-call eng request) [:edn :detail :holder])) "no driver: not-taken, no holder")
    (post eng {:op "take" :who "claude" :why "hold" :idleS 30})
    (doseq [req [request {:op :cancel :who "other" :request-id "r1"} {:op :inventory :who "other"}]]
      (let [edn (:edn (world-call eng req))]
        (is (= "not-driver" (:reason edn)))
        (is (= "claude" (get-in edn [:detail :holder])))
        (is (number? (get-in edn [:detail :idle-left-s])))))))

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
          (is (= {} (:controls @state)))
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
          (set! (.-pathWorld p) nil) ; no path sensing: move-to is the primitive, whose promises this test holds
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

;; ---------------------------------------------------------------- world move-to walks like go-to

(def gate-world
  {:blocks (merge (floor -2 -3 40 3) (assoc (box 5 64 -6 5 64 6 "oak_fence") "5,64,0" "oak_fence_gate"))
   :states {"5,64,0" {:open false :facing "east"}}})

(defn ^:async submit-and-wait!
  "Take the body, submit one world move-to, wait until it is no longer running; the operation as the status op shows it."
  [eng args]
  (post eng {:op "take" :who "claude" :why "walk" :idleS 30})
  (world-call eng {:op :submit :who "claude" :request-id "walk-1" :action :move-to :args args})
  (loop [i 0]
    (let [op (get-in (world-call eng {:op :status :who "claude" :request-id "walk-1"}) [:edn :operation])]
      (if (or (not= :running (:status op)) (> i 200))
        op
        (do (await (js/Promise. #(js/setTimeout % 5)))
            (recur (inc i)))))))

(deftest world-move-to-passes-a-shut-gate-and-shuts-it-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup gate-world)
              op (await (submit-and-wait! eng {:pos {:x 10 :y 64 :z 0} :range 0}))
              pos (.-pos (.self p))]
          (is (= :done (:status op)))
          (is (= "arrived" (get-in op [:result :status])))
          (is (= [10 64 0] [(.-x pos) (.-y pos) (.-z pos)]))
          (is (false? (:open (js->clj (.-properties (.blockAt p #js {:x 5 :y 64 :z 0})) :keywordize-keys true)))))))))

(deftest world-move-to-names-why-it-cannot-get-there
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:blocks (merge (floor -2 -3 40 3) (box 5 64 -6 5 66 6 "stone"))})
              op (await (submit-and-wait! eng {:pos {:x 10 :y 64 :z 0} :range 0}))]
          (is (= :done (:status op)))
          (is (contains? #{"blocked" "partial"} (get-in op [:result :status])))
          (is (seq (str (get-in op [:result :reason])))
              (str "reason: " (get-in op [:result :reason]))))))))

(deftest move-to-timeout-message-says-what-to-do-for-a-longer-walk
  (let [{:keys [eng]} (setup {})]
    (post eng {:op "take" :who "claude" :why "walk" :idleS 30})
    (let [detail (get-in (world-call eng {:op :submit :who "claude" :request-id "long" :action :move-to
                                          :args {:pos {:x 1 :y 64 :z 0} :timeoutS 20}})
                         [:edn :detail])]
      (is (re-find #"1\.\.10" detail))
      (is (re-find #"go-to" detail)))))

;; ---------------------------------------------------------------- world dig holds the carried tool

(defn ^:async dig-and-wait!
  "Take the body, submit one world dig at pos, wait until it is no longer running; the operation as the status op shows it."
  [eng pos]
  (post eng {:op "take" :who "claude" :why "dig" :idleS 30})
  (world-call eng {:op :submit :who "claude" :request-id "dig-1" :action :dig :args {:pos pos}})
  (loop [i 0]
    (let [op (get-in (world-call eng {:op :status :who "claude" :request-id "dig-1"}) [:edn :operation])]
      (if (or (not= :running (:status op)) (> i 200))
        op
        (do (await (js/Promise. #(js/setTimeout % 5)))
            (recur (inc i)))))))

(defn dig-world [block inventory]
  {:blocks (assoc (floor -2 -3 4 3) "1,64,0" block) :inventory inventory})

(deftest world-dig-holds-the-best-carried-tool-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (dig-world "stone" [{:name "wooden_pickaxe" :count 1} {:name "iron_pickaxe" :count 1}]))
              op (await (dig-and-wait! eng {:x 1 :y 64 :z 0}))]
          (is (= "dug" (get-in op [:result :status])))
          (is (= "iron_pickaxe" (.-held (.self p)))))))))

(deftest world-dig-of-a-shovel-block-by-hand-needs-no-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (dig-world "dirt" []))
              op (await (dig-and-wait! eng {:x 1 :y 64 :z 0}))]
          (is (= "dug" (get-in op [:result :status])))
          (is (nil? (.-held (.self p)))))))))

(deftest world-dig-of-a-block-a-carried-pickaxe-harvests-digs
  (doseq [[block tool] [["iron_ore" "stone_pickaxe"] ["iron_ore" "copper_pickaxe"] ["amethyst_block" "wooden_pickaxe"]
                        ["obsidian" "diamond_pickaxe"] ["dirt" "wooden_pickaxe"]]]
    (async done
      (tu/run-async done
        (fn ^:async t []
          (let [{:keys [eng]} (setup (dig-world block [{:name tool :count 1}]))
                op (await (dig-and-wait! eng {:x 1 :y 64 :z 0}))]
            (is (= "dug" (get-in op [:result :status])) (str block " with " tool))))))))

(deftest world-dig-refuses-a-block-no-carried-tool-can-harvest
  (doseq [[block inventory needed]
          [["stone" [] "pickaxe"]
           ["iron_ore" [] "stone_pickaxe"]
           ["iron_ore" [{:name "wooden_pickaxe" :count 1}] "stone_pickaxe"]
           ["diamond_ore" [{:name "stone_pickaxe" :count 1}] "iron_pickaxe"]
           ["obsidian" [{:name "iron_pickaxe" :count 1}] "diamond_pickaxe"]
           ["amethyst_block" [] "wooden_pickaxe"]
           ["magma_block" [] "wooden_pickaxe"]
           ["dispenser" [] "wooden_pickaxe"]
           ["bone_block" [] "wooden_pickaxe"]
           ["copper_block" [{:name "wooden_pickaxe" :count 1}] "stone_pickaxe"]]]
    (async done
      (tu/run-async done
        (fn ^:async t []
          (let [{:keys [eng p]} (setup (dig-world block inventory))
                op (await (dig-and-wait! eng {:x 1 :y 64 :z 0}))]
            (is (= "no-tool" (get-in op [:result :status])) block)
            (is (= block (get-in op [:result :block])))
            (is (re-find (re-pattern needed) (str (get-in op [:result :reason]))) block)
            (is (= block (.-name (.blockAt p #js {:x 1 :y 64 :z 0}))) "the block is still there")))))))

;; ---------------------------------------------------------------- a dig's lease covers its expected dig time

(defn submit-dig-with-digtime
  "Take the body for idle-s, make a dig take dig-ms (the digTime of the chosen tool), submit one dig; the submit reply."
  [{:keys [eng state]} idle-s dig-ms]
  (post eng {:op "take" :who "claude" :why "dig" :idleS idle-s})
  (swap! state assoc :dig-ms dig-ms)
  (world-call eng {:op :submit :who "claude" :request-id "dig-1" :action :dig :args {:pos {:x 1 :y 64 :z 0}}}))

(deftest world-dig-lease-is-the-expected-dig-time-plus-a-margin
  (doseq [[dig-ms idle-s accepted? minimum] [[1000 10 false 11]    ; a short dig keeps the old 10 s floor
                                             [1000 11 true nil]
                                             [12000 15 false 21]   ; 12 s dig + margin: a lease that would expire mid-dig is refused
                                             [12000 21 true nil]
                                             [9400 30 true nil]
                                             [250000 60 false 61]    ; the hard bound: never longer than 60 s
                                             [250000 61 true nil]]]
    (let [reply (submit-dig-with-digtime (setup (dig-world "dirt" [])) idle-s dig-ms)]
      (is (= accepted? (true? (get-in reply [:edn :ok]))) (str dig-ms " ms, idleS " idle-s))
      (is (= minimum (get-in reply [:edn :detail :minimum-idleS])) (str dig-ms " ms, idleS " idle-s)))))

(deftest world-dig-no-tool-answer-comes-before-the-lease-check
  (doseq [[block inventory] [["obsidian" [{:name "iron_pickaxe" :count 1}]]
                             ["obsidian" []]]]
    (let [reply (submit-dig-with-digtime (setup (dig-world block inventory)) 11 250000)]
      (is (true? (get-in reply [:edn :ok])) (str block " " (count inventory) " tools: accepted at the floor lease")))))

(deftest a-twelve-second-world-dig-is-not-cut-by-the-lease
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng world clock] :as w} (setup (dig-world "dirt" []))
              release (.hold world "dig")]
          (is (true? (get-in (submit-dig-with-digtime w 21 12000) [:edn :ok])))
          (swap! clock + 12500)
          (takeover/tick! eng opts)
          (is (true? (core/manual? eng)) "the lease is held through the dig")
          (is (= :running (get-in (world-call eng {:op :status :who "claude" :request-id "dig-1"}) [:edn :operation :status])))
          (release)
          (await (js/Promise. #(js/setTimeout % 20)))
          (is (= :done (get-in (world-call eng {:op :status :who "claude" :request-id "dig-1"}) [:edn :operation :status]))))))))

;; ---------------------------------------------------------------- a dropped connection answers offline

(deftest world-actions-while-the-connection-is-down-answer-offline-and-never-run
  (doseq [[action args] [[:move-to {:pos {:x 0 :y 64 :z 0} :range 2}]
                         [:dig {:pos {:x 1 :y 64 :z 0}}]]]
    (let [{:keys [eng state world]} (setup (dig-world "dirt" []))]
      (post eng {:op "take" :who "claude" :why "outage" :idleS 30})
      (swap! state assoc :offline true)
      (let [reply (world-call eng {:op :submit :who "claude" :request-id "down-1" :action action :args args})]
        (is (= [409 "offline"] [(:status reply) (get-in reply [:edn :reason])]) (str action))
        (is (empty? (.-calls world)) (str action " reached no primitive"))))))

(deftest a-world-move-to-whose-connection-drops-mid-walk-answers-offline-never-arrived
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng state world]} (setup {:blocks (floor -2 -3 40 3)})
              drop! (fn [_ _ _] (swap! state assoc :offline true) (js/Promise.resolve #js {:status "offline"}))]
          (.override world "steer" drop!)
          (.override world "moveTo" drop!)
          (let [op (await (submit-and-wait! eng {:pos {:x 10 :y 64 :z 0} :range 0}))]
            (is (= :done (:status op)))
            (is (= "offline" (get-in op [:result :status])))))))))
