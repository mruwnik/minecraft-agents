(ns engine.core-test
  (:require [engine.contract :as contract]
            [cljs.test :refer [use-fixtures deftest is are async testing]]
            [cljs.reader :as reader]
            [engine.core :as core]
            [engine.core.base :as base]
            [engine.ctx :as ctx]
            [engine.hurt :as hurt]
            [engine.job-api :as job-api]
            [engine.memory :as mem]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.fake :as fake]
            [engine.test-util :as tu]
            [engine.settings :as settings]
            [engine.triggers :as triggers]
            ["fs" :as fs]
            ["path" :as path]))

;; ---------------------------------------------------------------- test jobs

(use-fixtures :once tu/real-memory-disk-fixture)

(defn done [c] (ctx/end c :done nil))

(defn ^:async count-round [c]
  (ctx/update-mem! c update :n (fnil inc 0))
  :continue)

(defn ^:async walk-round [c]
  (let [r (await (ctx/act c :moveTo #js {:pos (clj->js (:pos (:args c)))}))]
    (ctx/update-mem! c assoc :walked (.-status r))
    (done c)))

(defn ^:async parent-walk-round [c]
  (ctx/update-mem! c assoc :before true)
  (await (ctx/act c :moveTo #js {:pos #js {:x 5 :y 64 :z 0}}))
  (done c))

(defn ^:async eat-round [c]
  (await (ctx/act c :eat #js {}))
  (done c))

(defn ^:async swim-round [c]
  (let [r (await (ctx/act c :swim #js {:ms 3000}))]
    (ctx/update-mem! c assoc :swim [(.-status r) (.. r -oxygen -after)])
    (done c)))

(defn ^:async fail-round [_]
  (throw (js/Error. "nope")))

(defn ^:async attention-round [c]
  (ctx/request-attention! c :storage_blocked :chest_full {:item "oak_log" :count 24} "Chest is full")
  :continue)

(defn ^:async attention-done-round [c]
  (ctx/request-attention! c :storage_blocked :chest_full {:item "oak_log" :count 24} "Chest is full")
  (done c))

(defn ^:async write-fail-round [c]
  (ctx/update-mem! c assoc :x 1)
  (throw (js/Error. "nope")))

(defn ^:async spin-round
  "Never yields the event loop: a microtask loop that only asks alive?."
  [c]
  (loop []
    (ctx/alive? c)
    (await (js/Promise.resolve))
    (recur)))

(defn ^:async bad-result-round [_]
  :not-ready)

(defn ^:async child-round [c]
  (let [n (:n (ctx/mem c) 0)]
    (ctx/update-mem! c assoc :n (inc n))
    (if (>= (inc n) (:rounds (:args c) 2)) (done c) :continue)))

(def child-job (tu/job child-round))

(defn ^:async parent-round [c]
  (let [a (await (ctx/call c :a child-job {:rounds 2}))]
    (ctx/update-mem! c update :seen (fnil conj []) (if (= :continue a) a (:kind a)))
    (if (= :continue a) :continue (done c))))

(defn ^:async submit-round [c]
  (ctx/submit! c '(count) {})
  (done c))

(def flag (atom false))

(def gated-job (tu/job [(fn [_] (if @flag (contract/ok :go "go") (contract/later :not-ready "not ready")))] count-round))

(defn ^:async recurse-round
  "Calls itself as a child in slot :deeper until :depth levels exist."
  [c]
  (let [depth (:depth (:args c))]
    (ctx/update-mem! c assoc :at depth)
    (if (zero? depth)
      (done c)
      (let [r (await (ctx/call c :deeper (tu/job recurse-round) {:depth (dec depth)}))]
        (if (= :continue r) r (ctx/end c (:kind r) (:cause r)))))))

(defn throwing-check [_] (throw (js/Error. "broken check")))

(def registry
  {'count (tu/job count-round)
   'bad-check (tu/job [throwing-check] count-round)
   'walk (tu/job walk-round)
   'parent-walk (tu/job parent-walk-round)
   'eat (tu/job eat-round)
   'fail (tu/job fail-round)
   'attention (tu/job attention-round)
   'attention-done (tu/job attention-done-round)
   'write-fail (tu/job write-fail-round)
   'bad-result (tu/job bad-result-round)
   'spin (tu/job spin-round)
   'child child-job
   'recurse (tu/job recurse-round)
   'parent (tu/job parent-round)
   'submitter (tu/job submit-round)
   'jobs.movement.look-around (get registry/jobs 'jobs.movement.look-around)
   'jobs.time.noop (get registry/jobs 'jobs.time.noop)
   'gated gated-job
   'any-gated (tu/job (fn ^:async any-gated-round [c]
                        (let [r (await (ctx/call c :g gated-job {}))]
                          (cond (= :continue r) :continue
                                (= :done (:kind r)) (done c)
                                :else (ctx/end c (:kind r) (:cause r))))))
   'give-up (tu/job (fn [c] (ctx/end c :nothing-possible :no-way)))
   'eat-parent (tu/job (fn ^:async eat-parent-round [c]
                         (let [r (await (ctx/call c :e (tu/job eat-round) {}))]
                           (if (= :continue r) r (done c)))))})

(def triggers
  {:hurt {:name :hurt :job '(eat) :persistence :retry
          :when (fn [w _ _] (<= (.-health (.self w)) 8))}
   :hungry {:name :hungry :job '(eat) :persistence :cooldown :cooldown-s 30
            :when (fn [w _ _] (< (.-food (.self w)) 10))}
   :near {:name :near :job '(walk {:pos {:x 50 :y 64 :z 0}}) :persistence :stop
          :when (fn [w _ _] (seq (.entities w #js {:kind "hostile" :radius 8})))}
   ;; a look-around on a 45 s schedule, read from the :looked entry the job writes (no timer trigger ships)
   :interval {:name :interval :job '(jobs.movement.look-around) :persistence :cooldown :cooldown-s 0
              :when (fn [_ m _] (let [t (:t (mem/latest m :looked))] (or (nil? t) (>= (- (:now m) t) 45000))))}
   :never {:name :never :job '(eat) :when (constantly false)}
   :boom {:name :boom :job '(fail) :persistence :retry :when (constantly true)}
   :gated {:name :gated :job '(gated) :persistence :retry :when (constantly true)}
   :gated-child {:name :gated-child :job '(any-gated) :persistence :retry :when (constantly true)}})

(defn setup
  ([] (setup {}))
  ([world] (setup world (tu/tmp-dir)))
  ([world dir]
   (setup world dir false))
  ([world dir canonical?]
   (let [clock (atom 1000000)
         [seen sink] (if canonical? (tu/capture-sink) (tu/legacy-capture-sink))
         p (tu/fake world)
         eng (core/create {:primitives p :jobs registry :triggers triggers :dir dir :now #(deref clock)
                           :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
     {:eng eng :p p :clock clock :seen seen :dir dir})))

(declare restore-with)

(defn listed [eng] (:list (core/state eng)))
(defn job-mem
  "A job's memory without the engine's :job, :args, :children, :queued-t and :contract keys."
  ([eng id] (job-mem eng id []))
  ([eng id slots] (dissoc (mem/job-mem (mem/view (:store eng)) id slots) :job :args :children :queued-t :contract)))

(defn memory-on-disk [dir]
  (reader/read-string (fs/readFileSync (path/join dir "memory.edn") "utf8")))
(defn ran [seen] (->> @seen (filter #(= :round_started (:kind %))) (mapv :job)))

;; ---------------------------------------------------------------- pure register

(def reg [{:id :a} {:id :b} {:id :c}])

(defn are-order [expected changes]
  (is (= expected (mapv :id (core/effective-register {:register reg :changes changes} 1000)))
      (str changes)))

(deftest effective-register-applies-moves-and-mutes
  (are-order [:a :b :c] {})
  (are-order [:a :c] {:b {:mute {:until 2000}}})
  (are-order [:a :b :c] {:b {:mute {:until 500}}})
  (are-order [:c :a :b] {:c {:position {:value {:above :a} :until 2000}}})
  (are-order [:b :a :c] {:a {:position {:value {:below :b} :until 2000}}})
  (are-order [:b :c :a] {:a {:position {:value {:above :gone} :until nil}}})
  (are-order [:c :b] {:c {:position {:value {:above :a} :until 2000}} :a {:mute {:until nil}}}))


;; ---------------------------------------------------------------- the list

(deftest round-robin-over-ready-jobs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (core/submit! eng '(count) {})
          (core/submit! eng '(count) {})
          (core/submit! eng '(count) {})
          (dotimes [_ 5] (await (core/tick! eng)))
          (is (= ["j1" "j2" "j3" "j1" "j2"] (ran seen)))
          (is (= {:n 2} (job-mem eng "j1")))
          (is (= {:n 1} (job-mem eng "j3"))))))))

(deftest round-started-and-yielded-are-debug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup)
              raw (atom [])
              emit base/emit!]
          (set! base/emit! (fn [eng e] (swap! raw conj e) (emit eng e)))
          (try
            (core/submit! eng '(count) {})
            (await (core/tick! eng))
            (finally (set! base/emit! emit)))
          (is (= [[:round_started :debug] [:yielded :debug]]
                 (->> @raw (filter #(#{:round_started :yielded} (:kind %))) (mapv (juxt :kind :level))))
              "the canonical log drops :level; the emitted event carries it"))))))

(deftest a-declining-job-is-skipped-and-costs-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (reset! flag false)
          (core/submit! eng '(gated) {})
          (is (nil? (core/tick! eng)) "every check declines: idle")
          (core/submit! eng '(count) {})
          (dotimes [_ 2] (await (core/tick! eng)))
          (is (= ["j2" "j2"] (ran seen)))
          (reset! flag true)
          (dotimes [_ 2] (await (core/tick! eng)))
          (is (= ["j2" "j2" "j1" "j2"] (ran seen)) "round-robin once it passes")
          (is (not-any? #(= :blocked (:kind %)) @seen)))))))

(deftest a-condition-sees-the-job-memory-and-args
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [seen-ctx (atom nil)
              {:keys [eng]} (setup)
              eng (assoc-in eng [:jobs 'peek]
                            {:can-run? (fn [c] (reset! seen-ctx [(:args c) (dissoc (ctx/mem c) :args :children :contract)])
                                         (contract/ok :seen "seen"))
                             :round count-round})]
          (core/submit! eng (list 'peek {:a 1}) {})
          (await (core/tick! eng))
          (core/tick! eng)
          (is (= [{:a 1} {:n 1}] @seen-ctx)))))))

(deftest a-round-returning-anything-else-fails-the-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (core/submit! eng '(bad-result) {})
          (await (core/tick! eng))
          (is (= ["j1"] (listed eng)))
          (is (some #(= :failed (:kind %)) @seen)))))))

(deftest done-removes-the-job-and-its-memory
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen dir]} (setup)]
          (core/submit! eng (list 'walk {:pos {:x 3 :y 64 :z 0}}) {})
          (core/submit! eng '(count) {})
          (await (core/tick! eng))
          (is (= ["j2"] (listed eng)))
          (is (nil? (get-in (memory-on-disk dir) [:entries :job/j1])) "its memory kind is deleted")
          (is (some #(= [:completed "j1"] [(:kind %) (:job %)]) @seen))
          (await (core/tick! eng))
          (is (= ["j1" "j2"] (ran seen))))))))

(deftest a-throwing-listed-job-stays-listed-with-its-memory-marked-failed-and-is-skipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (setup)]
          (core/submit! eng '(write-fail) {})
          (core/submit! eng '(count) {})
          (await (core/tick! eng))
          (is (= ["j1" "j2"] (listed eng)) "still listed")
          (is (= {:x 1} (job-mem eng "j1")) "memory intact")
          (is (= {"j1" {:error "Error: nope" :t @clock}} (:failed (core/state eng))))
          (let [[request-id request] (first (:attention (core/state eng)))]
            (is (string? request-id) "a parked failure opens a stable required request")
            (is (= :round-failed (:reason request)))
            (is (= :required (:attention (some #(when (= request-id (:request-id %)) %) @seen)))))
          (is (some #(and (= :failed (:kind %)) (= :required (:attention %))) @seen)
              "failure is represented by a required attention event")
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= ["j1" "j2" "j2" "j2"] (ran seen)) "the scheduler skips j1"))))))

(deftest a-job-that-never-yields-the-event-loop-is-failed-as-spinning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup)]
          (core/submit! eng '(spin) {})
          (core/submit! eng '(count) {})
          (await (core/tick! eng))
          (is (re-find #"^Error: spinning: job j1 " (:error (get (:failed (core/state eng)) "j1"))))
          (await (core/tick! eng))
          (is (= {:n 1} (job-mem eng "j2")) "the next job still runs"))))))

(deftest a-round-returning-junk-marks-the-job-failed-too
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup)]
          (core/submit! eng '(bad-result) {})
          (await (core/tick! eng))
          (is (= ["j1"] (listed eng)))
          (is (contains? (:failed (core/state eng)) "j1")))))))

(deftest retry-clears-the-failed-mark-so-the-job-runs-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (core/submit! eng '(write-fail) {})
          (await (core/tick! eng))
          (let [request-id (ffirst (:attention (core/state eng)))]
          (is (nil? (core/tick! eng)) "skipped while failed")
          (is (true? (core/retry! eng "j1")))
          (is (= {} (:failed (core/state eng))))
          (is (empty? (:attention (core/state eng))))
          (is (some #(and (= :resolved (:kind %)) (= request-id (:request-id %))
                          (= :job-retried (:reason %))) @seen))
          (is (= {:x 1} (job-mem eng "j1")))
          (is (some #(= [:job :retried "j1"] [(:source %) (:kind %) (:job %)]) @seen))
          (await (core/tick! eng))
          (is (= ["j1" "j1"] (ran seen)))
          (is (false? (core/retry! eng "nope")))))))))

(deftest cancel-removes-a-failed-job-and-its-mark
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (core/submit! eng '(write-fail) {})
          (await (core/tick! eng))
          (let [request-id (ffirst (:attention (core/state eng)))]
            (core/cancel! eng "j1")
            (is (= [] (listed eng)))
            (is (= {} (:failed (core/state eng))))
            (is (= {} (core/outstanding eng)))
            (is (some #(and (= :resolved (:kind %)) (= request-id (:request-id %))
                            (= :job-cancelled (:reason %))) @seen)
                "cancellation resolves the parked failure request")))))))

(deftest a-throwing-reflex-job-is-dropped-with-the-same-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (core/register-reflex! eng {:trigger :boom})
          (await (core/tick! eng))
          (is (= {} (:instances (core/state eng))) "one chance")
          (is (= {} (:failed (core/state eng))))
          (is (some #(and (= :failed (:kind %)) (= :boom (:reflex %))) @seen)))))))

(deftest a-restart-keeps-the-failed-mark
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng dir]} (setup)]
          (core/submit! eng '(write-fail) {})
          (await (core/tick! eng))
          (let [[again _] (restore-with dir registry triggers)]
            (is (= ["j1"] (listed again)))
            (is (= #{"j1"} (set (keys (:failed (core/state again))))))
            (is (= "Error: nope" (get-in (core/state again) [:failed "j1" :error])))
            (is (nil? (core/tick! again)) "still skipped")
            (core/retry! again "j1")
            (let [round (core/tick! again)]
              (is (some? round) "retry runs it")
              (await round))))))))

(deftest required-attention-persists-replays-with-same-id-and-resolves-idempotently
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              {:keys [eng]} (setup {} dir)]
          (core/submit! eng '(attention) {})
          (await (core/tick! eng))
          (let [[request-id request] (first (:attention (core/state eng)))
                generation (:generation-id (core/state eng))
                on-disk (reader/read-string (fs/readFileSync (path/join dir "engine.edn") "utf8"))
                {:keys [eng seen]} (setup {} dir)]
            (is (= request (get-in on-disk [:attention request-id])) "request persisted in authoritative state")
            (is (= generation (:generation-id (core/state eng))) "generation survives process restart")
            (is (some #(and (= request-id (:request-id %)) (= :required (:attention %))) @seen)
                "startup replays the request with its original ID")
            (let [round (core/tick! eng)]
              (await round)
              (is (= #{request-id} (set (keys (:attention (core/state eng))))) "re-observation deduplicates")
              (is (= 1 (count (filter #(= request-id (:request-id %)) @seen))) "unchanged request is not re-emitted"))
            (is (= :resolved (core/resolve-attention! eng request-id :handled)))
            (is (= :already-resolved (core/resolve-attention! eng request-id :handled)))
            (is (empty? (:attention (core/state eng))))))))))

(deftest resolving-parked-failure-does-not-reopen-it-on-restart
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              {:keys [eng]} (setup {} dir)]
          (core/submit! eng '(write-fail) {})
          (await (core/tick! eng))
          (let [request-id (ffirst (:attention (core/state eng)))]
            (core/resolve-attention! eng request-id :handled)
            (is (true? (get-in (core/state eng) [:failed "j1" :attention-closed?])))
            (let [{again :eng} (setup {} dir)]
              (is (= {} (:attention (core/state again))) "an explicit acknowledgment stays closed"))))))))

(deftest job-completion-resolves-its-open-request
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {} (tu/tmp-dir) true)]
          (core/submit! eng '(attention-done) {})
          (await (core/tick! eng))
          (is (= [] (listed eng)))
          (is (empty? (:attention (core/state eng))))
          (is (some #(and (= :resolved (:kind %)) (= :job-completed (get-in % [:data :reason]))) @seen))
          (is (some #(and (= :completed (:kind %)) (= :notice (:attention %))) @seen)
              "a completed job produces a completion notice"))))))

(deftest a-child-lives-in-its-parents-memory-under-children
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng dir]} (setup)]
          (core/submit! eng '(parent) {})
          (await (core/tick! eng))
          (is (= {:seen [:continue]} (job-mem eng "j1")))
          (is (= {:n 1} (job-mem eng "j1" [:a])))
          (is (= {:rounds 2} (:args (mem/job-mem (mem/view (:store eng)) "j1" [:a]))) "created with its args")
          (is (= 1 (get-in (memory-on-disk dir) [:entries :job/j1 0 :data :children :a :n]))
              "saved at round end")
          (await (core/tick! eng))
          (is (= [] (listed eng)))
          (is (nil? (get-in (memory-on-disk dir) [:entries :job/j1])) "done takes the subtree"))))))

(deftest the-same-slot-resumes-and-a-new-slot-is-fresh
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup)
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})]
          (.setOwner (:primitives eng) "tx")
          (is (= :continue (await (ctx/call c :x child-job {:rounds 2}))))
          (is (= :done (:kind (await (ctx/call c :x child-job {:rounds 2})))))
          (is (= {} (job-mem eng "j9" [:x])) "a done child's memory is cleared")
          (is (= :continue (await (ctx/call c :x child-job {:rounds 2}))) "so the next call starts fresh")
          (is (= :continue (await (ctx/call c :y child-job {:rounds 2}))))
          (is (= {:n 1} (job-mem eng "j9" [:y]))))))))

(def stopping-child (tu/job (fn [c] (ctx/end c :nothing-possible :no-way))))

(defn child-events [seen]
  (->> @seen
       (filter #(#{:child_started :child_ended} (:kind %)))
       (mapv #(select-keys % [:kind :job :slot :chain :status :reason]))))

(deftest every-child-call-emits-a-debug-start-and-end
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})]
          (.setOwner (:primitives eng) "tx")
          (await (ctx/call c :x child-job {:rounds 2}))
          (await (ctx/call c :g (tu/job [(constantly (contract/later :no-seeds "no seeds"))] count-round) {}))
          (await (ctx/call c :s stopping-child {}))
          (let [chain-x ["j9" "j9/x"]]
            (is (= [{:kind :child_started :job "j9/x" :slot :x :chain chain-x}
                    {:kind :child_ended :job "j9/x" :slot :x :chain chain-x :status :continue}
                    {:kind :child_started :job "j9/g" :slot :g :chain ["j9" "j9/g"]}
                    {:kind :child_ended :job "j9/g" :slot :g :chain ["j9" "j9/g"] :status :stopped
                     :reason :no-seeds}
                    {:kind :child_started :job "j9/s" :slot :s :chain ["j9" "j9/s"]}
                    {:kind :child_ended :job "j9/s" :slot :s :chain ["j9" "j9/s"] :status :stopped
                     :reason :no-way}]
                   (child-events seen)))))))))

(deftest a-child-ended-carries-the-childs-report
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self (assoc fake/default-self :food 2) :inventory [{:name "bread" :count 2}]})]
          (core/submit! eng '(eat-parent) {})
          (await (core/tick! eng))
          (is (= {:uses {:items {"bread" 1} :job-time 0 :wall-time 0}}
                 (:report (first (filter #(= :child_ended (:kind %)) @seen))))))))))

(deftest a-child-returning-a-bad-result-emits-child-ended-error-and-throws
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})
              failing (tu/job (fn [_] :bogus))]
          (.setOwner (:primitives eng) "tx")
          (is (thrown-with-msg? js/Error #"not :continue or an ending" (await (ctx/call c :f failing {}))))
          (is (= [{:kind :child_started :job "j9/f" :slot :f :chain ["j9" "j9/f"]}
                  {:kind :child_ended :job "j9/f" :slot :f :chain ["j9" "j9/f"] :status :error}]
                 (child-events seen))))))))

(deftest children-can-call-children-without-a-depth-cap
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (core/submit! eng (list 'recurse {:depth 12}) {})
          (await (core/tick! eng))
          (is (= [] (listed eng)) "the innermost :done bubbles up")
          (is (not-any? #(= :failed (:kind %)) @seen)))))))

(deftest a-child-of-a-cut-round-is-not-run
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup)
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "old" :args {} :round 1})]
          (.setOwner (:primitives eng) "new")
          (is (= :cut (try (await (ctx/call c :x child-job {}))
                           (catch :default e (when (core/cut? e) :cut)))))
          (is (= {} (job-mem eng "j9" [:x]))))))))

(deftest submit-from-a-round-appends-with-the-next-id
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup)]
          (core/submit! eng '(submitter) {})
          (core/submit! eng '(count) {})
          (await (core/tick! eng))
          (is (= ["j2" "j3"] (listed eng))))))))

;; ---------------------------------------------------------------- reflexes and cuts

(defn ^:async in-act
  "Waits until the world got a call to name (a few macrotasks at most): a contract round reaches its first act a few
  microtasks after the tick that starts it."
  [world name]
  (loop [i 0]
    (when (and (< i 50) (not-any? #(= name (.-name %)) (array-seq (.-calls world))))
      (await (js/Promise. (fn [r] (js/setTimeout r 0))))
      (recur (inc i)))))

(deftest a-reflex-cuts-a-listed-job-which-resumes-next
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup {:inventory [{:name "bread" :count 3}]})
              world (.-world p)]
          (core/register-reflex! eng {:trigger :hurt})
          (core/submit! eng (list 'walk {:pos {:x 5 :y 64 :z 0}}) {})
          (core/submit! eng '(count) {})
          (.hold world "moveTo")
          (let [walking (core/tick! eng)]
            (await (in-act world "moveTo"))
            (swap! (.. world -state) assoc-in [:self :health] 6)
            (let [reflex-round (core/tick! eng)]
              (await walking)
              (await reflex-round)))
          (is (= {} (job-mem eng "j1")) "the cut round could not commit")
          (is (= [:fired :cut] (->> @seen (map :kind) (filter #{:fired :cut}))))
          (let [fired (first (filter #(= :fired (:kind %)) @seen))
                cut (first (filter #(= :cut (:kind %)) @seen))]
            (is (= (:seq fired) (:cause cut)))
            (is (= "j1" (:interrupted fired))))
          (swap! (.. world -state) assoc-in [:self :health] 20)
          (await (core/tick! eng))
          (is (= ["j1" "j3" "j1"] (ran seen)) "the cut job runs before j2")
          (is (= ["j2"] (listed eng))))))))

(deftest an-interval-reflex-cuts-a-listed-job-which-resumes-after-the-look
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p clock]} (setup {})
              world (.-world p)]
          (core/submit! eng (list 'walk {:pos {:x 5 :y 64 :z 0}}) {})
          (core/submit! eng '(count) {})
          (.hold world "moveTo")
          (let [walking (core/tick! eng)]
            (await (in-act (.-world p) "moveTo"))
            (core/register-reflex! eng {:trigger :interval})
            (let [reflex-round (core/tick! eng)]
              (await walking)
              (await reflex-round)))
          (is (= [:fired :cut] (->> @seen (map :kind) (filter #{:fired :cut}))))
          (is (= 8 (count (filter #(= "look" (.-name %)) (.-calls world)))))
          (await (core/tick! eng))
          (is (= ["j1" "j3" "j1"] (ran seen)) "the cut job resumes once the look is done")
          (swap! clock + 45000)
          (is (some? (core/tick! eng)) "the interval elapsed: it fires again"))))))

(deftest a-higher-reflex-drops-a-lower-one-and-a-lower-one-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup {:self {:food 5}
                                          :entities [{:id 1 :name "zombie" :kind "hostile" :pos {:x 3 :y 64 :z 0}}]})
              world (.-world p)]
          (core/register-reflex! eng {:trigger :near})
          (core/register-reflex! eng {:trigger :hungry})
          (.hold world "moveTo")
          (let [release-eat (.hold world "eat")
                r1 (core/tick! eng)
                _ (await (in-act (.-world p) "moveTo"))]
            (is (= :near (:reflex (core/running eng))) "the first true entry in order fires")
            (core/move! eng :hungry {:below :near} 60)
            (is (nil? (core/tick! eng)) "a lower reflex does not interrupt")
            (core/move! eng :hungry {:above :near} 60)
            (let [r2 (core/tick! eng)]
              (await r1)
              (is (= :hungry (:reflex (core/running eng))))
              (is (some #(and (= :ended (:kind %)) (= "dropped" (name (:how %)))) @seen))
              (release-eat)
              (await r2)
              (is (nil? (core/running eng))))))))))

(deftest persistence-cooldown-retry-and-stop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (testing "cooldown"
          (let [{:keys [eng seen clock]} (setup {:self {:food 5}})]
            (core/register-reflex! eng {:trigger :hungry})
            (await (core/tick! eng))
            (is (some #(= "completed_not_cleared" (some-> (:how %) name)) @seen))
            (is (nil? (core/tick! eng)) "cooling down")
            (swap! clock + 31000)
            (await (core/tick! eng))
            (is (= 2 (count (ran seen))))))
        (testing "retry"
          (let [{:keys [eng seen]} (setup {:self {:health 5}})]
            (core/register-reflex! eng {:trigger :hurt})
            (await (core/tick! eng))
            (await (core/tick! eng))
            (is (= 2 (count (ran seen))))))
        (testing "stop until the condition was false once"
          (let [{:keys [eng seen p]} (setup {:entities [{:id 1 :name "zombie" :kind "hostile" :pos {:x 3 :y 64 :z 0}}]
                                             :unreachable ["50,64,0"]})
                state (.. p -world -state)]
            (core/register-reflex! eng {:trigger :near})
            (await (core/tick! eng))
            (is (nil? (core/tick! eng)))
            (swap! state assoc :entities [])
            (core/tick! eng)
            (swap! state assoc :entities [{:id 2 :name "zombie" :kind "hostile" :pos [3 64 0] :health 20}])
            (await (core/tick! eng))
            (is (= 2 (count (ran seen))))))))))

(deftest mutes-carry-a-ttl-replace-and-revert
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (setup {:self {:health 5}})]
          (core/register-reflex! eng {:trigger :hurt})
          (core/mute! eng :hurt 10)
          (core/mute! eng :hurt 60)
          (is (nil? (core/tick! eng)))
          (swap! clock + 30000)
          (is (nil? (core/tick! eng)) "the second mute replaced the first")
          (swap! clock + 31000)
          (await (core/tick! eng))
          (is (= 1 (count (filter #(= :reverted (:kind %)) @seen))))
          (is (= 1 (count (ran seen)))))))))

(deftest a-wall-clock-jump-does-not-end-a-mute
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (setup {:self {:health 5}})
              mono (atom @clock)
              eng (assoc eng :mono #(deref mono))]
          (core/register-reflex! eng {:trigger :hurt})
          (core/mute! eng :hurt 60)
          (swap! clock + 6000000)
          (is (nil? (core/tick! eng)) "a host suspend moved the wall clock, the monotonic one stood still")
          (is (pos? (:clock-lag (core/state eng))) "the lag is saved with the state")
          (swap! clock + 61000)
          (swap! mono + 61000)
          (await (core/tick! eng))
          (is (= 1 (count (filter #(= :reverted (:kind %)) @seen)))))))))

(deftest built-in-reflexes-cannot-be-removed
  (let [{:keys [eng]} (setup)]
    (core/register-reflex! eng {:trigger :hurt :builtin? true})
    (core/register-reflex! eng {:trigger :never})
    (is (false? (core/remove-reflex! eng :hurt)))
    (is (true? (core/remove-reflex! eng :never)))
    (is (= [:hurt] (mapv :id (:register (core/state eng)))))))

(deftest do-now-cuts-and-the-cut-job-continues-once-the-new-one-yields
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup)]
          (core/submit! eng (list 'walk {:pos {:x 5 :y 64 :z 0}}) {})
          (.hold (.-world p) "moveTo")
          (let [walking (core/tick! eng)]
            (await (in-act (.-world p) "moveTo"))
            (core/do-now! eng '(count))
            (await walking))
          (is (= ["j2" "j1"] (listed eng)))
          (await (core/tick! eng))
          (await (core/tick! eng))
          (is (= ["j1" "j2" "j1"] (ran seen)) "the new job yields after one round, then the cut job runs: no hold")
          (is (= ["j2"] (listed eng)) "the cut job walked on and is done; the new job stays listed")
          (core/cancel! eng "j2")
          (is (some #(= :cancelled (:kind %)) @seen)))))))

(deftest cancelling-a-cut-job-clears-its-round-entry
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup)]
          (core/submit! eng (list 'walk {:pos {:x 5 :y 64 :z 0}}) {})
          (.hold (.-world p) "moveTo")
          (let [walking (core/tick! eng)]
            (await (in-act (.-world p) "moveTo"))
            (core/do-now! eng '(count))
            (await walking))
          (is (contains? @(:rounds eng) "j1") "the cut job keeps its round entry for its next round")
          (core/cancel! eng "j1")
          (is (not (contains? @(:rounds eng) "j1")) "cancelled: nothing left to resume")))))) 

(deftest a-job-cancelled-while-its-round-runs-stays-gone-after-a-restart
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              {:keys [eng p]} (setup {} dir)]
          (core/submit! eng (list 'walk {:pos {:x 5 :y 64 :z 0}}) {})
          (let [release (.hold (.-world p) "moveTo")
                walking (core/tick! eng)]
            (await (in-act (.-world p) "moveTo"))
            (core/cancel! eng "j1")
            (release)
            (await walking))
          (is (= [] (listed eng)))
          (let [[again _] (restore-with dir registry triggers)]
            (is (= [] (listed again)) "the saved list does not bring it back")
            (is (nil? (:resume (core/state again))))
            (is (nil? (get-in (core/state again) [:instances "j1"])))))))))

(deftest dropping-a-reflex-job-clears-its-round-entry
  (let [{:keys [eng]} (setup)]
    (swap! (:state eng) assoc-in [:instances "r1"] {:spec {:op :leaf :job 'night}})
    (swap! (:rounds eng) assoc "r1" {:acts {}})
    (core/drop-reflex-job! eng "r1" :night :dropped {:how :dropped :by :death})
    (is (not (contains? @(:rounds eng) "r1")) "cut, death and shutdown drops leave no round entry behind")))

(deftest an-interrupted-job-continues-right-after-the-jobs-done-now-before-it
  ;; X and A take turns; A is cut by B, B by C, each before its round's body ran. Each job done now goes directly
  ;; before the one it cut, so once C and B are done the next round is A's again, not the round-robin's next pick (X).
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)
              x (core/submit! eng '(count) {})
              a (core/submit! eng '(count) {})
              _ (await (core/tick! eng))
              a-round (core/tick! eng)
              b (core/do-now! eng '(child {:rounds 1}))
              _ (await a-round)
              b-round (core/tick! eng)
              c (core/do-now! eng '(child {:rounds 1}))]
          (await b-round)
          (is (= [x c b a] (listed eng)) "each job done now sits directly before the job it cut")
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= [x a b c b a] (ran seen)) "C, then B, then the job cut first continues")
          (is (= [x a] (listed eng)) "B and C are done")
          (is (= {:n 1} (job-mem eng a)) "its one round that was not cut"))))))

(deftest a-job-done-now-between-rounds-goes-where-the-next-scan-starts
  (are [state expected] (= expected (:list (core/insert-now (merge {:list [] :cursor 0} state) "X")))
    {:list ["A" "B" "C"] :current "B"} ["A" "X" "B" "C"]
    {:list ["A" "B" "C"] :resume "C" :cursor 0} ["A" "B" "X" "C"]
    {:list ["A" "B" "C"] :cursor 2} ["A" "B" "X" "C"]
    {:list ["A" "B"] :cursor 9} ["A" "B" "X"]
    {:list [] :cursor 0} ["X"]))

(deftest insert-next-puts-the-job-directly-after-the-current-one
  (are [state expected] (= expected (:list (core/insert-next (merge {:list [] :cursor 0} state) "X")))
    {:list ["A" "B" "C" "D"] :current "C"} ["A" "B" "C" "X" "D"]
    {:list ["A" "B" "C" "D"] :cursor 3} ["A" "B" "C" "X" "D"]
    {:list ["A" "B" "C" "D"] :current "D" :cursor 0} ["A" "B" "C" "D" "X"]
    {:list [] :cursor 0} ["X"]
    {:list ["A" "B"] :cursor 0} ["X" "A" "B"]
    {:list ["A" "B"] :cursor 9} ["A" "B" "X"]
    {:list ["A" "B" "C"] :current "gone" :cursor 1} ["A" "X" "B" "C"]
    {:list ["A" "B" "C"] :resume "B" :cursor 0} ["A" "B" "X" "C"]))

(deftest two-front-jobs-in-one-round-each-go-directly-after-the-current-one
  (let [s {:list ["A" "B" "C" "D"] :current "C" :cursor 0}]
    (is (= ["A" "B" "C" "Y" "X" "D"] (:list (core/insert-next (core/insert-next s "X") "Y"))))))

(deftest a-front-job-submitted-during-a-round-runs-in-the-very-next-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup)
              release (do (core/submit! eng '(walk {:pos {:x 5 :y 64 :z 0}}) {})
                          (.hold (.-world p) "moveTo"))
              walking (core/tick! eng)
              _ (await (in-act (.-world p) "moveTo"))
              plain (core/submit! eng '(count) {})
              front (core/submit! eng '(count) {:next? true})]
          (is (= ["j1" front plain] (listed eng)) "the front job sits directly after the running one, a plain submit appends")
          (is (= "j1" (:id (core/running eng))) "the running round is not cut")
          (release)
          (await walking)
          (await (core/tick! eng))
          (await (core/tick! eng))
          (is (= ["j1" front plain] (ran seen))))))))

(deftest a-front-job-submitted-between-rounds-runs-next
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (dotimes [_ 3] (core/submit! eng '(count) {}))
          (await (core/tick! eng))
          (await (core/tick! eng))
          (let [front (core/submit! eng '(count) {:next? true})]
            (is (= ["j1" "j2" front "j3"] (listed eng)))
            (dotimes [_ 3] (await (core/tick! eng)))
            (is (= ["j1" "j2" front "j3" "j1"] (ran seen)))))))))

(deftest two-front-submits-in-one-round-run-the-later-one-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup)
              release (do (core/submit! eng '(walk {:pos {:x 5 :y 64 :z 0}}) {})
                          (core/submit! eng '(count) {})
                          (.hold (.-world p) "moveTo"))
              walking (core/tick! eng)
              _ (await (in-act (.-world p) "moveTo"))
              x (core/submit! eng '(count) {:next? true})
              y (core/submit! eng '(count) {:next? true})]
          (is (= ["j1" y x "j2"] (listed eng)))
          (release)
          (await walking)
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= ["j1" y x "j2"] (ran seen))))))))

(deftest a-front-job-whose-check-is-false-is-skipped-like-any-other
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (reset! flag false)
          (core/submit! eng '(count) {})
          (core/submit! eng '(count) {})
          (await (core/tick! eng))
          (let [gated (core/submit! eng '(gated) {:next? true})]
            (is (= ["j1" gated "j2"] (listed eng)))
            (await (core/tick! eng))
            (is (= ["j1" "j2"] (ran seen)) "the round goes to the job after the gated one")))))))

(deftest a-restart-keeps-the-front-order
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              {:keys [eng p]} (setup {} dir)
              release (do (core/submit! eng '(walk {:pos {:x 5 :y 64 :z 0}}) {})
                          (core/submit! eng '(count) {})
                          (.hold (.-world p) "moveTo"))]
          (core/tick! eng)
          (await (in-act (.-world p) "moveTo"))
          (let [front (core/submit! eng '(count) {:next? true})
                {again :eng seen :seen} (setup {} dir)]
            (is (= ["j1" front "j2"] (listed again)))
            (is (= "j1" (:resume (core/state again))) "the in-flight job resumes first, then the front job")
            (await (core/tick! again))
            (await (core/tick! again))
            (is (= ["j1" front] (ran seen)))
            (release)))))))

(deftest a-restart-between-rounds-keeps-the-front-job-next
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              {:keys [eng]} (setup {} dir)]
          (dotimes [_ 3] (core/submit! eng '(count) {}))
          (await (core/tick! eng))
          (await (core/tick! eng))
          (let [front (core/submit! eng '(count) {:next? true})
                {again :eng seen :seen} (setup {} dir)]
            (is (= ["j1" "j2" front "j3"] (listed again)))
            (await (core/tick! again))
            (is (= [front] (ran seen)))))))))

;; ---------------------------------------------------------------- body events and restart

(deftest body-events-become-records
  (let [{:keys [eng p seen]} (setup)]
    (.emit (.-world p) #js {:kind "hurt" :health 5})
    (is (= [{:health 5}] (mapv :data (mem/entries (mem/view (:store eng)) :hurt))))
    (hurt/flush! eng #(core/emit! eng %))
    (is (some #(= [:body :hurt] [(:source %) (:kind %)]) @seen) "the merged :hurt event follows its window")))

(defn continued-warns [seen] (filterv #(= [:reflex :continued] [(:source %) (:kind %)]) @seen))

(deftest a-reflex-job-that-continues-ends-declined-with-a-warn-once-an-hour
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (setup {:self {:health 5}})]
          (core/register-reflex! eng {:trigger :hurt :job '(count)})
          (await (core/tick! eng))
          (is (= {} (:instances (core/state eng))) "the reflex job ran one round and is gone")
          (is (nil? (core/holder eng)) "nothing holds the body between rounds")
          (is (= [["j1" :declined]] (->> @seen (filter #(= [:reflex :ended] [(:source %) (:kind %)]))
                                          (mapv (juxt :job :outcome)))))
          (is (= [[:hurt "j1"]] (mapv (juxt :reflex :job) (continued-warns seen))))
          (await (core/tick! eng))
          (is (= ["j1" "j2"] (ran seen)) "the trigger still holds: a :retry entry fires again")
          (is (= 1 (count (continued-warns seen))) "warned once per reflex per hour")
          (swap! clock + 3600000)
          (await (core/tick! eng))
          (is (= 2 (count (continued-warns seen))) "and again after an hour"))))))

(deftest a-reflex-job-that-stops-ends-stopped-with-its-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:health 5}})]
          (core/register-reflex! eng {:trigger :hurt :job '(give-up)})
          (await (core/tick! eng))
          (let [ended (->> @seen (filter #(= [:reflex :ended] [(:source %) (:kind %)])) first)
                stopped (->> @seen (filter #(= [:job :stopped] [(:source %) (:kind %)])) first)]
            (is (= [:stopped :no-way] ((juxt :outcome :reason) ended)))
            (is (= :completed_not_cleared (:how ended)) "the trigger still holds")
            (is (= [:hurt "j1" :no-way] ((juxt :reflex :job :reason) stopped))
                "a job.stopped event carries the reflex and the reason")))))))

(deftest the-tick-loop-parks-a-failed-round-and-keeps-ticking
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              [seen sink] (tu/legacy-capture-sink)
              eng (core/create {:primitives (tu/fake {}) :jobs registry :triggers triggers :dir (tu/tmp-dir)
                                :now #(deref clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})
              stop (do (core/submit! eng '(write-fail) {})
                       (core/submit! eng '(count) {})
                       (core/start! eng {:tick-ms 5}))]
          (await (js/Promise. (fn [resolve] (js/setTimeout resolve 60))))
          (stop)
          (is (some #(and (= :failed (:kind %)) (= :required (:attention %))) @seen)
              "the failure parks the job with a required attention request")
          (is (<= 2 (count (filter #(= :round_started (:kind %)) @seen))) "the loop kept ticking"))))))

(deftest the-tick-loop-survives-a-failing-hook-whose-report-also-fails
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              calls (atom 0)
              logdir (path/join (tu/tmp-dir) "log")
              _ (fs/mkdirSync logdir)
              eng (core/create {:primitives (tu/fake {}) :jobs registry :triggers triggers :dir (tu/tmp-dir)
                                :now #(deref clock)
                                :events (events/make {:body "Fake" :file (path/join logdir "events.edn") :now #(deref clock)})})
              _ (fs/rmSync logdir #js {:recursive true})
              stop (core/start! eng {:tick-ms 5 :before-tick #(do (swap! calls inc) (throw (js/Error. "hook")))})]
          (await (js/Promise. (fn [resolve] (js/setTimeout resolve 60))))
          (stop)
          (is (<= 3 @calls) "the timer is re-armed although the error report threw"))))))

(deftest the-tick-loop-handles-tick-returning-non-promise
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              [seen sink] (tu/legacy-capture-sink)
              eng (core/create {:primitives (tu/fake {}) :jobs registry :triggers triggers :dir (tu/tmp-dir)
                                :now #(deref clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})
              original-tick! core/tick!
              tick-count (atom 0)
              stop (do (core/submit! eng '(count) {})
                       (set! core/tick! (fn [e]
                                         (swap! tick-count inc)
                                         (if (= 1 @tick-count) nil (original-tick! e))))
                       (core/start! eng {:tick-ms 5}))]
          (await (js/Promise. (fn [resolve] (js/setTimeout resolve 60))))
          (set! core/tick! original-tick!)
          (stop)
          (is (>= @tick-count 3) "the loop ticked multiple times including when tick! returned nil"))))))

(deftest the-tick-loop-handles-tick-throwing-synchronously
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              [seen sink] (tu/legacy-capture-sink)
              eng (core/create {:primitives (tu/fake {}) :jobs registry :triggers triggers :dir (tu/tmp-dir)
                                :now #(deref clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})
              original-tick! core/tick!
              tick-count (atom 0)
              stop (do (core/submit! eng '(count) {})
                       (set! core/tick! (fn [e]
                                         (swap! tick-count inc)
                                         (if (= 1 @tick-count) (throw (js/Error. "sync tick error")) (original-tick! e))))
                       (core/start! eng {:tick-ms 5}))]
          (await (js/Promise. (fn [resolve] (js/setTimeout resolve 60))))
          (set! core/tick! original-tick!)
          (stop)
          (is (>= @tick-count 3) "the loop ticked multiple times despite a synchronous throw")
          (is (some #(and (= :error (:kind %)) (re-find #"sync tick error" (str (:text %)))) @seen)
              "the error was reported"))))))

(deftest a-throwing-check-parks-the-job-with-one-error
  (let [{:keys [eng seen clock]} (setup {})
        id (core/submit! eng '(bad-check) {})]
    (dotimes [_ 40]
      (swap! clock + 250)
      (core/tick! eng))
    (is (= 1 (count (filter #(and (= :error (:kind %)) (re-find #"check of.*broken check" (str (:text %)))) @seen)))
        "the throw is told once, with the message")
    (is (re-find #"broken check" (str (get-in (core/state eng) [:failed id :error]))) "the job is parked, not waiting")
    (is (= 1 (count (filter #(= :failed (:kind %)) @seen))) "one job.failed")
    (is (empty? (ran seen)) "its round never ran")))

(deftest a-failed-state-write-warns-but-does-not-break-the-change
  (let [dir (tu/tmp-dir)
        {:keys [eng seen]} (setup {} dir)]
    (fs/mkdirSync (path/join dir "engine.edn.tmp")) ; the atomic write cannot create its temp file
    (is (= "j1" (core/submit! eng '(count) {})) "the submit goes through")
    (is (some #(= :save-failed (:kind %)) @seen) "the failed write is told")))

(deftest picked-up-is-a-debug-event-and-an-entry-of-its-own
  (let [{:keys [eng p seen]} (setup)]
    (.emit (.-world p) #js {:kind "picked-up" :item "stick" :count 3})
          (is (= [{:item "stick" :count 3}] (mapv :data (mem/entries (mem/view (:store eng)) :picked-up))))
    (is (some #(and (= :body (:source %)) (= :picked-up (:kind %))) @seen))))

(deftest bot-errors-and-a-failed-reconnect-are-error-level-events
  (let [{:keys [eng p seen]} (setup)]
    (.emit (.-world p) #js {:kind "error" :reason "boom"})
    (.emit (.-world p) #js {:kind "reconnect-failed" :reason "refused"})
    (.emit (.-world p) #js {:kind "disconnected" :reason "end"})
    (is (= [:error :reconnect-failed :disconnected]
           (->> @seen (filter #(= :body (:source %))) (mapv :kind))))))

(deftest restart-restores-the-list-register-and-changes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              {:keys [eng p]} (setup {} dir)]
          (core/register-reflex! eng {:trigger :hurt})
          (core/register-reflex! eng {:trigger :never})
          (core/mute! eng :never 600)
          (core/submit! eng (list 'walk {:pos {:x 5 :y 64 :z 0}}) {})
          (core/submit! eng '(count) {})
          (.hold (.-world p) "moveTo")
          (core/tick! eng)
          (await (in-act (.-world p) "moveTo"))
          (let [{again :eng seen :seen} (setup {} dir)
                s (core/state again)]
            (is (= ["j1" "j2"] (:list s)))
            (is (= [:hurt :never] (mapv :id (:register s))))
            (is (= #{:never} (set (keys (:changes s)))))
            (is (= "j1" (:resume s)) "the in-flight round is lost and resumes first")
            (is (= 2 (count (mem/entries (mem/view (:store again)) :restart))) "one per start")
            (is (some #(= :restored (:kind %)) @seen))
            (core/submit! again '(count) {})
            (is (= ["j1" "j2" "j3"] (listed again)) "ids continue")
            (await (core/tick! again))
            (is (= ["j1"] (ran seen)))))))))

;; ---------------------------------------------------------------- shutdown

(deftest a-death-cancels-the-listed-jobs-but-keeps-one-submitted-to-survive-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p clock]} (setup)
              world (.-world p)
              kept (core/submit! eng (list 'walk {:pos {:x 5 :y 64 :z 0}}) {:survive-death? true})
              plain (core/submit! eng '(count) {})]
          (.hold world "moveTo")
          (let [round (core/tick! eng)]
            (await (in-act (.-world p) "moveTo"))
            (core/record-body-event! eng #js {:kind "died"})
            (await round))
          (is (= [kept] (listed eng)) "the flagged job stays listed")
          (is (= [[plain :death]] (->> @seen (filter #(= :cancelled (:kind %))) (mapv (juxt :job :by)))))
          (is (= [[kept :death]] (->> @seen (filter #(= :cut (:kind %))) (mapv (juxt :job :by))))
              "its round in flight is cut, its memory kept")
          (is (= [kept] (:survived (first (filter #(= :died (:kind %)) @seen)))) "the death event names it")
          (is (nil? (core/tick! eng)) "a dead body starts no listed job")
          (swap! clock + 1000)
          (.respawn world)
          (await (core/tick! eng))
          (is (= [kept kept] (ran seen)) "after the respawn it runs again")
          (is (= [] (listed eng))))))))

(deftest a-cut-the-engine-did-not-make-keeps-the-job-listed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup)]
          (.override (.-world p) "moveTo"
                     (fn [_ _ _] (js/Promise.reject (core/cut-error))))
          (core/submit! eng '(parent-walk) {})
          (await (core/tick! eng))
          (is (= ["j1"] (listed eng)) "a cut is not a failure")
          (is (= {:before true} (job-mem eng "j1")) "its memory survives")
          (is (= "j1" (:resume (core/state eng))))
          (is (not-any? #(= :failed (:kind %)) @seen))
          (is (some #(= :cut (:kind %)) @seen)))))))

(deftest shutdown-keeps-the-in-flight-job-for-the-restart
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              {:keys [eng p seen]} (setup {} dir)]
          (core/submit! eng '(parent-walk) {})
          (.hold (.-world p) "moveTo")
          (let [walking (core/tick! eng)]
            (await (in-act (.-world p) "moveTo"))
            (core/shutdown! eng)
            (await walking))
          (is (= ["j1"] (listed eng)))
          (is (not-any? #(= :failed (:kind %)) @seen))
          (let [{again :eng seen2 :seen} (setup {} dir)]
            (is (= ["j1"] (listed again)))
            (is (= {:before true} (job-mem again "j1")))
            (is (= "j1" (:resume (core/state again))))
            (await (core/tick! again))
            (is (= ["j1"] (ran seen2)) "the body resumes from the list")))))))

;; ---------------------------------------------------------------- act

(defn ^:async intent-round
  "Writes an intent, then walks; the intent must be on disk during the walk."
  [c]
  (ctx/update-mem! c assoc :intent :walk-east)
  (await (ctx/act c :moveTo #js {:pos #js {:x 5 :y 64 :z 0}}))
  (ctx/update-mem! c assoc :walked true)
  (await (ctx/act c :look #js {:pos #js {:x 9 :y 64 :z 0}}))
  (done c))

(deftest act-saves-memory-before-and-after-and-emits-debug-events
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen dir clock]} (setup)
              eng (assoc-in eng [:jobs 'intent] (tu/job intent-round))
              release (.hold (.-world p) "moveTo")
              release-look (.hold (.-world p) "look")]
          (core/submit! eng '(intent) {})
          (swap! clock + 3000)
          (let [r (core/tick! eng)]
            (await (in-act (.-world p) "moveTo"))
            (is (= :walk-east (get-in (memory-on-disk dir) [:entries :job/j1 0 :data :intent]))
                "saved before the primitive")
            (swap! clock + 3000)
            (swap! clock + 3000)
            (release)
            (await (js/Promise. (fn [ok] (js/setTimeout ok 0))))
            (swap! clock + 3000)
            (core/tick! eng)
            (is (true? (get-in (memory-on-disk dir) [:entries :job/j1 0 :data :walked]))
                "flushed once the save gap has passed")
            (release-look)
            (await r))
          (is (= [[:action :started "moveTo"] [:action :done "moveTo"]
                  [:action :started "look"] [:action :done "look"]]
                 (->> @seen (filter #(= :action (:source %))) (mapv (juxt :source :kind :name)))))
          (let [actions (filter #(= :action (:source %)) @seen)]
            (is (= 2 (count (set (map :action-id actions)))) "repeated calls get different IDs")
            (is (= (mapv :action-id (take-nth 2 actions))
                   (mapv :action-id (take-nth 2 (rest actions)))) "each completion pairs with its start"))
          (is (= 4 (count (filter #(= :action (:source %)) @seen)))
              "primitive calls have start/done events"))))))

(defn ^:async blocked-walk-round [c]
  (await (ctx/act c :moveTo #js {:pos #js {:x 9 :y 64 :z 9}}))
  (await (ctx/act c :look #js {:pos #js {:x 9 :y 64 :z 0}}))
  (done c))

(deftest action-done-carries-reason-and-rounded-distance-when-the-result-has-them
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:noPath ["9,64,9"]})
              eng (assoc-in eng [:jobs 'walker] (tu/job blocked-walk-round))]
          (set! (.-moveTo p) (fn [_t _a] (js/Promise.resolve #js {:status "blocked" :reason "noPath" :distance 12.7279221})))
          (core/submit! eng '(walker) {})
          (await (core/tick! eng))
          (is (= [{:name "moveTo" :status "blocked" :reason "noPath" :distance 12.73}
                  {:name "look" :status "ok"}]
                 (->> @seen (filter #(and (= :action (:source %)) (= :done (:kind %))))
                      (mapv #(select-keys % [:name :status :reason :distance]))))))))))

(deftest swim-is-an-acting-primitive-through-ctx-act
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:pos {:x 0 :y 60 :z 0} :oxygen 2}
                                          :blocks {"0,60,0" "water" "0,61,0" "water"}})
              eng (assoc-in eng [:jobs 'swimmer] (tu/job swim-round))]
          (core/submit! eng '(swimmer) {})
          (await (core/tick! eng))
          (is (= ["swim"] (mapv #(.-name %) (array-seq (.. p -world -calls)))))
          (is (= [20 61] [(.-oxygen (.self p)) (.-y (.-pos (.self p)))])))))))

(deftest a-check-cannot-act-or-write
  (let [{:keys [eng]} (setup)
        c (core/make-ctx eng {:root "j1" :slots [] :chain ["j1"] :token nil :args {} :round 0})]
    (core/submit! eng '(count) {})
    (is (thrown? js/Error (ctx/update-mem! c assoc :x 1)))
    (is (thrown? js/Error (ctx/remember! c :seen {})))))

(deftest a-stale-token-is-cut-at-act-without-calling-the-primitive
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup)
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "old" :args {} :round 1})]
          (.setOwner p "new")
          (is (= :cut (try (await (ctx/act c :look #js {:pos #js {:x 0 :y 64 :z 0}}))
                           (catch :default e (when (core/cut? e) :cut)))))
          (is (= 0 (.-length (.-calls (.-world p))))))))))

(deftest a-stale-token-drops-the-rounds-events
  (let [{:keys [eng p seen]} (setup)
        c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "old" :args {} :round 1})]
    (.setOwner p "old")
    (ctx/emit! c :said :info {:n 1})
    (.setOwner p "new")
    (ctx/emit! c :said :info {:n 2})
    (is (= [1] (mapv :n (filter #(= :said (:kind %)) @seen))) "only the event emitted while the round owned the body")))

(deftest a-check-ctx-still-emits
  (let [{:keys [eng seen]} (setup)
        c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token nil :args {} :round 1})]
    (ctx/emit! c :said :info {})
    (is (= 1 (count (filter #(= :said (:kind %)) @seen))))))

(deftest alive-says-whether-the-round-still-owns-the-body
  (let [{:keys [eng p]} (setup)
        c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "old" :args {} :round 1})
        check (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token nil :args {} :round 1})]
    (.setOwner p "old")
    (is (true? (ctx/alive? c)))
    (.setOwner p "new")
    (is (false? (ctx/alive? c)) "cut: a search loop stops")
    (is (true? (ctx/alive? check)) "a check has no token and runs synchronously")
    (is (true? (ctx/alive? {})) "a ctx with no engine behind it (a unit test) is alive")))

;; ---------------------------------------------------------------- doing nothing

(defn ^:async parked-round
  "Awaits its gate's promise with no act (a round doing nothing), then acts once if :act-after, and ends :continue."
  [gate c]
  (when (:hold (:args c)) (ctx/hold-still! c (:hold (:args c))))
  (await (js/Promise. (fn [resolve] (reset! gate resolve))))
  (when (:act-after (:args c))
    (await (ctx/act c :look #js {:pos #js {:x 0 :y 64 :z 0}}))
    (await (js/Promise. (fn [resolve] (reset! gate resolve)))))
  :continue)

(defn ^:async why-wait-round [c]
  (await (ctx/act c :wait #js {:ms 1000 :why "daylight"}))
  :continue)

(defn ^:async hold-then-clear-round
  "Holds :night until its gate, clears the hold, then parks with no act until the gate again."
  [gate c]
  (ctx/hold-still! c :night)
  (await (js/Promise. (fn [resolve] (reset! gate resolve))))
  (ctx/hold-still! c nil)
  (await (js/Promise. (fn [resolve] (reset! gate resolve))))
  :continue)

(defn idle-registry
  "Registry whose parked jobs wait on gate, so no test sees another's rounds."
  [gate]
  (merge registry {'hold-clear (tu/job (partial hold-then-clear-round gate))
                   'parked (assoc (tu/job (partial parked-round gate)) :args {:hold {:default nil} :act-after {:default false}})
                   'why-wait (tu/job why-wait-round)}))

(defn idle-setup [opts]
  (let [clock (atom 1000000)
        gate (atom nil)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake {})
        eng (core/create (merge {:primitives p :jobs (idle-registry gate) :triggers triggers :dir (tu/tmp-dir)
                                 :now #(deref clock)
                                 :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})}
                                opts))]
    {:eng eng :seen seen :clock clock :p p :gate gate}))

(defn ^:async settle-until
  "Let timers run until (pred) holds, at most n times."
  [pred n]
  (loop [i 0]
    (when (and (< i n) (not (pred)))
      (await (js/Promise. (fn [r] (js/setTimeout r 0))))
      (recur (inc i)))))

(defn idles [seen] (filterv #(= [:job :idle] [(:source %) (:kind %)]) @seen))
(defn holdings [seen] (filterv #(= [:job :holding] [(:source %) (:kind %)]) @seen))

(deftest a-round-with-no-act-for-idle-s-warns-once-per-spell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock gate]} (idle-setup {})
              _ (core/submit! eng (list 'parked {:act-after true}) {})
              round (core/tick! eng)
              _ (await (settle-until #(some? @gate) 50))]
          (swap! clock + 10000)
          (core/tick! eng)
          (is (= [] (idles seen)) "10 s is not more than the default idle-s")
          (swap! clock + 1)
          (let [r (core/tick! eng)]
            (is (or (nil? r) (instance? js/Promise r)) "the tick that warns idle returns nil or a promise (start! calls .catch on it)"))
          (core/tick! eng)
          (is (= [["j1" 10001]] (mapv (juxt :job :idle-ms) (idles seen))) "warned once")
          (let [first-gate @gate]
            (first-gate nil)
            (await (settle-until #(not= first-gate @gate) 50)))
          (swap! clock + 10001)
          (core/tick! eng)
          (is (= 2 (count (idles seen))) "an act ends the spell; a new one warns again")
          (@gate nil)
          (await round)
          (swap! clock + 20000)
          (core/tick! eng)
          (is (= 2 (count (idles seen))) "no round, nothing is idle"))))))

(deftest a-declared-hold-is-not-idle-and-is-told
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock gate]} (idle-setup {:idle-s 5})
              _ (core/submit! eng (list 'parked {:hold :night}) {})
              round (core/tick! eng)
              _ (await (settle-until #(seq (holdings seen)) 50))]
          (is (= [["j1" :night 1000000]] (mapv (juxt :job :reason :since) (holdings seen))))
          (is (= {:reason :night :since 1000000} (core/holding eng "j1")))
          (is (= {:reason :night :since 1000000} (:holding (job-api/summary eng "j1"))) "agents see it in jobs show")
          (swap! clock + 60000)
          (core/tick! eng)
          (is (= [] (idles seen)))
          (@gate nil)
          (await round)
          (is (nil? (core/holding eng "j1")) "a hold ends with its round"))))))

(deftest clearing-a-hold-restarts-the-idle-clock
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock gate]} (idle-setup {:idle-s 5})
              _ (core/submit! eng '(hold-clear) {})
              round (core/tick! eng)
              _ (await (settle-until #(some? @gate) 50))]
          (swap! clock + 60000)
          (let [first-gate @gate]
            (first-gate nil)
            (await (settle-until #(not= first-gate @gate) 50)))
          (core/tick! eng)
          (is (= [] (idles seen)) "the hold's 60 s are not idle time")
          (swap! clock + 5001)
          (core/tick! eng)
          (is (= 1 (count (idles seen))) "idle-s after the clear it warns")
          (@gate nil)
          (await round))))))

(deftest a-wait-with-why-is-a-declared-hold
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (idle-setup {})
              release (.hold (.-world p) "wait")
              _ (core/submit! eng '(why-wait) {})
              round (core/tick! eng)]
          (await (settle-until #(seq (holdings seen)) 50))
          (is (= [["j1" "daylight"]] (mapv (juxt :job :reason) (holdings seen))))
          (is (= "daylight" (:reason (core/holding eng "j1"))))
          (release)
          (await round)
          (is (nil? (core/holding eng "j1"))))))))

(deftest a-check-ctx-cannot-hold-still
  (let [{:keys [eng]} (idle-setup {})
        c (core/make-ctx eng {:root "j1" :slots [] :chain ["j1"] :token nil :args {} :round 0})]
    (is (thrown? js/Error (ctx/hold-still! c :night)))))

;; ---------------------------------------------------------------- sweep timer

(deftest ticks-sweep-memory-on-a-timer
  (let [{:keys [eng clock dir]} (setup)
        store (:store eng)]
    (mem/write! store :seen {} {:cap 5 :ttl 1000})
    (mem/save! store)
    (swap! clock + 2000)
    (core/tick! eng)
    (is (some? (get-in (memory-on-disk dir) [:entries :seen])) "not yet: the timer has not run")
    (swap! clock + core/default-sweep-ms)
    (core/tick! eng)
    (is (nil? (get-in (memory-on-disk dir) [:entries :seen])))))

;; ---------------------------------------------------------------- job expressions

(defn ^:async twice-round
  "Done on its second round; records the rounds it ran."
  [c]
  (let [n (inc (:n (ctx/mem c) 0))]
    (ctx/update-mem! c assoc :n n)
    (if (>= n 2) (done c) :continue)))

(deftest a-trigger-whose-jobs-check-declines-is-held-and-cuts-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (reset! flag false)
          (core/submit! eng '(count) {})
          (core/register-reflex! eng {:trigger :gated})
          (dotimes [_ 2] (await (core/tick! eng)))
          (is (= ["j1" "j1"] (ran seen)) "the trigger did not fire: the listed job kept the body")
          (is (= {:n 2} (job-mem eng "j1")))
          (is (some? (core/held-trigger eng :gated))))))))

(def flag-b (atom true))

(def declining-reflexes
  "Reflex triggers whose job is a combinator whose children all decline."
  (into {} (for [[k job] {:gated-any '(any (gated))
                          :gated-seq '(seq (gated) (count))
                          :gated-repeat '(repeat (gated))}]
             [k {:name k :job job :persistence :cooldown :cooldown-s 10 :when (constantly true)}])))

(defn declined-setup []
  (let [s (setup)]
    (update s :eng assoc :triggers (merge triggers declining-reflexes))))

(deftest a-reflex-combinator-whose-children-decline-is-held-until-they-pass
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [trigger [:gated-any :gated-seq :gated-repeat]]
          (let [{:keys [eng seen clock]} (declined-setup)]
            (reset! flag false)
            (core/register-reflex! eng {:trigger trigger})
            (await (core/tick! eng))
            (is (empty? (ran seen)) (str trigger " held"))
            (is (empty? (filter #(= [:reflex :fired] [(:source %) (:kind %)]) @seen)))
            (is (some? (core/held-trigger eng trigger)))
            (reset! flag true)
            (swap! clock + 5000)
            (await (core/tick! eng))
            (is (= ["j1"] (ran seen)) (str trigger " fires once the child's check passes"))))))))


(def expr-registry
  (merge registry {'twice (tu/job twice-round)
                   'twice-b (tu/job [(fn [_] (if @flag-b (contract/ok :go "go") (contract/later :not-ready "not ready")))] twice-round)
                   'defaults (assoc (tu/job count-round) :args {:a {:doc "a" :default 1} :b {:doc "b" :default 2}})}))

(defn expr-setup []
  (let [s (setup)]
    (update s :eng assoc :jobs expr-registry)))

(defn child-n [eng id slots] (:n (job-mem eng id slots)))

(deftest a-leaf-runs-with-its-args-merged-over-the-defaults
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (expr-setup)]
          (core/submit! eng '(defaults {:b 5}) {})
          (is (= {:a 1 :b 5} (:args (mem/job-mem (mem/view (:store eng)) "j1" [])))))))))

(deftest seq-runs-its-children-in-order-and-is-done-after-the-last
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (expr-setup)]
          (core/submit! eng '(seq (twice) (twice-b)) {})
          (dotimes [_ 2] (await (core/tick! eng)))
          (is (= 1 (:at (job-mem eng "j1"))) "the first child is done")
          (is (nil? (child-n eng "j1" [:c0])) "and its memory cleared")
          (is (nil? (child-n eng "j1" [:c1])) "the second child has not run yet")
          (reset! flag-b false)
          (is (nil? (core/tick! eng)) "check = the next unfinished child's check")
          (reset! flag-b true)
          (dotimes [_ 2] (await (core/tick! eng)))
          (is (= [] (listed eng)))
          (is (= "(seq twice twice-b)" (:name (first (filter #(= :completed (:kind %)) @seen))))
              "events name the expression"))))))

(deftest any-runs-the-first-child-whose-check-passes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (expr-setup)]
          (reset! flag false)
          (core/submit! eng '(any (gated) (twice)) {})
          (await (core/tick! eng))
          (is (= 1 (child-n eng "j1" [:c1])) "the gated child declined")
          (await (core/tick! eng))
          (is (= [] (listed eng)) "done when that child is done"))))))

(deftest any-declines-when-every-child-declines
  (let [{:keys [eng]} (expr-setup)]
    (reset! flag false)
    (reset! flag-b false)
    (core/submit! eng '(any (gated) (twice-b)) {})
    (is (nil? (core/tick! eng)))
    (reset! flag-b true)))

(deftest repeat-starts-its-child-fresh-each-time-it-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (expr-setup)]
          (core/submit! eng '(repeat (twice)) {})
          (dotimes [_ 5] (await (core/tick! eng)))
          (is (= ["j1"] (listed eng)) "never done")
          (is (= 2 (:runs (job-mem eng "j1"))))
          (is (= 1 (child-n eng "j1" [:c0])) "the third run started fresh"))))))

(deftest repeat-declines-when-its-child-declines
  (let [{:keys [eng]} (expr-setup)]
    (reset! flag false)
    (core/submit! eng '(repeat (gated)) {})
    (is (nil? (core/tick! eng)))))

(deftest repeat-with-a-count-is-done-after-that-many-runs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (expr-setup)]
          (core/submit! eng '(repeat 2 (twice)) {})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= ["j1"] (listed eng)) "one run to go")
          (await (core/tick! eng))
          (is (= [] (listed eng)) "done after the second run"))))))

(deftest until-runs-its-child-while-the-guard-check-fails-and-is-done-when-it-passes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (expr-setup)]
          (reset! flag-b false)
          (core/submit! eng '(until (twice-b) (twice)) {})
          (dotimes [_ 5] (await (core/tick! eng)))
          (is (= ["j1"] (listed eng)) "the guard has not passed: the child repeats")
          (is (= 1 (child-n eng "j1" [:c1])) "the third run is in its first round")
          (reset! flag-b true)
          (await (core/tick! eng))
          (is (= [] (listed eng)) "done once the guard's check passes"))))))

(deftest combinators-nest-and-survive-a-restart
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              boot #(core/create {:primitives (tu/fake {}) :jobs expr-registry :triggers triggers
                                  :dir dir :now (constantly 1000000)
                                  :events (events/make {:body "Fake" :sinks []})})
              eng (boot)]
          (core/submit! eng '(seq (repeat (any (twice))) (count)) {})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= 1 (child-n eng "j1" [:c0 :c0 :c0])) "slots nest by position")
          (let [again (boot)]
            (is (= {:op :seq :children [{:op :repeat :child {:op :any :children [{:op :leaf :job 'twice :args {}}]}}
                                        {:op :leaf :job 'count :args {}}]}
                   (get-in (core/state again) [:instances "j1" :spec])))
            (await (core/tick! again))
            (is (= 2 (:runs (job-mem again "j1" [:c0]))) "the resumed child finished its second run")))))))

(deftest a-bad-spec-is-refused-at-submit-and-register
  (let [{:keys [eng]} (expr-setup)]
    (is (thrown-with-msg? js/Error #"unknown job or combinator nope" (core/submit! eng '(nope) {})))
    (is (thrown-with-msg? js/Error #"unknown job or combinator hold" (core/submit! eng '(hold (eat)) {})))
    (is (= [] (listed eng)))))

(deftest a-restored-job-whose-namespace-is-gone-is-dropped-with-a-warn
  (let [dir (tu/tmp-dir)
        {:keys [eng]} (setup {} dir)]
    (core/submit! eng '(count) {})
    (let [clock (atom 1000000)
          [seen sink] (tu/legacy-capture-sink)
          again (core/create {:primitives (tu/fake {}) :jobs (dissoc registry 'count) :triggers triggers
                              :dir dir :now #(deref clock)
                              :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
      (is (= [] (listed again)))
      (is (some #(= :failed (:kind %)) @seen)))))

(defn restore-with
  "An engine restored from dir with the given jobs and triggers; [eng seen]."
  [dir jobs triggers]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)]
    [(core/create {:primitives (tu/fake {}) :jobs jobs :triggers triggers :dir dir :now #(deref clock)
                   :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})
     seen]))

(deftest a-restored-register-entry-whose-trigger-is-gone-is-dropped-with-a-warn
  (let [dir (tu/tmp-dir)
        {:keys [eng]} (setup {} dir)]
    (core/register-reflex! eng {:trigger :hurt})
    (core/register-reflex! eng {:trigger :never})
    (core/mute! eng :never 600)
    (let [[again seen] (restore-with dir registry (dissoc triggers :never))]
      (is (= [:hurt] (mapv :id (:register (core/state again)))))
      (is (not (contains? (:changes (core/state again)) :never)) "its changes go with it")
      (is (some #(and (= :system (:source %)) (= :dropped (:kind %))) @seen))
      (is (nil? (core/tick! again)) "the tick no longer throws"))))

(deftest a-restored-register-entry-whose-job-is-gone-is-dropped-with-a-warn
  (let [dir (tu/tmp-dir)
        {:keys [eng]} (setup {} dir)]
    (core/register-reflex! eng {:trigger :hurt})
    (core/register-reflex! eng {:trigger :never :job '(count)})
    (let [[again seen] (restore-with dir (dissoc registry 'count) triggers)]
      (is (= [:hurt] (mapv :id (:register (core/state again)))))
      (is (some #(and (= :system (:source %)) (= :dropped (:kind %))) @seen)))))

(deftest a-saved-hold-form-is-dropped-on-restore-and-the-rest-of-the-register-starts
  (let [dir (tu/tmp-dir)
        {:keys [eng]} (setup {} dir)]
    (core/register-reflex! eng {:trigger :hurt})
    (core/register-reflex! eng {:trigger :never :job '(count)})
    (swap! (:state eng) update :register
           (fn [r] (mapv #(if (= :never (:id %)) (assoc % :job '(hold (count))) %) r)))
    (let [[again seen] (restore-with dir registry triggers)]
      (is (= [:hurt] (mapv :id (:register (core/state again)))))
      (is (some #(and (= :system (:source %)) (= :dropped (:kind %))) @seen)))))

(def stale-registry
  "The registry after a job dropped its :old arg."
  (assoc registry 'walk (assoc (tu/job walk-round) :args {:pos nil})))

(defn attention-reasons [eng]
  (set (map :reason (vals (:attention (core/state eng))))))

(deftest a-restored-register-entry-with-a-stale-arg-is-kept-without-it-and-the-agent-is-told
  (let [dir (tu/tmp-dir)
        {:keys [eng]} (setup {} dir)]
    (core/register-reflex! eng {:trigger :hurt :job '(walk {:pos {:x 1 :y 64 :z 2}})})
    (swap! (:state eng) update :register
           (fn [r] (mapv #(if (= :hurt (:id %)) (assoc % :job '(walk {:pos {:x 1 :y 64 :z 2} :old 12 :older 8})) %) r)))
    (let [[again seen] (restore-with dir stale-registry triggers)
          entry (first (:register (core/state again)))]
      (is (= [:hurt] (mapv :id (:register (core/state again)))) "the reflex is kept")
      (is (= '(walk {:pos {:x 1 :y 64 :z 2}}) (:job entry)) "only the stale args went")
      (is (some #(and (= :system (:source %)) (= :reflex-repaired (:kind %)) (re-find #":old :older" (:text %))) @seen))
      (is (= #{:reflex-repaired} (attention-reasons again)) "a required request the agent sees")
      (is (some #(and (= :reflex-repaired (:kind %)) (= :required (:attention %))) @seen)))))

(deftest a-restored-register-entry-that-cannot-be-kept-raises-an-attention-request
  (let [dir (tu/tmp-dir)
        {:keys [eng]} (setup {} dir)]
    (core/register-reflex! eng {:trigger :never :job '(count)})
    (let [[again seen] (restore-with dir (dissoc registry 'count) triggers)]
      (is (= [] (:register (core/state again))))
      (is (= #{:reflex-dropped} (attention-reasons again)))
      (is (some #(and (= :dropped (:kind %)) (re-find #"DROPPED" (:text %))) @seen)))))

(deftest a-restored-listed-job-with-a-stale-arg-runs-without-it-and-the-agent-is-told
  (let [dir (tu/tmp-dir)
        {:keys [eng]} (setup {} dir)]
    (core/submit! eng '(walk {:pos {:x 1 :y 64 :z 2}}) {})
    (swap! (:state eng) assoc-in [:instances "j1" :spec :args :old] 7)
    (let [[again _] (restore-with dir stale-registry triggers)]
      (is (= ["j1"] (listed again)))
      (is (= {:pos {:x 1 :y 64 :z 2}} (get-in (core/state again) [:instances "j1" :spec :args])))
      (is (= #{:stale-args} (attention-reasons again))))))

(deftest a-child-by-symbol-gets-the-registry-defaults
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (expr-setup)
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})]
          (.setOwner (:primitives eng) "tx")
          (await (ctx/call c :x 'defaults {:b 9}))
          (is (= {:a 1 :b 9} (:args (mem/job-mem (mem/view (:store eng)) "j9" [:x])))))))))

;; ---------------------------------------------------------------- save measurement

(defn memory-events [seen kind]
  (filterv #(= [:memory kind] [(:source %) (:kind %)]) @seen))

(deftest saves-emit-no-per-save-event
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (core/submit! eng '(count) {})
          (await (core/tick! eng))
          (is (empty? (memory-events seen :saved))))))))

(deftest a-failed-memory-save-emits-a-warn-event-with-file-and-error-and-throws
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen dir]} (setup)
              file (path/join dir "memory.edn")]
          (fs/rmSync file #js {:force true})
          (fs/mkdirSync (path/join file "blocker") #js {:recursive true}) ; a rename onto it fails
          (is (thrown? js/Error (core/save-memory! eng true)))
          (let [[e & more] (memory-events seen :save-failed)]
            (is (empty? more))
            (is (= "memory.edn" (:file e)))
            (is (string? (:error e)))))))))

(defn ^:async durable-round
  [c]
  (ctx/remember! c :scaffold {:cell [1 2 3]} {:cap 1 :ttl :forever :durable? true})
  (await (ctx/act c :moveTo #js {:pos #js {:x 5 :y 64 :z 0}}))
  (done c))

(deftest a-durable-memory-write-reaches-disk-before-the-next-act-despite-the-save-gap
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p dir]} (setup)
              eng (assoc-in eng [:jobs 'durable] (tu/job durable-round))
              release (.hold (.-world p) "moveTo")]
          (core/submit! eng '(durable) {})
          (let [r (core/tick! eng)]
            (await (in-act (.-world p) "moveTo"))
            (is (= {:cell [1 2 3]} (get-in (memory-on-disk dir) [:entries :scaffold 0 :data])))
            (release)
            (await r)))))))

(deftest acts-within-the-save-gap-skip-the-memory-write-and-housekeeping-flushes-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock dir]} (setup)
              eng (assoc-in eng [:jobs 'intent] (tu/job intent-round))
              release (.hold (.-world p) "moveTo")
              on-disk #(get-in (memory-on-disk dir) [:entries :job/j1 0 :data :intent])]
          (core/submit! eng '(intent) {})
          (let [r (core/tick! eng)]
            (await (in-act (.-world p) "moveTo"))
            (is (nil? (on-disk)) "the act's save is skipped within the gap")
            (swap! clock + 3000)
            (core/tick! eng)
            (is (= :walk-east (on-disk)) "housekeeping flushes the skipped save")
            (release)
            (await r)))))))

(deftest a-save-stats-summary-is-emitted-once-a-minute
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (setup)
              stats #(filterv (fn [e] (= [:memory :save-stats] [(:source e) (:kind e)])) @seen)]
          (core/submit! eng '(count) {})
          (await (core/tick! eng))
          (is (empty? (stats)) "not before a minute is up")
          (swap! clock + 60000)
          (core/tick! eng)
          (let [[e & more] (stats)]
            (is (empty? more))
            (is (pos? (:count e)))
            (is (pos? (:bytes e)))
            (is (>= (:ms e) (:max-ms e) 0))))))))

(defn wait-calls [world]
  (filterv #(= "wait" (.-name %)) (.-calls world)))

(deftest a-pause-parks-its-round-so-ticks-do-not-spin
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (setup {})
              world (.-world p)]
          (core/submit! eng '(repeat (jobs.time.noop {:seconds 2 :why :idle})) {})
          (.hold world "wait")
          (core/tick! eng)
          (loop [i 0]
            (when (and (< i 1000) (empty? (wait-calls world)))
              (await (js/Promise. (fn [resolve] (js/setTimeout resolve 1))))
              (recur (inc i))))
          (dotimes [_ 10]
            (core/tick! eng)
            (await (js/Promise. (fn [resolve] (js/setTimeout resolve 5))))
            (swap! clock + 250))
          (is (= 1 (count (wait-calls world))) "the round is parked in its wait, so ticks start nothing")
          (is (= [2000] (mapv #(.. % -args -ms) (wait-calls world)))
              "it waits the pause's seconds"))))))

(deftest a-cut-rejects-a-pause-s-wait-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup {:inventory [{:name "bread" :count 3}]})
              world (.-world p)]
          (core/register-reflex! eng {:trigger :hurt})
          (core/submit! eng '(repeat (jobs.time.noop {:seconds 2 :why :idle})) {})
          (.hold world "wait")
          (let [looking (core/tick! eng)]
            (swap! (.. world -state) assoc-in [:self :health] 6)
            (let [reflex-round (core/tick! eng)]
              (await looking)
              (await reflex-round)))
          (is (= [:fired :cut] (->> @seen (map :kind) (filter #{:fired :cut})))))))))

;; ---------------------------------------------------------------- reflex events

(defn reflex-events [seen kind]
  (filterv #(= [:reflex kind] [(:source %) (:kind %)]) @seen))

(def hostile-world
  {:entities [{:id 1 :name "zombie" :kind "hostile" :pos {:x 3 :y 64 :z 0}}]})

(deftest reflex-fired-and-ended-name-the-reflex-and-its-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:health 5}})]
          (core/register-reflex! eng {:trigger :hurt})
          (await (core/tick! eng))
          (is (= ["hurt → eat"] (mapv :text (reflex-events seen :fired))))
          (let [[e] (reflex-events seen :ended)]
            (is (= "hurt → eat: done" (:text e)))
            (is (= :done (:outcome e)))))))))

(deftest every-reflex-exit-emits-exactly-one-ended-with-its-outcome
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [outcomes (fn [seen] (mapv :outcome (reflex-events seen :ended)))]
          (testing "failed"
            (let [{:keys [eng seen]} (setup)]
              (core/register-reflex! eng {:trigger :boom})
              (await (core/tick! eng))
              (is (= [:failed] (outcomes seen)))))
          (testing "cut by a primitive"
            (let [{:keys [eng seen p]} (setup hostile-world)]
              (.override (.-world p) "moveTo" (fn [_ _ _] (js/Promise.reject (core/cut-error))))
              (core/register-reflex! eng {:trigger :near})
              (await (core/tick! eng))
              (is (= [:cut] (outcomes seen)))))
          (testing "dropped by a higher reflex"
            (let [{:keys [eng seen p]} (setup {:self {:food 5} :entities (:entities hostile-world)})
                  world (.-world p)]
              (core/register-reflex! eng {:trigger :near})
              (core/register-reflex! eng {:trigger :hungry})
              (.hold world "moveTo")
              (let [release-eat (.hold world "eat")
                    r1 (core/tick! eng)
                    _ (await (in-act (.-world p) "moveTo"))]
                (core/move! eng :hungry {:above :near} 60)
                (let [r2 (core/tick! eng)]
                  (await r1)
                  (release-eat)
                  (await r2)
                  (is (= [:dropped :done] (outcomes seen)))
                  (is (= 2 (count (reflex-events seen :fired))))))))
          (testing "shutdown mid-round"
            (let [{:keys [eng seen p]} (setup hostile-world)]
              (core/register-reflex! eng {:trigger :near})
              (.hold (.-world p) "moveTo")
              (let [round (core/tick! eng)]
                (await (in-act (.-world p) "moveTo"))
                (core/shutdown! eng)
                (await round)
                (is (= [:dropped] (outcomes seen)))
                (is (= {} (:instances (core/state eng)))))))
          (testing "death mid-round"
            (let [{:keys [eng seen p]} (setup hostile-world)]
              (core/register-reflex! eng {:trigger :near})
              (.hold (.-world p) "moveTo")
              (let [round (core/tick! eng)]
                (await (in-act (.-world p) "moveTo"))
                (core/record-body-event! eng #js {:kind "died"})
                (await round)
                (is (= [:dropped] (outcomes seen)))
                (is (= {} (:instances (core/state eng)))))))
          (testing "a crash left a reflex job behind"
            (let [dir (tu/tmp-dir)
                  {:keys [eng p]} (setup hostile-world dir)]
              (core/register-reflex! eng {:trigger :near})
              (.hold (.-world p) "moveTo")
              (core/tick! eng)
              (await (in-act (.-world p) "moveTo"))
              (let [[_ seen2] (restore-with dir registry triggers)]
                (is (= [:dropped] (outcomes seen2)))))))))))

(deftest world-not-loaded-is-a-body-event
  (let [{:keys [p seen]} (setup)]
    (.emit (.-world p) #js {:kind "world-not-loaded" :ms 10000})
    (is (= [:world-not-loaded]
           (->> @seen (filter #(= :body (:source %))) (mapv :kind))))))


(deftest physics-stalled-is-a-body-event
  (let [{:keys [p seen]} (setup)]
    (.emit (.-world p) #js {:kind "physics-stalled" :pos #js {:x 0 :y 64 :z 0} :ms 2100})
    (is (= [:physics-stalled]
           (->> @seen (filter #(= :body (:source %))) (mapv :kind))))))

(deftest memory-view-reads-the-world-age-without-a-self-snapshot
  (let [{:keys [eng p]} (setup {:self {:worldAge 740}})
        self-calls (atom 0)
        self-fn (.-self p)]
    (set! (.-self p) (fn [] (swap! self-calls inc) (self-fn)))
    (is (= 740 (:age (mem/view (:store eng)))))
    (is (zero? @self-calls))))

(deftest ended-jobs-leave-a-bounded-result-record-that-is-saved
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng dir]} (setup)
              key :engine.core.base/recent-results-limit]
          (swap! settings/state assoc-in [:values key] 2)
          (try
            (core/submit! eng '(walk {:pos {:x 3 :y 64 :z 0}}) {})
            (await (core/tick! eng))
            (let [rec (core/recent-results eng)]
              (is (= :completed (get-in rec ["j1" :status])))
              (is (= "walk" (get-in rec ["j1" :name])))
              (is (number? (get-in rec ["j1" :ended-at]))))
            (core/submit! eng '(count) {})
            (core/submit! eng '(count) {})
            (core/submit! eng '(count) {})
            (core/cancel! eng "j2")
            (core/cancel! eng "j3")
            (core/cancel! eng "j4")
            (let [rec (core/recent-results eng)]
              (is (= #{"j3" "j4"} (set (keys rec))) "bounded to the setting, oldest dropped")
              (is (= :cancelled (get-in rec ["j4" :status])))
              (is (= "count" (get-in rec ["j4" :name])) "a cancelled job keeps its name"))
            (is (contains? (reader/read-string (.readFileSync fs (path/join dir "results.edn") "utf8")) "j4")
                "saved in its own file")
            (is (not (contains? (core/state eng) :recent-results)) "not in the state (snapshot, engine.edn)")
            (is (not (contains? (reader/read-string (.readFileSync fs (path/join dir "engine.edn") "utf8")) :recent-results)))
            (finally (swap! settings/state update :values dissoc key))))))))

(deftest ended-job-records-survive-a-restart
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng dir]} (setup)]
          (core/submit! eng '(walk {:pos {:x 3 :y 64 :z 0}}) {})
          (await (core/tick! eng))
          (let [[again _] (restore-with dir registry triggers)]
            (is (= :completed (get-in (core/recent-results again) ["j1" :status]))
                "results.edn is read back when the engine restarts")))))))

(deftest restore-drops-a-legacy-recent-results-key
  (is (not (contains? (core/restore {:list [] :recent-results {"j1" {:status :completed}}}) :recent-results))))

(deftest reflex-ends-are-kept-apart-from-listed-jobs-and-carry-the-job-name
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:self {:health 5}})
              key :engine.core.base/reflex-results-limit]
          (swap! settings/state assoc-in [:values key] 2)
          (try
            (core/submit! eng '(count) {})
            (core/cancel! eng "j1")
            (core/register-reflex! eng {:trigger :hurt :job '(give-up)})
            (dotimes [_ 5] (await (core/tick! eng)))
            (let [rec (core/recent-results eng)
                  reflexes (filter (comp :reflex? val) rec)]
              (is (= "count" (get-in rec ["j1" :name])) "the listed job survives a reflex flood")
              (is (= 2 (count reflexes)) "reflex ends are capped on their own")
              (is (every? #(= "give-up" (:name (val %))) reflexes) "a stopped reflex has its name"))
            (finally (swap! settings/state update :values dissoc key))))))))

(deftest recent-results-keep-the-newest-for-ends-in-one-millisecond
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup)
              key :engine.core.base/recent-results-limit]
          (swap! settings/state assoc-in [:values key] 10)
          (try
            (dotimes [_ 12] (core/cancel! eng (core/submit! eng '(count) {})))
            (is (= (set (map #(str "j" %) (range 3 13))) (set (keys (core/recent-results eng))))
                "the newest ten, though every end shares one millisecond")
            (finally (swap! settings/state update :values dissoc key))))))))

(deftest dropping-a-root-forgets-its-last-done-status
  (let [{:keys [eng]} (idle-setup {})
        id (core/submit! eng '(count) {})]
    (swap! (:seen-done eng) assoc (mem/path->id id []) [:ok nil {}])
    (base/drop-instance! eng id :ended)
    (is (= {} @(:seen-done eng)))))

(deftest stats-interval-comes-from-the-setting-unless-given
  (let [mk (fn [opts] (core/create (merge {:primitives (tu/fake {}) :jobs registry :triggers triggers :dir (tu/tmp-dir)} opts)))]
    (is (= 60000 (:stats-ms (mk {}))))
    (is (= 2000 (:stats-ms (settings/with-settings {:engine.core.base/stats-ms 2000} #(mk {})))))
    (is (= 500 (:stats-ms (settings/with-settings {:engine.core.base/stats-ms 2000} #(mk {:stats-ms 500})))))))

(def silent-registry
  (merge expr-registry
         {'looky (tu/job [(fn [_] (contract/look :exposed "light not seen" :look-at [[1 2 3]]))] count-round)
          'idle (tu/job (fn ^:async idle-round [_] :continue))}))

(deftest repeat-over-a-child-that-needs-a-look-calls-it-and-rests-with-a-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (update (expr-setup) :eng assoc :jobs silent-registry)]
          (core/submit! eng '(repeat (looky)) {})
          (dotimes [_ 12] (swap! clock + 3000) (await (core/tick! eng)))
          (is (= 2 (count (ran seen))) "a round at the start and one after the 30 s rest, not one per tick")
          (is (= :resting (:reason (core/waiting eng "j1")))))))))

(deftest a-job-with-silent-rounds-for-minutes-raises-job-silent-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (update (expr-setup) :eng assoc :jobs silent-registry)
              silent #(filter (fn [e] (= :silent (:kind e))) @seen)]
          (core/submit! eng '(idle) {})
          (dotimes [_ 10] (swap! clock + 10000) (await (core/tick! eng)))
          (is (empty? (silent)) "under three minutes")
          (dotimes [_ 10] (swap! clock + 10000) (await (core/tick! eng)))
          (is (= 1 (count (silent))) "once per spell")
          (is (= 1 (count (:attention (core/state eng))))))))))

(deftest an-act-in-flight-past-the-limit-is-aborted-and-names-the-act
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (update (setup {}) :eng assoc :jobs
                                           (assoc registry 'hang (tu/job (fn ^:async hang-round [c] (await (ctx/act c :wait #js {:ms 10})) (done c)))))]
          (aset p "wait" (fn [& _] (js/Promise. (fn [_ _]))))
          (core/submit! eng '(hang) {})
          (await (settings/with-settings {:engine.core.round/act-max-s 1} #(core/tick! eng)))
          (is (some #(and (= :stuck (:kind %)) (= "wait" (:name %))) @seen) "action.stuck names the act")
          (is (re-find #"act-stuck: wait" (str (get-in (core/state eng) [:failed "j1" :error]))) "the job is parked failed")
          (is (nil? (core/holder eng)) "the body is free"))))))
