(ns engine.core-test
  (:require [cljs.test :refer [deftest is are async testing]]
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
            [engine.triggers :as triggers]
            ["fs" :as fs]
            ["path" :as path]))

;; ---------------------------------------------------------------- test jobs

(defn ^:async count-round [c]
  (ctx/update-mem! c update :n (fnil inc 0))
  :continue)

(defn ^:async walk-round [c]
  (let [r (await (ctx/act c :moveTo #js {:pos (clj->js (:pos (:args c)))}))]
    (ctx/update-mem! c assoc :walked (.-status r))
    :done))

(defn ^:async parent-walk-round [c]
  (ctx/update-mem! c assoc :before true)
  (await (ctx/act c :moveTo #js {:pos #js {:x 5 :y 64 :z 0}}))
  :done)

(defn ^:async eat-round [c]
  (await (ctx/act c :eat #js {}))
  :done)

(defn ^:async swim-round [c]
  (let [r (await (ctx/act c :swim #js {:ms 3000}))]
    (ctx/update-mem! c assoc :swim [(.-status r) (.. r -oxygen -after)])
    :done))

(defn ^:async fail-round [_]
  (throw (js/Error. "nope")))

(defn ^:async attention-round [c]
  (ctx/request-attention! c :storage_blocked :chest_full {:item "oak_log" :count 24} "Chest is full")
  :continue)

(defn ^:async attention-done-round [c]
  (ctx/request-attention! c :storage_blocked :chest_full {:item "oak_log" :count 24} "Chest is full")
  :done)

(defn ^:async write-fail-round [c]
  (ctx/update-mem! c assoc :x 1)
  (throw (js/Error. "nope")))

(defn ^:async bad-result-round [_]
  :not-ready)

(defn ^:async child-round [c]
  (let [n (:n (ctx/mem c) 0)]
    (ctx/update-mem! c assoc :n (inc n))
    (if (>= (inc n) (:rounds (:args c) 2)) :done :continue)))

(def always (constantly true))

(def child-job {:name :child :check always :round child-round})

(defn ^:async parent-round [c]
  (let [a (await (ctx/call-child c :a child-job {:rounds 2}))]
    (ctx/update-mem! c update :seen (fnil conj []) a)
    (if (= a :done) :done :continue)))

(defn ^:async submit-round [c]
  (ctx/submit! c '(count) {})
  :done)

(def flag (atom false))

(def gated-job {:name :gated :round count-round :check (fn [_] @flag)})

(defn ^:async declined-parent-round [c]
  (let [r (await (ctx/call-child c :a gated-job {}))]
    (ctx/update-mem! c assoc :child r)
    :continue))

(defn ^:async recurse-round
  "Calls itself as a child in slot :deeper until :depth levels exist."
  [c]
  (let [depth (:depth (:args c))]
    (ctx/update-mem! c assoc :at depth)
    (if (zero? depth)
      :done
      (await (ctx/call-child c :deeper {:name :recurse :check always :round recurse-round}
                             {:depth (dec depth)})))))

(defn ^:async result-child-round
  "Hands {:n n} over every round; done on the second."
  [c]
  (let [n (inc (:n (ctx/mem c) 0))]
    (ctx/update-mem! c assoc :n n)
    (ctx/result! c {:n n})
    (if (>= n 2) :done :continue)))

(def result-child {:name :result-child :check always :round result-child-round})

(defn ^:async declining-round-child [c]
  (let [n (inc (:n (ctx/mem c) 0))]
    (ctx/update-mem! c assoc :n n)
    (ctx/result! c {:n n})
    (if (= n 1) :declined :done)))

(def declining-round-job {:name :declining-round-child :check always :round declining-round-child})

(def registry
  {'count {:check always :round count-round}
   'walk {:check always :round walk-round}
   'parent-walk {:check always :round parent-walk-round}
   'eat {:check always :round eat-round}
   'fail {:check always :round fail-round}
   'attention {:check always :round attention-round}
   'attention-done {:check always :round attention-done-round}
   'write-fail {:check always :round write-fail-round}
   'bad-result {:check always :round bad-result-round}
   'child child-job
   'recurse {:check always :round recurse-round}
   'parent {:check always :round parent-round}
   'declined-parent {:check always :round declined-parent-round}
   'submitter {:check always :round submit-round}
   'jobs.movement.look-around (get registry/jobs 'jobs.movement.look-around)
   'gated gated-job
   'any-gated {:check always :round (fn ^:async any-gated-round [c]
                                      (let [r (await (ctx/call-child c :g gated-job {}))]
                                        (if (= :done r) :done :continue)))}
   'no-check {:round count-round}})

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
  "A job's memory without the engine's :args and :children keys."
  ([eng id] (job-mem eng id []))
  ([eng id slots] (dissoc (mem/job-mem (mem/view (:store eng)) id slots) :args :children)))

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

(deftest the-check-sees-the-job-memory-and-args
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [seen-ctx (atom nil)
              {:keys [eng]} (setup)
              eng (assoc-in eng [:jobs 'peek]
                            {:round count-round
                             :check (fn [c] (reset! seen-ctx [(:args c) (dissoc (ctx/mem c) :args :children)]) true)})]
          (core/submit! eng (list 'peek {:a 1}) {})
          (await (core/tick! eng))
          (core/tick! eng)
          (is (= [{:a 1} {:n 1}] @seen-ctx)))))))

(deftest a-job-without-a-check-is-refused
  (let [{:keys [eng]} (setup)]
    (is (thrown? js/Error (core/submit! eng '(no-check) {})))))

(deftest a-round-returning-anything-else-fails-the-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (core/submit! eng '(bad-result) {})
          (await (core/tick! eng))
          (is (= ["j1"] (listed eng)))
          (is (some #(= :failed (:kind %)) @seen)))))))

(deftest a-holding-job-whose-check-declines-idles-the-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (reset! flag false)
          (core/submit! eng '(count) {})
          (core/submit! eng '(gated) {:hold? true})
          (is (nil? (core/tick! eng)) "the holder declines; the others wait")
          (reset! flag true)
          (dotimes [_ 2] (await (core/tick! eng)))
          (is (= ["j2" "j2"] (ran seen))))))))

(deftest a-declining-child-returns-declined
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup)]
          (reset! flag false)
          (core/submit! eng '(declined-parent) {})
          (await (core/tick! eng))
          (is (= {:child :declined} (job-mem eng "j1"))))))))

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

(deftest a-holding-job-is-always-chosen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (core/submit! eng '(count) {})
          (core/submit! eng '(count) {:hold? true})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= ["j2" "j2" "j2"] (ran seen))))))))

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

(deftest a-round-returning-junk-marks-the-job-failed-too
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup)]
          (core/submit! eng '(bad-result) {})
          (await (core/tick! eng))
          (is (= ["j1"] (listed eng)))
          (is (contains? (:failed (core/state eng)) "j1")))))))

(deftest a-failed-holding-job-does-not-hold-the-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (core/submit! eng '(fail) {:hold? true})
          (core/submit! eng '(count) {})
          (await (core/tick! eng))
          (dotimes [_ 2] (await (core/tick! eng)))
          (is (= ["j1" "j2" "j2"] (ran seen))))))))

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
          (is (= :continue (await (ctx/call-child c :x child-job {:rounds 2}))))
          (is (= :done (await (ctx/call-child c :x child-job {:rounds 2}))))
          (is (= {} (job-mem eng "j9" [:x])) "a done child's memory is cleared")
          (is (= :continue (await (ctx/call-child c :x child-job {:rounds 2}))) "so the next call starts fresh")
          (is (= :continue (await (ctx/call-child c :y child-job {:rounds 2}))))
          (is (= {:n 1} (job-mem eng "j9" [:y]))))))))

(deftest a-declining-child-keeps-its-memory-so-debts-survive
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup)
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})]
          (.setOwner (:primitives eng) "tx")
          (reset! flag true)
          (is (= :continue (await (ctx/call-child c :x gated-job {}))))
          (is (= {:n 1} (job-mem eng "j9" [:x])) "a continuing child keeps its memory")
          (reset! flag false)
          (is (= :declined (await (ctx/call-child c :x gated-job {}))))
          (is (= {:n 1} (job-mem eng "j9" [:x])) "a declined child keeps its memory")
          (reset! flag true)
          (is (= :continue (await (ctx/call-child c :x gated-job {}))))
          (is (= {:n 2} (job-mem eng "j9" [:x])) "and resumes from it"))))))

(deftest a-round-declining-child-keeps-its-memory-and-resumes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup)
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})]
          (.setOwner (:primitives eng) "tx")
          (is (= :declined (await (ctx/call-child c :x declining-round-job {}))))
          (is (= {:n 1} (job-mem eng "j9" [:x])) "a declined round preserves child memory")
          (is (nil? (ctx/child-result c :x)) "a declined round does not hand over its result")
          (is (= :done (await (ctx/call-child c :x declining-round-job {}))) "the same slot resumes")
          (is (= {:n 2} (ctx/child-result c :x)) "only the done round hands over its result")
          (is (= {} (job-mem eng "j9" [:x])) "done clears the resumed child's memory"))))))

(def stopping-child
  {:name :stopping-child :check always
   :round (fn [c] (ctx/result! c {:status :stopped :reason :no-way}) :done)})

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
          (await (ctx/call-child c :x child-job {:rounds 2}))
          (reset! flag false)
          (await (ctx/call-child c :g {:name :waits :round count-round
                                       :check (fn [c] (ctx/wait c :no-seeds))} {}))
          (await (ctx/call-child c :s stopping-child {}))
          (let [chain-x ["j9" "j9/x"]]
            (is (= [{:kind :child_started :job "j9/x" :slot :x :chain chain-x}
                    {:kind :child_ended :job "j9/x" :slot :x :chain chain-x :status :continue}
                    {:kind :child_started :job "j9/g" :slot :g :chain ["j9" "j9/g"]}
                    {:kind :child_ended :job "j9/g" :slot :g :chain ["j9" "j9/g"] :status :declined
                     :reason :no-seeds}
                    {:kind :child_started :job "j9/s" :slot :s :chain ["j9" "j9/s"]}
                    {:kind :child_ended :job "j9/s" :slot :s :chain ["j9" "j9/s"] :status :stopped
                     :reason :no-way}]
                   (child-events seen)))))))))

(deftest a-child-returning-a-bad-result-emits-child-ended-error-and-throws
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})
              failing {:name :failing :check always :round (fn [_] :bogus)}]
          (.setOwner (:primitives eng) "tx")
          (is (thrown-with-msg? js/Error #"not :done, :continue or :declined" (await (ctx/call-child c :f failing {}))))
          (is (= [{:kind :child_started :job "j9/f" :slot :f :chain ["j9" "j9/f"]}
                  {:kind :child_ended :job "j9/f" :slot :f :chain ["j9" "j9/f"] :status :error}]
                 (child-events seen))))))))

(deftest a-done-child-hands-its-result-to-the-parent-for-the-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup)
              base {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1}
              c (core/make-ctx eng base)]
          (.setOwner (:primitives eng) "tx")
          (is (nil? (ctx/child-result c :x)) "nothing before the call")
          (is (= :continue (await (ctx/call-child c :x result-child {}))))
          (is (nil? (ctx/child-result c :x)) "a continuing child's result is not handed over")
          (is (= :done (await (ctx/call-child c :x result-child {}))))
          (is (= {:n 2} (ctx/child-result c :x)))
          (is (nil? (ctx/child-result c :y)) "per slot")
          (is (nil? (ctx/child-result (core/make-ctx eng (assoc base :round 2)) :x))
              "gone once the parent's round ends"))))))

(deftest a-child-result-reaches-a-parent-through-the-engine
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup)
              seen (atom [])
              parent {:check always
                      :round (fn ^:async result-parent-round [c]
                               (let [r (await (ctx/call-child c :x result-child {}))]
                                 (swap! seen conj [r (ctx/child-result c :x)])
                                 r))}]
          (let [eng (assoc eng :jobs (assoc registry 'result-parent parent))]
            (core/submit! eng '(result-parent) {})
            (dotimes [_ 2] (await (core/tick! eng)))
            (is (= [[:continue nil] [:done {:n 2}]] @seen))
            (is (= [] (listed eng)))))))))

(deftest a-check-cannot-hand-over-a-result
  (let [{:keys [eng]} (setup)
        c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token nil :args {} :round 1})]
    (is (core/cut? (try (ctx/result! c {:x 1}) (catch :default e e))))))

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
          (is (= :cut (try (await (ctx/call-child c :x child-job {}))
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
            (core/register-reflex! eng {:trigger :interval})
            (let [reflex-round (core/tick! eng)]
              (await walking)
              (await reflex-round)))
          (is (= [:fired :cut] (->> @seen (map :kind) (filter #{:fired :cut}))))
          (is (= ["look"] (mapv #(.-name %) (filter #(= "look" (.-name %)) (.-calls world)))))
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
                r1 (core/tick! eng)]
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

(deftest built-in-reflexes-cannot-be-removed
  (let [{:keys [eng]} (setup)]
    (core/register-reflex! eng {:trigger :hurt :builtin? true})
    (core/register-reflex! eng {:trigger :never})
    (is (false? (core/remove-reflex! eng :hurt)))
    (is (true? (core/remove-reflex! eng :never)))
    (is (= [:hurt] (mapv :id (:register (core/state eng)))))))

(deftest do-now-cuts-and-holds-at-the-front-and-cancel-removes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup)]
          (core/submit! eng (list 'walk {:pos {:x 5 :y 64 :z 0}}) {})
          (.hold (.-world p) "moveTo")
          (let [walking (core/tick! eng)]
            (core/do-now! eng '(count))
            (await walking))
          (is (= ["j2" "j1"] (listed eng)))
          (await (core/tick! eng))
          (await (core/tick! eng))
          (core/cancel! eng "j2")
          (await (core/tick! eng))
          (is (= ["j1" "j2" "j2" "j1"] (ran seen)))
          (is (some #(= :cancelled (:kind %)) @seen)))))))

(deftest an-interrupted-job-continues-right-after-the-jobs-done-now-before-it
  ;; X and A take turns; A is cut by B, B by C. Each job done now goes directly before the one it cut, so once C and
  ;; B are done the next round is A's again, not the round-robin's next pick (X).
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)
              x (core/submit! eng '(count) {})
              a (core/submit! eng '(count) {})
              _ (await (core/tick! eng))
              a-round (core/tick! eng)
              b (core/do-now! eng '(child {:rounds 2}))
              _ (await a-round)
              b-round (core/tick! eng)
              c (core/do-now! eng '(child {:rounds 2}))]
          (await b-round)
          (is (= [x c b a] (listed eng)) "each job done now sits directly before the job it cut")
          (dotimes [_ 4] (await (core/tick! eng)))
          (is (= [x a b c c b a] (ran seen)) "C, then B, then the job cut first continues")
          (is (= [x a] (listed eng)) "B and C are done")
          (is (= {:n 2} (job-mem eng a)) "with its memory"))))))

(deftest a-job-done-now-between-rounds-goes-where-the-next-scan-starts
  (are [state expected] (= expected (:list (core/insert-now (merge {:list [] :cursor 0} state) "X")))
    {:list ["A" "B" "C"] :current "B"} ["A" "X" "B" "C"]
    {:list ["A" "B" "C"] :resume "C" :cursor 0} ["A" "B" "X" "C"]
    {:list ["A" "B" "C"] :cursor 2} ["A" "B" "X" "C"]
    {:list ["A" "B"] :cursor 9} ["A" "B" "X"]
    {:list [] :cursor 0} ["X"]))

(deftest insert-front-puts-the-job-directly-after-the-current-one
  (are [state expected] (= expected (:list (core/insert-front (merge {:list [] :cursor 0} state) "X")))
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
    (is (= ["A" "B" "C" "Y" "X" "D"] (:list (core/insert-front (core/insert-front s "X") "Y"))))))

(deftest a-front-job-submitted-during-a-round-runs-in-the-very-next-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup)
              release (do (core/submit! eng '(walk {:pos {:x 5 :y 64 :z 0}}) {})
                          (.hold (.-world p) "moveTo"))
              walking (core/tick! eng)
              plain (core/submit! eng '(count) {})
              front (core/submit! eng '(count) {:front? true})]
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
          (let [front (core/submit! eng '(count) {:front? true})]
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
              x (core/submit! eng '(count) {:front? true})
              y (core/submit! eng '(count) {:front? true})]
          (is (= ["j1" y x "j2"] (listed eng)))
          (release)
          (await walking)
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= ["j1" y x "j2"] (ran seen))))))))

(deftest a-front-job-waits-behind-a-held-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (core/submit! eng '(count) {:hold? true})
          (await (core/tick! eng))
          (let [front (core/submit! eng '(count) {:front? true})]
            (is (= ["j1" front] (listed eng)))
            (dotimes [_ 3] (await (core/tick! eng)))
            (is (= ["j1" "j1" "j1" "j1"] (ran seen)) "the held job keeps the body")))))))

(deftest a-front-job-whose-check-is-false-is-skipped-like-any-other
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (reset! flag false)
          (core/submit! eng '(count) {})
          (core/submit! eng '(count) {})
          (await (core/tick! eng))
          (let [gated (core/submit! eng '(gated) {:front? true})]
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
          (let [front (core/submit! eng '(count) {:front? true})
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
          (let [front (core/submit! eng '(count) {:front? true})
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
  :done)

(deftest act-saves-memory-before-and-after-and-emits-debug-events
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen dir]} (setup)
              eng (assoc-in eng [:jobs 'intent] {:check always :round intent-round})
              release (.hold (.-world p) "moveTo")
              release-look (.hold (.-world p) "look")]
          (core/submit! eng '(intent) {})
          (let [r (core/tick! eng)]
            (is (= :walk-east (get-in (memory-on-disk dir) [:entries :job/j1 0 :data :intent]))
                "saved before the primitive")
            (release)
            (await (js/Promise. (fn [ok] (js/setTimeout ok 0))))
            (is (true? (get-in (memory-on-disk dir) [:entries :job/j1 0 :data :walked]))
                "saved before the next primitive")
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
  :done)

(deftest action-done-carries-reason-and-rounded-distance-when-the-result-has-them
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:noPath ["9,64,9"]})
              eng (assoc-in eng [:jobs 'walker] {:check always :round blocked-walk-round})]
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
              eng (assoc-in eng [:jobs 'swimmer] {:check always :round swim-round})]
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

(def gate (atom nil))

(defn ^:async parked-round
  "Awaits @gate's promise with no act (a round doing nothing), then acts once if :act-after, and ends :continue."
  [c]
  (when (:hold (:args c)) (ctx/hold-still! c (:hold (:args c))))
  (await (js/Promise. (fn [resolve] (reset! gate resolve))))
  (when (:act-after (:args c))
    (await (ctx/act c :look #js {:pos #js {:x 0 :y 64 :z 0}}))
    (await (js/Promise. (fn [resolve] (reset! gate resolve)))))
  :continue)

(defn ^:async why-wait-round [c]
  (await (ctx/act c :wait #js {:ms 1000 :why "daylight"}))
  :continue)

(def idle-registry
  (merge registry {'parked {:check always :round parked-round :args {:hold {:default nil} :act-after {:default false}}}
                   'why-wait {:check always :round why-wait-round}}))

(defn idle-setup [opts]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake {})
        eng (core/create (merge {:primitives p :jobs idle-registry :triggers triggers :dir (tu/tmp-dir)
                                 :now #(deref clock)
                                 :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})}
                                opts))]
    {:eng eng :seen seen :clock clock :p p}))

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
        (let [{:keys [eng seen clock]} (idle-setup {})
              _ (core/submit! eng (list 'parked {:act-after true}) {})
              round (core/tick! eng)]
          (swap! clock + 10000)
          (core/tick! eng)
          (is (= [] (idles seen)) "10 s is not more than the default idle-s")
          (swap! clock + 1)
          (core/tick! eng)
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
        (let [{:keys [eng seen clock]} (idle-setup {:idle-s 5})
              _ (core/submit! eng (list 'parked {:hold :night}) {})
              round (core/tick! eng)]
          (is (= [["j1" :night 1000000]] (mapv (juxt :job :reason :since) (holdings seen))))
          (is (= {:reason :night :since 1000000} (core/holding eng "j1")))
          (is (= {:reason :night :since 1000000} (:holding (job-api/summary eng "j1"))) "agents see it in jobs show")
          (swap! clock + 60000)
          (core/tick! eng)
          (is (= [] (idles seen)))
          (@gate nil)
          (await round)
          (is (nil? (core/holding eng "j1")) "a hold ends with its round"))))))

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
    (if (>= n 2) :done :continue)))

(deftest a-reflex-job-runs-its-round-even-when-its-check-declines
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (reset! flag false)
          (core/submit! eng '(count) {})
          (core/register-reflex! eng {:trigger :gated})
          (dotimes [_ 2] (await (core/tick! eng)))
          (is (= ["j2" "j3"] (ran seen)) "the trigger fired the job twice and its check was never asked")
          (is (= {} (job-mem eng "j1")) "the listed job waits"))))))

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

(deftest a-declining-reflex-job-is-dropped-and-refires-only-after-its-cooldown
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [trigger [:gated-any :gated-seq :gated-repeat]]
          (let [{:keys [eng seen clock]} (declined-setup)]
            (reset! flag false)
            (core/register-reflex! eng {:trigger trigger})
            (await (core/tick! eng))
            (is (= ["j1"] (ran seen)) (str trigger " fired"))
            (is (nil? (get-in (core/state eng) [:instances "j1"])) (str trigger " is gone"))
            (is (= 1 (count (filter #(= [:reflex :declined trigger] [(:source %) (:kind %) (:reflex %)]) @seen)))
                "one reflex.declined event")
            (is (nil? (core/tick! eng)) "cooling down: it does not fire again")
            (swap! clock + 9000)
            (is (nil? (core/tick! eng)) "still cooling down")
            (swap! clock + 1000)
            (await (core/tick! eng))
            (is (= ["j1" "j2"] (ran seen)) (str trigger " fires again after the cooldown"))))))))


(def expr-registry
  (merge registry {'twice {:check always :round twice-round}
                   'twice-b {:check (fn [_] @flag-b) :round twice-round}
                   'defaults {:check always :round count-round
                              :args {:a {:doc "a" :default 1} :b {:doc "b" :default 2}}}}))

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

(deftest hold-makes-the-list-entry-hold-the-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (expr-setup)]
          (core/submit! eng '(count) {})
          (core/submit! eng '(hold (repeat (twice))) {})
          (is (true? (get-in (core/state eng) [:instances "j2" :hold?])))
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= ["j2" "j2" "j2"] (ran seen))))))))

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
    (is (thrown-with-msg? js/Error #"hold is not allowed here" (core/register-reflex! eng {:trigger :hurt :job '(hold (eat))})))
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

(def stale-registry
  "The registry after a job dropped its :old arg."
  (assoc registry 'walk {:check always :round walk-round :args {:pos nil}}))

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
          (await (ctx/call-child c :x 'defaults {:b 9}))
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
          (is (thrown? js/Error (core/save-memory! eng)))
          (let [[e & more] (memory-events seen :save-failed)]
            (is (empty? more))
            (is (= "memory.edn" (:file e)))
            (is (string? (:error e)))))))))

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

(defn look-calls [world]
  (filterv #(= "look" (.-name %)) (.-calls world)))

(deftest look-around-looks-once-then-waits-so-ticks-do-not-spin
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (setup {})
              world (.-world p)]
          (core/submit! eng '(repeat (jobs.movement.look-around)) {})
          (.hold world "wait")
          (dotimes [_ 10]
            (core/tick! eng)
            (swap! clock + 250))
          (await (js/Promise. (fn [resolve] (js/setTimeout resolve 20))))
          (is (= 1 (count (look-calls world))) "the round is parked in its wait, so ticks start nothing")
          (is (= [{:ms 2000}] (mapv #(js->clj (.-args %) :keywordize-keys true)
                                    (filter #(= "wait" (.-name %)) (.-calls world))))
              "it waits :every-ms, default 2000"))))))

(deftest a-cut-rejects-look-around-s-wait-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup {:inventory [{:name "bread" :count 3}]})
              world (.-world p)]
          (core/register-reflex! eng {:trigger :hurt})
          (core/submit! eng '(repeat (jobs.movement.look-around)) {})
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
          (testing "declined"
            (let [{:keys [eng seen]} (declined-setup)]
              (reset! flag false)
              (core/register-reflex! eng {:trigger :gated-any})
              (await (core/tick! eng))
              (is (= [:declined] (outcomes seen)))))
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
                    r1 (core/tick! eng)]
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
                (core/shutdown! eng)
                (await round)
                (is (= [:dropped] (outcomes seen)))
                (is (= {} (:instances (core/state eng)))))))
          (testing "a crash left a reflex job behind"
            (let [dir (tu/tmp-dir)
                  {:keys [eng p]} (setup hostile-world dir)]
              (core/register-reflex! eng {:trigger :near})
              (.hold (.-world p) "moveTo")
              (core/tick! eng)
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
