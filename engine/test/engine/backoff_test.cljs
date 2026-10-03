(ns engine.backoff-test
  "Backoff: a job whose rounds all fail at once is not given rounds as fast as
  the scheduler can; see engine.backoff and README.md, Backoff."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.backoff :as backoff]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]))

(def blocked {:act :moveTo :status "blocked" :reason "no path"})

(defn round-of
  "The round tracker after the acts, each [status] or [status reason]."
  [& statuses]
  (reduce (fn [r s] (backoff/note-act r :moveTo s nil nil)) backoff/empty-round statuses))

(deftest failure-statuses-are-exactly-the-listed-ones
  (are [status failed?] (= failed? (backoff/failure? status))
    "blocked" true "failed" true "unreachable" true "cannot" true "timeout" true "gone" true
    "out-of-reach" true "no-item" true "no-support" true "no-headroom" true "occupied" true
    "full" true "disconnected" true "unsupported" true "not-night" true "monsters-near" true
    "arrived" false "partial" false "dug" false "missing" false "placed" false "ok" false
    "hit" false "killed" false "collected" false "sleeping" false "landed" false
    "surfaced" false "done" false nil false))

(deftest a-round-is-fruitless-when-it-acted-and-every-act-failed
  (are [statuses fruitless?] (= fruitless? (backoff/fruitless-round? (apply round-of statuses)))
    [] false
    ["blocked"] true
    ["blocked" "timeout" "failed"] true
    ["blocked" "arrived"] false
    ["arrived" "blocked"] false
    ["partial"] false))

(deftest the-last-failing-act-is-kept-with-its-reason
  (let [r (-> backoff/empty-round
              (backoff/note-act :moveTo "blocked" "no path" nil)
              (backoff/note-act :dig "timeout" nil nil))]
    (is (= {:act :dig :status "timeout" :reason nil} (:last r)))))

(deftest neutral-acts-and-walks-that-moved-the-body-are-not-counted
  (are [act status moved counted failed] (= {:acts counted :failed failed}
                                            (select-keys (backoff/note-act backoff/empty-round act status nil moved)
                                                         [:acts :failed]))
    :look "ok" nil 0 0
    :wait "ok" nil 0 0
    :equip "no-item" nil 0 0
    :moveTo "timeout" 1.5 0 0
    :moveTo "blocked" 1 0 0
    :moveTo "timeout" 0.2 1 1
    :moveTo "blocked" nil 1 1
    :moveTo "arrived" 0 1 0
    :moveTo "arrived" 3 1 0
    :dig "timeout" 5 1 1)
  (is (not (backoff/fruitless-round? (-> backoff/empty-round
                                         (backoff/note-act :look "ok" nil nil)
                                         (backoff/note-act :wait "ok" nil nil))))))

(deftest config-merges-levels-and-false-turns-it-off
  (are [levels expected] (= expected (apply backoff/config levels))
    [] {:after 3 :first-s 1 :max-s 30}
    [nil nil nil] {:after 3 :first-s 1 :max-s 30}
    [{:after 5} nil nil] {:after 5 :first-s 1 :max-s 30}
    [{:after 5} {:max-s 10} {:after 2}] {:after 2 :first-s 1 :max-s 10}
    [nil nil false] false
    [false nil nil] false
    [false nil {:after 2}] {:after 2 :first-s 1 :max-s 30}))

(deftest the-delay-starts-at-first-doubles-and-is-capped
  (let [cfg (backoff/config)
        step (fn [e] (backoff/fruitless e cfg 0 blocked))
        entries (take 10 (iterate step nil))]
    (is (= [1 2] (mapv :fruitless (take 2 (rest entries)))) "counting before the threshold")
    (is (= [nil nil nil 1000 2000 4000 8000 16000 30000 30000] (mapv :delay-ms entries)))))

(deftest backing-off-lasts-until-the-delay-has-passed
  (let [e (backoff/fruitless {:fruitless 2} (backoff/config) 5000 blocked)]
    (are [t backing?] (= backing? (backoff/backing-off? e t))
      5000 true 5999 true 6000 false)
    (is (false? (backoff/backing-off? nil 0)))))

(deftest alerts-are-due-at-start-and-then-every-alert-ms
  (are [entry t due?] (= due? (backoff/alert-due? entry t 300000))
    {:fruitless 2} 0 false
    {:since 0 :until 1000} 0 true
    {:since 0 :alerted 0} 299999 false
    {:since 0 :alerted 0} 300000 true))

(deftest validate-accepts-maps-of-positive-numbers-and-false
  (are [cfg] (nil? (backoff/validate! cfg))
    nil false {} {:after 1} {:after 3 :first-s 0.5 :max-s 60})
  (are [cfg] (thrown? js/Error (backoff/validate! cfg))
    true {:after 0} {:after "3"} {:bogus 1} {:max-s -1} 5))

;; ---------------------------------------------------------------- through the engine

(def t0 1000000)

(def status (atom "blocked"))

(defn ^:async script-round
  "One moveTo per entry of :statuses (the fake answers with that status), then
  :end (:continue, :done or :throw)."
  [c]
  (doseq [s (:statuses (:args c))]
    (await (ctx/act c :moveTo #js {:pos #js {:x 0 :y 64 :z 0} :status s})))
  (when (= :throw (:end (:args c))) (throw (js/Error. "boom")))
  (:end (:args c) :continue))

(defn ^:async bump-round
  "One moveTo, answered with whatever status holds."
  [c]
  (await (ctx/act c :moveTo #js {:pos #js {:x 0 :y 64 :z 0} :status @status}))
  :continue)

(defn ^:async bump-once-round [c]
  (await (ctx/act c :moveTo #js {:pos #js {:x 0 :y 64 :z 0} :status @status}))
  :done)

(defn ^:async bump-decline-round [c]
  (await (ctx/act c :moveTo #js {:pos #js {:x 0 :y 64 :z 0} :status @status}))
  :declined)

(def plan (atom []))

(defn ^:async planned-round
  "The acts of @plan, each [act status moved]: moveTo answers status and moves the body x by moved."
  [c]
  (doseq [[a s m] @plan]
    (await (ctx/act c a #js {:pos #js {:x 0 :y 64 :z 0} :status s :move m})))
  :continue)

(defn ^:async idle-round [c]
  (ctx/update-mem! c update :n (fnil inc 0))
  :continue)

(def always (constantly true))

(def jobs
  (merge registry/jobs
         {'script {:check always :round script-round :args {:statuses {:default []} :end {:default :continue}}}
          'bump {:check always :round bump-round}
          'bump-once {:check always :round bump-once-round}
          'bump-decline {:check always :round bump-decline-round}
          'idle {:check always :round idle-round}
          'quick {:check always :round (fn [_] :done)}
          'planned {:check always :round planned-round}
          'bump-fast {:check always :round bump-round :backoff {:after 1}}
          'bump-never {:check always :round bump-round :backoff false}}))

(def triggers
  {:always {:name :always :job '(bump-once) :persistence :retry :when always}
   :declining {:name :declining :job '(bump-decline) :persistence :retry :when always}
   :holding {:name :holding :job '(bump) :persistence :retry :when always}
   :low {:name :low :job '(quick) :persistence :stop :when always}})

(defn setup
  ([] (setup {}))
  ([opts]
   (let [clock (atom t0)
         [seen sink] (tu/capture-sink)
         p (tu/fake {})
         eng (core/create (merge {:primitives p :jobs jobs :triggers triggers :dir (tu/tmp-dir)
                                  :now #(deref clock)
                                  :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})}
                                 opts))]
     (reset! status "blocked")
     (reset! plan [])
     (.override (.-world p) "moveTo"
                (fn [_ args _]
                  (when-let [m (.-move args)]
                    (let [pos (.-pos (.-self (.-state (.-world p))))]
                      (set! (.-x pos) (+ (.-x pos) m))))
                  (js/Promise. (fn [resolve] (js/setTimeout #(resolve #js {:status (.-status args) :reason "no path"}) 0)))))
     {:eng eng :p p :seen seen :clock clock})))

(defn ran [seen] (->> @seen (filter #(= :round_started (:kind %))) (mapv :job)))
(defn of-kind [seen kind] (filterv #(= kind (:kind %)) @seen))
(defn entry [eng k] (core/backoff-entry eng k))

(defn ^:async tick-at [{:keys [eng clock]} t]
  (reset! clock t)
  (await (core/tick! eng)))

(defn ^:async fruitless-after
  "The fruitless count of j1 after one round of (script ...)."
  [statuses end]
  (let [{:keys [eng] :as r} (setup)]
    (core/submit! eng (list 'script {:statuses statuses :end end}) {})
    (await (tick-at r t0))
    (:fruitless (entry eng "j1") 0)))

(deftest only-rounds-whose-acts-all-failed-count
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[statuses end expected] [[[] :continue 0]
                                         [["blocked"] :continue 1]
                                         [["blocked" "timeout"] :continue 1]
                                         [["blocked" "arrived"] :continue 0]
                                         [["arrived" "blocked"] :continue 0]
                                         [["blocked"] :throw 0]]]
          (is (= expected (await (fruitless-after statuses end))) (str statuses end)))))))

(deftest backoff-starts-after-three-and-the-delay-doubles-up-to-the-cap
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng] :as r} (setup)
              delays (atom [])]
          (core/submit! eng '(bump) {})
          (await (tick-at r t0))
          (await (tick-at r t0))
          (is (nil? (:until (entry eng "j1"))) "two fruitless rounds: no backoff yet")
          (loop [t t0 i 0]
            (await (tick-at r t))
            (let [d (:delay-ms (entry eng "j1"))]
              (swap! delays conj d)
              (when (< i 6) (recur (+ t d) (inc i)))))
          (is (= [1000 2000 4000 8000 16000 30000 30000] @delays)))))))

(deftest a-backing-off-job-gets-no-round-and-the-others-do
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup)]
          (core/submit! eng '(bump) {})
          (core/submit! eng '(idle) {})
          (dotimes [_ 6] (await (tick-at r t0)))
          (is (= ["j1" "j2" "j1" "j2" "j1" "j2"] (ran seen)))
          (dotimes [_ 3] (await (tick-at r (+ t0 999))))
          (is (= ["j2" "j2" "j2"] (drop 6 (ran seen))) "j1 is passed over, never removed")
          (is (= ["j1" "j2"] (:list (core/state eng))))
          (await (tick-at r (+ t0 1000)))
          (is (= "j1" (last (ran seen))) "its delay has passed")
          (is (= 2000 (:delay-ms (entry eng "j1")))))))))

(deftest a-holding-job-in-backoff-leaves-the-body-idle
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup)]
          (core/submit! eng '(bump) {:hold? true})
          (core/submit! eng '(idle) {})
          (dotimes [_ 6] (await (tick-at r t0)))
          (is (= ["j1" "j1" "j1"] (ran seen)) "the holder is passed over like a declining one: the body idles"))))))

(deftest a-progress-act-resets-and-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup)]
          (core/submit! eng '(bump) {})
          (dotimes [_ 3] (await (tick-at r t0)))
          (is (some? (entry eng "j1")))
          (reset! status "arrived")
          (await (tick-at r (+ t0 1000)))
          (is (nil? (entry eng "j1")))
          (is (= [[:job :info "j1"]]
                 (mapv (juxt :source :level :job) (of-kind seen :recovered))))
          (is (= [:recovered :yielded] (->> @seen (map :kind) (filter #{:recovered :yielded}) (take-last 2)))
              "at the act, not at the end of the round"))))))

(deftest a-repeat-keeps-its-count-across-child-restarts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup)]
          (core/submit! eng '(repeat (bump-once)) {})
          (dotimes [_ 4] (await (tick-at r t0)))
          (is (= ["j1" "j1" "j1"] (ran seen)))
          (is (= 3 (:fruitless (entry eng "j1"))))
          (is (= ["j1"] (:list (core/state eng)))))))))

(deftest cancelling-a-backing-off-job-drops-its-entry
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng] :as r} (setup)]
          (core/submit! eng '(bump) {})
          (dotimes [_ 3] (await (tick-at r t0)))
          (core/cancel! eng "j1")
          (is (= {} (core/backoff-entries eng))))))))

(deftest a-reflex-in-backoff-ends-its-job-and-does-not-fire-until-the-delay-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup)
              fired #(count (of-kind seen :fired))
              outcomes #(mapv :outcome (filterv (fn [e] (= :reflex (:source e))) (of-kind seen :ended)))]
          (core/register-reflex! eng {:trigger :always})
          (dotimes [_ 3] (await (tick-at r t0)))
          (is (= [:done :done :done] (outcomes)) "a round that ended the job keeps its own outcome")
          (is (= 3 (:fruitless (entry eng :always))) "keyed by the reflex id across firings")
          (dotimes [_ 3] (await (tick-at r (+ t0 999))))
          (is (= 3 (fired)))
          (await (tick-at r (+ t0 1000)))
          (is (= 4 (fired)))
          (is (= [:done :done :done :done] (outcomes)))
          (is (= 2000 (:delay-ms (entry eng :always)))))))))

(deftest a-declined-round-never-counts-for-a-reflex
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup)
              fired #(count (of-kind seen :fired))]
          (core/register-reflex! eng {:trigger :declining})
          (dotimes [_ 3] (await (tick-at r t0)))
          (is (= 3 (fired)))
          (is (nil? (entry eng :declining)) "no fruitless count, no backoff")
          (await (tick-at r t0))
          (is (= 4 (fired)) "it fires again at once"))))))

(deftest a-declined-round-never-counts-for-a-listed-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup)]
          (core/submit! eng '(bump-decline) {})
          (dotimes [_ 4] (await (tick-at r t0)))
          (is (nil? (entry eng "j1")) "no fruitless count, no backoff")
          (is (= 4 (count (ran seen)))))))))

(deftest a-continuing-fruitless-round-reaching-backoff-ends-backoff
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup)
              outcomes #(mapv :outcome (filterv (fn [e] (= :reflex (:source e))) (of-kind seen :ended)))]
          (core/register-reflex! eng {:trigger :holding})
          (dotimes [_ 3] (await (tick-at r t0)))
          (is (= :backoff (last (outcomes)))))))))

(deftest a-progress-act-in-any-job-of-a-reflex-resets-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup)]
          (core/register-reflex! eng {:trigger :always})
          (dotimes [_ 3] (await (tick-at r t0)))
          (reset! status "arrived")
          (await (tick-at r (+ t0 1000)))
          (is (nil? (entry eng :always)))
          (is (= [[:reflex :always]] (mapv (juxt :source :reflex) (of-kind seen :recovered)))))))))

(defn ^:async rounds-before-backoff
  "How many rounds j1 got in 10 ticks at one instant, for a job submitted with spec and opts."
  [engine-opts spec opts]
  (let [{:keys [eng seen] :as r} (setup engine-opts)]
    (core/submit! eng spec opts)
    (dotimes [_ 10] (await (tick-at r t0)))
    (count (ran seen))))

(deftest config-levels-the-most-specific-wins
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label engine-opts spec opts expected]
                [["default" {} '(bump) {} 3]
                 ["engine" {:backoff {:after 2}} '(bump) {} 2]
                 ["engine off" {:backoff false} '(bump) {} 10]
                 ["job def" {} '(bump-fast) {} 1]
                 ["job def off" {} '(bump-never) {} 10]
                 ["job def, engine override loses to it" {:backoff {:after 5}} '(bump-fast) {} 1]
                 ["submit opt over job def" {} '(bump-fast) {:backoff {:after 4}} 4]
                 ["submit opt false" {} '(bump) {:backoff false} 10]
                 ["wrapper" {} '(backoff {:after 2} (bump)) {} 2]
                 ["wrapper false" {} '(backoff false (bump)) {} 10]
                 ["wrapper in hold" {} '(hold (backoff {:after 2} (bump))) {} 2]
                 ["hold in wrapper" {} '(backoff {:after 2} (hold (bump))) {} 2]
                 ["opt over wrapper" {} '(backoff {:after 2} (bump)) {:backoff {:after 4}} 4]]]
          (is (= expected (await (rounds-before-backoff engine-opts spec opts))) label))))))

(deftest a-register-entry-takes-backoff
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label spec expected] [["off" {:backoff false} 8]
                                       ["after 1" {:backoff {:after 1}} 1]
                                       ["after 2 and a delay" {:backoff {:after 2 :first-s 5}} 2]]]
          (let [{:keys [eng seen] :as r} (setup)]
            (core/register-reflex! eng (merge {:trigger :always} spec))
            (dotimes [_ 8] (await (tick-at r t0)))
            (is (= expected (count (of-kind seen :fired))) label)))
        (let [{:keys [eng]} (setup)]
          (is (thrown? js/Error (core/register-reflex! eng {:trigger :always :backoff {:after 0}}))))))))

(deftest the-warn-names-the-failing-act-and-repeats-at-most-every-alert-interval
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup {:backoff {:after 1 :first-s 1 :max-s 1} :backoff-alert-ms 5000})
              warns #(of-kind seen :backoff)]
          (core/submit! eng '(bump) {})
          (await (tick-at r t0))
          (let [w (first (warns))]
            (is (= [:job :warn "j1" :moveTo "blocked" "no path" 1000 1]
                   ((juxt :source :level :job :act :status :reason :delay-ms :fruitless) w)))
            (is (string? (:text w))))
          (doseq [s (range 1 5)]
            (await (tick-at r (+ t0 (* 1000 s) -500)))
            (await (tick-at r (+ t0 (* 1000 s)))))
          (is (= 1 (count (warns))) "quiet inside the interval")
          (await (tick-at r (+ t0 5000)))
          (is (= 2 (count (warns))))
          (let [w (second (warns))]
            (is (= [6 t0] ((juxt :fruitless :since) w)))
            (is (pos? (:passes w)) "ticks that skipped the job")))))))

(deftest the-alert-interval-defaults-to-five-minutes
  (is (= 300000 core/default-backoff-alert-ms)))

(deftest a-restart-does-not-carry-the-backoff
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              {:keys [eng] :as r} (setup {:dir dir})]
          (core/submit! eng '(bump) {})
          (dotimes [_ 3] (await (tick-at r t0)))
          (is (some? (entry eng "j1")))
          (let [again (:eng (setup {:dir dir}))]
            (is (= {} (core/backoff-entries again)))
            (is (nil? (:backoff (core/state again))))))))))

(deftest neutral-acts-neither-count-nor-reset
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[acts expected] [[[[:look "ok"] [:moveTo "blocked"]] 1]
                                 [[[:equip "ok"] [:wait "ok"] [:moveTo "blocked"]] 1]
                                 [[[:look "ok"]] 0]
                                 [[[:moveTo "timeout" 1.5]] 0]
                                 [[[:moveTo "blocked" 1.5]] 0]
                                 [[[:moveTo "timeout" 1.5] [:moveTo "blocked"]] 1]
                                 [[[:moveTo "timeout" 0.2]] 1]]]
          (let [{:keys [eng] :as r} (setup)]
            (reset! plan acts)
            (core/submit! eng '(planned) {})
            (await (tick-at r t0))
            (is (= expected (:fruitless (entry eng "j1") 0)) (pr-str acts))))))))

(deftest a-round-of-neutral-acts-does-not-reset-a-backoff
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [acts [[[:look "ok"]] [[:wait "ok"]] [[:equip "ok"]] [[:moveTo "timeout" 1.5]]]]
          (let [{:keys [eng seen] :as r} (setup)]
            (reset! plan [[:moveTo "blocked"]])
            (core/submit! eng '(planned) {})
            (dotimes [_ 3] (await (tick-at r t0)))
            (reset! plan acts)
            (await (tick-at r (+ t0 1000)))
            (is (= 3 (:fruitless (entry eng "j1"))) (pr-str acts))
            (is (empty? (of-kind seen :recovered)))))))))

(deftest a-job-that-looks-and-then-gets-a-blocked-walk-backs-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup)]
          (reset! plan [[:look "ok"] [:moveTo "blocked"]])
          (core/submit! eng '(planned) {})
          (dotimes [_ 6] (await (tick-at r t0)))
          (is (= 3 (count (ran seen))))
          (is (some? (:until (entry eng "j1")))))))))

(deftest a-delay-is-not-persisted-and-ends-when-the-engine-leaves-a-pause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as r} (setup)
              world (.-world p)]
          (core/submit! eng '(bump) {})
          (dotimes [_ 3] (await (tick-at r t0)))
          (is (some? (:until (entry eng "j1"))))
          (dotimes [_ 2] (await (tick-at r t0)))
          (is (= 3 (count (ran seen))) "backing off, no pause")
          (.settle world true)
          (await (tick-at r t0))
          (is (some? (:until (entry eng "j1"))) "while paused nothing is cleared")
          (.settle world false)
          (await (tick-at r t0))
          (is (= 4 (count (ran seen))) "the first ready tick gives the job a round again")
          (is (= 1 (:fruitless (entry eng "j1"))))
          (is (nil? (:until (entry eng "j1")))))))))

(deftest a-pause-clears-every-count-not-only-the-delays
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as r} (setup)
              world (.-world p)]
          (core/submit! eng '(bump) {})
          (dotimes [_ 2] (await (tick-at r t0)))
          (is (= 2 (:fruitless (entry eng "j1"))))
          (.settle world true)
          (await (tick-at r t0))
          (.settle world false)
          (reset! status "arrived")
          (await (tick-at r t0))
          (is (nil? (entry eng "j1"))))))))

(deftest reset-backoff-empties-the-entries
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng] :as r} (setup)]
          (core/submit! eng '(bump) {})
          (dotimes [_ 3] (await (tick-at r t0)))
          (core/reset-backoff! eng)
          (is (= {} (core/backoff-entries eng))))))))

(deftest a-reflex-in-backoff-lets-a-lower-reflex-and-the-list-run-then-fires-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup)
              fired-reflexes #(mapv :reflex (of-kind seen :fired))]
          (core/register-reflex! eng {:trigger :always})
          (core/register-reflex! eng {:trigger :low})
          (core/submit! eng '(idle) {})
          (dotimes [_ 3] (await (tick-at r t0)))
          (is (= [:always :always :always] (fired-reflexes)))
          (is (some? (:until (entry eng :always))))
          (dotimes [_ 3] (await (tick-at r (+ t0 500))))
          (is (= [:always :always :always :low] (fired-reflexes))
              "the lower reflex fires while the first is in backoff")
          (is (= 2 (count (filter #{"j1"} (ran seen)))) "and the list gets rounds")
          (await (tick-at r (+ t0 1000)))
          (is (= :always (last (fired-reflexes))) "after the delay it fires again"))))))

;; ------------------------------------------------------- the job's own conclusion wins

(def conclusion-triggers
  {:once {:name :once :job '(script {:statuses ["blocked"] :end :done}) :persistence :retry :when always}
   :decline {:name :decline :job '(script {:statuses ["blocked"] :end :declined}) :persistence :retry :when always}
   :throws {:name :throws :job '(script {:statuses ["blocked"] :end :throw}) :persistence :retry :when always}})

(defn reflex-outcomes [seen]
  (mapv :outcome (filterv #(= :reflex (:source %)) (of-kind seen :ended))))

(deftest a-reflex-done-on-its-third-fruitless-round-ends-done-and-still-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup {:triggers conclusion-triggers})
              fired #(count (of-kind seen :fired))]
          (core/register-reflex! eng {:trigger :once})
          (dotimes [_ 3] (await (tick-at r t0)))
          (is (= [:done :done :done] (reflex-outcomes seen)) "never :backoff")
          (is (= 3 (:fruitless (entry eng :once))))
          (is (= 1000 (:delay-ms (entry eng :once))))
          (await (tick-at r (+ t0 999)))
          (is (= 3 (fired)) "cannot re-fire before the delay")
          (await (tick-at r (+ t0 1000)))
          (is (= 4 (fired))))))))

(deftest a-reflex-declined-on-its-third-round-ends-declined-without-backoff
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup {:triggers conclusion-triggers})]
          (core/register-reflex! eng {:trigger :decline})
          (dotimes [_ 3] (await (tick-at r t0)))
          (is (= [:declined :declined :declined] (reflex-outcomes seen)))
          (is (nil? (entry eng :decline)) "declined never counts"))))))

(def calls (atom 0))

(defn ^:async third-round
  "A fruitless round that continues twice, then ends with (:end args) on the third call."
  [c]
  (await (ctx/act c :moveTo #js {:pos #js {:x 0 :y 64 :z 0} :status "blocked"}))
  (when (and (= 3 (swap! calls inc)) (= :throw (:end (:args c)))) (throw (js/Error. "boom")))
  (if (< @calls 3) :continue (:end (:args c))))

(def third-jobs
  (assoc jobs 'third {:check always :round third-round :args {:end {:default :done}}}))

(defn third-setup [opts]
  (reset! calls 0)
  (setup (merge {:jobs third-jobs} opts)))

(deftest a-listed-job-done-on-its-third-fruitless-round-completes-with-no-backoff
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (third-setup {})]
          (core/submit! eng '(third) {})
          (dotimes [_ 2] (await (tick-at r t0)))
          (is (= 2 (:fruitless (entry eng "j1"))))
          (await (tick-at r t0))
          (is (= 1 (count (of-kind seen :completed))))
          (is (nil? (entry eng "j1")))
          (is (not (some #{"j1"} (:list (core/state eng))))))))))

(deftest a-round-that-throws-on-the-third-fruitless-round-fails
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (third-setup {})]
          (core/submit! eng '(third {:end :throw}) {})
          (dotimes [_ 3] (await (tick-at r t0)))
          (is (= 1 (count (of-kind seen :failed))))
          (is (contains? (:failed (core/state eng)) "j1")))
        (let [{:keys [eng seen] :as r} (third-setup {:triggers {:t {:name :t :job '(third {:end :throw}) :persistence :retry :when always}}})]
          (core/register-reflex! eng {:trigger :t})
          (dotimes [_ 3] (await (tick-at r t0)))
          (is (= [:failed] (reflex-outcomes seen))))))))

(deftest only-the-documented-registry-jobs-opt-out-of-backoff
  (is (= '#{jobs.survival.sleep jobs.survival.shelter jobs.maintenance.unstick}
         (set (keep (fn [[k v]] (when (false? (:backoff v)) k)) registry/jobs)))
      "a new :backoff false must be added to this set deliberately, with its reason in the job's docstring (README, Jobs, backoff)"))
