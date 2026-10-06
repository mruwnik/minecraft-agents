(ns engine.backoff-test
  "Backoff: a reflex whose runs keep failing is not fired as fast as the scheduler can (register entries only;
  listed jobs are never backed off, job.fruitless only flags them); see engine.backoff and README.md, Backoff."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.backoff :as backoff]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.triggers :as trigger-defaults]
            [engine.fake :as fake]
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
    "unchanged" true "no-room" true "missing" true
    "arrived" false "partial" false "dug" false "used" false "placed" false "ok" false
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
    :steer "done" 0 0 0
    :steer "timeout" 0 0 0
    :walk "partial" 3 0 0
    :walk "blocked" 50 0 0
    :walk "blocked" 8 0 0
    :walk "blocked" 7.9 1 1
    :walk "blocked" 0 1 1
    :walk "arrived" 0 1 0
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
  :end (:continue, :done, :declined, :stopped or :throw)."
  [c]
  (doseq [s (:statuses (:args c))]
    (await (ctx/act c :moveTo #js {:pos #js {:x 0 :y 64 :z 0} :status s})))
  (case (:end (:args c))
    :throw (throw (js/Error. "boom"))
    :stopped (do (ctx/result! c {:status :stopped :reason :no-way}) :done)
    (:end (:args c) :done)))

(defn ^:async bump-round
  "One moveTo, answered with whatever status holds."
  [c]
  (await (ctx/act c :moveTo #js {:pos #js {:x 0 :y 64 :z 0} :status @status}))
  :continue)

(defn ^:async bump-once-round [c]
  (await (ctx/act c :moveTo #js {:pos #js {:x 0 :y 64 :z 0} :status @status}))
  :done)

(def plan (atom []))

(defn ^:async planned-round
  "The acts of @plan, each [act status moved]: moveTo answers status and moves the body x by moved."
  [c]
  (doseq [[a s m] @plan]
    (await (ctx/act c a #js {:pos #js {:x 0 :y 64 :z 0} :status s :move m})))
  :done)

(defn ^:async idle-round [c]
  (ctx/update-mem! c update :n (fnil inc 0))
  :continue)

(defn ^:async use-on-round
  "One useOn, answered with whatever status holds."
  [c]
  (await (ctx/act c :useOn #js {:status @status}))
  :done)

(def alt-rounds (atom 0))

(defn ^:async alternating-round
  "A failed moveTo on odd rounds, no act on even ones."
  [c]
  (when (odd? (swap! alt-rounds inc))
    (await (ctx/act c :moveTo #js {:pos #js {:x 0 :y 64 :z 0} :status "blocked"})))
  :continue)

(def always (constantly true))

(def jobs
  (merge registry/jobs
         {'script {:check always :round script-round :args {:statuses {:default []} :end {:default :done}}}
          'bump {:check always :round bump-round}
          'use-on {:check always :round use-on-round}
          'bump-once {:check always :round bump-once-round}
          'idle {:check always :round idle-round}
          'alternating {:check always :round alternating-round}
          'quick {:check always :round (fn [_] :done)}
          'planned {:check always :round planned-round}}))

(def triggers
  {:always {:name :always :job '(bump-once) :persistence :retry :when always}
   :holding {:name :holding :job '(bump) :persistence :retry :when always}
   :low {:name :low :job '(quick) :persistence :stop :when always}
   :fast {:name :fast :job '(bump-once) :persistence :retry :when always :backoff {:after 1}}
   :never {:name :never :job '(bump-once) :persistence :retry :when always :backoff false}
   :use-on {:name :use-on :job '(use-on) :persistence :retry :when always}
   :planned {:name :planned :job '(planned) :persistence :retry :when always}})

(defn setup
  ([] (setup {}))
  ([opts]
   (let [clock (atom t0)
         [seen sink] (tu/legacy-capture-sink)
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
                    (fake/swap-self! p update-in [:pos 0] + m))
                  (js/Promise. (fn [resolve] (js/setTimeout #(resolve #js {:status (.-status args) :reason "no path"}) 0)))))
     {:eng eng :p p :seen seen :clock clock})))

(defn ran [seen] (->> @seen (filter #(= :round_started (:kind %))) (mapv :job)))
(defn of-kind [seen kind] (filterv #(= kind (:kind %)) @seen))
(defn fired [seen] (count (of-kind seen :fired)))
(defn entry [eng k] (core/backoff-entry eng k))

(defn ^:async tick-at [{:keys [eng clock]} t]
  (reset! clock t)
  (await (core/tick! eng)))

(defn ^:async fruitless-after
  "The fruitless count of reflex :t after one run of (script {:statuses statuses :end end})."
  [statuses end]
  (let [{:keys [eng] :as r} (setup {:triggers {:t {:name :t :job (list 'script {:statuses statuses :end end})
                                                   :persistence :retry :when always}}})]
    (core/register-reflex! eng {:trigger :t})
    (await (tick-at r t0))
    (:fruitless (entry eng :t) 0)))

(deftest a-run-is-fruitless-when-every-act-failed-or-it-gave-up-with-no-progress
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[statuses end expected] [[[] :done 0]
                                         [["blocked"] :done 1]
                                         [["blocked" "timeout"] :done 1]
                                         [["blocked" "arrived"] :done 0]
                                         [["arrived" "blocked"] :done 0]
                                         [["blocked"] :throw 0]
                                         [[] :declined 1]
                                         [["arrived"] :declined 0]
                                         [[] :stopped 1]
                                         [["blocked"] :stopped 1]
                                         [["arrived"] :stopped 0]]]
          (is (= expected (await (fruitless-after statuses end))) (str statuses end)))))))

(deftest backoff-starts-after-three-and-the-delay-doubles-up-to-the-cap
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng] :as r} (setup)
              delays (atom [])]
          (core/register-reflex! eng {:trigger :always})
          (await (tick-at r t0))
          (await (tick-at r t0))
          (is (nil? (:until (entry eng :always))) "two fruitless runs: no backoff yet")
          (loop [t t0 i 0]
            (await (tick-at r t))
            (let [d (:delay-ms (entry eng :always))]
              (swap! delays conj d)
              (when (< i 6) (recur (+ t d) (inc i)))))
          (is (= [1000 2000 4000 8000 16000 30000 30000] @delays)))))))

(deftest use-on-statuses-that-changed-nothing-back-off-and-used-does-not
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[st backs-off?] [["unchanged" true] ["no-room" true] ["missing" true] ["used" false]]]
          (let [{:keys [eng p] :as r} (setup)]
            (.override (.-world p) "useOn"
                       (fn [_ args _] (js/Promise. (fn [resolve] (resolve #js {:status (.-status args)})))))
            (reset! status st)
            (core/register-reflex! eng {:trigger :use-on})
            (dotimes [_ 3] (await (tick-at r t0)))
            (is (= backs-off? (some? (:until (entry eng :use-on)))) st)))))))

(deftest a-listed-job-is-never-backed-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup)]
          (core/submit! eng '(bump) {})
          (dotimes [_ 10] (await (tick-at r t0)))
          (is (= 10 (count (ran seen))) "every round failed and every tick gave it a round")
          (is (= {} (core/backoff-entries eng)))
          (is (empty? (of-kind seen :backoff))))))))

(deftest a-listed-job-done-after-fruitless-rounds-completes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup)]
          (core/submit! eng '(bump) {})
          (dotimes [_ 2] (await (tick-at r t0)))
          (core/submit! eng (list 'script {:statuses ["blocked"] :end :done}) {})
          (dotimes [_ 2] (await (tick-at r t0)))
          (is (= 1 (count (of-kind seen :completed))))
          (is (= {} (core/backoff-entries eng))))))))

(deftest a-reflex-in-backoff-ends-its-job-and-does-not-fire-until-the-delay-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup)
              outcomes #(mapv :outcome (filterv (fn [e] (= :reflex (:source e))) (of-kind seen :ended)))]
          (core/register-reflex! eng {:trigger :always})
          (dotimes [_ 3] (await (tick-at r t0)))
          (is (= [:done :done :done] (outcomes)) "a run that ended the job keeps its own outcome")
          (is (= 3 (:fruitless (entry eng :always))) "keyed by the reflex id across firings")
          (dotimes [_ 3] (await (tick-at r (+ t0 999))))
          (is (= 3 (fired seen)))
          (await (tick-at r (+ t0 1000)))
          (is (= 4 (fired seen)))
          (is (= [:done :done :done :done] (outcomes)))
          (is (= 2000 (:delay-ms (entry eng :always)))))))))

(deftest a-continuing-fruitless-round-reaching-backoff-ends-backoff
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup)
              outcomes #(mapv :outcome (filterv (fn [e] (= :reflex (:source e))) (of-kind seen :ended)))]
          (core/register-reflex! eng {:trigger :holding})
          (dotimes [_ 3] (await (tick-at r t0)))
          (is (= :backoff (last (outcomes)))))))))

(deftest a-progress-act-in-any-job-of-a-reflex-resets-it-and-says-so
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

(defn ^:async fires-in-ten-ticks
  "How often reflex :t fires in 10 ticks at one instant: trigger :t from trigger (merged over a bump-once one),
  registered with entry, on an engine made with engine-opts."
  [engine-opts trigger entry]
  (let [{:keys [eng seen] :as r} (setup (merge engine-opts
                                               {:triggers {:t (merge {:name :t :job '(bump-once) :persistence :retry
                                                                      :when always}
                                                                     trigger)}}))]
    (core/register-reflex! eng (merge {:trigger :t} entry))
    (dotimes [_ 10] (await (tick-at r t0)))
    (fired seen)))

(deftest config-levels-the-most-specific-wins
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label engine-opts trigger entry expected]
                [["default" {} {} {} 3]
                 ["engine" {:backoff {:after 2}} {} {} 2]
                 ["engine off" {:backoff false} {} {} 10]
                 ["trigger default" {} {:backoff {:after 1}} {} 1]
                 ["trigger default off" {} {:backoff false} {} 10]
                 ["trigger default over the engine" {:backoff {:after 5}} {:backoff {:after 1}} {} 1]
                 ["entry over trigger default" {} {:backoff {:after 1}} {:backoff {:after 4}} 4]
                 ["entry off" {} {} {:backoff false} 10]]]
          (is (= expected (await (fires-in-ten-ticks engine-opts trigger entry))) label))))))

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
            (is (= expected (fired seen)) label)))
        (let [{:keys [eng]} (setup)]
          (is (thrown? js/Error (core/register-reflex! eng {:trigger :always :backoff {:after 0}}))))))))

(deftest the-warn-names-the-failing-act-and-repeats-at-most-every-alert-interval
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup {:backoff {:after 1 :first-s 1 :max-s 1} :backoff-alert-ms 5000})
              warns #(of-kind seen :backoff)]
          (core/register-reflex! eng {:trigger :always})
          (await (tick-at r t0))
          (let [w (first (warns))]
            (is (= [:reflex :always :moveTo "blocked" "no path" 1000 1]
                   ((juxt :source :reflex :act :status :reason :delay-ms :fruitless) w)))
            (is (string? (:text w))))
          (doseq [s (range 1 5)]
            (await (tick-at r (+ t0 (* 1000 s) -500)))
            (await (tick-at r (+ t0 (* 1000 s)))))
          (is (= 1 (count (warns))) "quiet inside the interval")
          (await (tick-at r (+ t0 5000)))
          (is (= 2 (count (warns))))
          (let [w (second (warns))]
            (is (= [6 t0] ((juxt :fruitless :since) w)))
            (is (pos? (:passes w)) "ticks that skipped the reflex")))))))

(deftest the-alert-interval-defaults-to-five-minutes
  (is (= 300000 core/default-backoff-alert-ms)))

(deftest a-restart-does-not-carry-the-backoff
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              {:keys [eng] :as r} (setup {:dir dir})]
          (core/register-reflex! eng {:trigger :always})
          (dotimes [_ 3] (await (tick-at r t0)))
          (is (some? (entry eng :always)))
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
            (core/register-reflex! eng {:trigger :planned})
            (await (tick-at r t0))
            (is (= expected (:fruitless (entry eng :planned) 0)) (pr-str acts))))))))

(deftest a-run-of-neutral-acts-does-not-reset-a-backoff
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [acts [[[:look "ok"]] [[:wait "ok"]] [[:equip "ok"]] [[:moveTo "timeout" 1.5]]]]
          (let [{:keys [eng seen] :as r} (setup)]
            (reset! plan [[:moveTo "blocked"]])
            (core/register-reflex! eng {:trigger :planned})
            (dotimes [_ 3] (await (tick-at r t0)))
            (reset! plan acts)
            (await (tick-at r (+ t0 1000)))
            (is (= 3 (:fruitless (entry eng :planned))) (pr-str acts))
            (is (empty? (of-kind seen :recovered)))))))))

(deftest a-reflex-that-looks-and-then-gets-a-blocked-walk-backs-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup)]
          (reset! plan [[:look "ok"] [:moveTo "blocked"]])
          (core/register-reflex! eng {:trigger :planned})
          (dotimes [_ 6] (await (tick-at r t0)))
          (is (= 3 (count (ran seen))))
          (is (some? (:until (entry eng :planned)))))))))

(deftest a-delay-is-not-persisted-and-ends-when-the-engine-leaves-a-pause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as r} (setup)
              world (.-world p)]
          (core/register-reflex! eng {:trigger :always})
          (dotimes [_ 3] (await (tick-at r t0)))
          (is (some? (:until (entry eng :always))))
          (dotimes [_ 2] (await (tick-at r t0)))
          (is (= 3 (count (ran seen))) "backing off, no pause")
          (.settle world true)
          (await (tick-at r t0))
          (is (some? (:until (entry eng :always))) "while paused nothing is cleared")
          (.settle world false)
          (await (tick-at r t0))
          (is (= 4 (count (ran seen))) "the first ready tick lets it fire again")
          (is (= 1 (:fruitless (entry eng :always))))
          (is (nil? (:until (entry eng :always)))))))))

(deftest a-pause-clears-every-count-not-only-the-delays
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as r} (setup)
              world (.-world p)]
          (core/register-reflex! eng {:trigger :always})
          (dotimes [_ 2] (await (tick-at r t0)))
          (is (= 2 (:fruitless (entry eng :always))))
          (.settle world true)
          (await (tick-at r t0))
          (.settle world false)
          (reset! status "arrived")
          (await (tick-at r t0))
          (is (nil? (entry eng :always))))))))

(deftest reset-backoff-empties-the-entries
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng] :as r} (setup)]
          (core/register-reflex! eng {:trigger :always})
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

(defn reflex-outcomes [seen]
  (mapv :outcome (filterv #(= :reflex (:source %)) (of-kind seen :ended))))

(defn conclusion-setup [end]
  (setup {:triggers {:t {:name :t :job (list 'script {:statuses ["blocked"] :end end}) :persistence :retry
                         :when always}}}))

(deftest a-reflex-ending-on-its-third-fruitless-run-keeps-its-outcome-and-still-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [end [:done :declined :stopped]]
          (let [{:keys [eng seen] :as r} (conclusion-setup end)]
            (core/register-reflex! eng {:trigger :t})
            (dotimes [_ 3] (await (tick-at r t0)))
            (is (= (vec (repeat 3 (if (= :declined end) :declined :done))) (reflex-outcomes seen)) "never :backoff")
            (is (= 3 (:fruitless (entry eng :t))) (str end))
            (is (= 1000 (:delay-ms (entry eng :t))))
            (await (tick-at r (+ t0 999)))
            (is (= 3 (fired seen)) "cannot re-fire before the delay")
            (await (tick-at r (+ t0 1000)))
            (is (= 4 (fired seen)))))))))

(deftest a-reflex-run-that-throws-fails-and-does-not-count
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (conclusion-setup :throw)]
          (core/register-reflex! eng {:trigger :t})
          (dotimes [_ 3] (await (tick-at r t0)))
          (is (= [:failed :failed :failed] (reflex-outcomes seen)))
          (is (nil? (entry eng :t))))))))

(deftest only-the-documented-default-triggers-opt-out-of-backoff
  (is (= #{:suffocating :burning :hostile-near :night :stuck}
         (set (keep (fn [[k v]] (when (false? (:backoff v)) k)) trigger-defaults/all)))
      "a new :backoff false in triggers/defaults.edn must be added here deliberately, with its reason beside it"))

;; ---------------------------------------------------------------- job.fruitless (listed jobs only flag)

(defn fruitless-requests [eng]
  (filterv #(= :fruitless (:reason %)) (vals (:attention (core/state eng)))))

(deftest three-fruitless-rounds-of-a-listed-job-warn-once-and-ask-for-attention
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as r} (setup)]
          (core/submit! eng '(bump) {})
          (dotimes [i 2] (await (tick-at r (+ t0 (* i 60000)))))
          (is (= [] (of-kind seen :fruitless)))
          (await (tick-at r (+ t0 120000)))
          (is (= [["j1" 3 :moveTo "blocked"]] (mapv (juxt :job :rounds :act :status) (of-kind seen :fruitless))))
          (is (= 1 (count (fruitless-requests eng))))
          (await (tick-at r (+ t0 180000)))
          (is (= 1 (count (of-kind seen :fruitless))) "once per spell")
          (is (= ["j1"] (:list (core/state eng))) "it only flags: the job stays listed")
          (reset! status "arrived")
          (await (tick-at r (+ t0 240000)))
          (is (= [] (fruitless-requests eng)) "a round with progress resolves the request"))))))

(deftest a-round-with-no-act-is-not-fruitless
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen eng] :as r} (setup)]
          (core/submit! eng '(idle) {})
          (dotimes [i 4] (await (tick-at r (+ t0 (* i 60000)))))
          (is (= [] (of-kind seen :fruitless))))))))

(deftest a-no-act-round-neither-counts-nor-resets-the-fruitless-spell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (reset! alt-rounds 0)
        (let [{:keys [seen eng] :as r} (setup)]
          (core/submit! eng '(alternating) {})
          (dotimes [i 6] (await (tick-at r (+ t0 (* i 60000)))))
          (is (= [["j1" 3]] (mapv (juxt :job :rounds) (of-kind seen :fruitless)))
              "three failed rounds raise it although yielded no-act rounds lie between"))))))

(def calls (atom 0))

(defn ^:async third-round
  "A blocked moveTo each round; :continue twice, then :done on the third."
  [c]
  (await (ctx/act c :moveTo #js {:pos #js {:x 0 :y 64 :z 0} :status "blocked"}))
  (if (< (swap! calls inc) 3) :continue :done))

(deftest a-round-that-ends-the-job-is-not-fruitless
  (async done
    (tu/run-async done
      (fn ^:async t []
        (reset! calls 0)
        (let [{:keys [seen eng] :as r} (setup {:jobs (assoc jobs 'third {:check always :round third-round})})]
          (core/submit! eng '(third) {})
          (dotimes [i 3] (await (tick-at r (+ t0 (* i 60000)))))
          (is (= 1 (count (of-kind seen :completed))))
          (is (= [] (of-kind seen :fruitless)) "the job reached its goal on its third fruitless-looking round"))))))
