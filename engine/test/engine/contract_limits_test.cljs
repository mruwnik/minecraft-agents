(ns engine.contract-limits-test
  "A call's limits (engine.contract, engine.core.limits): clamped to the caller's, checked at acts and call boundaries,
  the outermost crossed frame ends :limit-reached and job.limit_reached tells the line, its amount and the report."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [jobs.apiary.maintain :as apiary-maintain]))

(def world (atom {}))
(def clock (atom 0))

(defn setup [jobs]
  (reset! world {})
  (reset! clock 1000000)
  (let [[seen sink] (tu/capture-sink)
        p (tu/fake {})
        eng (core/create {:primitives p :dir (tu/tmp-dir) :now #(deref clock) :jobs jobs :triggers {}
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :seen seen :p p}))

(defn job-ev [seen kind] (filterv #(= [:job kind] [(:source %) (:kind %)]) @seen))
(defn stopped [seen] (select-keys (:data (first (job-ev seen :stopped))) [:kind :cause]))
(defn raw-mem [eng root slots] (mem/job-mem (mem/view (:store eng)) root slots))
(defn looks [p] (count (filter #(= "look" (.-name %)) (.-calls (.-world p)))))

(defn look! [c] (ctx/act c :look #js {:yaw 0 :pitch 0}))

(defn hurt! [c health] (fake/swap-self! (:primitives c) assoc :health health))

(def slow
  {:defaults {:spend {:job-time 5}}
   :round (fn [_] (swap! clock + 6000) (swap! world update :slow-rounds (fnil inc 0)) :continue)})

(deftest its-own-clock-ends-a-job-past-its-time
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'slow slow})]
          (core/submit! eng '(slow) {})
          (await (core/tick! eng))
          (await (core/tick! eng))
          (is (= 1 (:slow-rounds @world)) "the round after the crossing does not run")
          (is (= {:kind :limit-reached :cause {:line :job-time :spend 5}} (stopped seen))))))))

(def waits
  {:round (fn [_] (swap! world update :wait-rounds (fnil inc 0)) :continue)})

(deftest waiting-spends-no-time-but-counts-as-elapsed-from-the-start
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'waits waits})]
          (core/submit! eng '(waits {:spend {:job-time 5 :wall-time 10}}) {})
          (swap! clock + 60000)
          (await (core/tick! eng))
          (swap! clock + 4000)
          (await (core/tick! eng))
          (swap! clock + 4000)
          (await (core/tick! eng))
          (is (= 3 (:wait-rounds @world)) "yields spend no :time; time queued before the start is not elapsed")
          (swap! clock + 3000)
          (await (core/tick! eng))
          (is (= 3 (:wait-rounds @world)))
          (is (= {:kind :limit-reached :cause {:line :wall-time :spend 10}} (stopped seen)))
          (is (= {:job-time 0 :wall-time 11} (get-in (first (job-ev seen :limit_reached)) [:data :report :uses]))))))))

(defn slow-wait!
  "Make the fake body's wait take ms of the test clock."
  [p ms]
  (aset p "wait" (fn [_token _args] (swap! clock + ms) #js {:status "ok"})))

(def sheltering
  {:defaults {:spend {:job-time 5}}
   :round (fn ^:async r [c]
            (swap! clock + 4000)
            (await (ctx/act c :wait #js {:ms 3000 :why :sheltering}))
            (swap! world update :rounds (fnil inc 0))
            :continue)})

(def idling
  (assoc sheltering :round (fn ^:async r [c]
                             (await (ctx/act c :wait #js {:ms 3000}))
                             (swap! world update :rounds (fnil inc 0))
                             :continue)))

(deftest a-held-wait-spends-no-job-time-but-a-plain-wait-does
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup {'sheltering sheltering})]
          (slow-wait! p 60000)
          (core/submit! eng '(sheltering) {})
          (await (core/tick! eng))
          (is (= 1 (:rounds @world)) "the held wait is not refused for the 3 s it quotes, nor counted for the 60 s it took")
          (is (empty? (job-ev seen :limit_reached)))
          (await (core/tick! eng))
          (is (= {:kind :limit-reached :cause {:line :job-time :spend 5}} (stopped seen)) "only the 4 s of its own work add up: the second round crosses 5")
          (is (= 8 (get-in (first (job-ev seen :limit_reached)) [:data :report :uses :job-time]))))))))

(deftest a-plain-wait-spends-job-time
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup {'idling idling})]
          (slow-wait! p 6000)
          (core/submit! eng '(idling) {})
          (dotimes [_ 2] (await (core/tick! eng)))
          (is (= {:kind :limit-reached :cause {:line :job-time :spend 5}} (stopped seen))))))))

(deftest a-held-wait-still-spends-wall-time
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup {'sheltering (assoc sheltering :defaults {:spend {:wall-time 20}})})]
          (slow-wait! p 60000)
          (core/submit! eng '(sheltering) {})
          (dotimes [_ 2] (await (core/tick! eng)))
          (is (= {:kind :limit-reached :cause {:line :wall-time :spend 20}} (stopped seen))))))))

(deftest the-limit-that-ends-a-job-is-told-with-its-amount-and-report
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'slow slow})
              id (core/submit! eng '(slow) {})]
          (await (core/tick! eng))
          (await (core/tick! eng))
          (let [told (job-ev seen :limit_reached)
                data (:data (first told))]
            (is (= [id] (mapv #(get-in % [:context :job-id]) told)))
            (is (= {:name 'slow :cause {:line :job-time :spend 5} :used 6 :call {:spend {:job-time 5} :least 1}}
                   (select-keys data [:name :cause :used :call])))
            (is (= 6 (get-in data [:report :uses :job-time])))))))))

(deftest a-default-spend-may-be-a-fn-of-the-args
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'slow (assoc slow :defaults {:spend {:job-time (fn [{:keys [n]}] (* 2 n))}})})]
          (core/submit! eng '(slow {:n 2}) {})
          (await (core/tick! eng))
          (await (core/tick! eng))
          (is (= {:kind :limit-reached :cause {:line :job-time :spend 4}} (stopped seen))))))))

(def counted
  (assoc slow :done? (fn [_] (if (when-let [n (:done-throws @world)] (>= (:slow-rounds @world 0) n)) (throw (js/Error. "boom")) (contract/later :not-done "not done" :units :cells :done 5 :left 3 :result {:dug [1 2]})))))

(deftest a-limit-ending-reports-the-units-done-and-left-and-the-jobs-data
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'counted counted
                                         'top {:round (fn ^:async top [c]
                                                        (let [e (await (ctx/call c :kid 'counted {:least 6}))]
                                                          (if (= :continue e)
                                                            :continue
                                                            (do (swap! world assoc :kid-ending e) (ctx/end c :done nil)))))}})]
          (core/submit! eng '(counted {:least 6}) {})
          (dotimes [_ 2] (await (core/tick! eng)))
          (is (= :limit-reached (:kind (stopped seen))))
          (is (= {:units :cells :done 5 :left 3}
                 (select-keys (:report (:data (first (job-ev seen :stopped)))) [:units :done :left])))
          (is (= [1 2] (:dug (:data (first (job-ev seen :stopped))))) "done?'s :data is the ending's")
          (core/submit! eng '(top) {})
          (dotimes [_ 2] (await (core/tick! eng)))
          (is (= :limit-reached (:kind (:kid-ending @world))))
          (is (= {:units :cells :done 5 :left 3} (select-keys (:report (:kid-ending @world)) [:units :done :left]))
              "a child's too")
          (is (= {:dug [1 2]} (:data (:kid-ending @world))))
          (swap! world assoc :done-throws 3)
          (core/submit! eng '(counted {:least 6}) {})
          (dotimes [_ 2] (await (core/tick! eng)))
          (is (= :limit-reached (:kind (:data (second (job-ev seen :stopped)))))
              "a done? that throws does not hide the limit"))))))

(deftest a-limit-with-the-least-done-ends-done-with-the-limit-in-the-report
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'counted counted})]
          (core/submit! eng '(counted {:least 5}) {})
          (dotimes [_ 2] (await (core/tick! eng)))
          (let [d (:data (first (job-ev seen :completed)))]
            (is (= :done (:kind d)))
            (is (= [1 2] (:dug d)))
            (is (= {:line :job-time :spend 5} (:limit (:report d)))))
          (is (= 1 (count (job-ev seen :limit_reached))) "the limit is still told"))))))

;; ---------------------------------------------------------------- handed down

(defn kid [f]
  {:round (fn ^:async kid-round [c]
            (swap! world assoc :kid-call (ctx/frozen-call c))
            (await (f c))
            (await (look! c))
            (swap! world update :kid-acts (fnil inc 0))
            :continue)})

(defn caller [kid-def kid-args]
  {:round (fn ^:async caller-round [c]
            (let [e (await (ctx/call c :kid kid-def kid-args))]
              (swap! world assoc :kid-ending e)
              :continue))})

(deftest a-deadline-is-handed-down-and-ends-the-outermost-frame
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [jobs {'top (caller (kid (fn [_] (swap! clock + 5000))) {:deadline 9000000})}
              {:keys [eng seen]} (setup jobs)
              id (core/submit! eng '(top {:deadline 1002000}) {})]
          (await (core/tick! eng))
          (is (= 1002000 (:deadline (:kid-call @world))) "clamped to the caller's")
          (is (= [[:deadline]] (:lines (:data (first (job-ev seen :budget_clamped))))))
          (is (nil? (:kid-acts @world)) "the act past the deadline did not run")
          (is (nil? (:kid-ending @world)) "the child's boundary passed it on")
          (is (= {:kind :limit-reached :cause {:line :deadline :deadline 1002000}} (stopped seen)))
          (is (empty? (raw-mem eng id []))))))))

(deftest a-clamped-child-and-its-caller-cross-together-and-the-caller-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [jobs {'top (merge (caller (kid (fn [_] (swap! clock + 11000))) {:spend {:job-time 60 :health 4}})
                                {:defaults {:spend {:job-time 10}}})}
              {:keys [eng seen]} (setup jobs)]
          (core/submit! eng '(top) {})
          (await (core/tick! eng))
          (is (= {:job-time 10 :health 4} (:spend (:kid-call @world))) "time held to the caller's; health the caller has no line for")
          (is (= [[:spend :job-time]] (:lines (:data (first (job-ev seen :budget_clamped))))))
          (is (= {:kind :limit-reached :cause {:line :job-time :spend 10}} (stopped seen))))))))

(deftest a-childs-left-is-the-least-its-chain-has-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [jobs {'top (merge (caller (kid (fn [c] (swap! clock + 3000) (swap! world assoc :kid-left (ctx/left c))))
                                        {:spend {:health 4}})
                                {:defaults {:spend {:job-time 10} :floor {:health 5}}})}
              {:keys [eng]} (setup jobs)]
          (core/submit! eng '(top) {})
          (await (core/tick! eng))
          (is (= {:spend {:job-time 7 :health 4} :floor {:health 5}} (:kid-left @world))
              "the caller's time, which the child sets no line for, is the child's too"))))))

(deftest a-mob-hit-ends-the-child-whose-floor-it-crosses-not-the-caller
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [jobs {'top (merge (caller (kid #(hurt! % 12)) {:floor {:health 15}}) {:defaults {:floor {:health 5}}})}
              {:keys [eng seen]} (setup jobs)
              id (core/submit! eng '(top) {})]
          (await (core/tick! eng))
          (is (= {:kind :limit-reached :cause {:line :health :floor 15}} (dissoc (:kid-ending @world) :report)))
          (is (= [id] (:list (core/state eng))) "the caller goes on")
          (is (empty? (job-ev seen :stopped))))))))

(deftest a-callers-floor-ends-the-caller-from-a-grandchild
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [grandchild (kid #(hurt! % 11))
              jobs {'top (merge (caller (caller grandchild {}) {}) {:defaults {:floor {:health 12}}})}
              {:keys [eng seen]} (setup jobs)]
          (core/submit! eng '(top) {})
          (await (core/tick! eng))
          (is (= {:health 12} (:floor (:kid-call @world))) "the floor is handed down")
          (is (nil? (:kid-ending @world)) "neither boundary below the caller's ends it")
          (is (= {:kind :limit-reached :cause {:line :health :floor 12}} (stopped seen))))))))

(deftest a-child-called-below-its-floor-fails-its-conditions
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {'top (caller (kid (fn [_] nil)) {:floor {:health 15}})})]
          (fake/swap-self! p assoc :health 12)
          (core/submit! eng '(top) {})
          (await (core/tick! eng))
          (is (= {:kind :condition-failed :cause {:floor :health :min 15}} (dissoc (:kid-ending @world) :report)))
          (is (nil? (:kid-acts @world))))))))

(deftest a-listed-job-below-its-floor-from-settings-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [careful {:defaults {:floor {:health ::min-health}} :settings {::min-health {:default 10}}
                       :round (fn [_] (swap! world update :careful-rounds (fnil inc 0)) :continue)}
              {:keys [eng seen p]} (setup {'careful careful})]
          (fake/swap-self! p assoc :health 8)
          (core/submit! eng '(careful) {})
          (await (core/tick! eng))
          (is (nil? (:careful-rounds @world)))
          (is (= {:reason :health :cause {:floor :health :min 10}}
                 (select-keys (:data (first (job-ev seen :waiting))) [:reason :cause])))
          (fake/swap-self! p assoc :health 20)
          (await (core/tick! eng))
          (is (= 1 (:careful-rounds @world)))
          (fake/swap-self! p assoc :health 8)
          (await (core/tick! eng))
          (is (= 1 (:careful-rounds @world)) "a started job waits too, as after a reflex cut"))))))

;; ---------------------------------------------------------------- unwinding

(def cleans-up
  {:defaults {:floor {:health 12}}
   :round (fn ^:async cleans-up-round [c]
            (hurt! c 11)
            (try
              (await (look! c))
              (swap! world assoc :went-on true)
              (catch :default e
                (swap! world assoc :caught (contract/passes-through? e))
                (await (look! c))
                (try (await (ctx/call c :kid (kid (fn [_] nil)) {}))
                     (catch :default e2 (swap! world assoc :call-refused (contract/passes-through? e2))))
                (throw e)))
            :continue)})

(deftest an-unwinding-frame-may-act-but-not-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup {'cleans-up cleans-up})]
          (core/submit! eng '(cleans-up) {})
          (await (core/tick! eng))
          (is (true? (:caught @world)))
          (is (nil? (:went-on @world)))
          (is (= 1 (looks p)) "the cleanup act ran; the first one was stopped before it")
          (is (true? (:call-refused @world)))
          (is (nil? (:kid-call @world)))
          (is (= {:kind :limit-reached :cause {:line :health :floor 12}} (stopped seen))))))))

(def shuts-the-door
  {:defaults {:floor {:health 12}}
   :round (fn ^:async door-round [c]
            (hurt! c 11)
            (await (ctx/atomic c :comfort :door (fn ^:async door [] (await (look! c)) (await (look! c)))))
            (swap! world assoc :after-section true)
            :continue)})

(deftest a-line-crossed-inside-an-atomic-section-is-held-until-it-closes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup {'door shuts-the-door})]
          (core/submit! eng '(door) {})
          (await (core/tick! eng))
          (is (= 2 (looks p)) "both acts of the section ran")
          (is (nil? (:after-section @world)))
          (is (= {:kind :limit-reached :cause {:line :health :floor 12}} (stopped seen))))))))

(defn rejected [p] (-> p (.then (constantly nil)) (.catch identity)))

(deftest a-maintain-catch-all-passes-a-limit-through
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (contract/limit-error "j1" {:line :job-time :spend 5})
              c {:update-mem (fn [_ _] (swap! world assoc :booked true))}]
          (reset! world {})
          (is (contract/passes-through? e))
          (is (contract/passes-through? (doto (js/Error. "cut") (aset "code" "cut"))))
          (is (not (contract/passes-through? (js/Error. "boom"))))
          (let [runs (with-redefs [ctx/call (fn [_ _ _ _] (js/Promise.reject e))]
                       [(rejected (apiary-maintain/run-child! c :hives {}))])
                thrown (await (js/Promise.all (clj->js runs)))]
            (is (every? #(identical? e %) thrown))
            (is (nil? (:booked @world)) "not booked as a failed step")))))))


(deftest a-quotes-pause-adds-to-its-job-time-against-wall-time
  (let [state {:call {:spend {:wall-time 10}} :started-ms 0 :ran-ms 0}
        crossed (fn [uses] (map first (contract/most-crossings state {:uses uses} {} 0 0)))]
    (is (= [{:line :wall-time :spend 10}] (crossed {:job-time [6 6 6] :wall-time [6 6 6]})))
    (is (empty? (crossed {:job-time [6 6 6] :wall-time [3 3 3]})))
    (is (= [{:line :wall-time :spend 10}] (crossed {:job-time [12 12 12]})))))

;; ---------------------------------------------------------------- tries

(def failing-kid
  {:round (fn [c] (ctx/end c :condition-failed :already-closed))})

(def retrier
  {:defaults {:spend {:tries 3}}
   :round (fn ^:async retrier-round [c]
            (let [e (await (ctx/call c :kid (quote kid) {}))]
              (swap! world update :kid-calls (fnil inc 0))
              (swap! world assoc :kid-ending e)
              :continue))})

(deftest children-ending-other-than-done-spend-the-tries-line-across-rounds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'retrier retrier 'kid failing-kid})]
          (core/submit! eng '(retrier) {})
          (dotimes [_ 6] (await (core/tick! eng)))
          (is (= 3 (:kid-calls @world)) "the 4th try ends the job inside its call, before the caller goes on")
          (is (= {:kind :limit-reached :cause {:line :tries :spend 3 :last {:job 'kid :reason :already-closed}}}
                 (stopped seen)))
          (is (= 4 (get-in (first (job-ev seen :limit_reached)) [:data :report :uses :tries]))))))))

(deftest a-tries-line-must-be-a-number
  (is (some? (contract/call-problem 'x nil {:spend {:tries -1}})))
  (is (nil? (contract/call-problem 'x nil {:spend {:tries 2}}))))

(deftest a-child-that-succeeds-spends-a-try-too
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'retrier retrier 'kid {:round (fn [c] (ctx/end c :done :ok))}})]
          (core/submit! eng '(retrier) {})
          (dotimes [_ 6] (await (core/tick! eng)))
          (is (= 3 (:kid-calls @world)))
          (is (= {:kind :limit-reached :cause {:line :tries :spend 3 :last {:job 'kid :reason :ok}}} (stopped seen))
              "the cause names the last ending, done or not"))))))
