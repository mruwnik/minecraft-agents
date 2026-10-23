(ns engine.contract-status-test
  "Statuses [kind text data] (docs/contract-conditions-design.md 1.1, 2.1, 2.3): constructors, verdict, the reader of
  old condition returns, and the can-start?/can-run? exports as the engine asks them."
  (:require [cljs.test :refer [deftest is async]]
            [engine.composite :as composite]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [jobs.lib.child :as child]
            [jobs.lib.pace :as pace]))

(deftest constructors-give-kind-text-and-data-with-a-reason
  (is (= [:ok "aboard" {:reason :mounted}] (contract/ok :mounted "aboard")))
  (is (= [:later "torches 0 < 4" {:reason :no-torches :have 0 :want 4}]
         (contract/later :no-torches "torches 0 < 4" :have 0 :want 4)))
  (is (= [:never "diamond does not smelt" {:reason :not-smeltable :item "diamond"}]
         (contract/never :not-smeltable "diamond does not smelt" :item "diamond")))
  (is (= [:look "no tree seen" {:reason :no-tree}] (contract/look :no-tree "no tree seen"))))

(def ok (constantly (contract/ok :fine "fine")))
(def later (constantly (contract/later :not-yet "not yet")))
(def never (constantly (contract/never :bad-args "count 0")))
(def peek (constantly (contract/look :unseen "not seen")))

(deftest verdict-is-any-never-else-the-first-later-else-the-first-look-else-ok
  (let [kind #(first (contract/verdict {} %))]
    (is (= :ok (kind {:can-start? ok :can-run? ok})))
    (is (= :ok (kind {})) "no export counts as :ok")
    (is (= :never (kind {:can-start? later :can-run? never})))
    (is (= :later (kind {:can-start? peek :can-run? later})))
    (is (= :look (kind {:can-start? ok :can-run? peek})))
    (is (= "not yet" (second (contract/verdict {} {:can-start? later}))))))

(defn odd-check [_] :not-a-status)

(deftest an-export-that-returns-no-status-is-an-error-naming-it
  (is (thrown-with-msg? js/Error #"odd-check" (contract/verdict {} {:can-run? odd-check})))
  (is (thrown-with-msg? js/Error #"can-run\? of jobs.x" (contract/verdict {} {:sym 'jobs.x :can-run? (fn [_] :odd)})))
  (is (thrown-with-msg? js/Error #"can-start\? of seq" (contract/verdict {} {:label "seq" :can-start? (fn [_] :odd)}))))

;; ---------------------------------------------------------------- the engine

(def world (atom {}))

(defn setup
  ([jobs] (setup jobs {}))
  ([jobs triggers]
  (reset! world {})
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        p (tu/fake {})
        eng (core/create {:primitives p :dir (tu/tmp-dir) :now #(deref clock) :jobs jobs :triggers triggers
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :seen seen :clock clock :p p})))

(defn job-ev [seen kind] (filterv #(= [:job kind] [(:source %) (:kind %)]) @seen))

(defn gate
  "A check reading world key k: true :ok, :never :never, else :later."
  [k]
  (fn [_]
    (case (get @world k)
      true (contract/ok k "go")
      :never (contract/never :gone "gone for good" :item "x")
      (contract/later :paused "torches 0 < 4" :have 0))))

(def gated
  {:can-start? (gate :start) :can-run? (gate :run)
   :defaults {:spend {:wall-time 60}}
   :round (fn [_] (swap! world update :rounds (fnil inc 0)) :continue)})

(deftest a-listed-job-waits-on-can-start-with-its-text-and-stops-asking-it-once-started
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'gated gated})
              id (core/submit! eng '(gated) {})]
          (swap! world assoc :run true)
          (await (core/tick! eng))
          (is (nil? (:rounds @world)))
          (is (= ["waiting: torches 0 < 4"] (mapv :message (job-ev seen :waiting))))
          (swap! world assoc :start true)
          (await (core/tick! eng))
          (is (= 1 (:rounds @world)))
          (swap! world assoc :start false)
          (await (core/tick! eng))
          (is (= 2 (:rounds @world)) "can-start? is not asked once started")
          (is (= [id] (:list (core/state eng)))))))))

(deftest can-start-never-ends-impossible-with-the-status-as-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'gated gated})]
          (swap! world assoc :start :never :run true)
          (core/submit! eng '(gated) {})
          (await (core/tick! eng))
          (is (= {:kind :impossible :cause {:reason :gone :item "x" :text "gone for good"} :reason :gone}
                 (select-keys (:data (first (job-ev seen :stopped))) [:kind :cause :reason])))
          (is (= "gone for good" (:message (first (job-ev seen :stopped))))))))))

(deftest can-run-later-yields-a-started-job-until-its-elapsed-budget-ends-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (setup {'gated gated})
              id (core/submit! eng '(gated) {})]
          (swap! world assoc :start true :run true)
          (await (core/tick! eng))
          (swap! world assoc :run false)
          (await (core/tick! eng))
          (await (core/tick! eng))
          (is (= 1 (:rounds @world)) "no round while can-run? says :later")
          (is (= [id] (:list (core/state eng))) "it waits, still listed")
          (is (= {:reason :paused :have 0 :text "torches 0 < 4"} (select-keys (:data (last (job-ev seen :waiting))) [:reason :have :text])))
          (swap! world assoc :run true)
          (await (core/tick! eng))
          (is (= 2 (:rounds @world)) "runs again once can-run? holds")
          (swap! world assoc :run false)
          (swap! clock + 61000)
          (await (core/tick! eng))
          (is (= {:kind :limit-reached :reason :wall-time}
                 (select-keys (:data (first (job-ev seen :stopped))) [:kind :reason]))))))))

(deftest can-run-never-ends-a-started-job-impossible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'gated gated})]
          (swap! world assoc :start true :run true)
          (core/submit! eng '(gated) {})
          (await (core/tick! eng))
          (swap! world assoc :run :never)
          (await (core/tick! eng))
          (is (= 1 (:rounds @world)))
          (is (= :impossible (:kind (:data (first (job-ev seen :stopped)))))))))))

(def stepper
  "Steps until :steps-left runs out; can-run? reads :run."
  {:can-run? (gate :run)
   :round (fn ^:async stepper [c]
            (await (pace/steps! c (fn []
                                    (swap! world update :steps (fnil inc 0))
                                    (when (= 1 (:steps @world)) (swap! world assoc :run (:then @world)))
                                    :again))))})

(deftest can-run-is-asked-between-steps
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'stepper stepper})
              id (core/submit! eng '(stepper) {})]
          (swap! world assoc :run true :then false)
          (await (core/tick! eng))
          (is (= 1 (:steps @world)) "a :later after the first step yields the round")
          (is (= [id] (:list (core/state eng))))
          (swap! world assoc :run true :then :never :steps 0)
          (await (core/tick! eng))
          (is (= 1 (:steps @world)))
          (is (= {:kind :impossible :reason :gone} (select-keys (:data (first (job-ev seen :stopped))) [:kind :reason]))))))))

(deftest a-called-child-whose-can-start-says-later-ends-condition-failed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {'parent {:can-run? ok
                                             :round (fn ^:async parent [c]
                                                      (swap! world assoc :child (await (ctx/call c :kid gated {})))
                                                      (ctx/end c :done nil))}})]
          (swap! world assoc :run true)
          (core/submit! eng '(parent) {})
          (await (core/tick! eng))
          (is (= {:kind :condition-failed :cause {:reason :paused :have 0 :text "torches 0 < 4"}}
                 (select-keys (:child @world) [:kind :cause]))))))))

(deftest a-combinator-passes-its-childs-status-on-with-its-slot
  (let [c {:view (constantly {:data mem/empty-data :now 0}) :slots []}
        kid [{:can-start? (gate :start) :round #(ctx/end % :done nil)} {}]]
    (reset! world {})
    (with-redefs [ctx/mem (constantly {})]
      (is (= [:later "torches 0 < 4" {:reason :paused :have 0 :slot :c0}]
             (ctx/check-status c (composite/seq-def [kid]))))
      (is (= :c0 (:slot (nth (ctx/check-status c (composite/repeat-def kid nil)) 2)))))))

(defn looks-at [p] (count (filter #(= "look" (.-name %)) (.-calls (.-world p)))))

(defn kinds-job
  "A job whose can-start? asks for a look and whose can-run? says status k."
  [k]
  {:can-start? (constantly (contract/look :unseen "unseen"))
   :can-run? (constantly (case k :later (contract/later :paused "paused") :never (contract/never :gone "gone")))
   :round (fn [c] (ctx/end c :done nil))})

(deftest a-look-is-not-taken-when-another-status-is-never-or-later
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {'later-look (kinds-job :later) 'never-look (kinds-job :never)})]
          (core/submit! eng '(later-look) {})
          (await (core/tick! eng))
          (is (zero? (looks-at p)))
          (is (= [:paused] (mapv #(:reason (:data %)) (job-ev seen :waiting))))
          (core/submit! eng '(never-look) {})
          (await (core/tick! eng))
          (await (core/tick! eng))
          (is (zero? (looks-at p)))
          (is (= :impossible (:kind (:data (first (job-ev seen :stopped)))))))))))

(deftest a-goal-that-holds-ends-done-already-even-when-can-start-says-never
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'held {:can-start? (constantly (contract/never :not-yet "no")) :done? (constantly (contract/ok :done "done" :units :items :done 0 :left 0))
                                                :round (fn [c] (ctx/end c :done nil))}})]
          (core/submit! eng '(held) {})
          (await (core/tick! eng))
          (is (= [:already] (mapv #(:cause (:data %)) (job-ev seen :completed)))))))))

(deftest can-run-is-asked-once-per-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {'counted {:can-run? (fn [_] (swap! world update :asks (fnil inc 0)) (contract/ok :go "go"))
                                              :round (fn [_] :continue)}})]
          (core/submit! eng '(counted) {})
          (await (core/tick! eng))
          (is (= 1 (:asks @world)) "the first round")
          (await (core/tick! eng))
          (is (= 2 (:asks @world)) "a started job's next round"))))))

(deftest a-called-child-whose-can-run-turns-later-ends-condition-failed-for-its-parent
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'parent {:can-run? ok
                                                  :round (fn ^:async parent [c]
                                                           (swap! world assoc :child (await (ctx/call c :kid gated {})))
                                                           :continue)}})]
          (swap! world assoc :start true :run true)
          (core/submit! eng '(parent) {})
          (await (core/tick! eng))
          (swap! world assoc :run false)
          (await (core/tick! eng))
          (is (= :condition-failed (:kind (:child @world))))
          (is (empty? (job-ev seen :waiting))))))))

(deftest a-reflex-calling-a-child-that-waits-on-can-run-gets-the-condition-failed-ending-and-its-tick-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {'parent {:can-run? ok
                                             :round (fn ^:async parent [c]
                                                      (loop []
                                                        (let [r (await (ctx/call c :kid gated {}))]
                                                          (if (= :continue r)
                                                            (do (await (pace/pace! c)) (recur))
                                                            (do (swap! world assoc :child r) (ctx/end c :done nil))))))}}
                                   {:wait {:name :wait :job '(parent) :persistence :retry :when (constantly true)}})]
          (swap! world assoc :start true :run false)
          (core/register-reflex! eng {:trigger :wait})
          (let [ticked (await (js/Promise.race #js [(.then (core/tick! eng) (constantly true))
                                                    (js/Promise. (fn [res _] (js/setTimeout #(res false) 3000)))]))]
            (is ticked "the reflex round ended instead of spinning on its child's wait")
            (is (= :condition-failed (:kind (:child @world))))))))))

(deftest a-can-start-never-bad-args-beats-a-goal-that-holds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'bad {:can-start? (constantly (contract/never :bad-args "count 0"))
                                               :done? (constantly (contract/ok :done "done" :units :items :done 0 :left 0))
                                               :round (fn [c] (ctx/end c :done nil))}})]
          (core/submit! eng '(bad) {})
          (await (core/tick! eng))
          (is (= [:impossible] (mapv #(:kind (:data %)) (job-ev seen :stopped))))
          (is (empty? (job-ev seen :completed))))))))

(deftest a-job-refused-for-bad-args-starts-no-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'bad {:can-start? (constantly (contract/never :bad-args "count 0"))
                                               :round (fn [c] (ctx/end c :done nil))}})]
          (core/submit! eng '(bad) {})
          (await (core/tick! eng))
          (is (= 1 (count (job-ev seen :stopped))) "refused with its reason")
          (is (empty? (job-ev seen :round_started))))))))

(def probe
  {:can-run? (fn [c] (swap! world update :asks (fnil conj []) (:n (:args c)))
               (if (get @world :run) (contract/ok :go "go") (contract/later :paused "paused")))
   :round (fn [_] (swap! world update :rounds (fnil inc 0)) :continue)})

(defn asker
  "A parent whose round does what the world's :step says: ask the child's status, or call the child with args {:n n}."
  []
  {:can-run? ok
   :round (fn ^:async parent [c]
            (let [{:keys [step n]} @world]
              (if (= :ask step)
                (ctx/child-status c :kid 'probe {:n n})
                (await (ctx/call c :kid 'probe {:n n})))
              :continue))})

(deftest a-child-status-answer-is-not-reused-by-a-call-in-a-later-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {'asker (asker) 'probe probe})]
          (swap! world assoc :run true :step :ask :n 1)
          (core/submit! eng '(asker) {})
          (await (core/tick! eng))
          (swap! world assoc :run false :step :call)
          (await (core/tick! eng))
          (is (nil? (:rounds @world)) "the call asked can-run? again and found it paused"))))))

(deftest a-child-status-answer-is-not-reused-by-a-call-with-other-args
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {'asker (asker) 'probe probe})
              asks (fn [] (:asks @world))]
          (swap! world assoc :run true :step :ask :n 1)
          (core/submit! eng '(asker) {})
          (await (core/tick! eng))
          (is (= [1] (asks)))
          (swap! world assoc :step :call :n 2)
          (await (core/tick! eng))
          (is (= 2 (last (asks))) "the call asked can-run? under its own args"))))))

(deftest a-child-status-of-a-registry-child-passes-its-never-and-look-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [kid (fn [status] {:can-start? (constantly status) :round (fn [c] (ctx/end c :done nil))})
              parent {:can-run? ok
                      :round (fn [c]
                               (swap! world assoc :answers (mapv #(tu/kind-reason (ctx/child-status c :kid % {})) ['never-kid 'look-kid 'later-kid 'ok-kid]))
                               (ctx/end c :done nil))}
              {:keys [eng]} (setup {'parent parent
                                    'never-kid (kid (contract/never :gone "gone"))
                                    'look-kid (kid (contract/look :unseen "unseen"))
                                    'later-kid (kid (contract/later :paused "paused"))
                                    'ok-kid (kid (contract/ok :go "go"))})]
          (core/submit! eng '(parent) {})
          (await (core/tick! eng))
          (is (= [[:never :gone] [:look :unseen] [:later :paused] [:ok :ready]] (:answers @world))))))))
