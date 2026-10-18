(ns engine.contract-cleanup-test
  "A job's cleanup export (contract-conditions design 2.4): run once however the job ends, before the caller sees the
  ending, on its own budget; a cancel or a parent's end runs it innermost frame first; a death runs none."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.test-util :as tu]))

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
(defn log! [x] (swap! world update :log (fnil conj []) x))
(defn looks [p] (count (filter #(= "look" (.-name %)) (.-calls (.-world p)))))
(defn look! [c] (ctx/act c :look #js {:yaw 0 :pitch 0}))

(def tidy
  {:round (fn [c] (log! :round) (ctx/end c :done nil {:a 1}))
   :cleanup (fn [c e]
              (log! [:cleanup (:kind e)])
              (swap! clock + 2000)
              (ctx/end c :done nil {:still-led false}))})

(deftest cleanup-runs-once-after-a-done-ending-and-its-data-and-time-join-the-jobs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'tidy tidy})]
          (core/submit! eng '(tidy) {})
          (await (core/tick! eng))
          (await (core/tick! eng))
          (is (= [:round [:cleanup :done]] (:log @world)))
          (let [d (:data (first (job-ev seen :completed)))]
            (is (= {:a 1 :still-led false :kind :done} (select-keys d [:a :still-led :kind])))
            (is (= 2 (get-in (first (job-ev seen :completed)) [:data :report :uses :job-time]))
                "cleanup's time is in the job's report")))))))

(def stuck
  {:round (fn [c] (ctx/end c :nothing-possible :stuck))
   :cleanup (fn [c e]
              (log! [:cleanup (:kind e) (:cause e)])
              (if (< (:n (swap! world update :n (fnil inc 0))) 3) :continue (ctx/end c :done nil {:let-go true})))})

(def parent
  {:round (fn ^:async parent [c]
            (let [e (await (ctx/call c :kid 'stuck {}))]
              (if (= :continue e)
                (do (log! :parent-continue) :continue)
                (do (swap! world assoc :kid e) (ctx/end c :done nil)))))})

(deftest a-childs-cleanup-runs-before-its-parent-sees-the-ending-and-may-continue
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {'stuck stuck 'parent parent})]
          (core/submit! eng '(parent) {})
          (dotimes [_ 4] (await (core/tick! eng)))
          (is (= [[:cleanup :nothing-possible :stuck] :parent-continue
                  [:cleanup :nothing-possible :stuck] :parent-continue
                  [:cleanup :nothing-possible :stuck]]
                 (:log @world)))
          (is (= {:kind :nothing-possible :cause :stuck :data {:let-go true}}
                 (select-keys (:kid @world) [:kind :cause :data]))))))))

(def fails
  {:round (fn [c] (ctx/end c :done nil))
   :cleanup (fn [c _] (ctx/end c :nothing-possible :gate-stuck {:gate 1}))})

(deftest a-failed-cleanup-keeps-the-ending-and-is-told
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'fails fails})]
          (core/submit! eng '(fails) {})
          (await (core/tick! eng))
          (is (= {:kind :done :cleanup {:kind :nothing-possible :cause :gate-stuck}}
                 (select-keys (:data (first (job-ev seen :completed))) [:kind :cleanup :gate])))
          (is (= [{:kind :nothing-possible :cause :gate-stuck}]
                 (mapv #(select-keys (:data %) [:kind :cause]) (job-ev seen :cleanup_failed)))))))))

(def overruns
  {:cleanup-defaults {:spend {:job-time 1}}
   :round (fn [c] (ctx/end c :nothing-possible :stuck))
   :cleanup (fn [_ _] (log! :cleanup) (swap! clock + 2000) :continue)})

(deftest a-cleanup-past-its-own-budget-ends-and-the-ending-stays
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'overruns overruns})]
          (core/submit! eng '(overruns) {})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= [:cleanup] (:log @world)))
          (is (= {:kind :nothing-possible :cause :stuck :cleanup {:kind :limit-reached :cause {:line :job-time :spend 1}}}
                 (select-keys (:data (first (job-ev seen :stopped))) [:kind :cause :cleanup])))
          (is (= 1 (count (job-ev seen :cleanup_failed)))))))))

(def slow
  {:defaults {:spend {:job-time 5}}
   :round (fn [_] (swap! clock + 6000) :continue)
   :cleanup (fn ^:async cleanup [c e]
              (await (look! c))
              (log! [:slow-cleanup (:kind e) (:cause e)])
              (ctx/end c :done nil))})

(deftest the-jobs-own-spent-budget-does-not-stop-its-cleanup
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup {'slow slow})]
          (core/submit! eng '(slow) {})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= [[:slow-cleanup :limit-reached {:line :job-time :spend 5}]] (:log @world)))
          (is (= 1 (looks p)) "cleanup acts though the job's time is spent")
          (is (= :limit-reached (:kind (:data (first (job-ev seen :stopped)))))))))))

(def boss
  {:defaults {:spend {:job-time 5}}
   :round (fn ^:async boss [c]
            (let [e (await (ctx/call c :kid 'runner {}))]
              (if (= :continue e) :continue (ctx/end c :done nil))))
   :cleanup (fn [c e] (log! [:boss-cleanup (:kind e)]) (ctx/end c :done nil))})

(def runner
  {:round (fn [_] (swap! clock + 6000) :continue)
   :cleanup (fn [c e] (log! [:runner-cleanup (:kind e)]) (ctx/end c :done nil))})

(deftest a-parents-end-runs-the-cleanups-innermost-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'boss boss 'runner runner})]
          (core/submit! eng '(boss) {})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= [[:runner-cleanup :cut-off] [:boss-cleanup :limit-reached]] (:log @world)))
          (is (= :limit-reached (:kind (:data (first (job-ev seen :stopped)))))))))))

(def endless
  {:round (fn [_] (log! :round) :continue)
   :cleanup (fn [c e]
              (log! [:cleanup (:kind e) (:cause e)])
              (if (= 2 (count (filter vector? (:log @world)))) (ctx/end c :done nil) :continue))})

(deftest a-cancel-runs-cleanup-before-the-job-goes-and-a-second-cancel-does-not-cut-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'endless endless})
              id (core/submit! eng '(endless) {})]
          (await (core/tick! eng))
          (core/cancel! eng id)
          (is (= [] (job-ev seen :cancelled)) "not gone before its cleanup")
          (await (core/tick! eng))
          (core/cancel! eng id)
          (await (core/tick! eng))
          (await (core/tick! eng))
          (is (= [:round [:cleanup :cut-off :agent] [:cleanup :cut-off :agent]] (:log @world)))
          (is (= 1 (count (job-ev seen :cancelled))))
          (is (= [] (:list (core/state eng)))))))))

(def self-cancelling
  {:round (fn [_] (log! :round) :continue)
   :cleanup (fn [c _]
              (log! :cleanup)
              (core/cancel! (:eng @world) (:job @world))
              (ctx/end c :done nil))})

(deftest a-cancel-landing-in-a-cleanup-round-that-ends-it-still-ends-the-job-cancelled
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'self-cancelling self-cancelling})
              id (core/submit! eng '(self-cancelling) {})]
          (swap! world assoc :eng eng :job id)
          (await (core/tick! eng))
          (core/cancel! eng id)
          (dotimes [_ 4] (await (core/tick! eng)))
          (is (= [:round :cleanup] (:log @world)))
          (is (= 1 (count (job-ev seen :cancelled))))
          (is (= [] (:list (core/state eng)))))))))

(deftest a-death-runs-no-cleanup
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {'endless endless})
              id (core/submit! eng '(endless) {})]
          (await (core/tick! eng))
          (core/cancel! eng id :death)
          (await (core/tick! eng))
          (is (= [:round] (:log @world)))
          (is (= [] (:list (core/state eng)))))))))

(deftest a-round-that-throws-still-runs-its-cleanup-and-the-job-is-parked-failed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {'boom {:round (fn [_] (log! :round) (throw (js/Error. "boom")))
                                           :cleanup (fn [c e] (log! [:cleanup (:kind e) (get-in e [:cause :reason])])
                                                      (ctx/end c :done nil {}))}})
              id (core/submit! eng '(boom) {})]
          (await (core/tick! eng))
          (is (= [:round [:cleanup :nothing-possible :error]] (:log @world)))
          (is (= "Error: boom" (get-in (core/state eng) [:failed id :error]))))))))

(defn setup-reflex [jobs job]
  (reset! world {})
  (reset! clock 1000000)
  (let [[seen sink] (tu/capture-sink)
        eng (core/create {:primitives (tu/fake {}) :dir (tu/tmp-dir) :now #(deref clock) :jobs jobs
                          :triggers {:go {:name :go :job job :when (fn [_ _ _] (let [first? (not (:fired @world))] (swap! world assoc :fired true) first?))}}
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/register-reflex! eng {:trigger :go})
    {:eng eng :seen seen}))

(def stuck-thrice (assoc stuck :cleanup (fn [c e]
                                          (log! [:cleanup (:kind e)])
                                          (if (< (count (:log @world)) 3) :continue (ctx/end c :done nil {:let-go true})))))

(deftest a-reflex-roots-cleanup-runs-to-its-end-in-its-one-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup-reflex {'stuck stuck-thrice} '(stuck))]
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= [[:cleanup :nothing-possible] [:cleanup :nothing-possible] [:cleanup :nothing-possible]] (:log @world)))
          (is (= [{:kind :nothing-possible :let-go true}] (mapv #(select-keys (:data %) [:kind :let-go]) (job-ev seen :stopped)))))))))

(def waits-for-the-world
  {:round (fn [c] (ctx/end c :nothing-possible :stuck))
   :cleanup (fn [c _]
              (swap! clock + 1000)
              (when-not (:timer @world)
                (swap! world assoc :timer true)
                (js/setTimeout #(swap! world assoc :freed true) 0))
              (if (:freed @world) (ctx/end c :done nil {:let-go true}) :continue))})

(deftest a-drained-cleanup-lets-the-world-change-between-its-rounds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup-reflex {'waits waits-for-the-world} '(waits))]
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= [{:kind :nothing-possible :let-go true}]
                 (mapv #(select-keys (:data %) [:kind :let-go :cleanup]) (job-ev seen :stopped)))))))))

(def yields
  {:round (fn [_] (log! :round) :continue)
   :cleanup (fn [c e]
              (log! [:cleanup (:kind e)])
              (if (< (count (:log @world)) 3) :continue (ctx/end c :done nil)))})

(deftest an-exclusive-run-that-leaves-the-slot-runs-its-whole-cleanup
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {'yields yields})]
          (core/exclusive-run! eng '(yields))
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= [:round [:cleanup :cut-off] [:cleanup :cut-off]] (:log @world)))
          (is (= [] (:list (core/state eng)))))))))

(defn ends-slot-mid-cleanup [end!]
  {:round (fn [_] (log! :round) :continue)
   :cleanup (fn [c e]
              (log! [:cleanup (:kind e)])
              (when (= 2 (count (:log @world))) (end! (:engine c) (:root c)))
              (if (< (count (:log @world)) 4) :continue (ctx/end c :done nil)))})

(defn slot-cleanup-ends-with [end!]
  (fn ^:async t []
    (let [{:keys [eng]} (setup {'yields (ends-slot-mid-cleanup end!)})]
      (core/exclusive-run! eng '(yields))
      (dotimes [_ 4] (await (core/tick! eng)))
      (is (= [:round [:cleanup :cut-off] [:cleanup :cut-off] [:cleanup :cut-off]] (:log @world)))
      (is (= [] (:list (core/state eng)))))))

(deftest cancelling-the-exclusive-run-mid-cleanup-does-not-cut-its-cleanup
  (async done (tu/run-async done (slot-cleanup-ends-with (fn [eng id] (core/cancel! eng id :agent))))))

(def paused-low
  {:round (fn [c] (ctx/end c :nothing-possible :stuck))
   :cleanup (fn [c e]
              (log! [:low-cleanup (:kind e)])
              (when (= 1 (count (:log @world)))
                (swap! world assoc :alarm true)
                (js/setTimeout #(core/tick! (:engine c)) 0))
              (if (< (count (:log @world)) 4) :continue (ctx/end c :done nil {:let-go true})))})

(def high {:round (fn [c] (log! :high) (ctx/end c :done nil))})

(deftest a-reflex-cut-by-a-higher-one-mid-cleanup-resumes-its-cleanup-after
  (async done
    (tu/run-async done
      (fn ^:async t []
        (reset! world {})
        (reset! clock 1000000)
        (let [[seen sink] (tu/capture-sink)
              once (fn [k pred] (fn [_ _ _] (let [go? (and (pred) (not (k @world)))] (when go? (swap! world assoc k true)) go?)))
              eng (core/create {:primitives (tu/fake {}) :dir (tu/tmp-dir) :now #(deref clock) :jobs {'low paused-low 'high high}
                                :triggers {:high {:name :high :job '(high) :when (once :high-fired #(:alarm @world))}
                                           :low {:name :low :job '(low) :when (once :low-fired (constantly true))}}
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (core/register-reflex! eng {:trigger :high})
          (core/register-reflex! eng {:trigger :low})
          (dotimes [_ 4] (await (core/tick! eng)))
          (is (= [[:low-cleanup :nothing-possible] :high [:low-cleanup :nothing-possible] [:low-cleanup :nothing-possible]]
                 (:log @world)))
          (is (= [[:high :done] [:low :stopped]]
                 (mapv (juxt (comp :reflex-id :context) (comp :outcome :data)) (filter #(= [:reflex :ended] [(:source %) (:kind %)]) @seen)))))))))

(def high-removing-low
  {:round (fn [c] (log! :high) (core/remove-reflex! (:engine c) :low) (ctx/end c :done nil))})

(deftest a-paused-reflex-whose-entry-left-the-register-still-resumes-its-cleanup
  (async done
    (tu/run-async done
      (fn ^:async t []
        (reset! world {})
        (reset! clock 1000000)
        (let [once (fn [k pred] (fn [_ _ _] (let [go? (and (pred) (not (k @world)))] (when go? (swap! world assoc k true)) go?)))
              eng (core/create {:primitives (tu/fake {}) :dir (tu/tmp-dir) :now #(deref clock) :jobs {'low paused-low 'high high-removing-low}
                                :triggers {:high {:name :high :job '(high) :when (once :high-fired #(:alarm @world))}
                                           :low {:name :low :job '(low) :when (once :low-fired (constantly true))}}
                                :events (events/make {:body "Fake" :sinks [(second (tu/capture-sink))] :now #(deref clock)})})]
          (core/register-reflex! eng {:trigger :high})
          (core/register-reflex! eng {:trigger :low})
          (dotimes [_ 4] (await (core/tick! eng)))
          (is (= [[:low-cleanup :nothing-possible] :high [:low-cleanup :nothing-possible] [:low-cleanup :nothing-possible]]
                 (:log @world))))))))

(deftest a-paused-reflex-whose-trigger-still-holds-resumes-instead-of-firing-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (reset! world {})
        (reset! clock 1000000)
        (let [[seen sink] (tu/capture-sink)
              once (fn [k pred] (fn [_ _ _] (let [go? (and (pred) (not (k @world)))] (when go? (swap! world assoc k true)) go?)))
              eng (core/create {:primitives (tu/fake {}) :dir (tu/tmp-dir) :now #(deref clock) :jobs {'low paused-low 'high high}
                                :triggers {:high {:name :high :job '(high) :when (once :high-fired #(:alarm @world))}
                                           :low {:name :low :job '(low) :when (fn [_ _ _] (< (count (:log @world)) 4))}}
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (core/register-reflex! eng {:trigger :high})
          (core/register-reflex! eng {:trigger :low})
          (dotimes [_ 4] (await (core/tick! eng)))
          (is (= 1 (count (filter #(and (= [:reflex :fired] [(:source %) (:kind %)]) (= :low (-> % :context :reflex-id))) @seen)))))))))
