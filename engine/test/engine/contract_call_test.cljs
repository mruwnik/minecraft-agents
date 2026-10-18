(ns engine.contract-call-test
  "A call equal to one of its ancestors' (same job, same target args) is refused: condition-failed :duplicate-call,
  booked nowhere as a failure."
  (:require [cljs.test :refer [deftest is async]]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [jobs.lib.need :as need]))

(def endings (atom []))

(defn setup [jobs]
  (reset! endings [])
  (let [[seen sink] (tu/capture-sink)
        eng (core/create {:primitives (tu/fake {}) :dir (tu/tmp-dir) :now (constantly 1000000) :jobs jobs :triggers {}
                          :events (events/make {:body "Fake" :sinks [sink] :now (constantly 1000000)})})]
    {:eng eng :seen seen}))

(defn ^:async call-noting [c slot sym args]
  (let [e (await (ctx/call c slot sym args))]
    (swap! endings conj [sym args e])
    e))

(def jobs
  {'ping {:target [:pos] :args {:pos {:doc "target" :default nil} :again {:doc "calls" :default nil}}
          :round (fn ^:async ping [c]
                   (when-let [again (:again (:args c))]
                     (await (call-noting c :pong 'pong {:again again})))
                   (ctx/end c :done nil))}
   'pong {:args {:again {:doc "calls" :default nil}}
          :round (fn ^:async pong [c]
                   (doseq [pos (:again (:args c))]
                     (await (call-noting c (keyword (str "p" pos)) 'ping {:pos pos})))
                   (ctx/end c :done nil))}})

(deftest a-call-equal-to-an-ancestors-is-refused-and-not-booked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup jobs)]
          (core/submit! eng '(ping {:pos 1 :again [1 2]}) {})
          (await (core/tick! eng))
          (let [[[_ _ dup] [_ _ fine] [_ _ pong-e]] @endings]
            (is (= :condition-failed (:kind dup)) "a loop, not a verdict on the args: the parent tries another way")
            (is (= :duplicate-call (contract/cause-key dup)))
            (is (string? (get-in dup [:cause :of])) "names the ancestor's frame")
            (is (= :done (:kind fine)) "the same job on another target runs")
            (is (= :done (:kind pong-e)) "the refusal is an ending the caller reads, not an error"))
          (is (= 3 (mem/count-in (mem/view (:store eng)) :job-report 1000000))
              "ping 2, pong and the root ping are booked; the refused call is not"))))))

(deftest a-refusal-passed-up-through-need-does-not-end-the-root-impossible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'ping {:target [:pos] :args {:pos {:doc "target" :default nil}}
                                                :round (fn ^:async ping [c]
                                                         (let [e (await (ctx/call c :pong 'pong {}))]
                                                           (if (= :done (:kind e)) (ctx/end c :done nil) e)))}
                                         'pong {:args {}
                                                :round (fn ^:async pong [c]
                                                         (need/ending-for (await (ctx/call c :ping 'ping {:pos 1}))
                                                                          :need {:item "ping"}))}})]
          (core/submit! eng '(ping {:pos 1}) {})
          (await (core/tick! eng))
          (let [stopped (:data (first (filter #(= [:job :stopped] [(:source %) (:kind %)]) @seen)))]
            (is (= :condition-failed (:kind stopped)))
            (is (= {:reason :need :why :duplicate-call :item "ping"} (:cause stopped)))))))))

(deftest a-same-job-chain-on-new-targets-ends-too-deep
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [depth (atom 0)
              {:keys [eng]} (setup {'dive {:target [:pos] :args {:pos {:doc "target" :default 0}}
                                           :round (fn ^:async dive [c]
                                                    (swap! depth inc)
                                                    (let [pos (:pos (:args c))
                                                          _ (await (call-noting c :next 'dive {:pos (inc pos)}))]
                                                      (ctx/end c :done nil)))}})]
          (core/submit! eng '(dive {:pos 0}) {})
          (await (core/tick! eng))
          (let [deepest (first (filter #(= :condition-failed (:kind (nth % 2))) @endings))]
            (is (= 13 @depth) "the root plus 12 nested calls run; the 13th is refused")
            (is (= :too-deep (contract/cause-key (nth deepest 2))))))))))

(def knob-specs (a/declare-settings! {:jobs.callscope/knob {:default 7 :doc "A knob." :spec (a/int-in 0 nil)}}))
(def knob-seen (atom []))
(defn read-knob! [tag] (swap! knob-seen conj [tag (settings/get knob-specs :jobs.callscope/knob)]))

(def knob-jobs
  {'outer {:round (fn ^:async outer [c]
                    (read-knob! :outer-before)
                    (await (ctx/call c :a 'inner {}))
                    (await (ctx/call c :b 'inner {:settings {:jobs.callscope/knob 3}}))
                    (read-knob! :outer-after)
                    (ctx/end c :done nil))}
   'inner {:round (fn ^:async inner [c]
                    (await (js/Promise. (fn [ok] (js/setTimeout ok 1))))
                    (read-knob! :inner)
                    (ctx/end c :done nil))}})

(deftest a-calls-settings-reach-its-round-and-its-children-only
  (async done
    (tu/run-async done
      (fn ^:async t []
        (reset! knob-seen [])
        (let [{:keys [eng]} (setup knob-jobs)]
          (core/submit! eng '(outer {:settings {:jobs.callscope/knob 5}}) {})
          (await (core/tick! eng))
          (is (= [[:outer-before 5] [:inner 5] [:inner 3] [:outer-after 5]] @knob-seen)
              "the caller's override flows to a child, the child's own wins below it")
          (is (= 7 (settings/get knob-specs :jobs.callscope/knob)) "and ends with the run"))))))

(deftest a-settings-call-key-must-name-a-declared-setting-with-a-fitting-value
  (is (nil? (contract/call-problem 'x nil {:settings {:jobs.callscope/knob 3}})))
  (is (some? (contract/call-problem 'x nil {:settings {:jobs.callscope/knob -1}})))
  (is (some? (contract/call-problem 'x nil {:settings {:jobs.callscope/nope 1}}))))
