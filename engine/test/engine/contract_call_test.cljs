(ns engine.contract-call-test
  "A call equal to one of its ancestors' (same job, same target args) is refused: impossible :duplicate-call,
  booked nowhere as a failure."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.test-util :as tu]))

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
  {'ping {:conditions [] :target [:pos] :args {:pos {:doc "target" :default nil} :again {:doc "calls" :default nil}}
          :round (fn ^:async ping [c]
                   (when-let [again (:again (:args c))]
                     (await (call-noting c :pong 'pong {:again again})))
                   (ctx/end c :done nil))}
   'pong {:conditions [] :args {:again {:doc "calls" :default nil}}
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
            (is (= :impossible (:kind dup)))
            (is (= :duplicate-call (contract/cause-key dup)))
            (is (string? (get-in dup [:cause :of])) "names the ancestor's frame")
            (is (= :done (:kind fine)) "the same job on another target runs")
            (is (= :done (:kind pong-e)) "the refusal is an ending the caller reads, not an error"))
          (is (= 3 (mem/count-in (mem/view (:store eng)) :job-report 1000000))
              "ping 2, pong and the root ping are booked; the refused call is not"))))))
