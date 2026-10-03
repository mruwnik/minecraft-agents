(ns engine.offline-survival-test
  "An offline body answers self() with just {status: 'offline'}: events, memory
  writes and tick failures must not choke on the missing pos."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.test-util :as tu]))

(defn ^:async away-round [c]
  (await (ctx/act c :offline #js {:ms 20000}))
  :done)

(def jobs {'away {:check (constantly true) :round away-round}})

(defn engine-with-sink [p clock]
  (let [[seen sink] (tu/legacy-capture-sink)
        eng (core/create {:primitives p :jobs jobs :triggers {} :dir (tu/tmp-dir) :now #(deref clock)
                          :body "Fake"})]
    (swap! (:events eng) update :sinks conj sink)
    {:eng eng :seen seen}))

(defn ^:async go-offline [eng p]
  (let [round (core/tick! eng)]
    (await (js/Promise. (fn [resolve] (js/setTimeout resolve 5))))
    (is (true? (.isOffline p)))
    {:round round}))

(deftest self-pos-is-nil-for-the-offline-self
  (is (nil? (core/self-pos #js {:self (fn [] #js {:status "offline"})}))))

(deftest the-fake-offline-self-is-exactly-the-status
  (let [p (tu/fake {:offlineScale 0.01})]
    (set! (.-offline (.-state (.-world p))) true)
    (is (= {"status" "offline"} (js->clj (.self p))))))

(deftest an-event-emitted-while-offline-has-no-pos
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              {:keys [eng p seen]} (let [p (tu/fake {:offlineScale 0.01})] (assoc (engine-with-sink p clock) :p p))]
          (core/submit! eng '(away) {})
          (let [{:keys [round]} (await (go-offline eng p))]
            (is (number? (events/emit! (:events eng) {:source :system :kind :probe :level :info})))
            (is (nil? (:pos (last @seen))) "no pos while away")
            (await round)))))))

(deftest a-tick-and-a-tick-failure-report-while-offline-do-not-throw
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              p (tu/fake {:offlineScale 0.01})
              {:keys [eng seen]} (engine-with-sink p clock)]
          (core/submit! eng '(away) {})
          (let [{:keys [round]} (await (go-offline eng p))]
            (is (nil? (core/tick! eng)))
            (core/report-tick-failure! eng (js/Error. "boom"))
            (is (= :error (:kind (last @seen))))
            (is (nil? (:pos (last @seen))))
            (await round)))))))

(deftest memory-written-while-offline-has-no-world-time-and-does-not-throw
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              p (tu/fake {:offlineScale 0.01 :time 5000})
              {:keys [eng]} (engine-with-sink p clock)]
          (core/submit! eng '(away) {})
          (let [{:keys [round]} (await (go-offline eng p))
                entry (mem/write! (:store eng) :probe {:x 1})]
            (is (nil? (:wt entry)))
            (await round)
            (is (= 5000 (:wt (mem/write! (:store eng) :probe {:x 2}))) "world time is back after the return")))))))

(deftest the-engine-survives-a-job-going-offline-and-resumes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              p (tu/fake {:offlineScale 0.01})
              {:keys [eng seen]} (engine-with-sink p clock)]
          (core/submit! eng '(away) {})
          (let [{:keys [round]} (await (go-offline eng p))]
            (core/report-tick-failure! eng (js/Error. "mid-offline"))
            (await round)
            (is (false? (.isOffline p)))
            (is (some? (.-pos (.self p))) "sensing is back")
            (is (number? (events/emit! (:events eng) {:source :system :kind :probe :level :info})))
            (is (some? (:pos (last @seen))) "events carry a pos again")))))))
