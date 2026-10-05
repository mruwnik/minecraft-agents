(ns engine.offline-test
  "Offline is body state: while the body is away the engine pauses the register
  and the list, a cut ends the wait and brings the body back first."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as real-triggers]))

(def always (constantly true))

(defn ^:async away-round [c]
  (await (ctx/act c :offline #js {:ms 20000}))
  :done)

(defn ^:async eat-round [c]
  (await (ctx/act c :eat #js {}))
  :done)

(def probes (atom 0))

(def jobs
  {'away {:check always :round away-round}
   'eat {:check always :round eat-round}})

(def triggers
  {:probe {:name :probe :job '(eat) :when (fn [_ _ _] (swap! probes inc) false)}
   :hurt {:name :hurt :job '(eat) :persistence :retry :when (fn [w _ _] (<= (.-health (.self w)) 8))}})

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake world)
        eng (core/create {:primitives p :jobs jobs :triggers triggers :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn kinds-of [seen kind] (filterv #(= kind (:kind %)) @seen))

(defn ^:async tick-until-offline [eng p]
  (let [round (core/tick! eng)]
    (await (js/Promise. (fn [resolve] (js/setTimeout resolve 5))))
    (is (true? (.isOffline p)) "the body is away")
    {:round round}))

(deftest the-register-is-not-evaluated-while-the-body-is-offline
  (async done
    (tu/run-async done
      (fn ^:async t []
        (reset! probes 0)
        (let [{:keys [eng p seen]} (setup {:offlineScale 0.01 :self {:health 5}})]
          (core/submit! eng '(away) {})
          (core/register-reflex! eng {:trigger :probe})
          (let [{:keys [round]} (await (tick-until-offline eng p))
                before @probes]
            (is (= [nil nil nil] [(core/tick! eng) (core/tick! eng) (core/tick! eng)]) "the scheduler just waits")
            (is (= before @probes) "no trigger evaluated while away")
            (is (= [] (kinds-of seen :fired)) "no reflex fired")
            (await round)
            (is (false? (.isOffline p)))
            (is (< before (do (core/tick! eng) @probes)) "evaluation resumes once the body is back")))))))

(deftest a-cutting-reflex-waits-for-the-body-to-be-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:offlineScale 0.01 :self {:health 5}})]
          (core/submit! eng '(away) {})
          (let [{:keys [round]} (await (tick-until-offline eng p))]
            (core/register-reflex! eng {:trigger :hurt})
            (core/tick! eng)
            (is (= [] (kinds-of seen :fired)) "the hurt reflex does not fire while away")
            (await round)
            (is (empty? (filter #(= "eat" (.-name %)) (.-calls (.-world p)))) "nothing acted yet")
            (await (core/tick! eng))
            (is (= 1 (count (kinds-of seen :fired))) "after the return the reflex fires")))))))

(deftest a-cut-during-offline-reconnects-before-the-engine-ticks-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:offlineScale 0.5})
              started (js/Date.now)]
          (core/submit! eng '(away) {})
          (let [{:keys [round]} (await (tick-until-offline eng p))]
            (core/cancel! eng "j1")
            (is (true? (.isOffline p)) "still away until the reconnect is done")
            (is (nil? (core/tick! eng)) "no round starts mid-reconnect")
            (await round)
            (is (false? (.isOffline p)))
            (is (< (- (js/Date.now) started) 2000) "the wait ended early (a full wait is 10 s)")))))))

(def sleeper {:id 5 :name "Alex" :kind "player" :sleeping true :pos {:x 10 :y 64 :z 0}})

(deftest logging-out-for-the-night-works-end-to-end
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              [seen sink] (tu/legacy-capture-sink)
              p (tu/fake {:time 14000 :entities [sleeper] :offlineScale 0.01})
              eng (core/create {:primitives p :jobs registry/jobs :triggers real-triggers/all :dir (tu/tmp-dir)
                                :now #(deref clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (core/register-reflex! eng {:trigger :player-sleeping-nearby})
          (let [round (core/tick! eng)]
            (await (js/Promise. (fn [resolve] (js/setTimeout resolve 20))))
            (is (true? (.isOffline p)) "the log-out reflex took the body offline")
            (is (nil? (core/tick! eng)) "the register waits")
            (is (= 1 (count (kinds-of seen :fired))))
            (await round))
          (is (false? (.isOffline p)))
          (is (= "Fake" (.-username (.self p))) "fresh sensing after the return")
          (swap! clock + 5000)
          (await (core/tick! eng))
          (is (= 1 (count (kinds-of seen :fired))) "right after the return the log-out is too recent to fire again")
          (swap! clock + 31000)
          (await (core/tick! eng))
          (is (= 2 (count (kinds-of seen :fired))) "after the 30 s cooldown the sleeper is still asleep at night, so the reflex fires again"))))))

(deftest an-act-answering-offline-cuts-the-round-and-the-job-resumes-once-the-body-is-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {})
              world (.-world p)]
          (.override world "eat" (fn [_ _ _] (js/Promise.resolve #js {:status "offline"})))
          (core/submit! eng '(eat) {})
          (await (core/tick! eng))
          (is (= 1 (count (kinds-of seen :cut))) "the round is cut, not failed or done")
          (is (= [] (kinds-of seen :completed)))
          (is (= 1 (count (:list (core/state eng)))) "the job stays listed")
          (.override world "eat" nil)
          (await (core/tick! eng))
          (is (= 1 (count (kinds-of seen :completed))) "the next round runs it to the end"))))))
