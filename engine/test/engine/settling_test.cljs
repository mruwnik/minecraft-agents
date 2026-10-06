(ns engine.settling-test
  "Settling: right after a reconnect, respawn or teleport the senses are not
  trustworthy yet. No trigger is evaluated meanwhile, and a reflex job that ends
  then is judged on the first ready tick, its cooldown counted from the job end."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.fake :as fake]
            [jobs.lib.shelter :as sh]
            [engine.test-util :as tu]
            [engine.triggers :as real-triggers]))

(def t0 1000000)

(def sleeper {:id 5 :name "Alex" :kind "player" :sleeping true :pos {:x 10 :y 64 :z 0}})
(def sleeper-js (assoc sleeper :username "Alex"))
(def pest {:id 6 :name "pest" :kind "hostile" :pos {:x 2 :y 64 :z 0}})

(def probes (atom 0))

(defn pest-near? [w] (pos? (.-length (.entities w #js {:names #js ["pest"]}))))

;; a reflex job that ends by respawning: the pest is not sensed yet when it ends
(defn ^:async respawn-round [c]
  (let [world (.-world (:primitives c))]
    (swap! (.-state world) assoc :entities [])
    (.respawn world)
    :done))

(defn ^:async respawn-stopped-round [c]
  (await (respawn-round c))
  (ctx/result! c {:status :stopped :reason :no_land})
  :done)

(def jobs
  (merge registry/jobs
         {'respawn-stopped {:check (constantly true) :round respawn-stopped-round}
          'respawn-away {:check (constantly true) :round respawn-round}
          'nop {:check (constantly true) :round (fn [_] :done)}}))

(def triggers
  (merge real-triggers/all
         {:pest-stops {:name :pest-stops :job '(respawn-stopped) :persistence :cooldown :cooldown-s 30
                       :when (fn [w _ _] (pest-near? w))}
          :pest {:name :pest :job '(respawn-away) :persistence :cooldown :cooldown-s 30
                 :when (fn [w _ _] (pest-near? w))}
          :probe {:name :probe :job '(nop) :when (fn [_ _ _] (swap! probes inc) false)}
          :sleeper-out {:name :sleeper-out :job '(jobs.survival.log-out {:offline-ms 20000 :news-ms 0}) :persistence :cooldown :cooldown-s 30
                         :when (fn [w _ _] (boolean (seq (sh/sleeping-players w 128))))}}))

(defn setup [spec]
  (let [clock (atom t0)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake spec)
        eng (core/create {:primitives p :jobs jobs :triggers triggers :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :clock clock :seen seen :world (.-world p) :state (.-state (.-world p))}))

(defn kinds-of [seen kind] (filterv #(= kind (:kind %)) @seen))
(defn fired [seen] (count (kinds-of seen :fired)))
(defn pause [ms] (js/Promise. (fn [resolve] (js/setTimeout resolve ms))))

(defn ^:async tick-at [{:keys [eng clock]} t]
  (reset! clock t)
  (await (core/tick! eng)))

(deftest the-log-out-loop-is-broken-by-the-engine-alone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock state] :as r} (setup {:time 14000 :entities [sleeper] :offlineScale 0.01 :settles true})]
          (core/register-reflex! eng {:trigger :sleeper-out})
          (let [round (core/tick! eng)]
            (await (pause 5))
            (is (true? (.isOffline p)) "the log-out took the body away")
            (swap! state assoc :entities [])
            (await round))
          (is (true? (.isSettling p)) "back, but not yet sensing")
          (is (= [] (kinds-of seen :ended)) "no judgement while settling")
          (await (tick-at r (+ t0 100)))
          (is (= 1 (fired seen)) "no re-fire while settling")
          (swap! state assoc :entities [(fake/entity-in sleeper-js)])
          (.settle (.-world p) false)
          (await (tick-at r (+ t0 300)))
          (is (= [:completed_not_cleared] (mapv :how (kinds-of seen :ended))) "judged on the first ready tick")
          (is (= [300] (mapv :deferred-ms (kinds-of seen :ended))))
          (is (= 1 (fired seen)) "the cooldown holds")
          (await (tick-at r (+ t0 29999)))
          (is (= 1 (fired seen)) "30 s counted from the job end, not from the judgement")
          (let [round (tick-at r (+ t0 30000))]
            (await (pause 5))
            (is (= 2 (fired seen)) "after it the reflex fires again")
            (await round)))))))

(deftest a-reflex-job-ending-after-a-respawn-is-judged-once-settling-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen world state] :as r} (setup {:entities [pest] :settles true})]
          (core/register-reflex! eng {:trigger :pest})
          (await (tick-at r t0))
          (is (true? (.isSettling p)))
          (is (= [] (kinds-of seen :ended)))
          (swap! state assoc :entities [(fake/entity-in pest)])
          (await (tick-at r (+ t0 200)))
          (is (= 1 (fired seen)) "still settling: nothing fires")
          (.settle world false)
          (await (tick-at r (+ t0 250)))
          (is (= [:completed_not_cleared] (mapv :how (kinds-of seen :ended))))
          (await (tick-at r (+ t0 29999)))
          (is (= 1 (fired seen)) "no immediate re-fire: the cooldown counts from the job end"))))))

(deftest a-reflex-stopped-while-settling-keeps-its-reason-on-the-deferred-end
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng world seen state] :as r} (setup {:entities [pest] :settles true})]
          (core/register-reflex! eng {:trigger :pest-stops})
          (await (tick-at r t0))
          (is (= [] (kinds-of seen :ended)) "no judgement while settling")
          (swap! state assoc :entities [(fake/entity-in pest)])
          (.settle world false)
          (await (tick-at r (+ t0 250)))
          (let [[ended] (kinds-of seen :ended)]
            (is (= :stopped (:outcome ended)))
            (is (= :no_land (:reason ended)))
            (is (= 250 (:deferred-ms ended)))))))))

(deftest a-condition-really-gone-after-settling-is-cleared-and-may-fire-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen world state] :as r} (setup {:entities [pest] :settles true})]
          (core/register-reflex! eng {:trigger :pest})
          (await (tick-at r t0))
          (.settle world false)
          (await (tick-at r (+ t0 100)))
          (is (= [:cleared] (mapv :how (kinds-of seen :ended))))
          (is (= [100] (mapv :deferred-ms (kinds-of seen :ended))))
          (swap! state assoc :entities [(fake/entity-in pest)])
          (await (tick-at r (+ t0 150)))
          (is (= 2 (fired seen)) "no cooldown was applied"))))))

(deftest urgent-reflexes-wait-for-settling-and-then-fire
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng world seen] :as r} (setup {:self {:onFire true} :settles true})]
          (.settle world true)
          (core/register-reflex! eng {:trigger :burning})
          (is (nil? (await (tick-at r t0))))
          (is (= 0 (fired seen)) "nothing fires while settling")
          (.settle world false)
          (await (tick-at r (+ t0 50)))
          (is (= 1 (fired seen)) "the burning reflex fires on the first ready tick"))))))

(deftest no-trigger-is-evaluated-while-settling
  (async done
    (tu/run-async done
      (fn ^:async t []
        (reset! probes 0)
        (let [{:keys [eng world] :as r} (setup {})]
          (core/register-reflex! eng {:trigger :probe})
          (.settle world true)
          (is (= [nil nil] [(await (tick-at r t0)) (await (tick-at r (+ t0 1)))]))
          (is (= 0 @probes))
          (.settle world false)
          (await (tick-at r (+ t0 2)))
          (is (= 1 @probes) "evaluation resumes when ready"))))))
