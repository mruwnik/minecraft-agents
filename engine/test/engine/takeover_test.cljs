(ns engine.takeover-test
  "Manual takeover: take! cuts the holder like a reflex and pauses the scheduler until release!;
  the manual state is never persisted."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.takeover :as takeover]
            [engine.test-util :as tu]
            [engine.triggers :as real-triggers]
            ["fs" :as fs]
            ["path" :as path]))

(def t0 1000000)
(def probes (atom 0))

(defn ^:async eat-round [c]
  (await (.eat (:primitives c) (:token c) #js {}))
  :done)

(defn ^:async walk-round [c]
  (await (.moveTo (:primitives c) (:token c) #js {:pos #js {:x 5 :y 64 :z 0}}))
  :done)

(def jobs
  (merge registry/jobs
         {'walk {:check (constantly true) :round walk-round}
          'eat-up {:check (constantly true) :round eat-round}
          'nop {:check (constantly true) :round (fn [_] :done)}}))

(def triggers
  (merge real-triggers/all
         {:hurt {:name :hurt :job '(eat-up) :persistence :cooldown :cooldown-s 30
                 :when (fn [w _ _] (< (.-health (.self w)) 10))}
          :probe {:name :probe :job '(nop) :when (fn [_ _ _] (swap! probes inc) false)}}))

(defn setup [spec]
  (let [clock (atom t0)
        [seen sink] (tu/capture-sink)
        p (tu/fake spec)
        dir (tu/tmp-dir)
        eng (core/create {:primitives p :jobs jobs :triggers triggers :dir dir :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :clock clock :seen seen :dir dir :world (.-world p) :state (.-state (.-world p))}))

(defn kinds-of [seen kind] (filterv #(= kind (:kind %)) @seen))
(defn fired [seen] (count (kinds-of seen :fired)))
(defn ran [seen] (->> @seen (filter #(= :round_started (:kind %))) (mapv :job)))

(def me {:who "claude" :why "look around"})

(deftest take-cuts-a-running-job-which-resumes-after-release
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen world]} (setup {})]
          (core/submit! eng '(walk) {})
          (.hold world "moveTo")
          (let [walking (core/tick! eng)]
            (is (= {:ok true} (takeover/take! eng me)))
            (await walking))
          (is (= [:takeover] (mapv :by (kinds-of seen :cut))) "job.cut by :takeover")
          (is (true? (core/manual? eng)))
          (is (nil? (core/tick! eng)) "no round starts while manual")
          (takeover/release! eng {:who "claude" :reason "released" :held-ms 5})
          (is (false? (core/manual? eng)))
          (await (core/tick! eng))
          (is (= ["j1" "j1"] (ran seen)) "the cut job is the next round after release"))))))

(deftest take-drops-a-running-reflex-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen world state]} (setup {:inventory [{:name "bread" :count 3}]})]
          (core/register-reflex! eng {:trigger :hurt})
          (set! (.. state -self -health) 6)
          (.hold world "eat")
          (let [round (core/tick! eng)]
            (is (= 1 (fired seen)))
            (takeover/take! eng me)
            (await round))
          (is (= [:dropped] (mapv :outcome (kinds-of seen :ended))) "reflex.ended dropped"))))))

(deftest while-manual-no-trigger-is-evaluated-and-the-trigger-fires-after-release
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen state]} (setup {})]
          (core/register-reflex! eng {:trigger :probe})
          (core/register-reflex! eng {:trigger :hurt})
          (set! (.. state -self -health) 6)
          (takeover/take! eng me)
          (reset! probes 0)
          (await (core/tick! eng))
          (is (= 0 @probes) "no trigger evaluated")
          (is (= 0 (fired seen)) "a holding trigger does not fire")
          (takeover/release! eng {:who "claude" :reason "released" :held-ms 1})
          (await (core/tick! eng))
          (is (pos? @probes))
          (is (= 1 (fired seen)) "after release the trigger fires on the next tick"))))))

(deftest take-is-refused-when-offline-settling-or-held
  (let [{:keys [eng world state]} (setup {})]
    (set! (.-offline state) true)
    (is (= {:ok false :reason "offline"} (takeover/take! eng me)))
    (set! (.-offline state) false)
    (.settle world true)
    (is (= {:ok false :reason "settling"} (takeover/take! eng me)))
    (.settle world false)
    (is (= {:ok true} (takeover/take! eng me)))
    (is (= {:ok false :reason "held-by claude"} (takeover/take! eng {:who "view" :why "x"})))))

(deftest drive-moves-the-controls-and-release-clears-them
  (let [{:keys [eng state]} (setup {})]
    (takeover/take! eng me)
    (let [r (takeover/drive! eng #js {:controls #js {:forward true} :look #js {:yaw 90 :pitch 10}})]
      (is (= 90 (.-yaw r))))
    (is (= {:forward true} (js->clj (.-controls state) :keywordize-keys true)))
    (takeover/release! eng {:who "claude" :reason "idle" :held-ms 1})
    (is (= {} (js->clj (.-controls state))))))

(deftest events-carry-who-why-and-reason
  (let [{:keys [eng seen]} (setup {})]
    (takeover/take! eng me)
    (takeover/deadman! eng {:who "claude" :silent-ms 2100})
    (takeover/release! eng {:who "claude" :reason "idle" :held-ms 70})
    (let [[started] (kinds-of seen :takeover_started)
          [ended] (kinds-of seen :takeover_ended)
          [dead] (kinds-of seen :drive_deadman)]
      (is (= ["claude" "look around"] ((juxt :who :why) started)))
      (is (= :info (:level started)))
      (is (= "manual control by claude: look around; jobs and reflexes paused" (:text started)))
      (is (= ["claude" "idle" 70] ((juxt :who :reason :held-ms) ended)))
      (is (= "manual control by claude ended: idle; jobs and reflexes resume" (:text ended)))
      (is (= :warn (:level dead)))
      (is (= "driver claude silent 2100 ms: controls released" (:text dead))))))

(deftest manual-state-is-not-persisted
  (let [{:keys [eng dir]} (setup {})]
    (takeover/take! eng me)
    (is (not (re-find #"manual|claude" (fs/readFileSync (path/join dir "engine.edn") "utf8"))))))

(deftest the-adapter-speaks-the-control-interface
  (let [{:keys [eng]} (setup {})
        a (takeover/adapter eng)]
    (is (= #{"offline" "settling" "pos"} (set (js/Object.keys (.status a)))))
    (is (true? (.-ok (.take a #js {:who "claude" :why "x"}))))
    (is (= 30 (.-yaw (.drive a #js {:look #js {:yaw 30}}))))
    (.stopDriving a)
    (.release a #js {:who "claude" :reason "released" :heldMs 3})
    (is (false? (core/manual? eng)))))
