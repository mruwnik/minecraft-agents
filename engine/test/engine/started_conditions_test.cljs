(ns engine.started-conditions-test
  "A start-only condition holds a call back before its first round and never ends it once it began: the engine re-checks
  conditions at every re-entered round, and a frozen call (ctx/frozen-call, here a :quote-call) has begun."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.hostile-test :as h]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.lib.world-files :as ew]))

(defn ^:async ending-after
  "Submit spec over a fake world, tick ticks times (the job must still run), flip! the world, tick on until it ends:
  {:event :completed|:stopped :kind :reason}, or {:event :running}."
  [make-prims spec ticks flip!]
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        p (make-prims)
        now (tu/act-clock clock p 20)
        eng (core/create {:primitives p :jobs registry/jobs :dir (tu/tmp-dir) :now now :world (ew/of-data {} {} [])
                          :events (events/make {:sinks [sink] :now now})})
        ended (fn [] (first (filter #(#{:completed :stopped} (:kind %)) @seen)))]
    (core/submit! eng spec {})
    (dotimes [_ ticks] (await (core/tick! eng)))
    (assert (nil? (ended)) (str "ended early: " (pr-str (:data (ended)))))
    (flip! p)
    (loop [i 0]
      (when (and (< i 40) (not (ended)))
        (swap! clock + 700)
        (await (core/tick! eng))
        (recur (inc i))))
    (core/shutdown! eng)
    (if-let [e (ended)]
      (assoc (select-keys (:data e) [:kind :reason]) :event (:kind e))
      {:event :running})))

(defn fight-world
  "A fight in which every swing costs the body health, so the call yields :continue after its first swing."
  []
  (let [p (tu/fake-on-floor {:floor h/flight-floor :inventory h/sword :entities [(h/zombie 7 3 0)]})]
    (.override (.-world p) "attack" (fn [token a impl] (-> (impl token a) (.then (fn [r] (fake/swap-self! p assoc :health 15) r)))))
    p))
(def slow-fight '(jobs.combat.attack {:targets :hostiles :radius 4 :ranged-radius 16}))

(deftest a-running-hostile-attack-is-done-when-its-last-target-died
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= :completed
               (:event (await (ending-after fight-world slow-fight 1 #(h/set-entities! % []))))))))))
