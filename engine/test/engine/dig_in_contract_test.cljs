(ns engine.dig-in-contract-test
  "jobs.survival.dig-in as a contract job: what each way of ending a call is, read through ctx/call."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.dig-in-seen-test :as seen]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as world]))

(defn setup
  "An engine over p; a recording parent calls dig-in with args through ctx/call and keeps its ending in :out."
  [p args]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        out (atom :not-done)
        parent {:round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call c :kid 'jobs.survival.dig-in args))]
                           (if (= :continue r) r (do (reset! out r) (ctx/end c :done nil)))))}
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'recording-parent parent)
                          :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock) :world (world/of-data {} {} [])
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (.override (.-world p) "wait" (fn ^:async f [token a impl]
                                    (swap! clock + (.-ms a))
                                    (await (impl token a))))
    (core/submit! eng '(recording-parent) {})
    {:eng eng :p p :clock clock :out out}))

(defn ending-of [e] [(:kind e) (contract/cause-key e)])

(defn ^:async dig-in! [p args & {:keys [prep]}]
  (let [s (setup p args)]
    (when prep (prep (:eng s)))
    (await (seen/tick-out! s))
    @(:out s)))

(defn ^:async surface [spec] (seen/hover-over-digs! (await (seen/see-surface! (seen/sensing spec)))))

(deftest a-pit-that-roofs-the-body-ends-done-with-its-mode-and-roof
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (await (dig-in! (await (surface {:blocks seen/ground})) {}))]
          (is (= :done (:kind e)))
          (is (= :dig (:mode (:data e))))
          (is (some? (:roof (:data e)))))))))

(deftest no-block-to-roof-the-pit-is-a-fact-condition-failed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (await (surface {:blocks seen/ground :inventory [{:name "iron_pickaxe" :count 1}] :drops {"stone" "coal"}}))
              e (await (dig-in! p {}))]
          (is (= [:condition-failed :no-blocks] (ending-of e))))))))

(deftest a-plug-the-server-answers-occupied-is-a-try-that-failed-nothing-possible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (await (surface {:blocks seen/ground}))
              _ (.override (.-world p) "place" (fn ^:async f [_ _ _] #js {:status "occupied"}))
              e (await (dig-in! p {}))]
          (is (= [:nothing-possible :roof-failed] (ending-of e))))))))

(deftest by-day-the-start-fails-on-the-fact-and-places-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/fake {:self {:pos {:x 0 :y 65 :z 0}} :blocks seen/ground :inventory seen/kit :time 1000})
              e (await (dig-in! p {}))]
          (is (= [:condition-failed :day] (ending-of e)))
          (is (empty? (filter #(= "place" (.-name %)) (.-calls (.-world p))))))))))

(deftest a-futile-site-fails-the-start-with-its-position-and-why
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/fake {:self {:pos {:x 0 :y 65 :z 0}} :blocks seen/ground :inventory seen/kit :time 14000})
              e (await (dig-in! p {} :prep #(mem/write! (:store %) :dig-in-futile {:pos {:x 0 :y 65 :z 0} :reason :no-floor}
                                                        {:cap 5 :ttl 600000})))]
          (is (= [:condition-failed :futile] (ending-of e)))
          (is (= :no-floor (:why (:cause e)))))))))

(deftest a-body-already-shut-in-is-done-before-it-places-anything
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [room (merge seen/ground (tu/box -1 65 -1 1 67 1 "stone") {"0,65,0" "air" "0,66,0" "air"})
              p (tu/fake {:self {:pos {:x 0 :y 65 :z 0}} :blocks room :inventory seen/kit :time 14000})
              e (await (dig-in! p {}))]
          (is (= :done (:kind e)))
          (is (empty? (filter #(#{"place" "dig"} (.-name %)) (.-calls (.-world p))))))))))
