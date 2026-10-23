(ns engine.shut-in-night-contract-test
  "jobs.survival.shut-in with :for :night as a contract job: what each way of ending a call is, read through ctx/call."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.shut-in-pit-seen-test :as seen]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as world]))

(defn setup
  "An engine over p; a recording parent calls shut-in (seen/night args) through ctx/call and keeps its ending in :out."
  [p args]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        out (atom :not-done)
        parent {:round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call c :kid 'jobs.survival.shut-in (seen/night args)))]
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

(defn ^:async shut-in! [p args & {:keys [prep]}]
  (let [s (setup p args)]
    (when prep (prep (:eng s)))
    (await (seen/tick-out! s))
    @(:out s)))

(defn ^:async surface [spec] (seen/hover-over-digs! (await (seen/see-surface! (seen/sensing spec)))))

(deftest a-pit-that-roofs-the-body-ends-done-with-its-way-and-roof
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (await (shut-in! (await (surface {:blocks seen/ground})) {}))]
          (is (= :done (:kind e)))
          (is (= :pit (:way (:data e))))
          (is (some? (:roof (:data e)))))))))

(deftest no-block-to-roof-the-pit-is-a-way-dropped-for-its-stop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (await (surface {:blocks seen/ground :inventory [{:name "iron_pickaxe" :count 1}] :drops {"stone" "coal"}}))
              e (await (shut-in! p {}))]
          (is (= [:nothing-possible :no-way] (ending-of e)))
          (is (= :no-blocks (get-in e [:data :stops :pit :reason]))))))))

(deftest a-plug-the-server-answers-occupied-is-a-try-that-failed-nothing-possible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (await (surface {:blocks seen/ground}))
              _ (.override (.-world p) "place" (fn ^:async f [_ _ _] #js {:status "occupied"}))
              e (await (shut-in! p {:ways #{:pit}}))]
          (is (= [:nothing-possible :no-way] (ending-of e)))
          (is (= :roof-failed (get-in e [:data :stops :pit :reason]))))))))

(deftest a-way-that-failed-on-the-site-is-dropped-within-the-futile-radius
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/fake {:self {:pos {:x 0 :y 65 :z 0}} :blocks seen/ground :inventory seen/kit :time 14000})
              e (await (shut-in! p {:ways #{:pit}} :prep #(mem/write! (:store %) :shut-in-failed {:pos {:x 0 :y 65 :z 0} :way :pit :reason :no-floor}
                                                                      {:cap 5 :ttl 600000})))]
          (is (= [:nothing-possible :no-way] (ending-of e)))
          (is (= [:pit] (get-in e [:data :dropped])))
          (is (= :no-floor (get-in e [:data :stops :pit :reason]))))))))

(deftest a-body-already-shut-in-is-done-before-it-places-anything
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [room (merge seen/ground (tu/box -1 65 -1 1 67 1 "stone") {"0,65,0" "air" "0,66,0" "air"})
              p (tu/fake {:self {:pos {:x 0 :y 65 :z 0}} :blocks room :inventory seen/kit :time 14000})
              e (await (shut-in! p {}))]
          (is (= :done (:kind e)))
          (is (empty? (filter #(#{"place" "dig"} (.-name %)) (.-calls (.-world p))))))))))

(deftest no-night-shelter-is-made-with-the-body-in-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/fake {:self {:pos {:x 0 :y 65 :z 0}} :blocks (assoc seen/ground "0,65,0" "water") :inventory seen/kit :time 14000})
              e (await (shut-in! p {}))]
          (is (= [:nothing-possible :in-water] (ending-of e)))
          (is (empty? (:placed (:data e))) "nothing placed: the night leaves the water first"))))))
