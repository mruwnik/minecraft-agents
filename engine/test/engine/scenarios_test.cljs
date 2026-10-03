(ns engine.scenarios-test
  "The shipped scenarios run end to end against the fake primitives."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.scenario :as scenario]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(defn tree [x z height]
  (merge
   (into {} (for [y (range 64 (+ 64 height))] [(str x "," y "," z) "oak_log"]))
   {(str x "," (+ 64 height) "," z) "oak_leaves"
    (str (inc x) "," (+ 63 height) "," z) "oak_leaves"}))

(defn boot
  "An engine over the fake world loaded with the scenario file."
  [file world]
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        p (tu/fake world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})
        s (scenario/read-file file)]
    (is (= [] (scenario/problems registry/jobs triggers/all s)))
    (core/load-scenario! eng s)
    {:eng eng :p p :seen seen :clock clock}))

(defn ^:async run-ticks [eng clock n step-ms]
  (loop [i 0]
    (when (< i n)
      (await (core/tick! eng))
      (swap! clock + step-ms)
      (recur (inc i)))))

(defn names-started [seen]
  (set (keep #(when (= :round_started (:kind %)) (:name %)) @seen)))

(deftest woodcutter-runs-on-the-fake
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (boot "scenarios/woodcutter.edn"
                                               {:blocks (merge (tree 4 0 4) {"8,64,8" "chest"})
                                                :inventory [{:name "oak_sapling" :count 1}]
                                                :containers {"8,64,8" []}})]
          ;; The chest becomes known only after the harvest: deposit puts away
          ;; every non-tool stack, saplings included, so with a chest known
          ;; from the start it would take the sapling the replant needs.
          (await (run-ticks eng clock 20 1000))
          (is (= ["j2"] (:list (core/state eng))) "harvest finished; deposit declines without a chest")
          (mem/write! (:store eng) :chest {:pos {:x 8 :y 64 :z 8}} mem/place-policy)
          (await (run-ticks eng clock 20 1000))
          (is (= [] (:list (core/state eng))) "deposit finishes")
          (is (= #{"jobs.forestry.harvest-wood" "jobs.storage.deposit"} (names-started seen)))
          (is (= "oak_sapling" (.-name (.blockAt p #js {:x 4 :y 64 :z 0}))) "replanted")
          (is (not-any? #(#{:warn :error} (:level %)) @seen)))))))

(deftest pace-cuts-runs-on-the-fake-and-the-reflex-cuts-pace
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (boot "scenarios/pace-cuts.edn" {:self {:pos {:x -78 :y 69 :z -40}}})
              world (.-world p)]
          (await (core/tick! eng))
          (is (= ["jobs.movement.look-around"] (vec (names-started seen))) "every-interval fires at once")
          (let [release (.hold world "moveTo")
                pacing (core/tick! eng)]
            (swap! clock + 21000)
            (let [looking (core/tick! eng)]
              (release)
              (await pacing)
              (await looking)))
          (is (some #(= [:job :cut] [(:source %) (:kind %)]) @seen) "the reflex cut the pace round")
          (await (run-ticks eng clock 30 1000))
          (is (= [] (:list (core/state eng))) "pace resumes and finishes its rounds")
          (is (not-any? #(#{:warn :error} (:level %)) @seen)))))))
