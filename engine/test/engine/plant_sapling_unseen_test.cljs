(ns engine.plant-sapling-unseen-test
  "jobs.forestry.plant-sapling looks at a planting spot it has never seen before it drops the spot."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [jobs.forestry.plant-sapling :as plant-sapling]
            [engine.library-test :as lt]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]
            [plan.shape :as shape]))

(def spot #js {:x 3 :y 64 :z 0})

(defn looked-at-spot?
  "A look at the spot's cell was made."
  [p]
  (boolean (some #(when-let [q (and (= "look" (.-name %)) (some-> (.-args %) .-pos))]
                    (and (= 3 (js/Math.floor (.-x q))) (= 64 (js/Math.floor (.-y q))) (= 0 (js/Math.floor (.-z q)))))
                 (.-calls (.-world p)))))

(defn ^:async plant-unseen
  "plant-sapling at the spot, whose cell reads as never seen until (seen-now? p)."
  [seen-now?]
  (let [{:keys [eng p]} (lt/setup {:blocks {} :inventory [{:name "oak_sapling" :count 1}]})
        omniscient (.-sensedAt p)]
    (aset p "sensedAt" (fn [pos] (if (and (= 3 (.-x pos)) (= 64 (.-y pos)) (= 0 (.-z pos)) (not (seen-now? p)))
                                   #js {:unknown true}
                                   (omniscient pos))))
    (await (tu/child-outcome eng 'jobs.forestry.plant-sapling {:at {:x 3 :y 64 :z 0} :species "oak"} 60))
    p))

(deftest an-unseen-spot-is-looked-at-before-it-is-dropped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (await (plant-unseen looked-at-spot?))]
          (is (= "oak_sapling" (.-name (.blockAt p spot))) "planted after the look"))))))

(deftest a-sapling-is-planted-in-the-footprint-of-its-own-plan-but-not-another-bodys
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[author planted] [["Fake" "oak_sapling"] ["Miles" "air"]]]
          (let [plan (shape/with-author {:id "hut" :parts [{:id "p" :box [[3 64 0] [3 64 0]] :want "stone"}]} author)
                clock (atom 1000000)
                [_ sink] (tu/legacy-capture-sink)
                p (tu/seeing-all (tu/fake-on-floor {:blocks {} :inventory [{:name "oak_sapling" :count 1}]}))
                eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                                  :world (ew/of-data {"hut" plan} {} [])
                                  :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
            (await (tu/child-outcome eng 'jobs.forestry.plant-sapling {:at {:x 3 :y 64 :z 0} :species "oak"} 60))
            (is (= planted (.-name (.blockAt p spot))) author)))))))

(deftest a-log-on-the-spot-is-a-later-of-can-run
  (let [def (get registry/jobs 'jobs.forestry.plant-sapling)
        c {:primitives (tu/seeing-all (tu/fake {:blocks {"3,64,0" "oak_log"}})) :args {}}
        status (fn [f] (with-redefs [ctx/mem (constantly {})
                                     plant-sapling/current-target (constantly {:pos {:x 3 :y 64 :z 0} :species "oak"})]
                         (f c)))]
    (is (= [:later :log-on-spot] ((juxt first (comp :reason last)) (status (:can-run? def)))))))
