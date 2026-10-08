(ns engine.plant-sapling-unseen-test
  "jobs.forestry.plant-sapling looks at a planting spot it has never seen before it drops the spot."
  (:require [cljs.test :refer [deftest is async]]
            [engine.library-test :as lt]
            [engine.test-util :as tu]))

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

(deftest a-spot-that-stays-unseen-is-not-planted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (await (plant-unseen (constantly false)))]
          (is (= "air" (.-name (.blockAt p spot)))))))))
