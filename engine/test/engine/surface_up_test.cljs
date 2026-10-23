(ns engine.surface-up-test
  (:require [cljs.test :refer [deftest is async]]
            [engine.ctx :as ctx]
            [engine.test-util :as tu :refer [box]]
            [jobs.lib.child :as child]
            [jobs.lib.surface-up :as surface]))

;; a pond 7 x 7, 1 deep, on a stone floor, dry ground all round; every walk out of it fails
(deftest a-walk-out-of-water-that-fails-tries-another-stand-and-stays-after-max-failed-stands
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/seeing-all (tu/fake {:self {:inWater true :pos {:x 0.5 :y 64 :z 0.5}}
                                         :blocks (merge (box -8 63 -8 8 63 8 "stone") (box -3 64 -3 3 64 3 "water"))}))
              m (atom {})
              walked (atom [])
              c {:primitives p}
              ends (with-redefs [ctx/mem (fn [_] @m)
                                 ctx/update-mem! (fn [_ f & args] (apply swap! m f args))
                                 ctx/emit! (fn ([_ _ _]) ([_ _ _ _]))
                                 child/call-walk* (fn [_ _ args] (swap! walked conj (:pos args)) (js/Promise.resolve {:kind :nothing-possible :data {:arrived false}}))]
                     (let [a (await (surface/leave-water! c)) b (await (surface/leave-water! c)) d (await (surface/leave-water! c))
                           e (await (surface/leave-water! c)) f (await (surface/leave-water! c))]
                       [a b d e f]))]
          (is (= [:again :again :again :out :out] ends))
          (is (= 3 (count (distinct @walked))) (str "each failed stand is not walked to again: " @walked)))))))
