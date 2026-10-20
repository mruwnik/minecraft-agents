(ns engine.torch-stock-test
  "jobs.lib.torch-stock/torches!, run by jobs.gather.mine: the stocking trip to a chest, and the line a dig cuts next."
  (:require [cljs.test :refer [deftest is async]]
            [engine.mine-support :as mt]
            [engine.test-util :as tu]
            [jobs.lib.trip :as trip]))

(deftest the-torch-stocking-trip-to-a-far-chest-does-not-move-the-tunnel
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [chest "12,64,0"
              w (-> (mt/torch-world mt/bare-pickaxe)
                    (update :blocks #(merge % (mt/cells "air" (range 1 12) [64 65] [0]) {chest "chest"}))
                    (assoc :containers {chest [{:name "torch" :count 20}]}))
              s (await (mt/scenario {:block "iron_ore" :level false :count 1 :direction "north" :tunnel-length 4 :trip true} w 300))]
          (is (pos? (get (mt/inv s) "torch" 0)) "the chest's torches were taken")
          (is (seq (mt/events-of s :torch.stocked)))
          (is (seq (mt/dug-cells s)))
          (is (every? (fn [[x]] (<= x 2)) (mt/dug-cells s)) (pr-str (mt/dug-cells s))))))))

(deftest the-line-a-dig-cuts-next-is-two-cells-ahead-at-three-heights
  (let [p (tu/fake {:self {:pos {:x 5.5 :y 64 :z 2.5}}})]
    (is (= [[6 63 2] [6 64 2] [6 65 2] [7 63 2] [7 64 2] [7 65 2]] (trip/line-ahead {:args {:heading :east} :primitives p})))
    (is (= [[5 63 1] [5 64 1] [5 65 1] [5 63 0] [5 64 0] [5 65 0]] (trip/line-ahead {:args {:direction "north"} :primitives p})))))
