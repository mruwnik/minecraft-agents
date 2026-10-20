(ns engine.torch-stock-test
  "jobs.lib.torch-stock/torches!, run by jobs.gather.mine: the stocking trip to a chest, and the line a dig cuts next."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.mine-support :as mt]
            [engine.test-util :as tu]
            [jobs.lib.trip :as trip]))

(def log-world
  (-> (mt/torch-world [{:name "iron_pickaxe" :count 1} {:name "oak_log" :count 11} {:name "stick" :count 8}])
      (assoc :recipes {"furnace" {:count 1 :needs {"cobblestone" 8}}
                       "torch" {:count 4 :needs {"charcoal" 1 "stick" 1}}})))

(defn ^:async mine-with-cooking!
  "A mine run over log-world with the furnace cooking as the clock runs (10 ticks per 500 ms tick)."
  []
  (let [{:keys [eng clock p] :as s} (mt/start {:world log-world})]
    (core/submit! eng (mt/spec {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 15 :trip true}) {})
    (loop [i 0]
      (when (and (< i 900) (seq (:list (core/state eng))))
        (swap! clock + 500)
        (.advance (.-world p) 10)
        (await (core/tick! eng))
        (recur (inc i))))
    s))

(defn own "The events of kind the mine call itself emitted, not its children." [kind s]
  (filter #(= "j1" (:job %)) (mt/events-of s kind)))

(deftest mine-with-logs-and-stone-makes-a-furnace-and-loads-the-logs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (mine-with-cooking!))]
          (is (= 1 (count (filter #(re-find #"put a furnace down" (:text %)) (mt/events-of s :smelt.furnace)))) "a furnace was put down")
          (is (= 1 (count (mt/events-of s :smelt.loaded))) "the logs went in to be charcoal"))))))

(deftest mine-waits-for-the-charcoal-smelt-instead-of-digging-dark
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (mine-with-cooking!))]
          (is (empty? (mt/events-of s :torch.no-stock)) "the smelt is waited for, and the furnace's cobblestone child does not warn for the get it is part of")
          (is (seq (own :torch.stocked s))))))))

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
