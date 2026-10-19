(ns engine.torch-stock-test
  "jobs.access.stock/torches!: a tunnel that carries few torches stocks up first, or says it digs dark."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.mine-support :as mt]
            [engine.tunnel-test :as st]
            [engine.test-util :as tu]
            [jobs.lib.trip :as trip]))

(def chest "2,65,1")
(def torch-chest {:blocks (assoc st/ground chest "chest") :containers {chest [{:name "torch" :count 20}]}})
(def east {:target [6 57 0]})

(def log-world
  {:blocks st/ground :drops {"stone" "cobblestone"}
   :recipes {"furnace" {:count 1 :needs {"cobblestone" 8}}
             "torch" {:count 4 :needs {"charcoal" 1 "stick" 1}}}
   :inventory (into st/pick [{:name "oak_log" :count 11} {:name "stick" :count 8}])})

(defn ^:async tunnel-with-cooking!
  "The tunnel over log-world with the furnace cooking as the clock runs (10 ticks per 500 ms tick)."
  [args]
  (let [{:keys [eng clock p] :as s} (st/setup log-world args tu/seeing-all)]
    (loop [i 0]
      (when (and (< i 900) (seq (:list (core/state eng))))
        (swap! clock + 500)
        (.advance (.-world p) 10)
        (await (core/tick! eng))
        (recur (inc i))))
    s))

(defn kinds [s] (set (map :kind @(:seen s))))

(deftest a-tunnel-with-no-torches-takes-them-from-a-seen-chest-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (st/tunnel! (assoc torch-chest :inventory st/pick) east tu/seeing-all))]
          (is (= :done (:status @(:out s))))
          (is (contains? (kinds s) :torch.stocked))
          (is (not (contains? (kinds s) :torch.no-stock))))))))

(deftest a-tunnel-with-no-torches-and-nothing-to-make-them-from-says-it-digs-dark
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (st/tunnel! {:blocks st/ground :inventory st/pick} east (fn [_])))]
          (is (= :done (:status @(:out s))))
          (is (= 1 (count (st/events-of s :torch.no-stock))))
          (is (not (contains? (kinds s) :torch.stocked))))))))

(deftest a-tunnel-with-enough-torches-fetches-none
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (st/tunnel! (assoc torch-chest :inventory (conj st/pick {:name "torch" :count 10})) east tu/seeing-all))]
          (is (= :done (:status @(:out s))))
          (is (not-any? #{:torch.stocked :torch.no-stock} (kinds s))))))))

(deftest a-tunnel-with-two-coal-and-logs-makes-the-torches-the-coal-allows-and-plans-no-charcoal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (st/tunnel! {:blocks st/ground :inventory (into st/pick [{:name "coal" :count 2} {:name "oak_log" :count 11}])} east tu/seeing-all))
              carried (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self (:p s))))]
          (is (= :done (:status @(:out s))))
          (is (= [8] (map :held (st/events-of s :torch.stocked))) "2 coal make 8 torches")
          (is (nil? (get carried "crafting_table")))
          (is (empty? (filter #(and (= "place" (.-name %)) (not= "torch" (.-item (.-args %)))) (.-calls (.-world (:p s))))) "no table placed"))))))

(deftest a-tunnel-with-logs-and-no-furnace-says-why-no-torches-came
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (st/tunnel! {:blocks st/ground :inventory (conj st/pick {:name "oak_log" :count 11})} east tu/seeing-all))
              [ev] (st/events-of s :torch.no-stock)]
          (is (= :done (:status @(:out s))))
          (is (= :no-source (:reason ev)))
          (is (= :no-furnace-seen (:craft (:tried ev))))
          (is (re-find #"8 cobblestone" (:text ev)) "the text says what would make a furnace")
          (is (not (re-find #"no coal or logs" (:text ev))) "11 logs are carried"))))))

(deftest a-tunnel-with-logs-and-minable-stone-makes-a-furnace-and-loads-the-logs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (tunnel-with-cooking! east))]
          (is (= 1 (count (filter #(re-find #"put a furnace down" (:text %)) (st/events-of s :smelt.furnace)))) "a furnace was put down")
          (is (= 1 (count (st/events-of s :smelt.loaded))) "the logs went in to be charcoal"))))))

(deftest a-tunnel-waits-for-the-charcoal-smelt-instead-of-digging-dark
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (tunnel-with-cooking! east))]
          (is (empty? (st/events-of s :torch.no-stock)) "the smelt is waited for")
          (is (contains? (kinds s) :torch.stocked)))))))

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
          (is (seq (mt/dug-cells s)))
          (is (every? (fn [[x]] (<= x 2)) (mt/dug-cells s)) (pr-str (mt/dug-cells s))))))))

(deftest the-line-a-dig-cuts-next-is-two-cells-ahead-at-three-heights
  (let [p (tu/fake {:self {:pos {:x 5.5 :y 64 :z 2.5}}})]
    (is (= [[6 63 2] [6 64 2] [6 65 2] [7 63 2] [7 64 2] [7 65 2]] (trip/line-ahead {:args {:heading :east} :primitives p})))
    (is (= [[5 63 1] [5 64 1] [5 65 1] [5 63 0] [5 64 0] [5 65 0]] (trip/line-ahead {:args {:direction "north"} :primitives p})))))
