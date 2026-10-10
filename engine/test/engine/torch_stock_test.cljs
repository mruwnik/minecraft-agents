(ns engine.torch-stock-test
  "jobs.lib.trip/torches-ready!: a stair down that carries few torches stocks up first, or says it digs dark."
  (:require [cljs.test :refer [deftest is async]]
            [engine.stair-test :as st]
            [engine.test-util :as tu]))

(def chest "2,65,1")
(def torch-chest {:blocks (assoc st/ground chest "chest") :containers {chest [{:name "torch" :count 20}]}})
(def east (assoc st/east :steps 2))

(defn kinds [s] (set (map :kind @(:seen s))))

(deftest a-stair-down-with-no-torches-takes-them-from-a-seen-chest-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (st/stair! (assoc torch-chest :inventory st/pick) east tu/seeing-all))]
          (is (= :done (:status @(:out s))))
          (is (contains? (kinds s) :torch.stocked))
          (is (not (contains? (kinds s) :torch.no-stock))))))))

(deftest a-stair-down-with-no-torches-and-nothing-to-make-them-from-says-it-digs-dark
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (st/stair! {:blocks st/ground :inventory st/pick} east (fn [_])))]
          (is (= :done (:status @(:out s))))
          (is (= 1 (count (st/events-of s :torch.no-stock))))
          (is (not (contains? (kinds s) :torch.stocked))))))))

(deftest a-stair-with-enough-torches-fetches-none
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (st/stair! (assoc torch-chest :inventory (conj st/pick {:name "torch" :count 10})) east tu/seeing-all))]
          (is (= :done (:status @(:out s))))
          (is (not-any? #{:torch.stocked :torch.no-stock} (kinds s))))))))

(deftest a-stair-up-needs-no-torches
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (st/stair! {:blocks st/ground :inventory st/pick :self {:pos {:x 0 :y 55 :z 0}}}
                                  {:dir :up :heading :east :steps 2} (fn [_])))]
          (is (not-any? #{:torch.stocked :torch.no-stock} (kinds s))))))))

(deftest a-stair-down-with-two-coal-and-logs-makes-the-torches-the-coal-allows-and-plans-no-charcoal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (st/stair! {:blocks st/ground :inventory (into st/pick [{:name "coal" :count 2} {:name "oak_log" :count 11}])} east tu/seeing-all))
              carried (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self (:p s))))]
          (is (= :done (:status @(:out s))))
          (is (= 8 (get carried "torch")) "2 coal make 8 torches")
          (is (contains? (kinds s) :torch.stocked))
          (is (nil? (get carried "crafting_table")))
          (is (empty? (filter #(= "place" (.-name %)) (.-calls (.-world (:p s))))) "no table placed"))))))
