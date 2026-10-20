(ns engine.torch-craft-test
  "mine's torch round crafts torches in a child slot of its own: the stock-up's obtain (jobs.access.stock/torches!)
  leaves a :start map in its slot when it ends declined, and a craft reading it made a NaN count."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.memory :as mem]
            [engine.mine-support :as mt]
            [engine.test-util :as tu]))

(deftest a-torch-craft-does-not-read-the-stock-ups-leftover-memory
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (mt/start-lit (mt/torch-world [{:name "iron_pickaxe" :count 1} {:name "coal" :count 2} {:name "stick" :count 4}]) [0 0])
              id (core/submit! (:eng s) (mt/spec {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 12 :trip true :fetch false}) {})]
          (mem/update-job! (:store (:eng s)) id [] assoc-in [:children :torches]
                           {:args {:item "torch" :count 16} :children {} :start {:have 0 :target 16}})
          (await (mt/run-ticks s 300))
          (is (empty? (filter #(re-find #"craft count" (str (:error %) (:text %) (:message %))) @(:seen s))))
          (is (pos? (get (mt/inv s) "torch" 0))))))))
