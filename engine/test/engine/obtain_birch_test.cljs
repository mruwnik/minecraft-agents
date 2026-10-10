(ns engine.obtain-birch-test
  "items.obtain with only birch logs seen: any species serves planks, oak_log wants oak."
  (:require [cljs.test :refer [deftest is async]]
            [engine.fetch-test :as f]
            [engine.obtain-iron-test :as oi]
            [engine.test-util :as tu]))

(def birch-trees {"1,68,4" "birch_leaves" "2,67,4" "birch_leaves" "1,64,4" "birch_log" "1,65,4" "birch_log" "1,66,4" "birch_log" "1,67,4" "birch_log"})

(deftest a-stone-axe-gathers-the-birch-logs-seen-for-its-planks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (oi/ore-world birch-trees [{:name "cobblestone" :count 3}]) [])]
          (await (oi/obtain! s {:item "stone_axe"} 60))
          (is (= 4 (get (f/inv s) "birch_log"))))))))

(deftest oak-logs-with-only-birch-seen-say-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (oi/ore-world birch-trees []) [])]
          (await (oi/obtain! s {:item "oak_log"} 40))
          (let [w (first (filter #(= :waiting (:kind %)) (deref (:seen s))))]
            (is (= :no-source (:reason w)))
            (is (re-find #"birch_log" (:why w)) (:why w))))))))
