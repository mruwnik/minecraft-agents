(ns engine.found-goods-test
  "Goods the body picked up without a job reason (a destroyed chest's drops) are found goods: reported, never deposited."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.deposit-test :as dt]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [engine.withdraw-test :as wt]
            [jobs.lib.found :as found]))

(defn view [& picks]
  (let [s (mem/open (tu/tmp-dir) {:now (constantly 1000000)})]
    (doseq [[item n] picks] (mem/write! s :picked-up {:item item :count n :pos {:x 23 :y 64 :z -85}} {:cap 50 :ttl 3600000}))
    (mem/view s)))

(deftest bulk-of-an-item-nobody-gathers-is-found
  (is (= {"sugar_cane" {:count 574 :pos {:x 23 :y 64 :z -85}}}
         (found/goods (view ["sugar_cane" 64] ["sugar_cane" 510])))))

(deftest containers-are-found-at-any-count
  (is (= #{"chest" "hopper"} (set (keys (found/goods (view ["chest" 1] ["hopper" 1])))))))

(deftest gathered-materials-and-small-lots-are-not-found
  (is (= {} (found/goods (view ["cobblestone" 900] ["oak_log" 400] ["sugar_cane" 60])))))

(deftest deposit-keeps-found-goods-and-reports-them
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (wt/setup {:inventory [{:name "dirt" :count 5} {:name "sugar_cane" :count 574}]
                                              :containers {"10,64,0" []}})]
          (mem/write! (:store eng) :picked-up {:item "sugar_cane" :count 574 :pos {:x 23 :y 64 :z -85}})
          (await (tu/child-outcome eng dt/job {:chest dt/chest} 8))
          (is (= {"sugar_cane" 574} (wt/inv p)))
          (is (some #(and (= :goods.found (:kind %)) (= :notice (:attention %)) (= "sugar_cane" (:item %))) @seen)))))))
