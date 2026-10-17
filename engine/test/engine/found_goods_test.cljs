(ns engine.found-goods-test
  "Goods picked up with no work of the body near (a destroyed chest's drops) are found goods: reported, never deposited."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.deposit-test :as dt]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [engine.withdraw-test :as wt]
            [jobs.lib.found :as found]))

(def spot {:x 23 :y 64 :z -85})

(defn view [& picks]
  (let [s (mem/open (tu/tmp-dir) {:now (constantly 1000000)})]
    (doseq [[item n extra] picks]
      (mem/write! s :picked-up (merge {:item item :count n :pos spot} extra) {:cap 50 :ttl 3600000}))
    (mem/write! s :fetch/stock {:pos [0 64 0] :items {}} {:cap 64 :ttl 3600000})
    (mem/view s)))

(deftest bulk-of-an-item-nobody-gathers-is-found
  (is (= {"sugar_cane" {:count 574 :pos spot}}
         (found/goods (view ["sugar_cane" 64] ["sugar_cane" 510])))))

(deftest containers-are-found-at-any-count
  (is (= #{"chest" "hopper"} (set (keys (found/goods (view ["chest" 1] ["hopper" 1])))))))

(deftest small-lots-are-not-found
  (is (= {} (found/goods (view ["sugar_cane" 60])))))

(deftest own-work-pickups-are-not-found
  (is (= {} (found/goods (view ["sugar_cane" 574 {:own true}] ["cobblestone" 900 {:own true}]))))
  (is (= {"sugar_cane" {:count 300 :pos spot}}
         (found/goods (view ["sugar_cane" 300] ["sugar_cane" 400 {:own true}])))))

(deftest pickups-at-a-known-chest-are-not-found
  (let [s (mem/open (tu/tmp-dir) {:now (constantly 1000000)})]
    (mem/write! s :picked-up {:item "sugar_cane" :count 574 :pos spot} {:cap 50 :ttl 3600000})
    (mem/write! s :fetch/stock {:pos [24 64 -84] :items {}} {:cap 64 :ttl 3600000})
    (is (= {} (found/goods (mem/view s))))))

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
