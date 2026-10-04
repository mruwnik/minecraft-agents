(ns agent-tools.inventory-test
  (:require [cljs.test :refer [deftest is]]
            [agent-tools.inventory :as inventory]))

(deftest compact-inventory-aggregates-counts-and-keeps-slots-opt-in
  (let [source {:ok true
                :inventory [{:name "bread" :count 3 :slot 9}
                            {:name "bread" :count 2 :slot 10}
                            {:name "iron_pickaxe" :count 1 :slot 37}]
                :equipment {:head {:name "iron_helmet" :count 1 :durability 140}
                            :offHand nil}}
        summary (inventory/compact source :inventory)]
    (is (= {:total-items 6 :kinds 2
            :counts (sorted-map "bread" 5 "iron_pickaxe" 1)
            :equipment {:head {:name "iron_helmet" :count 1 :durability 140}}}
           summary))
    (is (= (:inventory source) (:slots (inventory/compact source :inventory true))))
    (is (= {:equipment {:head {:name "iron_helmet" :count 1 :durability 140}}}
           (inventory/compact source :equipment)))))

(deftest compact-inventory-omits-empty-gear-and-preserves-offline-refusals
  (is (= {:total-items 0 :kinds 0} (inventory/compact {:ok true :inventory []} :inventory)))
  (is (= {:equipment :none} (inventory/compact {:ok true :equipment {}} :equipment)))
  (is (= {:ok false :reason :offline}
         (inventory/compact {:ok false :reason :offline} :inventory))))
