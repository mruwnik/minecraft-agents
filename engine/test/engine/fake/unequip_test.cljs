(ns engine.fake.unequip-test
  "Unequip in the fake world; the cases of js/fake-unequip.test.mjs."
  (:require [cljs.test :refer [deftest is testing]]
            [engine.fake.unequip :as unequip]))

(defn stacks [n] (mapv #(hash-map :name (str "item_" %) :count 1) (range n)))

(deftest unequip-rows
  (doseq [{:keys [label held inv expect after]}
          [{:label "empty hand" :held nil :inv [] :expect {:status "empty"} :after nil}
           {:label "ok" :held "stick" :inv (stacks 3) :expect {:status "ok" :item "stick"} :after nil}
           {:label "full" :held "stick" :inv (stacks 36) :expect {:status "full"} :after "stick"}]]
    (testing label
      (let [[w r] (unequip/unequip {:self {:held held} :inventory inv})]
        (is (= expect r))
        (is (= after (get-in w [:self :held])))))))
