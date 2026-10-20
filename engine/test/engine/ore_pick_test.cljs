(ns engine.ore-pick-test
  "jobs.lib.ore-pick/candidates: which seen ores are picked from where the body stands."
  (:require [cljs.test :refer [deftest is]]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [jobs.lib.ore-pick :as ore-pick]))

(defn candidate-cells
  "Cells ore-pick offers for a body standing at [0 64 0] on stone (top y 63) with the ore at [2 63 0] and the extra blocks."
  [extra]
  (let [p (tu/seeing-all (tu/fake {:self {:pos {:x 0.5 :y 64 :z 0.5}}
                    :inventory [{:name "iron_pickaxe" :count 1}]
                    :blocks (merge (tu/box -4 58 -4 4 63 4 "stone") {"2,63,0" "coal_ore"} extra)}))
        mem* (atom {})
        c {:primitives p :args {:ignore-zones? true} :id "ore-stub" :root "ore-stub" :slots []
           :view (fn [] {:now 1000000 :data {:entries {(mem/job-kind "ore-stub") [{:t 1000000 :data @mem*}]}
                                              :policies {(mem/job-kind "ore-stub") mem/job-policy}}})
           :update-mem (fn [f more] (apply swap! mem* f more))}]
    (mapv (comp vec (juxt :x :y :z) :pos) (ore-pick/candidates c {}))))

(deftest an-ore-in-the-floor-is-picked-over-solid-rock
  (is (= [[2 63 0]] (candidate-cells {}))))

(deftest an-ore-in-the-floor-is-left-over-air
  (is (empty? (candidate-cells {"2,62,0" "air"}))))

