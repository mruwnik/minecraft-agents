(ns engine.spare-test
  "jobs.lib.trip/tool-ready!: a stair that would dig with the last worn pickaxe gets a spare first, or says why not."
  (:require [cljs.test :refer [deftest is async]]
            [engine.blocks-dig-test :as bd]
            [engine.mine-test :as m]
            [engine.stair-test :as st]
            [engine.test-util :as tu]))

(def worn {:name "stone_pickaxe" :count 1 :durability 10 :max-durability 131})
(def fresh {:name "stone_pickaxe" :count 1 :durability 120 :max-durability 131})
(def chest "2,65,1")
(def spare-chest {:blocks (assoc st/ground chest "chest") :containers {chest [{:name "stone_pickaxe" :count 1}]}})
(def east (assoc st/east :steps 2))

(defn kinds [s] (set (map :kind @(:seen s))))

(deftest a-worn-last-pickaxe-gets-a-spare-from-a-seen-chest-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (st/stair! (assoc spare-chest :inventory [worn]) east tu/seeing-all))]
          (is (= :done (:status @(:out s))))
          (is (contains? (kinds s) :tool.spare))
          (is (not (contains? (kinds s) :tool.no-spare))))))))

(deftest a-worn-last-pickaxe-with-no-spare-to-be-had-warns-and-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (st/stair! {:blocks st/ground :inventory [worn]} east (fn [_])))]
          (is (= :done (:status @(:out s))))
          (is (contains? (kinds s) :tool.no-spare))
          (is (= 1 (count (st/events-of s :tool.no-spare)))))))))

(deftest a-healthy-pickaxe-or-a-second-one-needs-no-spare
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [inv [[fresh] [worn fresh] [{:name "stone_pickaxe" :count 1 :durability 1}]]]
          (let [s (await (st/stair! (assoc spare-chest :inventory inv) east tu/seeing-all))]
            (is (not-any? #{:tool.spare :tool.no-spare} (kinds s)))))))))

(deftest a-worn-last-iron-pickaxe-gets-the-cheapest-pickaxe-that-harvests-stone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [worn-iron {:name "iron_pickaxe" :count 1 :durability 10 :max-durability 250}
              s (await (st/stair! (assoc spare-chest :inventory [worn-iron]) east tu/seeing-all))]
          (is (= :done (:status @(:out s))))
          (is (= ["stone_pickaxe"] (map :tool (st/events-of s :tool.spare)))))))))

(deftest a-low-tool-is-told-once-across-digs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (bd/setup {:self bd/body :blocks {"2,64,0" "stone" "2,64,1" "stone"}
                                            :inventory [worn]})]
          (await (bd/child-outcome eng 'jobs.blocks.dig {:pos [2 64 0]} 10))
          (await (bd/child-outcome eng 'jobs.blocks.dig {:pos [2 64 1]} 10))
          (is (= 1 (count (filter #(= :tool.low (:kind %)) @seen)))))))))

(deftest a-spare-fetched-from-afar-does-not-move-the-stairs-start
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [far "10,65,0"
              s (await (st/stair! {:blocks (assoc st/ground far "chest") :containers {far [{:name "stone_pickaxe" :count 1}]}
                                   :inventory [worn]}
                                  east tu/seeing-all))]
          (is (contains? (kinds s) :tool.spare))
          (is (= :done (:status @(:out s))))
          (is (= [2 63 0] (:at @(:out s)))))))))

(deftest a-spare-fetch-never-asks-for-a-spare-itself
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (m/scenario {:block "stone" :count 2} (assoc (m/rock-world {}) :inventory [worn {:name "stick" :count 2} {:name "crafting_table" :count 1}]) 60))
              spare-depth (fn [e] (count (re-seq #"/spare" (str (:job e)))))]
          (is (<= (apply max 0 (map spare-depth @(:seen s))) 1) "a spare's own digging has no spare child"))))))
