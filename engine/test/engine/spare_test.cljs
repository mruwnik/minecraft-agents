(ns engine.spare-test
  "jobs.lib.trip/tool-ready!: a stair that would dig with the last worn pickaxe gets a spare first, or says why not."
  (:require [cljs.test :refer [deftest is async]]
            [engine.blocks-dig-test :as bd]
            [engine.mine-support :as m]
            [engine.stair-test :as st]
            [engine.test-util :as tu]))

(def worn {:name "stone_pickaxe" :count 1 :durability 4 :max-durability 131})
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
          (is (= 2 (get (m/inv s) "stone_pickaxe")) "the spare is a second pickaxe on top of the worn one carried")
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
        (doseq [inv [[fresh] [worn fresh] [(assoc worn :durability 10)] [{:name "stone_pickaxe" :count 1 :durability 1}]]]
          (let [s (await (st/stair! (assoc spare-chest :inventory inv) east tu/seeing-all))]
            (is (not-any? #{:tool.spare :tool.no-spare} (kinds s)))))))))

(deftest a-worn-last-iron-pickaxe-gets-the-cheapest-pickaxe-that-harvests-stone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [worn-iron {:name "iron_pickaxe" :count 1 :durability 4 :max-durability 250}
              s (await (st/stair! (assoc spare-chest :inventory [worn-iron]) east tu/seeing-all))]
          (is (= :done (:status @(:out s))))
          (is (= ["stone_pickaxe"] (map :tool (st/events-of s :tool.spare)))))))))

(deftest a-low-tool-is-told-once-across-digs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (bd/setup {:self bd/body :blocks {"2,64,0" "stone" "2,64,1" "stone"}
                                            :inventory [worn]})]
          (await (tu/child-outcome eng 'jobs.blocks.dig {:pos [2 64 0]} 10))
          (await (tu/child-outcome eng 'jobs.blocks.dig {:pos [2 64 1]} 10))
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

(deftest a-spare-fetched-from-afar-does-not-move-the-mines-start
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [corridor (-> (m/cells "air" (range 1 7) [64 65] [0]) (assoc "7,64,0" "chest"))
              world (-> (m/rock-world corridor)
                        (assoc :inventory [worn] :containers {"7,64,0" [{:name "stone_pickaxe" :count 1}]}))
              s (await (m/scenario {:block "iron_ore" :level false :count 5 :tunnel-length 0} world 60))]
          (is (contains? (kinds s) :tool.spare))
          (is (= [0 64 0] (m/feet s))))))))

(deftest a-pickaxe-worn-down-mid-stair-gets-a-spare-instead-of-stopping-tool-spent
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [wear (fn [p]
                     (let [world (.-world p)]
                       (.override world "dig"
                                  (fn ^:async f [token a impl]
                                    (let [r (await (impl token a))]
                                      (swap! (.. world -state) assoc-in [:inventory 0 :durability] 3)
                                      r)))))
              s (await (st/stair! (assoc spare-chest :inventory [{:name "iron_pickaxe" :count 1 :durability 240 :max-durability 250}]) east (fn [p] (tu/seeing-all p) (wear p))))]
          (is (= :done (:status @(:out s))) (pr-str @(:out s)))
          (is (contains? (kinds s) :tool.spare) (pr-str (kinds s))))))))

(deftest a-spare-is-crafted-with-the-dig-line-kept-clear-of-the-table
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (st/stair! {:blocks st/ground :inventory [worn {:name "cobblestone" :count 12} {:name "stick" :count 4} {:name "crafting_table" :count 1}]}
                                  east tu/seeing-all))]
          (is (= :done (:status @(:out s))) (pr-str (st/events-of s :stair.stopped))))))))
