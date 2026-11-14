(ns plan.adopt-test
  (:require [clojure.test :refer [deftest is are]]
            [plan.adopt :as adopt]
            [plan.shape :as shape]))

(def natural? #{"dirt" "stone" "grass_block"})

(defn run
  "adopt over a box [[0 0 0] [x 0 0]] whose cells are the blocks of world (index -> block, absent = unseen)."
  [world & {:as opts}]
  (adopt/adopt (merge {:id "p" :box [[0 0 0] [(dec (count world)) 0 0]] :natural? natural? :body "B" :date "d"
                       :block-at (fn [[x]] (get world x))}
                      opts)))

(defn parts [result] (into {} (map (juxt :id #(select-keys % [:cells :want]))) (-> result :plan :parts)))

(deftest unseen-air-and-terrain-are-left-out
  (let [r (run [nil {:name "air"} {:name "dirt"} {:name "stone"} {:name "cobblestone"}])]
    (is (= {"cobblestone" {:cells [[4 0 0]] :want "cobblestone"}} (parts r)))
    (is (= 1 (:unseen r)))))

(deftest terrain-option-keeps-natural-blocks
  (is (= {"dirt" {:cells [[0 0 0]] :want "dirt"}} (parts (run [{:name "dirt"}] :terrain? true)))))

(deftest a-crop-is-a-crop-want-and-its-farmland-is-left-out
  (let [r (adopt/adopt {:id "p" :box [[0 0 0] [1 1 0]] :natural? natural? :body "B" :date "d"
                        :block-at {[0 0 0] {:name "farmland"} [0 1 0] {:name "wheat" :state {"age" "3"}}
                                   [1 0 0] {:name "farmland"} [1 1 0] {:name "air"}}})]
    (is (= {"wheat" {:cells [[0 1 0]] :want {:crop "wheat"}}
            "farmland" {:cells [[1 0 0]] :want "farmland"}}
           (parts r)))))

(deftest a-sapling-is-a-tree-want
  (is (= {:tree "oak"} (-> (run [{:name "oak_sapling"}]) :plan :parts first :want))))

(deftest a-gate-is-recorded-by-facing-not-by-open
  (let [r (run [{:name "oak_fence_gate" :state {"facing" "north" "open" "true" "powered" "false" "in_wall" "false"}}])]
    (is (= {"gate" {:cells [[0 0 0]] :want {:block "oak_fence_gate" :facing "north"}}} (parts r)))))

(deftest only-source-fluid-is-kept
  (let [r (run [{:name "water" :state {"level" "0"}} {:name "water" :state {"level" "3"}} {:name "lava" :state {"level" "8"}}])]
    (is (= {"water" {:cells [[0 0 0]] :want "water"}} (parts r)))))

(deftest waterlogging-and-connections-are-not-state-of-a-want
  (is (= "oak_fence" (-> (run [{:name "oak_fence" :state {"waterlogged" "true" "north" "true"}}]) :plan :parts first :want))))

(deftest same-family-different-wants-get-numbered-ids
  (let [r (run [{:name "chest" :state {"facing" "north"}} {:name "chest" :state {"facing" "south"}} {:name "chest" :state {"facing" "north"}}])]
    (is (= {"chest" [[0 0 0] [2 0 0]] "chest-2" [[1 0 0]]} (into {} (map (juxt :id :cells)) (-> r :plan :parts))))))

(deftest the-plan-is-a-valid-plan-with-a-note
  (let [r (run [{:name "oak_fence_gate" :state {"facing" "north"}} {:name "oak_fence"} {:name "white_bed" :state {"facing" "east" "part" "head"}} {:name "oak_wall"}])]
    (is (= [] (shape/plan-errors (:plan r) "p")))
    (is (= "adopted from B's sight d" (-> r :plan :note)))
    (is (= ["gate" "fence" "bed" "wall-oak"] (map :id (-> r :plan :parts))))
    (is (= 4 (:cells r)))))
