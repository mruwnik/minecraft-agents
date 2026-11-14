(ns plan.adopt-test
  (:require [clojure.test :refer [deftest is are]]
            [plan.adopt :as adopt]
            [plan.shape :as shape]))

(defn run
  "adopt over a box [[0 0 0] [x 0 0]] whose cells are the blocks of world (index -> block, absent = unseen)."
  [world & {:as opts}]
  (adopt/adopt (merge {:id "p" :box [[0 0 0] [(dec (count world)) 0 0]] :body "B" :date "d"
                       :block-at (fn [[x]] (get world x))}
                      opts)))

(defn parts [result] (into {} (map (juxt :id #(select-keys % [:cells :box :want]))) (-> result :plan :parts)))

(deftest unseen-and-air-are-left-out-and-terrain-is-kept-as-it-is
  (let [r (run [nil {:name "air"} {:name "dirt"} {:name "stone"} {:name "cobblestone"}])]
    (is (= {"dirt" {:cells [[2 0 0]] :want "dirt"} "stone" {:cells [[3 0 0]] :want "stone"} "cobblestone" {:cells [[4 0 0]] :want "cobblestone"}}
           (parts r)))
    (is (= 1 (:unseen r)))))

(deftest a-crop-is-a-crop-want-and-its-farmland-is-kept
  (let [r (adopt/adopt {:id "p" :box [[0 0 0] [1 1 0]] :body "B" :date "d"
                        :block-at {[0 0 0] {:name "farmland"} [0 1 0] {:name "wheat" :state {"age" "3"}}
                                   [1 0 0] {:name "farmland"} [1 1 0] {:name "air"}}})]
    (is (= {"wheat" {:cells [[0 1 0]] :want {:crop "wheat"}}
            "farmland" {:box [[0 0 0] [1 0 0]] :want "farmland"}}
           (parts r)))))

(deftest cane-and-stems-are-crop-wants-of-any-stage-and-fruit-is-left-out
  (let [r (run [{:name "sugar_cane" :state {"age" "3"}} {:name "sugar_cane"} {:name "melon"} {:name "pumpkin"} {:name "melon_stem"} {:name "attached_pumpkin_stem"}])]
    (is (= {"sugar_cane" {:box [[0 0 0] [1 0 0]] :want {:crop "sugar_cane"}}
            "melon_stem" {:cells [[4 0 0]] :want {:crop "melon_stem"}}
            "pumpkin_stem" {:cells [[5 0 0]] :want {:crop "pumpkin_stem"}}}
           (parts r)))))

(deftest cells-filling-a-box-are-one-box-part
  (let [r (run [{:name "dirt"} {:name "dirt"} {:name "dirt"} {:name "stone"} nil {:name "stone"}])]
    (is (= {"dirt" {:box [[0 0 0] [2 0 0]] :want "dirt"} "stone" {:cells [[3 0 0] [5 0 0]] :want "stone"}}
           (into {} (map (juxt :id #(select-keys % [:box :cells :want]))) (-> r :plan :parts))))
    (is (= {"dirt" 3 "stone" 2} (:parts r)))
    (is (= [] (shape/plan-errors (:plan r) "p")))))

(deftest an-any-area-is-a-part-and-none-of-its-cells-is-recorded
  (let [r (run [{:name "dirt"} {:name "stone"} {:name "stone"} {:name "dirt"}] :any [[[1 0 0] [2 0 0]]])]
    (is (= {"dirt" [[0 0 0] [3 0 0]]} (into {} (map (juxt :id :cells)) (filter :cells (-> r :plan :parts)))))
    (is (= {:box [[1 0 0] [2 0 0]] :want :any} (select-keys (-> r :plan :parts last) [:box :want])))
    (is (= [] (shape/plan-errors (:plan r) "p")))))

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

(deftest a-bed-keeps-its-part-so-head-and-foot-are-two-wants
  (let [r (run [{:name "white_bed" :state {"facing" "east" "part" "head" "occupied" "false"}}
                {:name "white_bed" :state {"facing" "east" "part" "foot"}}])]
    (is (= {"bed" {:cells [[0 0 0]] :want {:block "white_bed" :facing "east" :part "head"}}
            "bed-2" {:cells [[1 0 0]] :want {:block "white_bed" :facing "east" :part "foot"}}}
           (parts r)))))

(deftest a-pen-adds-a-part-with-box-and-herd-that-agrees-with-the-walls
  (let [r (run [{:name "oak_fence"}] :pen {:box [[0 0 0] [8 0 8]] :mob "cow" :target 4})]
    (is (= {:box [[0 0 0] [8 0 8]] :want :any :herd {:mob "cow" :target 4}} (select-keys (-> r :plan :parts last) [:box :want :herd])))
    (is (= "pen" (-> r :plan :parts last :id)))
    (is (= [] (shape/plan-errors (:plan r) "p")))))
