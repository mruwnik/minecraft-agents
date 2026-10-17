(ns engine.choose-fillers-test
  "jobs.lib.choose/fillers-for: the ordered filler blocks a body may place for a purpose, for an amount."
  (:require [cljs.test :refer [deftest is are]]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [jobs.forestry.fell-from-pillar :as fell-from-pillar]
            [jobs.gather.mine.mend :as mend]
            [jobs.lib.choose :as choose]
            [jobs.lib.escape :as escape]))

(defn body
  [items & overrides]
  (let [data (reduce #(mem/add-entry %1 :choose/override {:t 0 :data %2} {:cap 20 :ttl :forever}) mem/empty-data overrides)]
    {:primitives (tu/fake {:inventory items}) :view (fn [] {:data data :now 1})}))

(defn stack [name n] {:name name :count n})

(def mixed [(stack "oak_planks" 10) (stack "stone" 5) (stack "dirt" 3) (stack "sand" 64) (stack "oak_log" 4)])

(deftest the-list-keeps-the-order-of-the-purpose-and-short-is-what-the-amount-lacks
  (are [amount short] (= {:list [{:item "dirt" :count 3} {:item "stone" :count 5} {:item "oak_planks" :count 10}] :short short}
                         (choose/fillers-for (body mixed) :build amount))
    18 0
    10 0
    30 12))

(deftest each-purpose-has-its-own-set
  (are [purpose items] (= items (map :item (:list (choose/fillers-for (body mixed) purpose 1))))
    :pillar ["dirt"]
    :climb ["dirt" "stone" "oak_planks"]
    :cover ["stone" "dirt"]
    :shelter ["dirt" "stone" "oak_planks" "oak_log"]
    :seal ["stone" "dirt"]))

(deftest nothing-carried-is-all-short
  (is (= {:list [] :short 7} (choose/fillers-for (body []) :build 7))))

(deftest an-override-avoids-a-filler
  (is (= ["dirt" "oak_planks"]
         (map :item (:list (choose/fillers-for (body mixed {:chooser :fillers :avoid ["stone"]}) :build 1))))))

(deftest the-fetch-list-and-the-escape-pillar-block-skip-an-avoided-filler
  (let [c (body [(stack "dirt" 9) (stack "cobblestone" 3)] {:chooser :fillers :avoid ["dirt"]})]
    (is (= ["cobblestone"] (choose/filler-names c :pillar)))
    (is (= {:item "dirt" :count 9} (escape/pillar-item (body [(stack "dirt" 9) (stack "cobblestone" 3)]))))
    (is (= {:item "cobblestone" :count 3} (escape/pillar-item c)))))

(deftest the-pillar-and-mend-block-lists-skip-an-avoided-filler
  (let [c (assoc (body [] {:chooser :fillers :avoid ["dirt"]}) :args {:block "stone"})]
    (is (= ["cobblestone"] (fell-from-pillar/pillar-items c)))
    (is (not-any? #{"dirt"} (mend/fillers c)))))

(deftest an-unknown-purpose-throws
  (is (thrown? js/Error (choose/fillers-for (body []) :wall 1))))
