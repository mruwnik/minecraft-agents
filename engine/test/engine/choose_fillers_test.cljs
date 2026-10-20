(ns engine.choose-fillers-test
  "jobs.lib.choose/fillers-for: the ordered filler blocks a body may place for a purpose, for an amount."
  (:require [cljs.test :refer [deftest is are]]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [jobs.forestry.fell-from-pillar :as fell-from-pillar]
            [jobs.gather.mine.mend :as mend]
            [jobs.lib.blocks :as lb]
            [jobs.lib.choose :as choose]
            [jobs.lib.escape :as escape]
            [jobs.lib.reserve :as reserve]
            [jobs.survival.retreat-flight :as flight]))

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
  (let [c (assoc (body [(stack "stone_pickaxe" 1)] {:chooser :fillers :avoid ["dirt"]}) :args {:block "stone"})]
    (is (= ["cobblestone"] (fell-from-pillar/pillar-items c)))
    (is (not-any? #{"dirt"} (mend/fillers c)))))

(deftest the-pillar-is-not-built-of-cobblestone-the-cleanup-could-not-dig-back
  (is (= ["dirt"] (fell-from-pillar/pillar-items (body [(stack "cobblestone" 4) (stack "stone_axe" 1)]))))
  (is (= ["dirt" "cobblestone"] (fell-from-pillar/pillar-items (body [(stack "stone_pickaxe" 1)])))))

(deftest the-shelter-blocks-a-flight-places-skip-an-avoided-filler
  (let [c (body [(stack "dirt" 9) (stack "cobblestone" 3)] {:chooser :fillers :avoid ["cobblestone"]})]
    (is (not-any? #{"cobblestone"} (flight/blocks c)) "the bare list")
    (is (nil? (lb/pick c ["cobblestone"])) "carried skips an avoided stack")
    (is (= [{:name "dirt" :count 9}] (lb/carried c ["dirt" "cobblestone"])))))

(deftest an-unknown-purpose-throws
  (is (thrown? js/Error (choose/fillers-for (body []) :wall 1))))

(defn reserving
  "A body carrying items with register entries (strength 0.5 unless given) on each name."
  [items entries]
  (let [data (reduce #(mem/add-entry %1 :reserve {:t 0 :data %2} reserve/policy) mem/empty-data entries)]
    {:primitives (tu/fake {:inventory items}) :view (fn [] {:data data :now 1})}))

(defn soft [item & {:keys [strength] :or {strength 0.5}}] {:item item :all true :strength strength})

(deftest a-danger-placer-spends-soft-reserved-blocks-it-can-dig-back-out
  (are [items entries names] (= names (map :name (lb/carried (reserving items entries) ["dirt" "cobblestone" "glass" "oak_planks"])))
    [(stack "cobblestone" 4) (stack "stone_pickaxe" 1)] [(soft "cobblestone")] ["cobblestone"]
    [(stack "cobblestone" 4)] [(soft "cobblestone")] []
    [(stack "glass" 4) (stack "stone_pickaxe" 1)] [(soft "glass")] []
    [(stack "dirt" 4)] [(soft "dirt")] ["dirt"]
    [(stack "dirt" 4) (stack "stone_pickaxe" 1)] [(soft "dirt" :strength 1)] []))

(deftest unreserved-blocks-come-before-soft-reserved-ones
  (let [c (reserving [(stack "cobblestone" 4) (stack "dirt" 2) (stack "stone_pickaxe" 1)] [(soft "cobblestone")])]
    (is (= ["dirt" "cobblestone"] (map :name (lb/carried c ["cobblestone" "dirt"]))))
    (is (= ["dirt" "cobblestone"] (map :item (:list (choose/fillers-for c :cover 1)))))))

(deftest a-plain-build-still-leaves-soft-reserves-alone
  (is (= [] (:list (choose/fillers-for (reserving [(stack "cobblestone" 4) (stack "stone_pickaxe" 1)] [(soft "cobblestone")]) :build 1)))))
