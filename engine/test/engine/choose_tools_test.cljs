(ns engine.choose-tools-test
  "jobs.lib.choose/tools-for: the ordered tools a body may dig a block with, for an amount of uses."
  (:require [cljs.test :refer [deftest is are]]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [jobs.lib.choose :as choose]
            [jobs.lib.trip :as trip]))

(defn tool [name left max] {:name name :count 1 :durability left :max-durability max})

(defn body
  "A ctx with the fake primitives p holding items, and body memory with the override entries (each a data map)."
  [items & overrides]
  (let [data (reduce #(mem/add-entry %1 :choose/override {:t 0 :data %2} {:cap 20 :ttl :forever}) mem/empty-data overrides)]
    {:primitives (tu/fake {:inventory items}) :view (fn [] {:data data :now 1})}))

(def stone-and-iron [(tool "stone_pickaxe" 12 131) (tool "iron_pickaxe" 180 250)])

(deftest cheapest-material-first-and-short-is-what-the-uses-lack
  (are [amount short] (= {:list [{:item "stone_pickaxe" :uses-left 12} {:item "iron_pickaxe" :uses-left 180}] :short short}
                         (choose/tools-for (body stone-and-iron) [:dig "stone"] amount))
    40 0
    192 0
    200 8))

(deftest fast-puts-the-best-material-first
  (is (= ["iron_pickaxe" "stone_pickaxe"]
         (map :item (:list (choose/tools-for (body stone-and-iron) [:dig "stone"] 40 {:fast true}))))))

(deftest a-tool-that-cannot-harvest-or-of-another-kind-is-left-out
  (is (= {:list [{:item "iron_pickaxe" :uses-left 180}] :short 0}
         (choose/tools-for (body [(tool "wooden_pickaxe" 50 59) (tool "iron_axe" 100 250) (tool "iron_pickaxe" 180 250)])
                           [:dig "iron_ore"] 40))))

(deftest nothing-carried-is-all-short
  (is (= {:list [] :short 40} (choose/tools-for (body []) [:dig "stone"] 40))))

(deftest an-override-avoids-a-tool-and-prefers-others
  (let [c (body stone-and-iron {:chooser :tools :avoid ["stone_pickaxe"]})]
    (is (= ["iron_pickaxe"] (map :item (:list (choose/tools-for c [:dig "stone"] 40))))))
  (let [c (body stone-and-iron {:chooser :tools :prefer ["iron_pickaxe"]})]
    (is (= ["iron_pickaxe" "stone_pickaxe"] (map :item (:list (choose/tools-for c [:dig "stone"] 40)))))))

(deftest an-unknown-purpose-is-an-error
  (is (thrown? js/Error (choose/tools-for (body []) [:swim "stone"] 1))))

(deftest the-most-worn-of-a-material-goes-first
  (is (= [20 100] (map :uses-left (:list (choose/tools-for (body [(tool "stone_pickaxe" 100 131) (tool "stone_pickaxe" 20 131)]) [:dig "stone"] 1))))))

(deftest cobweb-is-dug-with-shears-else-a-sword
  (are [items expected] (= expected (map :item (:list (choose/tools-for (body items) [:dig "cobweb"] 1))))
    [(tool "iron_pickaxe" 100 250) (tool "shears" 200 238)] ["shears"]
    [(tool "iron_pickaxe" 100 250) (tool "stone_sword" 50 131) (tool "shears" 200 238)] ["shears"]
    [(tool "iron_pickaxe" 100 250) (tool "stone_sword" 50 131)] ["stone_sword"]
    [(tool "iron_pickaxe" 100 250)] []))

(deftest a-spare-is-wanted-when-the-uses-fall-under-the-worn-line
  (are [items wanted] (= wanted (some? (trip/spare-need (body items) "stone")))
    [(tool "stone_pickaxe" 20 131)] true
    [(tool "stone_pickaxe" 100 131)] false
    [] false))

(def worn-stone-and-iron [(tool "stone_pickaxe" 20 131) (tool "iron_pickaxe" 20 250)])

(deftest a-spare-is-wanted-when-the-listed-uses-fall-short-of-the-amount
  (are [items amount wanted] (= wanted (some? (trip/spare-need (body items) "stone" amount)))
    worn-stone-and-iron 40 false
    worn-stone-and-iron 41 true
    [(tool "stone_pickaxe" 20 131)] 5 false
    [(tool "stone_pickaxe" 20 131)] 21 true
    [] 5 false))

(deftest the-spare-of-a-worn-axe-for-a-log-is-a-stone-axe-not-its-own-kind
  (let [c (body [(tool "iron_axe" 2 250)])
        need (trip/spare-need c "oak_log" 8)]
    (is (= "stone_axe" (trip/spare-name (:primitives c) "oak_log" need)))))

(deftest kind-purposes-list-the-carried-tools-of-their-kinds
  (let [c (body [(tool "iron_pickaxe" 180 250) (tool "iron_axe" 100 250) (tool "stone_sword" 20 131) (tool "iron_hoe" 50 250) (tool "shears" 100 238)])]
    (are [purpose items] (= items (map :item (:list (choose/tools-for c purpose 1))))
      [:attack] ["iron_axe" "stone_sword"]
      [:till] ["iron_hoe"]
      [:shear] ["shears"])
    (is (= ["iron_axe" "stone_sword"] (map :item (:list (choose/tools-for c [:attack] 1 {:fast true})))))
    (is (= ["stone_sword" "stone_axe"] (map :item (:list (choose/tools-for (body [(tool "stone_axe" 50 131) (tool "stone_sword" 50 131)]) [:attack] 1 {:fast true})))) "a sword before an axe of the same material")
    (is (= ["stone_sword" "stone_axe"] (map :item (:list (choose/tools-for (body [(tool "stone_axe" 50 131) (tool "stone_sword" 50 131)]) [:attack] 1)))))))
