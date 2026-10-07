(ns engine.sensing-forestry-test
  "Forestry and apiary scans read what the body has seen, not the world through walls."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.library-test :as lt]
            [engine.apiary-harvest-test :as ah]
            [engine.test-util :as tu]
            [jobs.lib.trees :as trees]
            [jobs.lib.apiary :as apiary]))

(defn crowned-tree
  "A 4-log tree at x z with a 3x3x3 leaf crown on top."
  [x z]
  (merge (lt/tree x z "oak" 4)
         (into {} (for [dx (range -1 2) dy (range 0 3) dz (range -1 2)]
                    [(str (+ x dx) "," (+ 68 dy) "," (+ z dz)) "oak_leaves"]))))

(def forest
  "40 trees, 5 apart; the last one stands 35 blocks out."
  (apply merge (for [i (range 8) j (range 5)] (crowned-tree (+ 4 (* 5 i)) (* 5 j)))))

(deftest a-tree-the-body-has-not-seen-is-not-found
  (let [{:keys [p]} (lt/setup {:blocks (lt/tree 3 0 "oak" 3)})]
    (is (= 1 (count (trees/trees-near p 10 nil))) "seen: found")
    (is (empty? (trees/trees-near (tu/blind p) 10 nil)) "unseen: not a tree")))

(deftest fell-tree-does-not-dig-a-tree-it-has-not-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup {:blocks (lt/tree 3 0 "oak" 3)})]
          (tu/blind p)
          (core/submit! eng '(jobs.forestry.fell-tree {:radius 10}) {})
          (await (tu/run-until-empty eng 4))
          (is (= [] (lt/calls p "dig"))))))))

(defn seeing-after-look
  "p whose seen blocks stay empty until the body has turned (a look call), then answer the world."
  [p]
  (aset p "seenBlocks" (fn [q] (if (seq (lt/calls p "look")) (.blocks p q) #js [])))
  p)

(deftest fell-tree-looks-around-before-it-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup {:blocks (lt/tree 3 0 "oak" 3)})]
          (tu/blind p)
          (seeing-after-look p)
          (core/submit! eng '(jobs.forestry.fell-tree {:radius 10}) {})
          (await (tu/run-until-empty eng 40))
          (is (seq (lt/calls p "dig")) "the tree it saw after turning is dug"))))))

(deftest the-farthest-tree-of-a-dense-forest-keeps-its-leaves
  (let [{:keys [p]} (lt/setup {:blocks forest})
        found (trees/trees-near p 60 nil)]
    (is (= 40 (count found)))
    (is (= 39 (:x (:column (last found)))) "the one 35 blocks out is a tree")))

(deftest a-hive-the-body-has-not-seen-is-not-harvested
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (ah/setup (ah/world {:inventory (ah/inv "shears" 1)}))]
          (tu/blind p)
          (is (= :no-hive (:reason (await (tu/child-outcome eng 'jobs.apiary.harvest {} 40)))))
          (is (= [] (ah/calls p "useOn"))))))))

(deftest a-fire-the-body-has-not-seen-is-not-listed
  (let [{:keys [p]} (ah/setup (ah/world {}))
        area {:center {:x 0 :y 64 :z 0} :radius 12}]
    (is (= 1 (count (apiary/fires p area))))
    (is (empty? (apiary/fires (tu/blind p) area)))))
