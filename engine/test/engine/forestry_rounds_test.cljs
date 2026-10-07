(ns engine.forestry-rounds-test
  "The forestry jobs run as one whole attempt: a single tick of the engine fells, collects and replants (no step per round)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.forest-maintain-test :as fm :refer [oak-world oak-cell digs places item]]
            [engine.harvest-test :as h]
            [engine.library-test :as lt]
            [engine.test-util :as tu]))

(deftest fell-tree-fells-the-whole-tree-in-one-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (fm/start {:blocks (lt/tree 3 0 "oak" 4)} {})
              out (await (h/child-outcome eng 'jobs.forestry.fell-tree {:radius 10} 1))]
          (is (= {:base {:x 3 :y 64 :z 0}} out))
          (is (= 4 (count (digs p)))))))))

(deftest harvest-wood-fells-collects-and-replants-in-one-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup {:blocks (lt/tree 3 0 "oak" 3) :inventory [{:name "oak_sapling" :count 1}]})]
          (core/submit! eng '(jobs.forestry.harvest-wood {:species "oak" :radius 10}) {})
          (is (= 1 (await (lt/run-until-empty eng 1))) "the list is empty after one tick")
          (is (= 3 (get (lt/inv p) "oak_log")))
          (is (= "oak_sapling" (h/block-at p 3 64 0))))))))

(deftest maintain-fells-and-replants-in-one-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (fm/start oak-world {"forest" oak-cell})]
          (core/submit! eng (list fm/job {:plan "forest"}) {})
          (await (fm/ticks eng 1))
          (is (= 1 (count (h/events-of seen :forest.done))) "done after one tick")
          (is (= [[3 64 0 "oak_sapling"]] (places p))))))))

(deftest prepare-readies-a-cell-in-one-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (fm/start {:blocks (merge (fm/ground [[3 0]]) {"3,64,0" "short_grass"})
                                              :inventory [(item "oak_sapling" 1)]}
                                             {"forest" oak-cell})]
          (core/submit! eng '(jobs.forestry.prepare {:plan "forest"}) {})
          (await (fm/ticks eng 1))
          (is (= 1 (count (h/events-of seen :prepare.done))) "done after one tick")
          (is (= [[3 64 0 "oak_sapling"]] (places p))))))))
