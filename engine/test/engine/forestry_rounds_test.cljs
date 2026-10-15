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
              out (await (tu/child-outcome eng 'jobs.forestry.fell-tree {:radius 10} 1))]
          (is (= {:base {:x 3 :y 64 :z 0} :logs 4 :axe :skipped-small-tree :status :done} (dissoc out :ending :text)))
          (is (= 4 (count (digs p)))))))))

(deftest maintain-fells-and-replants-in-one-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (fm/start oak-world {"forest" oak-cell})]
          (core/submit! eng (list fm/job {:plan "forest"}) {})
          (await (fm/ticks eng 1))
          (is (= 1 (count (h/events-of seen :completed))) "done after one tick")
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
          (is (= 1 (count (h/events-of seen :completed))) "done after one tick")
          (is (= [[3 64 0 "oak_sapling"]] (places p))))))))
