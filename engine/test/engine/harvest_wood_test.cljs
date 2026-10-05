(ns engine.harvest-wood-test
  "jobs.forestry.harvest-wood ends when it has no sapling to plant (BaseMiner trial: it sat queued with the body idle)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.library-test :as lt]
            [engine.test-util :as tu]))

(def harvest '(jobs.forestry.harvest-wood {:species "oak" :radius 10}))

(deftest no-sapling-carried-ends-the-job-with-the-debt-owed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup {:blocks (lt/tree 3 0 "oak" 3)})]
          (core/submit! eng harvest {})
          (is (< (await (lt/run-until-empty eng 30)) 30) "finishes instead of waiting for a sapling")
          (is (= [] (:list (core/state eng))))
          (is (= 3 (get (lt/inv p) "oak_log")) "logs collected")
          (is (= 1 (count (lt/debts eng))) "the replant stays owed"))))))

(deftest a-sapling-carried-is-still-planted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup {:blocks (lt/tree 3 0 "oak" 3) :inventory [{:name "oak_sapling" :count 1}]})]
          (core/submit! eng harvest {})
          (is (< (await (lt/run-until-empty eng 30)) 30))
          (is (= "oak_sapling" (.-name (.blockAt p #js {:x 3 :y 64 :z 0}))))
          (is (= [] (lt/debts eng))))))))

(deftest no-tree-in-range-waits-and-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (lt/setup {:blocks (lt/tree 9 0 "oak" 3)})]
          (core/submit! eng '(jobs.forestry.harvest-wood {:radius 2}) {})
          (await (lt/run-until-empty eng 4))
          (is (= 1 (count (:list (core/state eng)))) "still queued")
          (is (= [{:reason :no-tree :radius 2}]
                 (mapv #(select-keys % [:reason :radius :species]) (filterv #(= :waiting (:kind %)) @seen)))
              "one job.waiting naming the reason"))))))
