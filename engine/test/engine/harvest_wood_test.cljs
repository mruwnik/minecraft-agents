(ns engine.harvest-wood-test
  "jobs.forestry.harvest-wood ends when it has no sapling to plant (BaseMiner trial: it sat queued with the body idle)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.library-test :as lt]
            [engine.memory :as mem]
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

(deftest any-species-wide-radius-no-sapling-ends-with-the-debt-owed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (lt/setup {:blocks (lt/tree 3 0 "oak" 3)})]
          (core/submit! eng '(jobs.forestry.harvest-wood {:radius 32}) {})
          (is (< (await (lt/run-until-empty eng 40)) 40))
          (is (= [] (filterv #(= :failed (:kind %)) @seen)) "no crash")
          (is (= 3 (get (lt/inv p) "oak_log")))
          (is (= 1 (count (lt/debts eng)))))))))

(deftest a-debt-on-unloaded-land-does-not-crash-the-check
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (lt/setup {:inventory [{:name "oak_sapling" :count 1}] :unloaded ["1500,66,1500"]})]
          (mem/write! (:store eng) :forestry/replant {:pos {:x 1500 :y 66 :z 1500} :species "oak"} {:cap 50 :ttl :forever})
          (core/submit! eng '(jobs.forestry.plant-sapling) {})
          (await (lt/run-until-empty eng 5))
          (is (= [] (filterv #(#{:error :failed} (:kind %)) @seen)) "no crash"))))))

(def far-debt {:pos {:x 1500 :y 66 :z 1500} :species "oak"})

(defn owe-far! [eng] (mem/write! (:store eng) :forestry/replant far-debt {:cap 50 :ttl :forever}))

(deftest a-far-debt-is-left-owed-and-the-near-tree-is-replanted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (lt/setup {:blocks (lt/tree 3 0 "oak" 3) :inventory [{:name "oak_sapling" :count 2}]
                                              :unloaded ["1500,66,1500"]})]
          (owe-far! eng)
          (core/submit! eng '(jobs.forestry.harvest-wood {:radius 32}) {})
          (is (< (await (lt/run-until-empty eng 40)) 40))
          (is (= [] (filterv #(#{:error :failed} (:kind %)) @seen)) "no crash")
          (is (= "oak_sapling" (.-name (.blockAt p #js {:x 3 :y 64 :z 0}))) "the felled tree's spot is planted")
          (is (= [far-debt] (lt/debts eng)) "the far debt stays owed")
          (is (empty? (filterv #(< 100 (js/Math.abs (or (some-> % .-args .-pos .-x) 0))) (lt/calls p "moveTo"))) "no walk far")
          (is (= 1 (count (filterv #(= :harvest-wood.debts-owed (:kind %)) @seen))) "the owed debt is reported"))))))

(deftest a-far-debt-and-no-sapling-ends-with-both-owed-and-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (lt/setup {:blocks (lt/tree 3 0 "oak" 3) :unloaded ["1500,66,1500"]})]
          (owe-far! eng)
          (core/submit! eng '(jobs.forestry.harvest-wood {:radius 32}) {})
          (is (< (await (lt/run-until-empty eng 40)) 40))
          (is (= [] (filterv #(#{:error :failed} (:kind %)) @seen)) "no crash")
          (is (= 2 (count (lt/debts eng))))
          (is (= 1 (count (filterv #(= :harvest-wood.debts-owed (:kind %)) @seen)))))))))

(deftest a-near-debt-on-an-unloaded-cell-is-not-planted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (lt/setup {:inventory [{:name "oak_sapling" :count 1}] :unloaded ["5,66,5"]})]
          (mem/write! (:store eng) :forestry/replant {:pos {:x 5 :y 66 :z 5} :species "oak"} {:cap 50 :ttl :forever})
          (core/submit! eng '(jobs.forestry.plant-sapling) {})
          (await (lt/run-until-empty eng 8))
          (is (= [] (filterv #(#{:error :failed} (:kind %)) @seen)))
          (is (= [] (lt/calls p "place")) "nothing placed into an unknown cell")
          (is (= 1 (count (lt/debts eng)))))))))
