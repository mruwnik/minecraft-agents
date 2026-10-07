(ns engine.harvest-wood-test
  "jobs.forestry.harvest-wood ends when it has no sapling to plant (BaseMiner trial: it sat queued with the body idle)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.ctx :as ctx]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.library-test :as lt]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [jobs.forestry.harvest-wood :as hw]
            [jobs.forestry.plant-sapling :as ps]
            [jobs.lib.trees :as trees]))

(def harvest '(jobs.forestry.harvest-wood {:species "oak" :radius 10}))

(deftest no-sapling-carried-ends-the-job-with-the-debt-owed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (lt/setup {:blocks (lt/tree 3 0 "oak" 3)})]
          (core/submit! eng harvest {})
          (is (< (await (tu/run-until-empty eng 30)) 30) "finishes instead of waiting for a sapling")
          (is (= [] (:list (core/state eng))))
          (is (= 3 (get (lt/inv p) "oak_log")) "logs collected")
          (is (= 1 (count (lt/debts eng))) "the replant stays owed")
          (let [w (filterv #(= :harvest-wood.replant-owed (:kind %)) @seen)]
            (is (= 1 (count w)) "the unplanted spot is reported")
            (is (= 1 (:count (first w))))
            (is (= :no-sapling (:reason (first w))))
            (is (= (:pos (first (lt/debts eng))) (:pos (first w))))
            (is (= "felled 1 tree, could not replant: no sapling" (:text (first w))))))))))

(deftest no-sapling-hands-over-replant-owed-and-is-still-completed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (lt/setup {:blocks (lt/tree 3 0 "oak" 3)})]
          (is (= {:replant-owed 1} (await (tu/child-outcome eng 'jobs.forestry.harvest-wood {:species "oak" :radius 10} 30)))))))))

(deftest a-sapling-carried-is-still-planted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup {:blocks (lt/tree 3 0 "oak" 3) :inventory [{:name "oak_sapling" :count 1}]})]
          (core/submit! eng harvest {})
          (is (< (await (tu/run-until-empty eng 30)) 30))
          (is (= "oak_sapling" (.-name (.blockAt p #js {:x 3 :y 64 :z 0}))))
          (is (= [] (lt/debts eng))))))))

(deftest no-tree-in-range-waits-and-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (lt/setup {:blocks (lt/tree 9 0 "oak" 3)})]
          (core/submit! eng '(jobs.forestry.harvest-wood {:radius 2}) {})
          (await (tu/run-until-empty eng 4))
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
          (is (< (await (tu/run-until-empty eng 40)) 40))
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
          (await (tu/run-until-empty eng 5))
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
          (is (< (await (tu/run-until-empty eng 40)) 40))
          (is (= [] (filterv #(#{:error :failed} (:kind %)) @seen)) "no crash")
          (is (= "oak_sapling" (.-name (.blockAt p #js {:x 3 :y 64 :z 0}))) "the felled tree's spot is planted")
          (is (= [far-debt] (lt/debts eng)) "the far debt stays owed")
          (is (empty? (filterv #(< 100 (js/Math.abs (or (some-> % .-args .-pos .-x) 0))) (lt/calls p "moveTo"))) "no walk far")
          (is (= 1 (count (filterv #(= :harvest-wood.debts-owed (:kind %)) @seen))) "the owed debt is reported")
          (is (= [] (filterv #(= :harvest-wood.replant-owed (:kind %)) @seen)) "nothing near is owed"))))))

(deftest a-far-debt-and-no-sapling-ends-with-both-owed-and-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (lt/setup {:blocks (lt/tree 3 0 "oak" 3) :unloaded ["1500,66,1500"]})]
          (owe-far! eng)
          (core/submit! eng '(jobs.forestry.harvest-wood {:radius 32}) {})
          (is (< (await (tu/run-until-empty eng 40)) 40))
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
          (await (tu/run-until-empty eng 8))
          (is (= [] (filterv #(#{:error :failed} (:kind %)) @seen)))
          (is (= [] (lt/calls p "place")) "nothing placed into an unknown cell")
          (is (= 1 (count (lt/debts eng)))))))))

(defn clear-drops!
  "Remove every entity (the dropped items) from the fake world."
  [p]
  (doseq [e (fake/entities p)]
    (swap! (fake/state p) update :entities (fn [es] (remove (fn [x] (= (:id e) (:id x))) es)))))

(defn after-last-dig!
  "Call (f) once the third log of the tree is dug: the felling is over, the collecting not begun."
  [p f]
  (let [n (atom 0)]
    (.override (.-world p) "dig"
               (fn ^:async dig [token args impl]
                 (let [r (await (impl token args))]
                   (when (= 3 (swap! n inc)) (f))
                   r)))))

(deftest a-body-moved-away-after-the-felling-still-collects-the-drops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup {:blocks (lt/tree 3 0 "oak" 3)})]
          (after-last-dig! p #(fake/swap-self! p assoc :pos [-30 64 0]))
          (core/submit! eng '(jobs.forestry.harvest-wood {:species "oak" :radius 10}) {})
          (await (tu/run-until-empty eng 30))
          (is (= 3 (get (lt/inv p) "oak_log")) "the drops at the tree are fetched, not forgotten")
          (is (= [] (:list (core/state eng)))))))))

(deftest nothing-collected-after-a-felling-is-not-completed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (lt/setup {:blocks (lt/tree 3 0 "oak" 3)})]
          (after-last-dig! p #(clear-drops! p))
          (core/submit! eng '(jobs.forestry.harvest-wood {:species "oak" :radius 10}) {})
          (await (tu/run-until-empty eng 30))
          (is (= [:nothing-collected] (mapv :reason (filterv #(= :stopped (:kind %)) @seen))) "no item came in: stopped with a reason, not completed")
          (is (zero? (get (lt/inv p) "oak_log" 0))))))))

(deftest logs-picked-up-during-the-felling-count-as-collected
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (lt/setup {:blocks (lt/tree 3 0 "oak" 3) :inventory [{:name "oak_sapling" :count 1}]})]
          (after-last-dig! p #(do (clear-drops! p) (fake/add-item! p "oak_log" 3)))
          (core/submit! eng '(jobs.forestry.harvest-wood {:species "oak" :radius 10}) {})
          (await (tu/run-until-empty eng 30))
          (is (= [] (filterv #(= :stopped (:kind %)) @seen)) "the logs are in the inventory: not stopped")
          (is (= "oak_sapling" (.-name (.blockAt p #js {:x 3 :y 64 :z 0}))) "the replant is done")
          (is (= [] (lt/debts eng))))))))

(deftest a-foreign-block-on-the-planting-spot-fails-and-keeps-the-debt
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (lt/setup {:blocks {"3,64,0" "stone" "3,63,0" "grass_block"} :inventory [{:name "oak_sapling" :count 1}]})]
          (mem/write! (:store eng) :forestry/replant {:pos {:x 3 :y 64 :z 0} :species "oak"} {:cap 50 :ttl :forever})
          (core/submit! eng '(jobs.forestry.plant-sapling) {})
          (await (tu/run-until-empty eng 12))
          (is (= 1 (count (lt/debts eng))) "the replant stays owed")
          (is (= ["cannot plant: occupied"]
                 (mapv :text (filterv #(= :plant_blocked (:kind %)) @seen)))))))))

(deftest the-same-sapling-already-on-the-planting-spot-counts-as-planted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (lt/setup {:blocks {"3,64,0" "oak_sapling" "3,63,0" "grass_block"} :inventory [{:name "oak_sapling" :count 1}]})]
          (mem/write! (:store eng) :forestry/replant {:pos {:x 3 :y 64 :z 0} :species "oak"} {:cap 50 :ttl :forever})
          (core/submit! eng '(jobs.forestry.plant-sapling) {})
          (await (tu/run-until-empty eng 12))
          (is (= [] (lt/debts eng)) "the debt is cleared")
          (is (= [] (filterv #(#{:error :failed} (:kind %)) @seen)) "no failure"))))))

(deftest the-plant-child-gets-the-fetch-arg-only-when-given
  (let [plant-args #(nth (some (fn [ph] (when (= :plant (first ph)) ph)) (hw/phases % nil nil)) 2)]
    (is (not (contains? (plant-args {:species "oak"}) :fetch)) "unset: the child's default (true) applies")
    (is (false? (:fetch (plant-args {:species "oak" :fetch false}))))
    (is (= {:log 4} (:fetch (plant-args {:fetch {:log 4}}))))))

(deftest the-sapling-list-is-derived-from-the-species-table
  (is (= (set (map trees/sapling-of trees/species)) (set ps/saplings)))
  (is (every? (set trees/species) ["oak" "mangrove" "crimson" "warped"])))

(defn ^:async ticks-of
  "Run harvest-wood with args for n ticks over one oak of 3 logs and a sapling carried: the :stopped events' reasons
  and whether the job is still listed."
  [n args]
  (let [{:keys [eng seen]} (lt/setup {:blocks (lt/tree 3 0 "oak" 3) :inventory [{:name "oak_sapling" :count 1}]})]
    (core/submit! eng (list 'jobs.forestry.harvest-wood args) {})
    (await (tu/run-until-empty eng n))
    [(mapv :reason (filterv #(= :stopped (:kind %)) @seen)) (boolean (seq (:list (core/state eng))))]))

(deftest count-starts-the-next-tree-until-enough-logs-are-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= [[:no-tree] false] (await (ticks-of 2 {:species "oak" :radius 10 :count 5})))
            "3 logs carried of 5: back to :fell, no tree left: stopped")
        (is (= [[] false] (await (ticks-of 1 {:species "oak" :radius 10 :count 3}))) "enough: the job ends")
        (is (= [[] false] (await (ticks-of 1 {:species "oak" :radius 10}))) "no count: one tree")))))

(deftest count-six-fells-two-three-log-trees
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup {:blocks (merge (lt/tree 3 0 "oak" 3) (lt/tree 3 6 "oak" 3) {"3,63,0" "grass_block" "3,63,6" "grass_block"})
                                         :inventory [{:name "oak_sapling" :count 2}]})]
          (core/submit! eng '(jobs.forestry.harvest-wood {:species "oak" :radius 12 :count 6}) {})
          (is (< (await (tu/run-until-empty eng 80)) 80) "the job ends")
          (is (= 6 (get (lt/inv p) "oak_log")) "both trees felled and collected")
          (is (= 2 (count (filter #(= "oak_sapling" (.-name (.blockAt p #js {:x 3 :y 64 :z %}))) [0 6]))) "both replanted"))))))

(deftest count-ends-no-tree-when-the-fell-finds-none-to-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (lt/setup {:blocks (lt/tree 3 0 "oak" 3) :inventory [{:name "oak_sapling" :count 1}]})
              eng (assoc-in eng [:jobs 'jobs.forestry.fell-tree :round] (fn ^:async f [_] :done))
              r (await (tu/child-outcome eng 'jobs.forestry.harvest-wood {:species "oak" :radius 10 :count 6} 60))]
          (is (= {:status :stopped :reason :no-tree :got 0} (select-keys r [:status :reason :got]))
              "a felling that finds no tree ends the job instead of starting the next"))))))

(deftest count-ends-no-tree-when-the-trees-seen-are-felled
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup {:blocks (merge (lt/tree 3 0 "oak" 3) {"3,63,0" "grass_block"})
                                         :inventory [{:name "oak_sapling" :count 1}]})
              r (await (tu/child-outcome eng 'jobs.forestry.harvest-wood {:species "oak" :radius 10 :count 6} 80))]
          (is (= {:status :stopped :reason :no-tree :got 3} (select-keys r [:status :reason :got]))
              "one tree felled, none left: ends with what it got, not waiting for ever")
          (is (= 3 (get (lt/inv p) "oak_log"))))))))

(deftest count-no-tree-after-a-fell-needs-the-fell-check-to-say-no-tree
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (lt/setup {:blocks (merge (lt/tree 3 0 "oak" 3) {"3,63,0" "grass_block"})})
              felled? (atom false)
              fell (get-in eng [:jobs 'jobs.forestry.fell-tree])
              eng (-> eng
                      (assoc-in [:jobs 'jobs.forestry.fell-tree :round]
                                (fn ^:async f [c] (let [r (await ((:round fell) c))] (when (= :done r) (reset! felled? true)) r)))
                      (assoc-in [:jobs 'jobs.forestry.fell-tree :check]
                                (fn [c] (if @felled?
                                          (do (ctx/wait c {:reason :need :any-of ["dirt"] :count 4}) false)
                                          ((:check fell) c)))))
              r (await (tu/child-outcome eng 'jobs.forestry.harvest-wood {:species "oak" :radius 10 :count 6} 80))]
          (is (= :not-done r) "the job has not ended: a fetch wait is not an empty forest")
          (is (some #(and (= :child_ended (:kind %)) (= :need (:reason %))) @seen) "the fell child ended :need, the job kept waiting")
          (is (not-any? #(= :no-tree (:reason %)) @seen)))))))

(deftest count-no-tree-after-a-fell-still-reports-the-replant-owed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (lt/setup {:blocks (merge (lt/tree 3 0 "oak" 3) {"3,63,0" "grass_block"})})
              r (await (tu/child-outcome eng 'jobs.forestry.harvest-wood {:species "oak" :radius 10 :count 6} 80))]
          (is (= {:status :stopped :reason :no-tree :replant-owed 1} (select-keys r [:status :reason :replant-owed])))
          (is (= 1 (count (filterv #(= :harvest-wood.replant-owed (:kind %)) @seen))) "the debt is warned"))))))
