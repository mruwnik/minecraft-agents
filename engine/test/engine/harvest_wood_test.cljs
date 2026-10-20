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

(deftest a-call-waits-while-the-body-is-outside-its-area
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup {:blocks (lt/tree 3 0 "oak" 3)})
              here (.-pos (.self p))]
          (core/submit! eng (list 'jobs.forestry.harvest-wood {:species "oak" :radius 10 :center [(+ 500 (.-x here)) (.-y here) (.-z here)]}) {})
          (await (tu/run-until-empty eng 8))
          (is (nil? (get (lt/inv p) "oak_log")) "no tree is felled 500 blocks from the area centre")
          (is (= 1 (count (:list (core/state eng)))) "the call waits"))))))

(deftest no-sapling-carried-ends-the-job-with-the-debt-owed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup {:blocks (lt/tree 3 0 "oak" 3)})]
          (core/submit! eng harvest {})
          (is (< (await (tu/run-until-empty eng 30)) 30) "finishes instead of waiting for a sapling")
          (is (= [] (:list (core/state eng))))
          (is (= 3 (get (lt/inv p) "oak_log")) "logs collected")
          (is (= 1 (count (lt/debts eng))) "the replant stays owed"))))))

(deftest no-sapling-hands-over-replant-owed-and-is-still-completed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (lt/setup {:blocks (lt/tree 3 0 "oak" 3)})]
          (is (= {:status :done :replant-owed 1} (await (tu/child-outcome eng 'jobs.forestry.harvest-wood {:species "oak" :radius 10} 30)))))))))

(deftest a-sapling-carried-is-still-planted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup {:blocks (merge (lt/tree 3 0 "oak" 3) {"3,63,0" "grass_block"}) :inventory [{:name "oak_sapling" :count 1}]})]
          (core/submit! eng harvest {})
          (is (< (await (tu/run-until-empty eng 30)) 30))
          (is (= "oak_sapling" (.-name (.blockAt p #js {:x 3 :y 64 :z 0}))))
          (is (= [] (lt/debts eng))))))))

(deftest no-tree-in-range-looks-once-and-waits-unstarted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (lt/setup {:blocks (lt/tree 9 0 "oak" 3)})
              id (core/submit! eng '(jobs.forestry.harvest-wood {:radius 2}) {})]
          (await (core/tick! eng))
          (let [n (count (lt/calls p "look"))]
            (is (pos? n) "it looked round")
            (await (tu/run-until-empty eng 5))
            (is (= n (count (lt/calls p "look"))) "once from this place"))
          (is (= [id] (:list (core/state eng))) "stays queued")
          (is (nil? (:call (:contract (mem/job-mem (mem/view (:store eng)) id [])))) "never started")
          (is (empty? (filterv #(= :stopped (:kind %)) @seen)))
          (is (= [:no-tree] (mapv :reason (filterv #(= :waiting (:kind %)) @seen)))))))))

(deftest a-waiting-job-looks-again-only-after-the-body-moved-away
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup {:blocks (lt/tree 9 0 "oak" 3)})
              looks #(count (lt/calls p "look"))
              move! (fn [x] (fake/swap-self! p assoc :pos [x 64 0]))]
          (core/submit! eng '(jobs.forestry.harvest-wood {:radius 2}) {})
          (await (tu/run-until-empty eng 3))
          (let [n (looks)]
            (move! 1)
            (await (tu/run-until-empty eng 3))
            (is (= n (looks)) "a one-block move is the same place")
            (move! -8)
            (await (tu/run-until-empty eng 3))
            (is (> (looks) n) "a new place is looked at")))))))

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
        (let [{:keys [eng p seen]} (lt/setup {:blocks (merge (lt/tree 3 0 "oak" 3) {"3,63,0" "grass_block"}) :inventory [{:name "oak_sapling" :count 2}]
                                              :unloaded ["1500,66,1500"]})]
          (owe-far! eng)
          (core/submit! eng '(jobs.forestry.harvest-wood {:radius 32}) {})
          (is (< (await (tu/run-until-empty eng 40)) 40))
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

(deftest logs-picked-up-during-the-felling-count-as-collected
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (lt/setup {:blocks (merge (lt/tree 3 0 "oak" 3) {"3,63,0" "grass_block"}) :inventory [{:name "oak_sapling" :count 1}]})]
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
          (is (= [:occupied] (mapv :reason (filterv #(= :stopped (:kind %)) @seen)))))))))

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

(deftest the-plant-child-gets-the-ways-arg-only-when-given
  (let [plant-args #(nth (some (fn [ph] (when (= :plant (first ph)) ph)) (hw/phases % nil)) 2)]
    (is (not (contains? (plant-args {:species "oak"}) :ways)) "unset: the child's default (all) applies")
    (is (= #{:carried} (:ways (plant-args {:species "oak" :ways #{:carried}}))))
    (is (= #{:carried :chest} (:ways (plant-args {:ways #{:carried :chest}}))))))

(deftest the-sapling-list-is-derived-from-the-species-table
  (is (= (set (map trees/sapling-of trees/species)) (set ps/saplings)))
  (is (every? (set trees/species) ["oak" "mangrove" "crimson" "warped" "pale_oak"])))

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

(deftest count-ends-with-the-fell-ending-when-the-trees-seen-cannot-be-reached
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (lt/setup {:blocks (lt/tree 3 0 "oak" 3) :inventory [{:name "oak_sapling" :count 1}]})
              eng (assoc-in eng [:jobs 'jobs.forestry.fell-tree :round]
                            (fn ^:async f [c] (ctx/end c :nothing-possible :tree-unreachable {:columns [[3 0]] :why :walk})))
              r (await (tu/child-outcome eng 'jobs.forestry.harvest-wood {:species "oak" :radius 10 :count 6} 60))]
          (is (= {:status :stopped :reason :tree-unreachable :got 0 :why :walk :columns [[3 0]]} (select-keys r [:status :reason :got :why :columns]))
              "a felling that reaches no tree ends the job with that ending, nothing collected or planted after it"))))))

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

(deftest a-fell-that-ends-need-ends-the-job-need-not-no-tree
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (lt/setup {:blocks (merge (lt/tree 3 0 "oak" 3) {"3,63,0" "grass_block"})})
              felled? (atom false)
              fell (get-in eng [:jobs 'jobs.forestry.fell-tree])
              eng (assoc-in eng [:jobs 'jobs.forestry.fell-tree :round]
                            (fn ^:async f [c]
                              (if @felled?
                                (ctx/end c :condition-failed {:reason :need :any-of ["dirt"] :count 4} nil)
                                (let [r (await ((:round fell) c))] (when (= :done (:kind r)) (reset! felled? true)) r))))
              r (await (tu/child-outcome eng 'jobs.forestry.harvest-wood {:species "oak" :radius 10 :count 6} 80))]
          (is (= {:status :stopped :reason :need :got 3} (select-keys r [:status :reason :got]))
              "a fetch that failed is not an empty forest")
          (is (not-any? #(= :no-tree (:reason %)) @seen)))))))

(deftest a-tree-lost-before-the-first-felling-ends-no-tree
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (lt/setup {:blocks (lt/tree 3 0 "oak" 3)})
              eng (assoc-in eng [:jobs 'jobs.forestry.fell-tree :round]
                            (fn ^:async f [c] (ctx/end c :condition-failed :no-tree {})))
              r (await (tu/child-outcome eng 'jobs.forestry.harvest-wood {:species "oak" :radius 10} 20))]
          (is (= {:status :stopped :reason :no-tree :got 0} (select-keys r [:status :reason :got]))
              "a started call whose tree is gone ends, it does not yield for ever"))))))

(deftest count-no-tree-after-a-fell-still-carries-the-replant-owed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (lt/setup {:blocks (merge (lt/tree 3 0 "oak" 3) {"3,63,0" "grass_block"})})
              r (await (tu/child-outcome eng 'jobs.forestry.harvest-wood {:species "oak" :radius 10 :count 6} 80))]
          (is (= {:status :stopped :reason :no-tree :replant-owed 1} (select-keys r [:status :reason :replant-owed]))))))))
