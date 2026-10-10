(ns engine.items-craft-test
  "jobs.items.craft against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [clojure.string :as str]
            [engine.fake]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.test-util :as tu :refer [child-outcome]]
            [engine.triggers :as triggers]
            [jobs.items.craft :as craft]
            [jobs.lib.child :as child]))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor world))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn inv [p] (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p))))

(def job 'jobs.items.craft)
(def table {:x 10 :y 64 :z 0})
(def table-block {"10,64,0" "crafting_table"})

(defn ^:async craft
  "Setup world, run the job with args; [result p seen eng]."
  [world args]
  (let [{:keys [eng p seen]} (setup world)
        result (await (child-outcome eng job args 8))]
    [result p seen eng]))

(deftest craft-check-wants-an-item-name
  (are [args ok] (= ok ((:check (get registry/jobs job)) {:args args}))
    {:item "stick"} true
    {:item nil} false
    {:item 3} false))

(deftest craft-2x2-needs-no-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen eng] (await (craft {:inventory [{:name "oak_log" :count 1}]} {:item "oak_planks" :count 4}))]
          (is (= {"oak_planks" 4} (inv p)))
          (is (= {:made 4} result))
          (is (= ["crafted 4 oak_planks"] (map :text (filter #(= :craft.done (:kind %)) @seen))) "a craft shows in the events")
          (is (empty? (tu/walked-to eng))))))))

(deftest craft-walks-to-a-table-then-crafts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [args [{:item "bread"} {:item "bread" :table table}]]
          (let [[result p _ eng] (await (craft {:inventory [{:name "wheat" :count 3}] :blocks table-block} args))]
            (is (= {"bread" 1} (inv p)))
            (is (= {:made 1} result))
            (is (= 1 (count (tu/walked-to eng))))))))))

(deftest craft-uses-a-seen-table-in-reach-without-walking
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen eng] (await (craft {:inventory [{:name "wheat" :count 3}] :blocks {"2,64,0" "crafting_table"}} {:item "bread"}))]
          (is (not-any? #(= :craft.gave-up (:kind %)) @seen))
          (is (= {"bread" 1} (inv p)))
          (is (= {:made 1} result))
          (is (empty? (tu/walked-to eng))))))))

(deftest craft-without-a-table-in-radius
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen eng] (await (craft {:inventory [{:name "wheat" :count 3}] :blocks {"60,64,0" "crafting_table"}} {:item "bread" :fetch false}))]
          (is (= {:made 0 :status :stopped :reason "no-table"} result))
          (is (empty? (tu/walked-to eng)))
          (is (some #(= :craft.no-table (:kind %)) @seen)))))))

(deftest craft-reports-what-is-missing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen eng] (await (craft {:inventory [{:name "wheat" :count 2}] :blocks table-block} {:item "bread"}))]
          (is (= {:made 0 :status :stopped :short {"wheat" 1} :text "craft bread is missing {\"wheat\" 1}"} result))
          (is (some #(= :craft.short (:kind %)) @seen)))))))

(deftest the-nearest-table-is-one-the-body-has-seen
  (are [see expected] (= expected (craft/nearest-table (see (:p (setup {:blocks table-block}))) 32))
    identity table
    tu/blind nil))

(deftest craft-a-cannot-after-a-partial-batch-reports-what-was-made
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "oak_log" :count 1}]})
              _ (.override (.-world p) "craft"
                           (fn ^:async f [token args impl]
                             (await (impl token args))
                             #js {:status "cannot" :reason "boom"}))
              result (await (child-outcome eng job {:item "oak_planks" :count 4} 8))]
          (is (= {:made 4 :status :stopped :reason "boom"} result)))))))

(deftest craft-an-unknown-item-cannot
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result _ seen] (await (craft {} {:item "dragon_egg"}))]
          (is (= {:made 0 :status :stopped :reason "no-recipe"} result))
          (is (some #(= :craft.cannot (:kind %)) @seen)))))))

(deftest craft-count-is-on-top-of-what-is-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p _ eng] (await (craft {:inventory [{:name "oak_planks" :count 4} {:name "oak_log" :count 1}]}
                                       {:item "oak_planks" :count 4}))]
          (is (= {"oak_planks" 8} (inv p)))
          (is (= {:made 4} result)))))))

(deftest craft-gives-up-on-an-unreachable-table
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [spec [:unreachable :noPath]]
          (let [[result p seen eng] (await (craft {:inventory [{:name "wheat" :count 3}] :blocks table-block spec ["10,64,0"]}
                                              {:item "bread"}))]
            (is (= {:made 0 :status :stopped :reason "unreachable"} result))
            (is (= 3 (count (tu/walked-to eng))))
            (is (some #(= :craft.gave-up (:kind %)) @seen))))))))

(deftest craft-does-not-repeat-the-craft-act-while-the-walk-to-its-table-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walks (atom 0)
              orig child/call-walk]
          (set! child/call-walk (fn [_c _slot _args] (swap! walks inc) (js/Promise.resolve :continue)))
          (try
            (let [[_ p _ _] (await (craft {:inventory [{:name "wheat" :count 3}] :blocks table-block} {:item "bread" :table table}))]
              (is (< 1 @walks) "the walk was asked in several rounds")
              (is (= 1 (count (calls p "craft"))) "one failed act, then only the walk until it arrives"))
            (finally (set! child/call-walk orig))))))))

(deftest craft-with-a-full-inventory-that-make-room-cannot-free-stops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [filler (mapv (fn [_] {:name "iron_pickaxe" :count 1}) (range 35))
              [result _ seen] (await (craft {:inventory (conj filler {:name "oak_log" :count 1})} {:item "oak_planks"}))]
          (is (= {:made 0 :status :stopped :reason "full"} result))
          (is (some #(= :craft.full (:kind %)) @seen)))))))

(deftest craft-with-a-full-inventory-of-junk-makes-room-and-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [filler (mapv (fn [_] {:name "dirt" :count 64}) (range 35))
              [result p] (await (craft {:inventory (conj filler {:name "oak_log" :count 1})} {:item "oak_planks"}))]
          (is (= {:made 4} result))
          (is (= 4 (get (inv p) "oak_planks"))))))))

(deftest craft-gives-up-when-the-table-arg-is-not-a-table
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result _ seen] (await (craft {:inventory [{:name "wheat" :count 3}] :blocks {"10,64,0" "stone"}}
                                            {:item "bread" :table table}))]
          (is (= {:made 0 :status :stopped :reason "not-a-table"} result))
          (is (some #(= :craft.no-table (:kind %)) @seen)))))))

(deftest craft-gives-up-when-in-reach-of-a-table-that-still-refuses
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory [{:name "wheat" :count 3}] :blocks {"2,64,0" "crafting_table"}})
              _ (.override (.-world p) "craft" (fn ^:async f [_ _ _] #js {:status "unreachable" :reason "too-far"}))
              result (await (child-outcome eng job {:item "bread"} 8))]
          (is (= {:made 0 :status :stopped :reason "unreachable"} result))
          (is (empty? (tu/walked-to eng)))
          (is (some #(= :craft.gave-up (:kind %)) @seen)))))))

(deftest craft-hands-over-the-alternatives-of-a-missing-ingredient
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "oak_log" :count 1}]})
              _ (.override (.-world p) "craft" (fn ^:async f [_ _ _] #js {:status "no-item" :have #js {} :recipes #js [#js {"birch_log" 1} #js {"oak_log" 1} #js {"spruce_log" 1}]}))
              result (await (child-outcome eng job {:item "birch_planks"} 8))]
          (is (= {:made 0 :status :stopped :short {"oak_log" 1} :alternatives {"oak_log" ["birch_log" "spruce_log"]} :text "craft birch_planks is missing {\"oak_log\" 1}"} result)))))))

(deftest craft-a-partial-batch-short-of-an-ingredient-hands-over-the-shortfall
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "oak_log" :count 1}]})
              _ (.override (.-world p) "craft" (fn ^:async f [_ _ _] #js {:status "partial" :reason "no-item" :made 4 :used #js {} :have #js {"stick" 1} :recipes #js [#js {"oak_planks" 2 "stick" 1}]}))
              result (await (child-outcome eng job {:item "wooden_pickaxe" :count 8} 8))]
          (is (= {"oak_planks" 2} (:short result))))))))

(deftest craft-an-arrival-is-not-a-failure
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "wheat" :count 3}] :blocks table-block})
              _ (.override (.-world p) "craft" (fn ^:async f [_ _ _] #js {:status "unreachable" :reason "too-far"}))
              result (await (child-outcome eng job {:item "bread"} 10))]
          (is (= {:made 0 :status :stopped :reason "unreachable"} result))
          (is (= 1 (count (tu/walked-to eng))))
          (is (= 4 (count (calls p "craft"))) "the walk is a round of its own, then 3 refusals in reach"))))))

(deftest craft-a-gone-table-is-searched-for-afresh
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "wheat" :count 3}]})
              n (atom 0)
              _ (.override (.-world p) "craft"
                           (fn ^:async f [_ _ _]
                             (if (= 1 (swap! n inc))
                               #js {:status "out-of-reach" :reason "too-far" :table #js {:x 10 :y 64 :z 0}}
                               #js {:status "unreachable" :reason "no-table"})))
              result (await (child-outcome eng job {:item "bread"} 10))]
          (is (= {:made 0 :status :stopped :reason "no-table"} result))
          (is (= 1 (count (tu/walked-to eng))) "walked to the remembered cell once, not again"))))))

(deftest craft-ignores-a-handed-over-table-beyond-the-radius
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen eng] (await (craft {:inventory [{:name "wheat" :count 3}] :blocks {"20,64,0" "crafting_table"}}
                                            {:item "bread" :radius 8}))]
          (is (= {:made 0 :status :stopped :reason "no-table"} result))
          (is (empty? (tu/walked-to eng)))
          (is (some #(= :craft.no-table (:kind %)) @seen)))))))

(deftest craft-walks-to-a-table-and-crafts-in-one-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "wheat" :count 3}] :blocks table-block})
              result (await (child-outcome eng job {:item "bread"} 1))]
          (is (= {"bread" 1} (inv p)))
          (is (= {:made 1} result))
          (is (= 1 (count (tu/walked-to eng)))))))))

(deftest craft-a-partial-batch-goes-on-in-the-same-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "oak_log" :count 2}]})
              n (atom 0)]
          (.override (.-world p) "craft"
                     (fn ^:async f [token args impl]
                       (if (= 1 (swap! n inc))
                         (do (await (impl token (clj->js (assoc (js->clj args) "count" 4))))
                             #js {:status "partial" :reason "interrupted" :made 4})
                         (await (impl token args)))))
          (let [result (await (child-outcome eng job {:item "oak_planks" :count 8} 1))]
            (is (= {:made 8} result))
            (is (= 2 (count (calls p "craft"))))))))))

(deftest craft-that-stops-short-ends-stopped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result] (await (craft {:inventory [{:name "wheat" :count 2}] :blocks table-block} {:item "bread"}))]
          (is (= {:made 0 :status :stopped :short {"wheat" 1} :text "craft bread is missing {\"wheat\" 1}"} result)))))))

(def wheat-chest {"-2,64,3" "chest"})
(defn wheat-world [] {:inventory [{:name "wheat" :count 2}] :blocks (merge table-block wheat-chest) :containers {"-2,64,3" [{:name "wheat" :count 1}]}})
(defn chest-wheat [p] (some #(when (= "wheat" (:name %)) (:count %)) (get-in @(engine.fake/state p) [:containers [-2 64 3]])))

(deftest craft-fetches-a-missing-ingredient-from-a-seen-chest-by-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (wheat-world))
              result (await (child-outcome eng job {:item "bread"} 80))]
          (is (= {:made 1} result))
          (is (= 1 (get (inv p) "bread")))
          (is (nil? (chest-wheat p))))))))

(defn wheat-batches-world [chest-count]
  {:blocks (merge table-block wheat-chest) :containers {"-2,64,3" [{:name "wheat" :count chest-count}]}})

(deftest craft-fetches-again-for-each-batch-while-fetches-bring-progress
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (wheat-batches-world 9))
              result (await (child-outcome eng job {:item "bread" :count 3} 200))]
          (is (= {:made 3} result))
          (is (= 3 (get (inv p) "bread"))))))))

(deftest craft-stops-with-a-text-when-a-fetch-brings-no-progress
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup (wheat-batches-world 3))
              result (await (child-outcome eng job {:item "bread" :count 3} 200))]
          (is (= 1 (:made result)))
          (is (= :stopped (:status result)))
          (is (not (str/blank? (:text result)))))))))

(deftest craft-fetch-false-reports-the-shortfall-and-leaves-the-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (wheat-world))
              result (await (child-outcome eng job {:item "bread" :fetch false} 80))]
          (is (= {:made 0 :status :stopped :short {"wheat" 1} :text "craft bread is missing {\"wheat\" 1}"} result))
          (is (= 1 (chest-wheat p))))))))


(deftest craft-places-a-carried-table-when-none-is-in-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "wheat" :count 3} {:name "crafting_table" :count 1}]})
              result (await (child-outcome eng job {:item "bread"} 40))]
          (is (= {:made 1 :spent {"crafting_table" 1}} (select-keys result [:made :spent])))
          (is (re-find #"^used 1 crafting_table \(crafting table at \[" (:text result)))
          (is (= 1 (get (inv p) "bread")))
          (is (nil? (get (inv p) "crafting_table"))))))))

(deftest craft-puts-a-carried-table-down-instead-of-walking-to-a-far-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "wheat" :count 3} {:name "crafting_table" :count 1}] :blocks table-block})
              result (await (child-outcome eng job {:item "bread"} 40))]
          (is (= 1 (get (inv p) "bread")))
          (is (nil? (get (inv p) "crafting_table")) "the carried table was put down")
          (is (= {"crafting_table" 1} (:spent result)))
          (is (empty? (tu/walked-to eng))))))))

(defn placed-at [p]
  (mapv #(let [a (.-args %)] (select-keys (js->clj (.-pos a) :keywordize-keys true) [:x :y :z])) (calls p "place")))

(deftest craft-never-puts-its-table-in-a-cell-the-caller-avoids
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:inventory [{:name "wheat" :count 3} {:name "crafting_table" :count 1}]}
              [_ p1] (await (craft world {:item "bread"}))
              [_ p2] (await (craft world {:item "bread" :avoid [[1 64 0] [1 65 0]]}))]
          (is (= [{:x 1 :y 64 :z 0}] (placed-at p1)) "unconstrained: the first cell beside the body")
          (is (= 1 (count (placed-at p2))))
          (is (not= {:x 1 :y 64 :z 0} (first (placed-at p2))) "the avoided cell is left free"))))))

(deftest putting-a-table-down-and-crafting-at-it-is-one-urgent-section
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:inventory [{:name "wheat" :count 3} {:name "crafting_table" :count 1}]})]
          (await (child-outcome eng job {:item "bread"} 40))
          (is (= [[:craft :urgent]] (->> @seen (filter #(and (= :section (:kind %)) (:open %))) (map (juxt :section :holds)) distinct vec))))))))

(deftest a-craft-at-a-table-already-in-reach-opens-no-section
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:inventory [{:name "wheat" :count 3}] :blocks {"2,64,0" "crafting_table"}})]
          (await (child-outcome eng job {:item "bread"} 40))
          (is (not-any? #(= :section (:kind %)) @seen)))))))

(deftest craft-in-a-stone-pocket-digs-a-cell-beside-by-hand-for-the-table
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pocket (into {"0,66,0" "stone"}
                           (for [x [-1 0 1] z [-1 0 1] y [63 64 65] :when (or (= 63 y) (not (and (zero? x) (zero? z))))]
                             [(str x "," y "," z) "stone"]))
              {:keys [eng p]} (setup {:inventory [{:name "wheat" :count 3} {:name "crafting_table" :count 1}] :blocks pocket})
              result (await (child-outcome eng job {:item "bread"} 40))]
          (is (= {:made 1 :spent {"crafting_table" 1}} (select-keys result [:made :spent])) (pr-str result))
          (is (= 1 (count (calls p "dig"))) "one cell beside dug, bare-handed")
          (is (= 1 (get (inv p) "bread"))))))))

(deftest craft-in-a-pocket-skips-the-cells-beside-water-and-digs-the-next
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pocket (into {"0,66,0" "stone" "-2,64,0" "water" "2,64,0" "water" "0,64,-2" "water"}
                           (for [x [-1 0 1] z [-1 0 1] y [63 64 65] :when (or (= 63 y) (not (and (zero? x) (zero? z))))]
                             [(str x "," y "," z) "stone"]))
              {:keys [eng p]} (setup {:inventory [{:name "wheat" :count 3} {:name "crafting_table" :count 1}] :blocks pocket})
              result (await (child-outcome eng job {:item "bread"} 60))]
          (is (= {:made 1 :spent {"crafting_table" 1}} (select-keys result [:made :spent])) (pr-str result))
          (is (= ["stone" "stone" "stone"] (mapv #(.-name (.blockAt p (clj->js %))) [{:x -1 :y 64 :z 0} {:x 1 :y 64 :z 0} {:x 0 :y 64 :z -1}])) "the cells beside water are not dug")
          (is (= 1 (get (inv p) "bread"))))))))

(deftest craft-fetches-a-missing-table-from-a-seen-chest-by-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "wheat" :count 3}] :blocks {"-2,64,3" "chest"}
                                      :containers {"-2,64,3" [{:name "crafting_table" :count 1}]}})
              result (await (child-outcome eng job {:item "bread"} 120))]
          (is (= {:made 1 :spent {"crafting_table" 1}} (select-keys result [:made :spent])))
          (is (= 1 (get (inv p) "bread"))))))))

(deftest craft-fetch-false-without-a-table-stops-no-table
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "wheat" :count 3} {:name "crafting_table" :count 1}]})
              result (await (child-outcome eng job {:item "bread" :fetch false} 40))]
          (is (= {:made 0 :status :stopped :reason "no-table"} result))
          (is (= 1 (get (inv p) "crafting_table"))))))))

(deftest craft-goes-on-after-a-timeout-that-made-something
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "oak_log" :count 1}]})]
          (.override (.-world p) "craft"
                     (fn ^:async f [token args impl]
                       (await (impl token args))
                       #js {:status "timeout" :inventoryChange #js {"oak_planks" 4 "oak_log" -1}}))
          (let [result (await (child-outcome eng job {:item "oak_planks" :count 4} 8))]
            (is (= {:made 4} result))
            (is (= 1 (count (calls p "craft"))))))))))

(deftest craft-looks-around-before-it-says-there-is-no-table
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "wheat" :count 3}] :blocks table-block})
              _ (tu/seeing-after-look p)
              n (atom 0)
              _ (.override (.-world p) "craft"
                           (fn ^:async f [token args impl]
                             (case (swap! n inc)
                               1 #js {:status "out-of-reach" :reason "too-far" :table #js {:x 10 :y 64 :z 0}}
                               2 #js {:status "unreachable" :reason "no-table"}
                               (await (impl token args)))))
              result (await (child-outcome eng job {:item "bread" :fetch false} 20))]
          (is (seq (calls p "look")) "the table behind the body is not known until it looks")
          (is (= {:made 1} result)))))))

(deftest craft-a-gate-fetches-the-planks-its-own-sticks-used-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [n [1]]
          (let [[result p] (await (craft {:blocks table-block
                                          :recipes {"oak_fence_gate" {:count 1 :needs {"oak_planks" 2 "stick" 4} :table true}}
                                          :inventory [{:name "oak_log" :count 11} {:name "oak_planks" :count 2}]}
                                         {:item "oak_fence_gate" :count n}))]
            (is (= n (get (inv p) "oak_fence_gate")) (str "gate x" n))
            (is (= {:made n} (select-keys result [:made])) (pr-str result))))))))
