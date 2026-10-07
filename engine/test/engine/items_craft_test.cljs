(ns engine.items-craft-test
  "jobs.items.craft against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.fake]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.items.craft :as craft]))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor world))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn ^:async run-until-empty [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (await (core/tick! eng))
          (recur (inc i))))))

(defn ^:async child-outcome
  "Run job with args as the child of a recording parent until the list is empty, at most n ticks; the child's result."
  [eng job args n]
  (let [out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid job args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
    (core/submit! eng '(recording-parent) {})
    (await (run-until-empty eng n))
    @out))

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
        (let [[result p _ eng] (await (craft {:inventory [{:name "oak_log" :count 1}]} {:item "oak_planks" :count 4}))]
          (is (= {"oak_planks" 4} (inv p)))
          (is (= {:made 4} result))
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
          (is (= {:made 0 :status :stopped :short {"wheat" 1}} result))
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

(deftest craft-with-a-full-inventory
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [filler (mapv #(hash-map :name (str "item_" %) :count 1) (range 35))
              [result _ seen] (await (craft {:inventory (conj filler {:name "oak_log" :count 1})} {:item "oak_planks"}))]
          (is (= {:made 0 :status :stopped :reason "full"} result))
          (is (some #(= :craft.full (:kind %)) @seen)))))))

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
          (is (= {:made 0 :status :stopped :short {"oak_log" 1} :alternatives {"oak_log" ["birch_log" "spruce_log"]}} result)))))))

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
          (is (= {:made 0 :status :stopped :short {"wheat" 1}} result)))))))

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

(deftest craft-fetch-false-reports-the-shortfall-and-leaves-the-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (wheat-world))
              result (await (child-outcome eng job {:item "bread" :fetch false} 80))]
          (is (= {:made 0 :status :stopped :short {"wheat" 1}} result))
          (is (= 1 (chest-wheat p))))))))


(deftest craft-places-a-carried-table-when-none-is-in-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "wheat" :count 3} {:name "crafting_table" :count 1}]})
              result (await (child-outcome eng job {:item "bread"} 40))]
          (is (= {:made 1} result))
          (is (= 1 (get (inv p) "bread")))
          (is (nil? (get (inv p) "crafting_table"))))))))

(deftest craft-fetches-a-missing-table-from-a-seen-chest-by-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "wheat" :count 3}] :blocks {"-2,64,3" "chest"}
                                      :containers {"-2,64,3" [{:name "crafting_table" :count 1}]}})
              result (await (child-outcome eng job {:item "bread"} 120))]
          (is (= {:made 1} result))
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
