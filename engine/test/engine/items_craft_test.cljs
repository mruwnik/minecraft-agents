(ns engine.items-craft-test
  "jobs.items.craft against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [clojure.string :as str]
            [engine.fake]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.contract :as contract]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.items.craft :as craft]
            [jobs.lib.child :as child]
            [jobs.lib.crafting-table :as crafting-table]
            [jobs.lib.spot :as spot]
            [jobs.lib.world-files :as world]))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor world))
        eng (core/create (cond-> {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                                  :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})}
                           (:plans world) (assoc :world (world/of-data (:plans world) {} []))))]
    {:eng eng :p p :seen seen}))

(defn ^:async child-outcome
  "tu/child-outcome without the :ending an old parent also reads."
  [eng job args n]
  (let [r (await (tu/child-outcome eng job args n))]
    (cond-> r (map? r) (dissoc :ending))))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn inv [p] (reduce (fn [m i] (update m (.-name i) (fnil + 0) (.-count i))) {} (.-inventory (.self p))))

(def job 'jobs.items.craft)
(def table {:x 10 :y 64 :z 0})
(def table-block {"10,64,0" "crafting_table"})

(defn ^:async craft
  "Setup world, run the job with args; [result p seen eng]."
  [world args]
  (let [{:keys [eng p seen]} (setup world)
        result (await (child-outcome eng job args 8))]
    [result p seen eng]))

(defn ^:async craft-counting-walks
  "Like craft, with the number of go-to calls the job made (one call may take several rounds)."
  [world args]
  (let [calls (atom 0)
        orig child/call-walk*]
    (set! child/call-walk* (fn [c slot a] (swap! calls inc) (orig c slot a)))
    (try
      (conj (await (craft world args)) @calls)
      (finally (set! child/call-walk* orig)))))

(deftest craft-wants-an-item-name
  (are [args ok] (= ok (= :ok (first ((:can-start? (get registry/jobs job)) {:args args}))))
    {:item "stick"} true
    {:item nil} false
    {:item 3} false))

(deftest a-table-recipe-with-no-table-in-sight-looks-first
  (let [status (fn [item world & [extra]]
                 (first ((:can-start? (get registry/jobs job))
                         {:primitives (tu/fake-on-floor world) :args (merge {:item item} extra) :view (fn [] {:now 0 :data {}}) :root "j" :slots []})))]
    (is (= :look (status "chest" {})) "a 3x3 recipe, no table known")
    (is (= :ok (status "chest" {:inventory [{:name "crafting_table" :count 1}]})) "a carried table is put down")
    (is (= :ok (status "chest" {} {:table {:x 1 :y 64 :z 0}})))
    (is (= :ok (status "stick" {})) "a 2x2 recipe needs no table")))

(deftest craft-2x2-needs-no-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen eng] (await (craft {:inventory [{:name "oak_log" :count 1}]} {:item "oak_planks" :count 4}))]
          (is (= {"oak_planks" 4} (inv p)))
          (is (= {:item "oak_planks" :made 4 :status :done} (dissoc result :text)))
          (is (empty? (tu/walked-to eng))))))))

(deftest craft-walks-to-a-table-then-crafts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [args [{:item "bread"} {:item "bread" :table table}]]
          (let [[result p _ eng] (await (craft {:inventory [{:name "wheat" :count 3}] :blocks table-block} args))]
            (is (= {"bread" 1} (inv p)))
            (is (= {:item "bread" :made 1 :status :done} (dissoc result :text)))
            (is (= 1 (count (tu/walked-to eng))))))))))

(deftest craft-uses-a-seen-table-in-reach-without-walking
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen eng] (await (craft {:inventory [{:name "wheat" :count 3}] :blocks {"2,64,0" "crafting_table"}} {:item "bread"}))]
          (is (= {"bread" 1} (inv p)))
          (is (= {:item "bread" :made 1 :status :done} (dissoc result :text)))
          (is (empty? (tu/walked-to eng))))))))

(deftest craft-without-a-table-in-reach-names-the-nearest-known-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen eng] (await (settings/with-settings {::craft/table-radius 40}
                                          #(craft {:inventory [{:name "wheat" :count 3}] :blocks {"60,64,0" "crafting_table"}} {:item "bread" :ways #{:carried}})))]
          (is (= {:item "bread" :made 0 :status :stopped :reason :no-table} (dissoc result :text)))
          (is (empty? (tu/walked-to eng)))
          (is (str/includes? (:text result) "[60 64 0]"))
          (is (str/includes? (:text result) "60 blocks")))))))

(deftest craft-walks-to-a-known-table-30-off-within-the-default-radius
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p _ eng] (await (craft {:inventory [{:name "wheat" :count 3}] :blocks {"30,64,0" "crafting_table"}} {:item "bread" :ways #{:carried}}))]
          (is (= {:item "bread" :made 1 :status :done} (dissoc result :text)))
          (is (= {"bread" 1} (inv p)))
          (is (= 1 (count (tu/walked-to eng)))))))))

(deftest craft-makes-a-table-of-carried-planks-when-none-is-known
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (craft {:inventory [{:name "wheat" :count 3} {:name "oak_planks" :count 4}]} {:item "bread" :keep-table true}))]
          (is (= 1 (:made result)))
          (is (= {"bread" 1} (inv p))))))))

(deftest craft-reports-what-is-missing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen eng] (await (craft {:inventory [{:name "wheat" :count 2}] :blocks table-block} {:item "bread"}))]
          (is (= {:made 0 :status :stopped :reason :need :short {"wheat" 1} :text "craft bread is missing {\"wheat\" 1}"} (dissoc result :item)))
          (is (empty? (filter #(str/starts-with? (name (:kind %)) "craft.") @seen)) "the ending carries it"))))))

(deftest craft-does-not-spend-an-ingredient-the-register-holds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "wheat" :count 3}] :blocks table-block})
              _ (await (child-outcome eng 'jobs.memory.reserve {:item "wheat" :count 2} 4))
              result (await (child-outcome eng job {:item "bread" :ways #{:carried}} 8))]
          (is (= {:made 0 :status :stopped :reason :need :short {"wheat" 2}} (dissoc result :item :text)))
          (is (= {"wheat" 3} (inv p))))))))

(deftest the-nearest-table-is-one-the-body-has-seen
  (are [see expected] (= expected (crafting-table/nearest (see (:p (setup {:blocks table-block}))) 32))
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
          (is (= {:item "oak_planks" :made 4 :status :done :detail "boom"} (dissoc result :text)) "all the :count made: the later stop does not matter"))))))

(deftest craft-a-cannot-after-enough-made-for-the-least-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "oak_log" :count 1}]})
              _ (.override (.-world p) "craft"
                           (fn ^:async f [token args impl]
                             (await (impl token args))
                             #js {:status "cannot" :reason "boom"}))
              result (await (child-outcome eng job {:item "oak_planks" :count 8 :least 4} 8))]
          (is (= {:made 4 :status :done} (select-keys result [:made :status]))))))))

(deftest craft-an-unknown-item-cannot
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result _ seen] (await (craft {} {:item "dragon_egg"}))]
          (is (= {:item "dragon_egg" :made 0 :status :stopped :reason :cannot :detail "no-recipe"} (dissoc result :text)))
          (is (empty? (filter #(str/starts-with? (name (:kind %)) "craft.") @seen)) "the ending carries it"))))))

(deftest craft-count-is-the-total-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p _ eng] (await (craft {:inventory [{:name "oak_planks" :count 4} {:name "oak_log" :count 1}]}
                                       {:item "oak_planks" :count 8}))]
          (is (= {"oak_planks" 8} (inv p)))
          (is (= {:made 4 :status :done} (select-keys result [:made :status]))))))))

(deftest craft-is-done-when-the-count-is-already-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p _ eng] (await (craft {:inventory [{:name "oak_planks" :count 4} {:name "oak_log" :count 1}]}
                                       {:item "oak_planks" :count 4}))]
          (is (= {"oak_planks" 4 "oak_log" 1} (inv p)))
          (is (= {:made 0 :status :done} (select-keys result [:made :status]))))))))

(deftest craft-gives-up-on-an-unreachable-table
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [spec [:unreachable :noPath]]
          (let [[result p seen eng walks] (await (craft-counting-walks {:inventory [{:name "wheat" :count 3}] :blocks table-block spec ["10,64,0"]}
                                                                       {:item "bread"}))]
            (is (= {:made 0 :status :stopped :reason :unreachable} (select-keys result [:made :status :reason])))
            (is (= 1 walks))
            (is (empty? (filter #(str/starts-with? (name (:kind %)) "craft.") @seen)) "the ending carries it")))))))

(deftest craft-does-not-repeat-the-craft-act-while-the-walk-to-its-table-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walks (atom 0)
              orig child/call-walk*]
          (set! child/call-walk* (fn [_c _slot _args] (swap! walks inc) (js/Promise.resolve :continue)))
          (try
            (let [[_ p _ _] (await (craft {:inventory [{:name "wheat" :count 3}] :blocks table-block} {:item "bread" :table table}))]
              (is (< 1 @walks) "the walk was asked in several rounds")
              (is (= 1 (count (calls p "craft"))) "one failed act, then only the walk until it arrives"))
            (finally (set! child/call-walk* orig))))))))

(deftest craft-with-a-full-inventory-that-make-room-cannot-free-stops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [filler (mapv (fn [_] {:name "iron_pickaxe" :count 1}) (range 35))
              [result _ seen] (await (craft {:inventory (conj filler {:name "oak_log" :count 1})} {:item "oak_planks"}))]
          (is (= {:item "oak_planks" :made 0 :status :stopped :reason :full} (dissoc result :text)))
          (is (empty? (filter #(str/starts-with? (name (:kind %)) "craft.") @seen)) "the ending carries it"))))))

(deftest craft-with-a-full-inventory-of-junk-makes-room-and-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [filler (mapv (fn [_] {:name "dirt" :count 64}) (range 35))
              [result p] (await (craft {:inventory (conj filler {:name "oak_log" :count 1})} {:item "oak_planks"}))]
          (is (= {:item "oak_planks" :made 4 :status :done} (dissoc result :text)))
          (is (= 4 (get (inv p) "oak_planks"))))))))

(deftest craft-with-a-full-inventory-never-tosses-the-recipes-inputs-to-make-room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [sticks (mapv (fn [_] {:name "stick" :count 64}) (range 35))
              [result p] (await (craft {:inventory (conj sticks {:name "coal" :count 1})} {:item "torch"}))]
          (is (= {:item "torch" :made 0 :status :stopped :reason :full} (dissoc result :text)))
          (is (= (* 35 64) (reduce + (for [i (.-inventory (.self p)) :when (= "stick" (.-name i))] (.-count i))))
              "the sticks the torch needs were not tossed"))))))

(deftest craft-gives-up-when-the-table-arg-is-not-a-table
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result _ seen] (await (craft {:inventory [{:name "wheat" :count 3}] :blocks {"10,64,0" "stone"}}
                                            {:item "bread" :table table}))]
          (is (= {:item "bread" :made 0 :status :stopped :reason :not-a-table} (dissoc result :text)))
          (is (empty? (filter #(str/starts-with? (name (:kind %)) "craft.") @seen)) "the ending carries it"))))))

(deftest craft-gives-up-when-in-reach-of-a-table-that-still-refuses
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory [{:name "wheat" :count 3}] :blocks {"2,64,0" "crafting_table"}})
              _ (.override (.-world p) "craft" (fn ^:async f [_ _ _] #js {:status "unreachable" :reason "too-far"}))
              result (await (child-outcome eng job {:item "bread"} 8))]
          (is (= {:item "bread" :made 0 :status :stopped :reason :unreachable} (dissoc result :text)))
          (is (empty? (tu/walked-to eng)))
          (is (empty? (filter #(str/starts-with? (name (:kind %)) "craft.") @seen)) "the ending carries it"))))))

(deftest craft-hands-over-the-alternatives-of-a-missing-ingredient
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "oak_log" :count 1}]})
              _ (.override (.-world p) "craft" (fn ^:async f [_ _ _] #js {:status "no-item" :have #js {} :recipes #js [#js {"birch_log" 1} #js {"oak_log" 1} #js {"spruce_log" 1}]}))
              result (await (child-outcome eng job {:item "birch_planks"} 8))]
          (is (= {:made 0 :status :stopped :reason :need :short {"oak_log" 1} :alternatives {"oak_log" ["birch_log" "spruce_log"]} :text "craft birch_planks is missing {\"oak_log\" 1}"} (dissoc result :item))))))))

(deftest craft-a-partial-batch-short-of-an-ingredient-hands-over-the-shortfall
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "oak_log" :count 1}]})
              _ (.override (.-world p) "craft" (fn ^:async f [token args impl]
                             (await (impl token args))
                             #js {:status "partial" :reason "no-item" :made 4 :used #js {} :have #js {} :recipes #js [#js {"oak_log" 1}]}))
              result (await (child-outcome eng job {:item "oak_planks" :count 8 :ways #{:carried}} 8))]
          (is (= {"oak_log" 1} (:short result)))
          (is (= 4 (:made result)))
          (is (= :stopped (:status result)) "a partial stop is not done"))))))

(deftest craft-an-arrival-is-not-a-failure
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "wheat" :count 3}] :blocks table-block})
              _ (.override (.-world p) "craft" (fn ^:async f [_ _ _] #js {:status "unreachable" :reason "too-far"}))
              result (await (child-outcome eng job {:item "bread"} 10))]
          (is (= {:item "bread" :made 0 :status :stopped :reason :unreachable} (dissoc result :text)))
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
          (is (= {:item "bread" :made 0 :status :stopped :reason :no-table} (dissoc result :text)))
          (is (= 1 (count (tu/walked-to eng))) "walked to the remembered cell once, not again"))))))

(deftest craft-ignores-a-handed-over-table-beyond-the-radius
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen eng] (await (settings/with-settings {::craft/table-radius 8}
                                          #(craft {:inventory [{:name "wheat" :count 3}] :blocks {"20,64,0" "crafting_table"}}
                                                  {:item "bread"})))]
          (is (= {:item "bread" :made 0 :status :stopped :reason :no-table} (dissoc result :text)))
          (is (empty? (tu/walked-to eng)))
          (is (empty? (filter #(str/starts-with? (name (:kind %)) "craft.") @seen)) "the ending carries it"))))))

(deftest craft-walks-to-a-table-and-crafts-in-one-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "wheat" :count 3}] :blocks table-block})
              result (await (child-outcome eng job {:item "bread"} 1))]
          (is (= {"bread" 1} (inv p)))
          (is (= {:item "bread" :made 1 :status :done} (dissoc result :text)))
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
            (is (= {:item "oak_planks" :made 8 :status :done} (dissoc result :text)))
            (is (= 2 (count (calls p "craft"))))))))))

(deftest craft-that-stops-short-ends-stopped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result] (await (craft {:inventory [{:name "wheat" :count 2}] :blocks table-block} {:item "bread"}))]
          (is (= {:made 0 :status :stopped :reason :need :short {"wheat" 1} :text "craft bread is missing {\"wheat\" 1}"} (dissoc result :item))))))))

(def wheat-chest {"-2,64,3" "chest"})
(defn wheat-world [] {:inventory [{:name "wheat" :count 2}] :blocks (merge table-block wheat-chest) :containers {"-2,64,3" [{:name "wheat" :count 1}]}})
(defn chest-wheat [p] (some #(when (= "wheat" (:name %)) (:count %)) (get-in @(engine.fake/state p) [:containers [-2 64 3]])))

(deftest craft-fetches-a-missing-ingredient-from-a-seen-chest-by-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (wheat-world))
              result (await (child-outcome eng job {:item "bread"} 80))]
          (is (= {:item "bread" :made 1 :status :done} (dissoc result :text)))
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
          (is (= {:item "bread" :made 3 :status :done} (dissoc result :text)))
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
              result (await (child-outcome eng job {:item "bread" :ways #{:carried}} 80))]
          (is (= {:made 0 :status :stopped :reason :need :short {"wheat" 1} :text "craft bread is missing {\"wheat\" 1}"} (dissoc result :item)))
          (is (= 1 (chest-wheat p))))))))


(deftest craft-places-a-carried-table-when-none-is-in-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "wheat" :count 3} {:name "crafting_table" :count 1}]})
              result (await (child-outcome eng job {:item "bread" :keep-table true} 40))]
          (is (= {:made 1 :spent {"crafting_table" 1}} (select-keys result [:made :spent])))
          (is (re-find #"^used 1 crafting_table \(crafting table at \[" (:text result)))
          (is (= 1 (get (inv p) "bread")))
          (is (nil? (get (inv p) "crafting_table"))))))))

(deftest craft-puts-a-carried-table-down-instead-of-walking-to-a-far-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "wheat" :count 3} {:name "crafting_table" :count 1}] :blocks table-block})
              result (await (child-outcome eng job {:item "bread" :keep-table true} 40))]
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

(deftest craft-steps-its-table-down-beside-a-body-on-a-stump
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pit (into {} (for [x [-1 0 1] z [-1 0 1] :when (not= [0 0] [x z]) [y n] [[62 "stone"] [63 "air"]]] [(str x "," y "," z) n]))
              [result p] (await (craft {:inventory [{:name "wheat" :count 3} {:name "crafting_table" :count 1}] :blocks pit} {:item "bread"}))]
          (is (= 1 (:made result)) (pr-str result))
          (is (= [63] (mapv :y (placed-at p)))))))))

(deftest craft-puts-its-table-down-on-snow-layers-beside-the-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [snow (into {} (for [x [-1 0 1] z [-1 0 1] :when (not= [0 0] [x z]) [y n] [[63 "stone"] [64 "snow"]]] [(str x "," y "," z) n]))
              [result p] (await (craft {:inventory [{:name "wheat" :count 3} {:name "crafting_table" :count 1}] :blocks snow} {:item "bread"}))]
          (is (= 1 (:made result)) (pr-str result))
          (is (= 1 (count (placed-at p)))))))))

(deftest craft-never-puts-its-table-over-a-bed-or-in-a-doorway
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [blocks [{"1,63,0" "red_bed"} {"2,64,0" "oak_door"} {"2,65,0" "oak_door"}]]
          (let [[_ p] (await (craft {:inventory [{:name "wheat" :count 3} {:name "crafting_table" :count 1}] :blocks blocks} {:item "bread"}))]
            (is (= 1 (count (placed-at p))))
            (is (not= {:x 1 :y 64 :z 0} (first (placed-at p))) (pr-str blocks))))))))

(deftest craft-in-a-one-wide-tunnel-puts-its-table-in-a-side-niche-not-the-corridor
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [tunnel (into {} (for [x (range -4 5) z [-1 0 1] y [63 64 65 66]
                                    :when (or (= 63 y) (= 66 y) (not (zero? z)))]
                                [(str x "," y "," z) "stone"]))
              [result p] (await (craft {:inventory [{:name "wheat" :count 3} {:name "crafting_table" :count 1}] :blocks tunnel} {:item "bread"}))]
          (is (= 1 (:made result)) (pr-str result))
          (is (every? #(not (zero? (:z %))) (placed-at p)) (pr-str (placed-at p))))))))

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
                           (for [x (range -2 3) z (range -2 3) y [63 64 65] :when (or (= 63 y) (not (and (zero? x) (zero? z))))]
                             [(str x "," y "," z) "stone"]))
              {:keys [eng p]} (setup {:inventory [{:name "wheat" :count 3} {:name "crafting_table" :count 1}] :blocks pocket})
              result (await (child-outcome eng job {:item "bread"} 40))]
          (is (= {:made 1 :spent {"crafting_table" 1}} (select-keys result [:made :spent])) (pr-str result))
          (is (= 2 (count (calls p "dig"))) "one cell beside dug bare-handed, then the table dug up")
          (is (= 1 (get (inv p) "bread"))))))))

(deftest craft-in-a-pocket-skips-the-cells-beside-water-and-digs-the-next
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pocket (into (into {"0,66,0" "stone"}
                                 (for [x (range -2 3) z (range -2 3) y [63 64 65] :when (or (= 63 y) (not (and (zero? x) (zero? z))))]
                                   [(str x "," y "," z) "stone"]))
                           {"-2,64,0" "water" "2,64,0" "water" "0,64,-2" "water"})
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

(deftest craft-fetch-false-still-puts-a-carried-table-down
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "wheat" :count 3} {:name "crafting_table" :count 1}]})
              result (await (child-outcome eng job {:item "bread" :ways #{:carried}} 40))]
          (is (= 1 (:made result)))
          (is (= 1 (get (inv p) "bread"))))))))

(deftest craft-fetch-false-without-a-table-stops-no-table
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (craft {:inventory [{:name "wheat" :count 3}]} {:item "bread" :ways #{:carried}}))]
          (is (= {:item "bread" :made 0 :status :stopped :reason :no-table} (dissoc result :text)))
          (is (nil? (get (inv p) "bread"))))))))

(deftest craft-does-not-walk-twice-to-a-walled-in-table-and-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result _ _ _ walks] (await (craft-counting-walks {:inventory [{:name "wheat" :count 3}] :blocks table-block :unreachable ["10,64,0"]}
                                                                {:item "bread" :ways #{:carried}}))]
          (is (= {:made 0 :status :stopped :reason :unreachable} (select-keys result [:made :status :reason])))
          (is (= 1 walks) "a table the walk cannot reach is not walked to again")
          (is (re-find #"cannot be reached" (:text result))))))))

(deftest craft-says-why-the-carried-table-could-not-be-put-down
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [around (vec (for [y [63 64 65] [dx dz] spot/table-offsets] [dx y dz]))
              [result] (await (craft {:inventory [{:name "wheat" :count 3} {:name "crafting_table" :count 1}] :blocks table-block :unreachable ["10,64,0"]}
                                     {:item "bread" :avoid around}))]
          (is (= :unreachable (:reason result)))
          (is (re-find #"carried crafting_table could not be put down" (:text result))))))))

(deftest craft-goes-on-after-a-timeout-that-made-something
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [timeout [#js {:status "timeout" :inventoryChange #js {"oak_planks" 4 "oak_log" -1}} #js {:status "timeout"}]]
          (let [{:keys [eng p]} (setup {:inventory [{:name "oak_log" :count 1}]})]
            (.override (.-world p) "craft"
                       (fn ^:async f [token args impl]
                         (await (impl token args))
                         timeout))
            (let [result (await (child-outcome eng job {:item "oak_planks" :count 4} 8))]
              (is (= {:made 4 :text "crafted 4 oak_planks" :status :done} (dissoc result :item)))
              (is (= 1 (count (calls p "craft")))))))))))

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
              result (await (child-outcome eng job {:item "bread" :ways #{:carried}} 20))]
          (is (seq (calls p "look")) "the table behind the body is not known until it looks")
          (is (= {:item "bread" :made 1 :status :done} (dissoc result :text))))))))

(deftest craft-a-gate-without-a-table-makes-it-from-the-carried-logs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [trees (into {} (for [x [4 6 8] y [64 65 66]] [(str x "," y ",3") "oak_log"]))
              [result p] (await (craft {:blocks trees
                                        :recipes {"oak_fence_gate" {:count 1 :needs {"oak_planks" 2 "stick" 4} :table true}}
                                        :inventory [{:name "oak_log" :count 6} {:name "oak_planks" :count 1}]}
                                       {:item "oak_fence_gate" :count 1}))]
          (is (= 1 (get (inv p) "oak_fence_gate")) (pr-str result))
          (is (every? #(< (.. % -args -pos -x) 4) (calls p "dig")) "no tree was felled (the one dig takes the table up)"))))))

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

(def own-stair-world
  "A stair rising east from the body, cut into rock: floors (0 63) (1 64) (2 65), feet and head cells carved."
  (merge (into {} (for [x (range -1 5) z [-1 0 1] y (range 63 69)] [(str x "," y "," z) "stone"]))
         (into {} (for [[x y] [[0 64] [0 65] [1 65] [1 66] [2 66] [2 67]]] [(str x "," y ",0") "air"]))))

(defn ^:async craft-in-own-stair [args]
  (let [{:keys [eng p]} (setup {:inventory [{:name "wheat" :count 3} {:name "crafting_table" :count 1}] :blocks own-stair-world})]
    (mem/write! (:store eng) :stair-way {:dim "overworld" :floors [[0 63 0] [1 64 0] [2 65 0]]})
    (let [result (await (child-outcome eng job (merge {:item "bread"} args) 60))]
      [result p])))

(deftest craft-on-its-own-stair-never-puts-the-table-on-a-step
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (craft-in-own-stair {:keep-table true}))
              way #{{:x 1 :y 65 :z 0} {:x 1 :y 66 :z 0} {:x 2 :y 66 :z 0} {:x 2 :y 67 :z 0}}]
          (is (= 1 (:made result)) (pr-str result))
          (is (= 1 (count (placed-at p))))
          (is (not-any? way (placed-at p)) (pr-str (placed-at p))))))))

(deftest craft-picks-its-table-up-again-when-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (craft {:inventory [{:name "wheat" :count 3} {:name "crafting_table" :count 1}]} {:item "bread"}))]
          (is (= 1 (:made result)) (pr-str result))
          (is (= 1 (count (calls p "dig"))) "the table was dug")
          (is (= 1 (get (inv p) "crafting_table")) "and is carried again"))))))

(deftest craft-cancelled-after-the-table-went-down-still-picks-its-table-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "wheat" :count 6} {:name "crafting_table" :count 1}]})
              calls-seen (atom 0)
              id (core/submit! eng (list job {:item "bread" :count 2}) {})]
          (.override (.-world p) "craft"
                     (fn [token a impl]
                       (if (= 1 (swap! calls-seen inc))
                         (impl token a)
                         (do (core/cancel! eng id)
                             #js {:status "timeout"}))))
          (await (tu/run-until-empty eng 20))
          (is (= 2 (count (calls p "craft"))) "cancelled by the call after the table went down")
          (is (= 1 (get (inv p) "crafting_table")) "the table is carried again"))))))

(deftest craft-whose-table-pick-up-fails-reports-table-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "wheat" :count 3} {:name "crafting_table" :count 1}]})
              _ (.override (.-world p) "dig" (fn ^:async f [_ _ _] #js {:status "unreachable" :reason "unreachable"}))
              result (await (child-outcome eng job {:item "bread"} 12))]
          (is (= {:kind :nothing-possible :cause :table-left} (:cleanup result)) (pr-str result))
          (is (nil? (get (inv p) "crafting_table")) "the table still stands"))))))

(deftest craft-count-zero-is-done-at-once
  (let [c {:primitives (tu/fake {}) :args {:item "bread" :count 0} :root "j1" :slots [] :view (fn [] {:now 0 :data {}})}]
    (is (= :ok (first ((:done? (get registry/jobs job)) c))))))

(deftest craft-with-keep-table-leaves-it-standing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[_ p] (await (craft {:inventory [{:name "wheat" :count 3} {:name "crafting_table" :count 1}]} {:item "bread" :keep-table true}))]
          (is (empty? (calls p "dig")))
          (is (nil? (get (inv p) "crafting_table"))))))))

(deftest craft-that-stops-short-still-picks-its-table-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (craft {:inventory [{:name "crafting_table" :count 1}]} {:item "bread" :ways #{:carried}}))]
          (is (some? (:short result)) (pr-str result))
          (is (= 1 (get (inv p) "crafting_table"))))))))

;; ------------------------------------------------------------------ the contract

(defn ^:async root-ending
  "{:event :kind :reason :data} of the first completed or stopped event of the craft run as the root job on a world."
  [world args]
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        p (tu/seeing-all (tu/fake-on-floor world))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})
        ended #(first (filter (comp #{:completed :stopped} :kind) @seen))]
    (core/submit! eng (list job args) {})
    (loop [i 0]
      (when (and (< i 40) (nil? (ended)))
        (swap! clock + 500)
        (await (core/tick! eng))
        (recur (inc i))))
    (let [e (ended)]
      (core/shutdown! eng)
      (assoc (select-keys (:data e) [:kind :reason :made :short]) :event (:kind e)))))

(deftest the-job-exports-the-contract-and-quotes-validly
  (let [def (get registry/jobs job)
        q ((:quote-of def) {:primitives (tu/fake {:inventory [{:name "oak_log" :count 2}]}) :args {:item "oak_planks" :count 4}})]
    (is (= [:item] (:target def)))
    (is (some? (:can-start? def)))
    (is (nil? (contract/quote-problem q)) (pr-str q))
    (is (= [4 4 4] (get-in q [:yields :items "oak_planks"])))
    (is (= [0 1 1] (get-in q [:uses :items "oak_log"])))))

(deftest a-craft-ends-with-a-kind-and-a-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [wheat2 [{:name "wheat" :count 2}]
              end (fn ^:async end* [world args] (await (root-ending world args)))]
          (is (= {:event :completed :kind :done :made 4}
                 (await (end {:inventory [{:name "oak_log" :count 1}]} {:item "oak_planks" :count 4}))))
          (is (= {:event :stopped :kind :condition-failed :reason :need :made 0 :short {"wheat" 1}}
                 (await (end {:inventory wheat2 :blocks table-block} {:item "bread" :ways #{:carried}}))) "an ingredient short")
          (is (= {:event :stopped :kind :condition-failed :reason :no-table :made 0}
                 (await (end {:inventory [{:name "wheat" :count 3}]} {:item "bread" :ways #{:carried}}))))
          (is (= {:event :stopped :kind :condition-failed :reason :not-a-table :made 0}
                 (await (end {:inventory [{:name "wheat" :count 3}] :blocks {"10,64,0" "stone"}} {:item "bread" :table table}))))
          (is (= {:event :stopped :kind :nothing-possible :reason :unreachable :made 0}
                 (await (end {:inventory [{:name "wheat" :count 3}] :blocks table-block :unreachable ["10,64,0"]} {:item "bread"}))) "a walk to the table failed")
          (is (= {:event :stopped :kind :nothing-possible :reason :cannot :made 0}
                 (await (end {} {:item "dragon_egg"})))))))))

(deftest a-quote-with-no-recipe-yields-nothing-and-is-finite
  (let [q ((:quote-of (get registry/jobs job)) {:primitives (tu/fake {}) :args {:item "dragon_egg" :count 1}})]
    (is (nil? (contract/quote-problem q)) (pr-str q))
    (is (nil? (get-in q [:yields :items "dragon_egg"])))
    (is (every? #(js/isFinite %) (get-in q [:uses :job-time])))))

(deftest a-quote-with-a-full-bag-is-finite-and-still-valid
  (let [filler (mapv (fn [_] {:name "iron_pickaxe" :count 1}) (range 35))
        q ((:quote-of (get registry/jobs job)) {:primitives (tu/fake {:inventory (conj filler {:name "oak_log" :count 1})}) :args {:item "oak_planks" :count 4}})]
    (is (nil? (contract/quote-problem q)) (pr-str q))
    (is (every? #(js/isFinite %) (get-in q [:uses :job-time])))
    (is (= [4 4 4] (get-in q [:yields :items "oak_planks"])))))

(deftest craft-missing-ingredients-do-not-put-the-carried-table-down
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (craft {:inventory [{:name "wheat" :count 2} {:name "crafting_table" :count 1}]} {:item "bread" :ways #{:carried}}))]
          (is (= {:item "bread" :made 0 :status :stopped :reason :need :short {"wheat" 1}} (dissoc result :text)))
          (is (empty? (calls p "place")) "no table placed")
          (is (= 1 (get (inv p) "crafting_table"))))))))

(def house {"house" {:id "house" :metadata {:by "Fake"} :parts [{:id "all" :cells (vec (for [x (range -2 3) y [64 65] z (range -2 3)] [x y z])) :want {:block "stone"}}]}})

(deftest craft-puts-its-table-in-the-callers-own-plan-footprint-only-when-told-it-is-the-callers
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args placed?] [[{:item "bread"} false] [{:item "bread" :for-plan "house"} true]]]
          (let [[_ p] (await (craft {:plans house :inventory [{:name "wheat" :count 3} {:name "crafting_table" :count 1}]} args))]
            (is (= placed? (boolean (seq (placed-at p)))) (pr-str args))
            (is (= placed? (boolean (seq (calls p "dig")))) (str "the table is dug up too " (pr-str args)))))))))

(def ring-cells (vec (for [x (range -2 3) z (range -2 3) :when (or (#{-2 2} x) (#{-2 2} z))] [x 64 z])))
(def pen-plan {"pen" {:id "pen" :metadata {:by "Fake"} :parts [{:id "fence" :cells ring-cells :want "oak_fence"}]}})

(deftest craft-keeps-its-table-out-of-the-inside-of-a-fence-ring-even-for-that-plan
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [args [{:item "bread"} {:item "bread" :for-plan "pen"}]]
          (let [[_ p] (await (craft {:plans pen-plan :blocks (into {} (map (fn [[x y z]] [(str x "," y "," z) "oak_fence"])) ring-cells)
                                     :inventory [{:name "wheat" :count 3} {:name "crafting_table" :count 1}]} args))]
            (is (empty? (placed-at p)) (pr-str args))))))))
