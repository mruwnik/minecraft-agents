(ns engine.library-test
  "The job and trigger library end to end against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            ["fs" :as fs]
            [engine.catalog :as catalog]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.scenario :as scenario]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(defn setup
  ([world] (setup world (tu/tmp-dir)))
  ([world dir]
   (let [clock (atom 1000000)
         [seen sink] (tu/capture-sink)
         p (tu/fake world)
         eng (core/create {:primitives p :catalog catalog/catalog :dir dir :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
     {:eng eng :p p :seen seen :clock clock})))

(defn ^:async run-until-empty
  "Tick until the list is empty, at most n ticks; returns the ticks used."
  [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (await (core/tick! eng))
          (recur (inc i))))))

(defn tree [x z species height]
  (merge
   (into {} (for [y (range 64 (+ 64 height))] [(str x "," y "," z) (str species "_log")]))
   {(str x "," (+ 64 height) "," z) (str species "_leaves")
    (str (inc x) "," (+ 63 height) "," z) (str species "_leaves")}))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))

(defn inv [p] (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p))))

(defn debts [eng] (mapv :data (mem/entries (mem/view (:store eng)) :forestry/replant)))

(defn job-mem [eng id slots] (mem/job-mem (mem/view (:store eng)) id slots))

(defn know-place! [eng kind pos] (mem/write! (:store eng) kind {:pos pos} mem/place-policy))

;; ---------------------------------------------------------------- fell-tree

(deftest fell-tree-digs-bottom-up-and-records-the-debt
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks (tree 3 0 "oak" 4)})]
          (core/submit! eng :fell-tree {:species "oak" :radius 10} {})
          (await (core/tick! eng))
          (is (= [{:x 3 :y 64 :z 0} {:x 3 :y 65 :z 0}]
                 (mapv #(js->clj (.-pos (.-args %)) :keywordize-keys true) (calls p "dig")))
              "one round digs at most two logs, lowest first")
          (is (= [{:pos {:x 3 :y 64 :z 0} :species "oak"}] (debts eng))
              "the replant debt is committed with the base position")
          (is (pos? (await (run-until-empty eng 6))))
          (is (= [] (:list (core/state eng))))
          (is (nil? (get (inv p) "oak_log")) "the logs are drops on the ground, not collected")
          (is (= [{:pos {:x 3 :y 64 :z 0} :species "oak"}] (debts eng))
              "the debt is recorded once"))))))

(deftest fell-tree-nearest-species-and-no-tree
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks (merge (tree 8 0 "birch" 3) (tree 2 2 "spruce" 3))})]
          (core/submit! eng :fell-tree {:radius 10} {})
          (await (core/tick! eng))
          (is (= [{:x 2 :y 64 :z 2}] (mapv #(js->clj (.-pos (.-args %)) :keywordize-keys true) (take 1 (calls p "dig"))))
              "nil species picks the nearest tree")
          (is (= "spruce" (:species (first (debts eng))))))))))

(deftest fell-tree-is-not-yet-without-a-tree-and-ignores-bare-log-piles
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"3,64,0" "oak_log" "3,65,0" "oak_log"}})]
          (core/submit! eng :fell-tree {:radius 10} {})
          (is (nil? (core/tick! eng)) "logs without leaves are not a tree")
          (is (= [] (calls p "dig"))))))))

;; fell-tree reachability

(defn dig-xs [p] (mapv #(.-x (.-pos (.-args %))) (calls p "dig")))

(deftest fell-tree-skips-a-blocked-tree-for-the-next-candidate
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks (merge (tree 8 0 "oak" 3) (tree 12 0 "oak" 3))
                                           :unreachable ["8,64,0"]})]
          (core/submit! eng :fell-tree {:radius 20} {})
          (is (pos? (await (run-until-empty eng 10))))
          (is (= [12 12 12] (dig-xs p)) "only the reachable tree is dug")
          (is (= [{:pos {:x 12 :y 64 :z 0} :species "oak"}] (debts eng))
              "no debt for the tree that was never felled")
          (is (not-any? #(= :tree_blocked (:kind %)) @seen)))))))

(deftest fell-tree-warns-and-finishes-when-every-tree-is-blocked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks (tree 8 0 "oak" 3) :unreachable ["8,64,0"]})]
          (core/submit! eng :fell-tree {:radius 20} {})
          (is (pos? (await (run-until-empty eng 10))))
          (is (= [] (:list (core/state eng))))
          (is (= [] (calls p "dig")))
          (is (= [] (debts eng)) "no debt without a felled tree")
          (is (= 1 (count (filter #(= :tree_blocked (:kind %)) @seen)))))))))

(deftest fell-tree-gives-up-on-a-tree-after-three-partials-in-a-row
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks (tree 8 0 "oak" 3)})]
          (.override (.-world p) "moveTo" (fn ^:async f [_ _ _] #js {:status "partial"}))
          (core/submit! eng :fell-tree {:radius 20} {})
          (await (run-until-empty eng 2))
          (is (empty? (:unreachable (job-mem eng "j1" []))) "two partials are still progress")
          (await (run-until-empty eng 10))
          (is (= [] (calls p "dig")))
          (is (= 1 (count (filter #(= :tree_blocked (:kind %)) @seen)))))))))

(deftest fell-tree-partials-reset-once-a-walk-succeeds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks (tree 8 0 "oak" 6)})
              calls-made (atom 0)]
          (.override (.-world p) "moveTo"
                     (fn ^:async f [token args impl]
                       (if (odd? (swap! calls-made inc))
                         #js {:status "partial"}
                         (await (impl token args)))))
          (core/submit! eng :fell-tree {:radius 20} {})
          (is (pos? (await (run-until-empty eng 30))))
          (is (= 6 (count (calls p "dig"))) "alternating partials never reach three in a row")
          (is (not-any? #(= :tree_blocked (:kind %)) @seen)))))))

(defn dig-unreachable-at
  "Make the dig primitive answer unreachable for logs in column x."
  [p x]
  (.override (.-world p) "dig"
             (fn ^:async f [token args impl]
               (if (= x (.-x (.-pos args)))
                 #js {:status "unreachable"}
                 (await (impl token args))))))

(deftest fell-tree-skips-a-tree-whose-logs-cannot-be-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks (merge (tree 8 0 "oak" 3) (tree 12 0 "oak" 3))})]
          (dig-unreachable-at p 8)
          (core/submit! eng :fell-tree {:radius 20} {})
          (is (pos? (await (run-until-empty eng 10))))
          (is (= [8 12 12 12] (dig-xs p)) "one failed dig on the ledge tree, then the next tree is felled")
          (is (= [{:pos {:x 12 :y 64 :z 0} :species "oak"}] (debts eng))
              "no debt for the tree that was never dug")
          (is (not-any? #(= :tree_blocked (:kind %)) @seen)))))))

(deftest fell-tree-warns-and-finishes-when-the-only-tree-cannot-be-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks (tree 8 0 "oak" 3)})]
          (dig-unreachable-at p 8)
          (core/submit! eng :fell-tree {:radius 20} {})
          (is (pos? (await (run-until-empty eng 10))))
          (is (= [] (:list (core/state eng))))
          (is (= [8] (dig-xs p)) "one dig attempt, no retries")
          (is (= [] (debts eng)))
          (is (= 1 (count (filter #(= :tree_blocked (:kind %)) @seen)))))))))

;; ------------------------------------------------------------ collect-drops

(deftest collect-drops-one-per-round-nearest-first-with-filter
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [item (fn [id x name] {:id id :name "item" :kind "item" :pos {:x x :y 64 :z 0} :item {:name name :count 1}})
              {:keys [eng p]} (setup {:entities [(item 1 6 "oak_log") (item 2 2 "stick") (item 3 4 "oak_sapling") (item 4 40 "oak_log")]})]
          (core/submit! eng :collect-drops {:radius 10 :filter ["oak_log" "oak_sapling"]} {})
          (await (core/tick! eng))
          (is (= [3] (mapv #(.-id (.-args %)) (calls p "collect"))) "nearest matching, one per round")
          (await (run-until-empty eng 5))
          (is (= [] (:list (core/state eng))))
          (is (= [3 1] (mapv #(.-id (.-args %)) (calls p "collect"))))
          (is (= {"oak_sapling" 1 "oak_log" 1} (inv p)) "stick and the far log are left"))))))

(deftest collect-drops-without-filter-takes-everything-and-skips-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [item (fn [id x name] {:id id :name "item" :kind "item" :pos {:x x :y 64 :z 0} :item {:name name :count 1}})
              {:keys [eng p]} (setup {:entities [(item 1 2 "stick") (item 2 3 "dirt")] :unreachable ["2,64,0"]})]
          (core/submit! eng :collect-drops {:radius 10} {})
          (is (<= (await (run-until-empty eng 8)) 4) "an unreachable item does not loop forever")
          (is (= {"dirt" 1} (inv p))))))))

;; ------------------------------------------------------------ plant-sapling

(deftest plant-sapling-reads-the-debt-places-and-clears-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "oak_sapling" :count 1}]})]
          (core/submit! eng :plant-sapling {} {})
          (await (run-until-empty eng 3))
          (is (= [] (:list (core/state eng))) "no debt and no position: nothing to plant, done")
          (is (= [] (calls p "place"))))))))

(deftest plant-sapling-uses-the-fell-debt
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks (tree 3 0 "oak" 2) :inventory [{:name "oak_sapling" :count 1}]})]
          (core/submit! eng :fell-tree {:species "oak" :radius 10} {})
          (await (run-until-empty eng 6))
          (core/submit! eng :plant-sapling {:species "oak"} {})
          (await (run-until-empty eng 4))
          (is (= "oak_sapling" (.-name (.blockAt p #js {:x 3 :y 64 :z 0}))))
          (is (= [] (debts eng)) "debt cleared"))))))

(deftest plant-sapling-at-a-position-and-not-yet-without-a-sapling
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {})]
          (core/submit! eng :plant-sapling {:at {:x 5 :y 64 :z 5}} {})
          (is (nil? (core/tick! eng)) "no sapling in the inventory: not yet")
          (set! (.. p -world -state -inventory) #js [#js {:name "birch_sapling" :count 1}])
          (await (run-until-empty eng 4))
          (is (= "birch_sapling" (.-name (.blockAt p #js {:x 5 :y 64 :z 5})))))))))

;; ------------------------------------------------------------------ deposit

(def chest-world
  {:inventory [{:name "oak_log" :count 5} {:name "stone_axe" :count 1} {:name "bread" :count 2}]
   :containers {"10,64,0" []}})

(deftest deposit-one-stack-per-round-keeps-tools
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup chest-world)]
          (core/submit! eng :deposit {:chest {:x 10 :y 64 :z 0}} {})
          (await (core/tick! eng))
          (is (= 1 (count (calls p "transfer"))) "one stack per round")
          (await (run-until-empty eng 6))
          (is (= {"stone_axe" 1} (inv p)))
          (is (= [{:name "oak_log" :count 5} {:name "bread" :count 2}]
                 (js->clj (.get (.. p -world -state -containers) "10,64,0") :keywordize-keys true))))))))

(deftest deposit-item-filter-and-chest-from-places
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup chest-world)]
          (core/submit! eng :deposit {:items ["bread"]} {})
          (is (nil? (core/tick! eng)) "no chest known: blocked")
          (know-place! eng :chest {:x 10 :y 64 :z 0})
          (await (run-until-empty eng 6))
          (is (= {"oak_log" 5 "stone_axe" 1} (inv p))))))))

(deftest deposit-gives-up-when-the-chest-is-full
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup chest-world)]
          (.override (.-world p) "transfer" (fn ^:async f [_ _ _] #js {:status "full" :moved 0}))
          (core/submit! eng :deposit {:chest {:x 10 :y 64 :z 0}} {})
          (await (run-until-empty eng 8))
          (is (= [] (:list (core/state eng))))
          (is (some #(= :chest_unusable (:kind %)) @seen)))))))

;; ------------------------------------------------------------- harvest-wood

(deftest harvest-wood-composes-the-three-children
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks (tree 3 0 "oak" 3) :inventory [{:name "oak_sapling" :count 1}]})]
          (core/submit! eng :harvest-wood {:species "oak" :radius 10} {})
          (await (core/tick! eng))
          (is (= {:x 3 :z 0} (:column (job-mem eng "j1" [:fell]))) "the child's memory lives under the parent's slot")
          (is (< (await (run-until-empty eng 20)) 20) "finishes")
          (is (= [] (:list (core/state eng))))
          (is (= 3 (get (inv p) "oak_log")) "logs collected")
          (is (= "oak_sapling" (.-name (.blockAt p #js {:x 3 :y 64 :z 0}))) "replanted at the base")
          (is (= [] (debts eng))))))))

;; --------------------------------------------------------- retreat / sleep

(deftest retreat-walks-away-until-no-hostile-within-radius
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:entities [{:id 7 :name "zombie" :kind "hostile" :pos {:x 5 :y 64 :z 0}}]})]
          (core/submit! eng :retreat {:radius 8} {})
          (await (core/tick! eng))
          (is (< (.-x (.-pos (.self p))) 0) "moved away from the zombie, along x")
          (await (run-until-empty eng 4))
          (is (= [] (:list (core/state eng)))))))))

(deftest retreat-is-bounded-when-the-hostile-keeps-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:entities [{:id 7 :name "zombie" :kind "hostile" :pos {:x 1 :y 64 :z 0}}]})]
          (.override (.-world p) "moveTo" (fn ^:async f [_ _ _] #js {:status "arrived"}))
          (core/submit! eng :retreat {:radius 8} {})
          (is (<= (await (run-until-empty eng 20)) 6) "gives up after a bounded number of moves"))))))

(deftest sleep-walks-to-the-bed-and-sleeps-at-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time 14000 :blocks {"6,64,0" "red_bed"}})]
          (know-place! eng :bed {:x 6 :y 64 :z 0})
          (core/submit! eng :sleep {} {})
          (await (run-until-empty eng 4))
          (is (= 1 (count (calls p "sleep"))))
          (is (.-isDay (.self p)) "slept through the night"))))))

(deftest sleep-is-not-yet-in-daytime
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:time 1000 :blocks {"6,64,0" "red_bed"}})]
          (know-place! eng :bed {:x 6 :y 64 :z 0})
          (core/submit! eng :sleep {} {})
          (is (nil? (core/tick! eng))))))))

;; ----------------------------------------------------------------- triggers

(deftest hostile-near-fires-retreat-from-a-scenario
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:entities [{:id 7 :name "zombie" :kind "hostile" :pos {:x 5 :y 64 :z 0}}]})]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :hostile-near}]}"))
          (await (core/tick! eng))
          (is (some #(= :fired (:kind %)) @seen))
          (is (some #(= :retreat (:name %)) @seen)))))))

(deftest night-and-bed-known-fires-sleep
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time 14000 :blocks {"6,64,0" "red_bed"}})]
          (know-place! eng :bed {:x 6 :y 64 :z 0})
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night-and-bed-known}]}"))
          (await (run-until-empty eng 1))
          (await (core/tick! eng))
          (is (= 1 (count (calls p "sleep")))))))))

(deftest inventory-nearly-full-fires-deposit-once-a-chest-is-known
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [stacks (mapv (fn [i] {:name (str "item_" i) :count 1}) (range 30))
              {:keys [eng p]} (setup {:inventory stacks :containers {"10,64,0" []}})]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :inventory-nearly-full}]}"))
          (is (nil? (core/tick! eng)) "no chest known: does not fire")
          (know-place! eng :chest {:x 10 :y 64 :z 0})
          (await (core/tick! eng))
          (is (= 1 (count (calls p "transfer")))))))))

;; ----------------------------------------------------------------- scenario

(deftest woodcutter-scenario-is-valid-and-registers-the-library
  (let [s (scenario/parse (fs/readFileSync "scenarios/woodcutter.edn" "utf8"))]
    (is (= [] (scenario/problems catalog/catalog s)))
    (is (= [:hostile-near :health-low :night-and-bed-known] (mapv :trigger (:register s))))
    (is (= [:harvest-wood :deposit] (mapv :job (:queue s))))))

;; ---------------------------------------------------------------------- pace

(def pace-args {:a {:x 5 :y 64 :z 0} :b {:x 10 :y 64 :z 0} :laps 3 :rounds 2})

(defn move-xs [p] (mapv #(.-x (.-pos (.-args %))) (calls p "moveTo")))

(deftest pace-walks-a-b-a-b-in-order-and-continues
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {})]
          (core/submit! eng :pace pace-args {})
          (await (core/tick! eng))
          (is (= [5 10 5 10 5 10] (move-xs p)))
          (is (= 1 (count (:list (core/state eng)))) "still listed after round one"))))))

(deftest pace-stops-the-round-early-on-a-blocked-leg
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:unreachable ["10,64,0"]})]
          (core/submit! eng :pace pace-args {})
          (await (core/tick! eng))
          (is (= [5 10] (move-xs p)) "ends at the blocked leg")
          (is (= 1 (count (:list (core/state eng)))) "a blocked leg does not throw or finish"))))))

(deftest pace-is-done-after-the-given-rounds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {})]
          (core/submit! eng :pace pace-args {})
          (await (core/tick! eng))
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))))
          (is (= 12 (count (calls p "moveTo")))))))))

;; ------------------------------------------------- look-around, every-interval

(deftest look-around-looks-once-and-records-when
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {})]
          (core/submit! eng :look-around {} {})
          (await (core/tick! eng))
          (is (= 1 (count (calls p "look"))))
          (is (= [] (:list (core/state eng))))
          (is (= 1000000 (:t (mem/latest (mem/view (:store eng)) :looked)))))))))

(def interval-ms 45000)

(defn view-with
  "A memory view at now-ms holding one entry of kind written at t, or none."
  [kind t data now-ms]
  {:data (if t (mem/add-entry mem/empty-data kind {:t t :data data} mem/place-policy) mem/empty-data)
   :now now-ms})

(defn holds-with [last now-ms]
  ((:when triggers/every-interval) nil (view-with :looked last {} now-ms) {:seconds 45}))

(deftest every-interval-holds-without-a-record-and-after-the-interval
  (is (true? (holds-with nil 1000)) "no record: fire at once")
  (is (false? (holds-with 1000 (+ 1000 interval-ms -1))))
  (is (true? (holds-with 1000 (+ 1000 interval-ms))))
  (is (true? (holds-with 1000 (+ 1000 (* 2 interval-ms))))))

(deftest every-interval-survives-a-restart-through-body-memory
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              {:keys [eng p clock]} (setup {} dir)
              spec "{:register [{:trigger :every-interval :args {:seconds 45}}]}"]
          (core/load-scenario! eng (scenario/parse spec))
          (await (core/tick! eng))
          (is (= 1 (count (calls p "look"))))
          (is (nil? (core/tick! eng)) "recorded: the trigger stopped holding, so no refire")
          (let [{again :eng p2 :p clock2 :clock} (setup {} dir)]
            (is (nil? (core/tick! again)) "the record was restored from disk")
            (reset! clock2 (+ @clock interval-ms))
            (await (core/tick! again))
            (is (= 1 (count (calls p2 "look"))))))))))

(deftest woodcutter-cuts-scenario-puts-every-interval-on-top
  (let [s (scenario/parse (fs/readFileSync "scenarios/woodcutter-cuts.edn" "utf8"))
        base (scenario/parse (fs/readFileSync "scenarios/woodcutter.edn" "utf8"))]
    (is (= [] (scenario/problems catalog/catalog s)))
    (is (= {:trigger :every-interval :args {:seconds 45}} (first (:register s))))
    (is (= (:register base) (rest (:register s))))
    (is (= (:queue base) (:queue s)))))

(deftest pace-cuts-scenario-puts-every-interval-first-and-queues-pace
  (let [s (scenario/parse (fs/readFileSync "scenarios/pace-cuts.edn" "utf8"))]
    (is (= [] (scenario/problems catalog/catalog s)))
    (is (= [:every-interval :hostile-near :health-low] (mapv :trigger (:register s))))
    (is (= [:pace] (mapv :job (:queue s))))))

(deftest fell-tree-keeps-walking-on-a-partial-move-instead-of-digging
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks (tree 90 0 "oak" 3)})]
          (core/submit! eng :fell-tree {:species "oak" :radius 120} {})
          (await (core/tick! eng))
          (is (= [] (calls p "dig")) "a partial walk is not in reach, so nothing is dug")
          (is (nil? (:failures (job-mem eng "j1" []))) "a partial walk is progress, not a failure")
          (is (pos? (await (run-until-empty eng 10))))
          (is (= [] (:list (core/state eng))))
          (is (= 3 (count (calls p "dig")))))))))

(deftest collect-drops-ignores-an-item-entity-whose-stack-is-unknown
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {})]
          (set! (.-entities p) (fn [_] #js [#js {:id 7 :name "item" :kind "item" :item nil :distance 2}]))
          (core/submit! eng :collect-drops {:filter ["oak_log"]} {})
          (await (core/tick! eng))
          (is (= [] (calls p "collect")) "an unknown stack is not collected when a filter is set")
          (is (= [] (:list (core/state eng))))
          (is (not-any? #(= :failed (:kind %)) @seen) "the job finishes instead of being dropped on a TypeError"))))))

;; ------------------------------------------------------ trigger args from the entry

(defn trigger-holds [trigger world args]
  ((:when trigger) (tu/fake world) (view-with :none nil nil 0) args))

(def zombie-at-12 {:entities [{:id 7 :name "zombie" :kind "hostile" :pos {:x 12 :y 64 :z 0}}]})

(deftest hostile-near-reads-its-radius-from-args
  (is (false? (trigger-holds triggers/hostile-near zombie-at-12 {:radius 8})))
  (is (true? (trigger-holds triggers/hostile-near zombie-at-12 {:radius 16}))))

(deftest health-low-reads-its-threshold-from-args
  (is (false? (trigger-holds triggers/health-low {:self {:health 10}} {:health 8})))
  (is (true? (trigger-holds triggers/health-low {:self {:health 10}} {:health 12}))))

(deftest inventory-nearly-full-reads-its-stack-count-from-args
  (let [world {:inventory (mapv (fn [i] {:name (str "item_" i) :count 1}) (range 5))}
        memory (view-with :chest 1 {:pos {:x 1 :y 64 :z 1}} 1000000)]
    (is (false? ((:when triggers/inventory-nearly-full) (tu/fake world) memory {:stacks 30})))
    (is (true? ((:when triggers/inventory-nearly-full) (tu/fake world) memory {:stacks 5})))))

(deftest a-scenario-radius-reaches-the-trigger-and-the-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup zombie-at-12)]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :hostile-near :args {:radius 16}}]}"))
          (is (= {:radius 16} (:args (first (:register (core/state eng))))))
          (await (core/tick! eng))
          (is (some #(= :fired (:kind %)) @seen)))))))
