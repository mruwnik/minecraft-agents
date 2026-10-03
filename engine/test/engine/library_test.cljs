(ns engine.library-test
  "The job and trigger library end to end against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            ["fs" :as fs]
            [engine.catalog :as catalog]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.scenario :as scenario]
            [engine.test-util :as tu]))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        p (tu/fake world)
        eng (core/create {:primitives p :catalog catalog/catalog :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock}))

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

(defn common [eng] (mem/scope (:store eng) :common))

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
          (is (= [{:pos {:x 3 :y 64 :z 0} :species "oak"}] (get-in (common eng) [:debts :replant]))
              "the replant debt is committed with the base position")
          (is (pos? (await (run-until-empty eng 6))))
          (is (= [] (:list (core/state eng))))
          (is (nil? (get (inv p) "oak_log")) "the logs are drops on the ground, not collected")
          (is (= [{:pos {:x 3 :y 64 :z 0} :species "oak"}] (get-in (common eng) [:debts :replant]))
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
          (is (= "spruce" (:species (first (get-in (common eng) [:debts :replant]))))))))))

(deftest fell-tree-is-not-yet-without-a-tree-and-ignores-bare-log-piles
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"3,64,0" "oak_log" "3,65,0" "oak_log"}})]
          (core/submit! eng :fell-tree {:radius 10} {})
          (is (nil? (core/tick! eng)) "logs without leaves are not a tree")
          (is (= [] (calls p "dig"))))))))

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
          (is (= [] (get-in (common eng) [:debts :replant])) "debt cleared"))))))

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
          (mem/commit! (:store eng) :common #(assoc-in % [:places :chest] [{:pos {:x 10 :y 64 :z 0}}]))
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
          (is (= {:x 3 :z 0} (:column (mem/job (:store eng) ["j1" :fell]))) "the child's memory lives under the parent's slot")
          (is (< (await (run-until-empty eng 20)) 20) "finishes")
          (is (= [] (:list (core/state eng))))
          (is (= 3 (get (inv p) "oak_log")) "logs collected")
          (is (= "oak_sapling" (.-name (.blockAt p #js {:x 3 :y 64 :z 0}))) "replanted at the base")
          (is (= [] (get-in (common eng) [:debts :replant]))))))))

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
          (mem/commit! (:store eng) :common #(assoc-in % [:places :bed] [{:pos {:x 6 :y 64 :z 0}}]))
          (core/submit! eng :sleep {} {})
          (await (run-until-empty eng 4))
          (is (= 1 (count (calls p "sleep"))))
          (is (.-isDay (.self p)) "slept through the night"))))))

(deftest sleep-is-not-yet-in-daytime
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:time 1000 :blocks {"6,64,0" "red_bed"}})]
          (mem/commit! (:store eng) :common #(assoc-in % [:places :bed] [{:pos {:x 6 :y 64 :z 0}}]))
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
          (mem/commit! (:store eng) :common #(assoc-in % [:places :bed] [{:pos {:x 6 :y 64 :z 0}}]))
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
          (mem/commit! (:store eng) :common #(assoc-in % [:places :chest] [{:pos {:x 10 :y 64 :z 0}}]))
          (await (core/tick! eng))
          (is (= 1 (count (calls p "transfer")))))))))

;; ----------------------------------------------------------------- scenario

(deftest woodcutter-scenario-is-valid-and-registers-the-library
  (let [s (scenario/parse (fs/readFileSync "scenarios/woodcutter.edn" "utf8"))]
    (is (= [] (scenario/problems catalog/catalog s)))
    (is (= [:hostile-near :health-low :night-and-bed-known] (mapv :trigger (:register s))))
    (is (= [:harvest-wood :deposit] (mapv :job (:queue s))))))

(deftest fell-tree-keeps-walking-on-a-partial-move-instead-of-digging
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks (tree 90 0 "oak" 3)})]
          (core/submit! eng :fell-tree {:species "oak" :radius 120} {})
          (await (core/tick! eng))
          (is (= [] (calls p "dig")) "a partial walk is not in reach, so nothing is dug")
          (is (nil? (:failures (mem/scope (:store eng) :job))) "a partial walk is progress, not a failure")
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
