(ns engine.get-kit-test
  "jobs.items.get-kit: a list of items got in order and the armour worn, against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.fetch-test :as f]
            [engine.game :as game]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.items.get-kit :as kit]
            [jobs.items.obtain.bill :as bill]
            [jobs.lib.world-files :as ew]))

(def job 'jobs.items.get-kit)

(def iron-recipes
  {"iron_pickaxe" {:count 1 :needs {"iron_ingot" 3 "stick" 2} :table true}
   "iron_helmet" {:count 1 :needs {"iron_ingot" 5} :table true}
   "iron_boots" {:count 1 :needs {"iron_ingot" 4} :table true}})

(defn world
  "A crafting table south of the body (where it faces), no chest, no ore, the body carrying inv and wearing equipment."
  [inv equipment]
  {:self {:pos {:x 0 :y 64 :z 0}}
   :blocks {"1,64,2" "crafting_table"}
   :inventory inv
   :equipment equipment
   :recipes iron-recipes})

(defn start
  "fetch-test's perception engine, with a recording parent that runs the job as a ctx/call child and keeps its ending in :out
  (the data, :status :done or :stopped, :kind, and for a stop the cause's keyword as :reason and a cause map's other fields)."
  ([w args] (start w args f/sight-opts))
  ([w args sight]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        raw (tu/fake-on-floor (assoc w :floor-block "andesite"))
        p (perception/wrap raw (perception/create (fake-raw/create raw) (assoc sight :now #(deref clock))))
        out (atom :not-done)
        parent {:round (fn ^:async recording-round [c]
                         (let [e (await (ctx/call c :kid job args))]
                           (when-not (= :continue e)
                             (reset! out (merge (:data e) (when (map? (:cause e)) (dissoc (:cause e) :reason))
                                                {:kind (:kind e)
                                                 :status (if (= :done (:kind e)) :done :stopped)}
                                                (when (not= :done (:kind e)) {:reason (contract/cause-key e)}))))
                           (if (= :continue e) :continue (ctx/end c :done nil))))}
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'recording-parent parent) :triggers triggers/all
                          :dir (tu/tmp-dir) :now #(deref clock) :world (ew/of-data {} {} [f/own-zone])
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (perception/pass! (aget p "perception"))
    (core/submit! eng '(recording-parent) {})
    {:eng eng :p p :seen seen :clock clock :out out})))

(defn worn [{:keys [p]}]
  (into {} (keep (fn [[k v]] (when (and v (#{"head" "torso" "legs" "feet"} k)) [k (get v "name")])))
        (js->clj (.-equipment (.self p)))))

(defn crafted [s] (mapv #(.-item (.-args %)) (f/calls s "craft")))

(deftest the-iron-kit-needs-32-ingots-from-the-furnace
  (is (= {"iron_ingot" 32} (bill/furnace-short game/default-version kit/iron-kit {}))))

(deftest carried-ingots-count-against-the-furnace-need
  (is (= {"iron_ingot" 1} (bill/furnace-short game/default-version ["iron_pickaxe" "iron_helmet" "iron_boots"] {"iron_ingot" 11}))))

(deftest nothing-is-short-when-the-ingots-are-carried
  (is (= {} (bill/furnace-short game/default-version ["iron_pickaxe"] {"iron_ingot" 3}))))

(deftest a-death-after-the-start-makes-the-memory-stale
  (is (kit/stale? {:since 100} {:t 200}))
  (is (not (kit/stale? {:since 300} {:t 200})))
  (is (not (kit/stale? {:since 300} nil))))

(deftest two-pieces-from-carried-ingots-are-made-and-the-helmet-worn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world [{:name "iron_ingot" :count 8} {:name "stick" :count 2}] {})
                       {:items ["iron_pickaxe" "iron_helmet"] :explore false})]
          (await (f/run-ticks s 120))
          (is (= 1 (get (f/inv s) "iron_pickaxe")))
          (is (= {"head" "iron_helmet"} (worn s)))
          (is (= {:status :done :made ["iron_pickaxe" "iron_helmet"]} (select-keys @(:out s) [:status :made])))
          (is (= [["iron_pickaxe" "iron_helmet"]] (mapv :items (f/events-of s :kit.plan))) "the plan is stated once at the start"))))))

(deftest carried-ingots-make-and-wear-every-piece-they-cover-before-the-rest-is-fetched
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (-> (world [{:name "iron_ingot" :count 10}] {})
                    (update :recipes assoc "iron_chestplate" {:count 1 :needs {"iron_ingot" 8} :table true}))
              s (start w {:items ["iron_helmet" "iron_chestplate" "iron_boots"] :explore false :food false})]
          (await (f/run-ticks s 200))
          (is (= #{"iron_helmet" "iron_boots"} (set (crafted s))))
          (is (= {"head" "iron_helmet" "feet" "iron_boots"} (worn s)))
          (is (= {:status :stopped :item "iron_chestplate" :reason :no-source}
                 (select-keys @(:out s) [:status :item :reason]))))))))

(deftest ingots-that-cannot-be-got-stop-the-job-at-once-naming-the-ingots
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world [{:name "stick" :count 2}] {})
                       {:items ["iron_pickaxe" "iron_helmet" "iron_boots"] :explore false :food false})]
          (await (f/run-ticks s 120))
          (is (not-any? #{"iron_pickaxe" "iron_helmet" "iron_boots"} (crafted s)))
          (is (= {:status :stopped :item "iron_ingot" :reason :no-source}
                 (select-keys @(:out s) [:status :item :reason])))
          (is (re-find #"^kit stopped at iron_ingot: no-source" (str (:text @(:out s)))))
          (is (re-find #"iron_ingot" (str (:why @(:out s))))))))))

(deftest carried-ingots-short-of-the-list-make-the-pieces-they-cover
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world [{:name "iron_ingot" :count 3} {:name "stick" :count 2}] {})
                       {:items ["iron_pickaxe" "iron_helmet"] :explore false :food false})]
          (await (f/run-ticks s 120))
          (is (= 1 (get (f/inv s) "iron_pickaxe")))
          (is (nil? (get (f/inv s) "iron_helmet")))
          (is (= {:status :stopped :item "iron_ingot" :reason :no-source}
                 (select-keys @(:out s) [:status :item :reason]))))))))

(deftest items-before-the-furnace-material-are-made-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (-> (world [{:name "cobblestone" :count 2} {:name "stick" :count 3}] {})
                    (update :recipes assoc "stone_sword" {:count 1 :needs {"cobblestone" 2 "stick" 1} :table true}))
              s (start w {:items ["stone_sword" "iron_pickaxe"] :explore false :food false})]
          (await (f/run-ticks s 120))
          (is (= #{"stone_sword"} (set (crafted s))))
          (is (= {:status :stopped :item "iron_ingot" :reason :no-source}
                 (select-keys @(:out s) [:status :item :reason]))))))))

(deftest a-worn-piece-is-skipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world [] {:head {:name "iron_helmet" :count 1}})
                       {:items ["iron_helmet" "iron_boots"] :explore false :food false})]
          (await (f/run-ticks s 40))
          (is (empty? (crafted s)))
          (is (= {:status :stopped :item "iron_ingot" :reason :no-source :have ["iron_helmet"]}
                 (select-keys @(:out s) [:status :item :reason :have]))))))))

(deftest a-carried-piece-is-worn-and-the-job-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world [{:name "iron_pickaxe" :count 1} {:name "iron_boots" :count 1}] {})
                       {:items ["iron_pickaxe" "iron_boots"]})]
          (await (f/run-ticks s 20))
          (is (empty? (crafted s)))
          (is (= {"feet" "iron_boots"} (worn s)))
          (is (= {:status :done :have ["iron_pickaxe" "iron_boots"]} (select-keys @(:out s) [:status :have]))))))))

(deftest a-carried-tool-worn-out-does-not-count-as-had
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world [{:name "iron_pickaxe" :count 1 :durability 1 :max-durability 250}] {})
                       {:items ["iron_pickaxe"] :explore false :food false})]
          (await (f/run-ticks s 60))
          (is (= {:status :stopped :reason :no-source} (select-keys @(:out s) [:status :reason]))))))))

(deftest a-fresh-pickaxe-is-made-beside-a-worn-one-and-the-worn-one-keeps-its-own-stack
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world [{:name "iron_pickaxe" :count 1 :durability 1 :max-durability 250}
                               {:name "iron_ingot" :count 3} {:name "stick" :count 2}] {})
                       {:items ["iron_pickaxe"] :explore false :food false})]
          (await (f/run-ticks s 60))
          (is (= :done (:status @(:out s))))
          (is (= #{{:count 1 :durability 1} {:count 1 :durability nil}}
                 (->> (:inventory @(fake/state (:p s))) (filter #(= "iron_pickaxe" (:name %)))
                      (map #(merge {:durability nil} (select-keys % [:count :durability]))) set))))))))

(defn hungry-world [inv food]
  (assoc-in (world inv {}) [:self :food] food))

(deftest with-food-false-no-food-child-runs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (hungry-world [] 5) {:items ["iron_pickaxe"] :food false :explore false})]
          (await (f/run-ticks s 60))
          (is (empty? (f/events-of s :kit.no-food)))
          (is (= {:status :stopped :item "iron_ingot" :reason :no-source}
                 (select-keys @(:out s) [:status :item :reason]))))))))

(deftest pure-crafting-from-carried-ingots-needs-no-food
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world [{:name "iron_ingot" :count 3} {:name "stick" :count 2}] {})
                       {:items ["iron_pickaxe"] :food false})]
          (await (f/run-ticks s 60))
          (is (= 1 (get (f/inv s) "iron_pickaxe")))
          (is (= :done (:status @(:out s)))))))))

(deftest a-body-with-no-food-to-be-found-stops-before-the-mining
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (hungry-world [] 0) {:items ["iron_pickaxe"] :explore false})]
          (await (f/run-ticks s 80))
          (is (= 1 (count (f/events-of s :kit.no-food))) "asked once, not again")
          (is (= {:status :stopped :item "iron_pickaxe" :reason :no-food}
                 (select-keys @(:out s) [:status :item :reason]))))))))

(deftest an-unarmed-body-makes-a-sword-before-the-mining
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (-> (world [{:name "cobblestone" :count 2} {:name "stick" :count 3} {:name "bread" :count 12}] {})
                    (update :recipes assoc "stone_sword" {:count 1 :needs {"cobblestone" 2 "stick" 1} :table true}))
              s (start w {:items ["iron_pickaxe"] :explore false})]
          (await (f/run-ticks s 120))
          (is (= #{"stone_sword"} (set (crafted s))))
          (is (= {:status :stopped :item "iron_ingot" :reason :no-source}
                 (select-keys @(:out s) [:status :item :reason]))))))))

(deftest with-a-food-stock-it-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world [{:name "iron_ingot" :count 3} {:name "stick" :count 2} {:name "bread" :count 4}] {})
                       {:items ["iron_pickaxe"]})]
          (await (f/run-ticks s 60))
          (is (= 1 (get (f/inv s) "iron_pickaxe")))
          (is (= :done (:status @(:out s)))))))))

(deftest wear-false-leaves-a-carried-piece-in-the-pack
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world [{:name "iron_boots" :count 1}] {}) {:items ["iron_boots"] :wear false})]
          (await (f/run-ticks s 20))
          (is (empty? (worn s)))
          (is (= {:status :done :have ["iron_boots"]} (select-keys @(:out s) [:status :have]))))))))

(deftest a-better-piece-worn-in-the-slot-is-skipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world [] {:head {:name "diamond_helmet" :count 1}}) {:items ["iron_helmet"]})]
          (await (f/run-ticks s 20))
          (is (empty? (crafted s)))
          (is (= {:status :done :have ["iron_helmet"]} (select-keys @(:out s) [:status :have]))))))))

(deftest a-death-restart-forgets-the-children-and-the-bill
  (is (= {:since 500 :other 1}
         (kit/restarted {:since 100 :children {:a 1} :bill {"iron_pickaxe" 1} :bill-done true :fed true :other 1} 500))))

(deftest a-wear-child-that-yields-yields-and-one-that-wore-nothing-stops
  (is (= :continue (kit/wear-outcome :continue)))
  (is (nil? (kit/wear-outcome (contract/ending :done nil {:worn ["iron_boots"]}))))
  (is (= {:kind :nothing-possible :reason :wear-failed}
         (select-keys (kit/wear-outcome (contract/ending :nothing-possible :failed {})) [:kind :reason])))
  (is (= {:kind :nothing-possible :reason :wear-failed}
         (select-keys (kit/wear-outcome (contract/ending :condition-failed :no-item nil)) [:kind :reason]))))

(deftest empty-items-are-impossible-bad-args
  (is (= [:never :bad-args] (let [[k _ d] ((:can-start? (get registry/jobs job)) {:args {:items []}})] [k (:reason d)]))))

(deftest a-fed-body-with-no-food-and-an-animal-near-stocks-up-before-the-work
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (assoc (hungry-world [] 20) :entities (for [[id x] [[7 5] [8 6] [9 7]]]
                                                                 {:id id :name "cow" :kind "passive" :pos {:x x :y 64 :z 0} :health 20
                                                                  :drops [{:name "beef" :count 1}]}))
                       {:items ["iron_pickaxe"] :explore false})]
          (let [timer (js/setInterval #(swap! (:clock s) + 500) 5)]
            (await (f/run-ticks s 80))
            (js/clearInterval timer))
          (is (pos? (get (f/inv s) "beef" 0)))
          (is (= {:status :stopped :item "iron_ingot" :reason :no-source}
                 (select-keys @(:out s) [:status :item :reason]))))))))

(deftest a-near-full-bar-with-no-food-source-warns-and-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (hungry-world [] 16) {:items ["iron_pickaxe"] :explore false})]
          (await (f/run-ticks s 80))
          (is (= [:no-carry] (mapv :cause (f/events-of s :kit.no-food))))
          (is (= {:status :stopped :item "iron_ingot" :reason :no-source}
                 (select-keys @(:out s) [:status :item :reason]))))))))

(deftest a-low-bar-with-no-food-source-stops-naming-no-carry
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (hungry-world [] 8) {:items ["iron_pickaxe"] :explore false})]
          (await (f/run-ticks s 80))
          (is (= [:no-carry] (mapv :cause (f/events-of s :kit.no-food))))
          (is (= {:status :stopped :item "iron_pickaxe" :reason :no-food}
                 (select-keys @(:out s) [:status :item :reason]))))))))

(defn raw-world
  "Nothing carried; a furnace at the camp (the crafting table is made), three oak trees on grass, then stone, coal and iron ore beyond the own zone, on an andesite floor."
  []
  (let [trees (for [x [-4 0 4]] (merge (into {} (for [y [64 65 66]] [(str x "," y ",5") "oak_log"]))
                                       {(str x ",67,5") "oak_leaves" (str x ",63,5") "grass_block"}))]
    (-> (assoc (world [] {}) :floor [-30 -10 40 30])
        (assoc :blocks (merge (apply merge trees)
                              (into {} (for [x (range -2 4) z [12 13] y [63 64]] [(str x "," y "," z) "stone"]))
                              {"2,64,15" "coal_ore" "2,63,15" "stone"}
                              (into {} (for [x [-1 0 1]] [(str x ",64,17") "iron_ore"]))
                              (into {} (for [x [-1 0 1]] [(str x ",63,17") "stone"]))))
        (assoc :drops {"oak_log" "oak_log" "stone" "cobblestone" "coal_ore" "coal" "iron_ore" "raw_iron"})
        (assoc-in [:blocks "2,64,2"] "furnace"))))

(deftest an-iron-pickaxe-is-got-from-an-empty-pack-wood-stone-coal-and-iron
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (assoc (raw-world) :inventory [{:name "bread" :count 12}]) {:items ["iron_pickaxe"] :food false :wear false} (assoc f/sight-opts :radius 48))]
          (loop [i 0]
            (await (f/run-ticks s 1))
            (.advance (.-world (:p s)) 40)
            (when (and (< i 3000) (= :not-done @(:out s)))
              (recur (inc i))))
          (is (= {:status :done :made ["iron_pickaxe"]} (select-keys @(:out s) [:status :made]))
              (pr-str (select-keys @(:out s) [:status :item :reason :why])))
          (is (= 1 (get (f/inv s) "iron_pickaxe"))))))))
