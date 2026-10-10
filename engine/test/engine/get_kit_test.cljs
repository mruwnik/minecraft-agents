(ns engine.get-kit-test
  "jobs.items.get-kit: a list of items got in order and the armour worn, against the fake world."
  (:require [cljs.test :refer [deftest is async]]
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
  "fetch-test's perception engine, with a recording parent that runs the job as a child and keeps its result in :out."
  [w args]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        raw (tu/fake-on-floor (assoc w :floor-block "andesite"))
        p (perception/wrap raw (perception/create (fake-raw/create raw) (assoc f/sight-opts :now #(deref clock))))
        out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid job args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'recording-parent parent) :triggers triggers/all
                          :dir (tu/tmp-dir) :now #(deref clock) :world (ew/of-data {} {} [f/own-zone])
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (perception/pass! (aget p "perception"))
    (core/submit! eng '(recording-parent) {})
    {:eng eng :p p :seen seen :clock clock :out out}))

(defn worn [{:keys [p]}]
  (into {} (keep (fn [[k v]] (when (and v (#{"head" "torso" "legs" "feet"} k)) [k (get v "name")])))
        (js->clj (.-equipment (.self p)))))

(defn crafted [s] (mapv #(.-item (.-args %)) (f/calls s "craft")))

(deftest the-iron-kit-needs-32-ingots-from-the-furnace
  (is (= {"iron_ingot" 32} (kit/furnace-short game/default-version kit/iron-kit {}))))

(deftest carried-ingots-count-against-the-furnace-need
  (is (= {"iron_ingot" 1} (kit/furnace-short game/default-version ["iron_pickaxe" "iron_helmet" "iron_boots"] {"iron_ingot" 11}))))

(deftest nothing-is-short-when-the-ingots-are-carried
  (is (= {} (kit/furnace-short game/default-version ["iron_pickaxe"] {"iron_ingot" 3}))))

(deftest a-death-after-the-start-makes-the-memory-stale
  (is (kit/stale? {:since 100} {:t 200}))
  (is (not (kit/stale? {:since 300} {:t 200})))
  (is (not (kit/stale? {:since 300} nil))))

(deftest two-pieces-from-carried-ingots-are-made-and-the-helmet-worn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world [{:name "iron_ingot" :count 8} {:name "stick" :count 2}] {})
                       {:items ["iron_pickaxe" "iron_helmet"] :explore false :min-food 0})]
          (await (f/run-ticks s 120))
          (is (= 1 (get (f/inv s) "iron_pickaxe")))
          (is (= {"head" "iron_helmet"} (worn s)))
          (is (= {:status :done :made ["iron_pickaxe" "iron_helmet"]} (select-keys @(:out s) [:status :made])))
          (is (= ["iron_pickaxe" "iron_helmet"] (mapv :item (f/events-of s :kit.progress)))))))))

(deftest ingots-that-cannot-be-got-stop-the-job-at-once-naming-the-ingots
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world [{:name "iron_ingot" :count 8} {:name "stick" :count 2}] {})
                       {:items ["iron_pickaxe" "iron_helmet" "iron_boots"] :explore false :min-food 0})]
          (await (f/run-ticks s 120))
          (is (not-any? #{"iron_pickaxe" "iron_helmet" "iron_boots"} (crafted s)))
          (is (= {:status :stopped :item "iron_ingot" :reason :no-source}
                 (select-keys @(:out s) [:status :item :reason])))
          (is (re-find #"iron_ingot" (str (:why @(:out s))))))))))

(deftest a-worn-piece-is-skipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world [] {:head {:name "iron_helmet" :count 1}})
                       {:items ["iron_helmet" "iron_boots"] :explore false :min-food 0})]
          (await (f/run-ticks s 40))
          (is (empty? (crafted s)))
          (is (= {:status :stopped :item "iron_ingot" :reason :no-source :have ["iron_helmet"]}
                 (select-keys @(:out s) [:status :item :reason :have]))))))))

(deftest a-carried-piece-is-worn-and-the-job-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world [{:name "iron_pickaxe" :count 1} {:name "iron_boots" :count 1}] {})
                       {:items ["iron_pickaxe" "iron_boots"] :min-food 0})]
          (await (f/run-ticks s 20))
          (is (empty? (crafted s)))
          (is (= {"feet" "iron_boots"} (worn s)))
          (is (= {:status :done :have ["iron_pickaxe" "iron_boots"]} (select-keys @(:out s) [:status :have]))))))))

(defn hungry-world [inv food]
  (assoc-in (world inv {}) [:self :food] food))

(deftest with-work-ahead-and-food-off-it-stops-needing-food
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world [{:name "bread" :count 1}] {})
                       {:items ["iron_pickaxe"] :min-food 20 :food false})]
          (await (f/run-ticks s 20))
          (is (empty? (crafted s)))
          (is (= {:status :stopped :item "iron_pickaxe" :reason :needs-food :food 5 :min-food 20}
                 (select-keys @(:out s) [:status :item :reason :food :min-food]))))))))

(deftest pure-crafting-from-carried-ingots-needs-no-food
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world [{:name "iron_ingot" :count 3} {:name "stick" :count 2}] {})
                       {:items ["iron_pickaxe"] :min-food 20 :food false})]
          (await (f/run-ticks s 60))
          (is (= 1 (get (f/inv s) "iron_pickaxe")))
          (is (= :done (:status @(:out s)))))))))

(deftest a-hungry-body-with-no-food-to-be-found-stops-needing-food-with-the-childs-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (hungry-world [] 5) {:items ["iron_pickaxe"] :min-food 20 :explore false})]
          (await (f/run-ticks s 60))
          (is (= {:status :stopped :item "iron_pickaxe" :reason :needs-food :cause :no-food}
                 (select-keys @(:out s) [:status :item :reason :cause]))))))))

(deftest with-a-food-stock-it-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world [{:name "iron_ingot" :count 3} {:name "stick" :count 2} {:name "bread" :count 4}] {})
                       {:items ["iron_pickaxe"] :min-food 20})]
          (await (f/run-ticks s 60))
          (is (= 1 (get (f/inv s) "iron_pickaxe")))
          (is (= :done (:status @(:out s)))))))))

(deftest wear-false-leaves-a-carried-piece-in-the-pack
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world [{:name "iron_boots" :count 1}] {}) {:items ["iron_boots"] :wear false :min-food 0})]
          (await (f/run-ticks s 20))
          (is (empty? (worn s)))
          (is (= {:status :done :have ["iron_boots"]} (select-keys @(:out s) [:status :have]))))))))

(deftest a-better-piece-worn-in-the-slot-is-skipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (world [] {:head {:name "diamond_helmet" :count 1}}) {:items ["iron_helmet"] :min-food 0})]
          (await (f/run-ticks s 20))
          (is (empty? (crafted s)))
          (is (= {:status :done :have ["iron_helmet"]} (select-keys @(:out s) [:status :have]))))))))

(deftest a-death-restart-forgets-the-children-and-what-was-made
  (is (= {:since 500 :other 1}
         (kit/restarted {:since 100 :children {:a 1} :materials #{"x"} :made ["iron_pickaxe"] :fed true :other 1} 500))))

(deftest a-wear-child-that-yields-yields-and-one-that-wore-nothing-stops
  (is (= :continue (kit/wear-outcome :continue nil nil)))
  (is (nil? (kit/wear-outcome :done {:worn ["iron_boots"]} nil)))
  (is (= {:reason :wear-failed :why "failed"} (kit/wear-outcome :done {:reason "failed"} nil)))
  (is (= {:reason :wear-failed :why :not-ready} (kit/wear-outcome :declined nil :not-ready))))

(deftest a-bad-args-check-waits
  (let [waited (atom nil)]
    (kit/check {:args {:items []} :wait waited})
    (is (= :bad-args (:reason @waited)))))

(deftest a-fed-body-with-no-food-and-an-animal-near-stocks-up-before-the-work
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (assoc (hungry-world [] 20) :entities [{:id 7 :name "cow" :kind "passive" :pos {:x 5 :y 64 :z 0} :health 20
                                                                :drops [{:name "beef" :count 1}]}])
                       {:items ["iron_pickaxe"] :min-food 3 :explore false})]
          (let [timer (js/setInterval #(swap! (:clock s) + 500) 5)]
            (await (f/run-ticks s 80))
            (js/clearInterval timer))
          (is (pos? (get (f/inv s) "beef" 0)))
          (is (= {:status :stopped :item "iron_ingot" :reason :no-source}
                 (select-keys @(:out s) [:status :item :reason]))))))))

(deftest a-fed-body-with-no-food-source-stops-needing-food-naming-no-stock
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (hungry-world [] 20) {:items ["iron_pickaxe"] :min-food 20 :explore false})]
          (await (f/run-ticks s 80))
          (is (= {:status :stopped :item "iron_pickaxe" :reason :needs-food :cause :no-stock}
                 (select-keys @(:out s) [:status :item :reason :cause]))))))))

(deftest carried-raw-iron-is-smelted-and-the-pieces-it-covers-are-made-when-the-materials-fail
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (assoc-in (world [{:name "raw_iron" :count 3} {:name "coal" :count 1} {:name "stick" :count 2}] {})
                                 [:blocks "-1,64,2"] "furnace")
                       {:items ["iron_pickaxe" "iron_helmet"] :explore false :min-food 0})]
          (loop [i 0]
            (await (f/run-ticks s 20))
            (.advance (.-world (:p s)) 2000)
            (when (and (< i 30) (= :not-done @(:out s)))
              (recur (inc i))))
          (is (= 1 (get (f/inv s) "iron_pickaxe")))
          (is (= {:status :stopped :item "iron_helmet" :made ["iron_pickaxe"]}
                 (select-keys @(:out s) [:status :item :made]))))))))

(deftest a-partial-kit-is-forgotten-by-a-death-restart
  (is (= {:since 500} (kit/restarted {:since 100 :partial true} 500))))
