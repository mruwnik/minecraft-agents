(ns engine.items-checks-test
  "The checks of the items, storage, food and memory jobs: can-start?, can-run? and done? statuses, no old conditions."
  (:require [cljs.test :refer [deftest is are]]
            [engine.contract :as contract]
            [engine.expr :as expr]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]))

(def area
  '[jobs.items.craft jobs.items.enchant jobs.items.equip jobs.items.get-kit jobs.items.get-tool jobs.items.interact
    jobs.items.kitchen jobs.items.obtain jobs.items.pick-up-loose jobs.items.smelt jobs.items.wear
    jobs.storage.deposit jobs.storage.haul jobs.storage.kit jobs.storage.make-room jobs.storage.withdraw
    jobs.food.fish jobs.memory.forget-place jobs.memory.remember jobs.memory.set-place])

(deftest no-job-of-the-area-keeps-the-old-conditions
  (doseq [sym area
          :let [job (get registry/jobs sym)]]
    (is (nil? (:conditions job)) (str sym " has no conditions"))
    (is (some some? ((juxt :can-start? :can-run?) job)) (str sym " has a check"))))

(defn view-with
  "A view whose job j holds memory m, with more memory entries {kind [data]}."
  ([m] (view-with m {}))
  ([m more]
   {:now 1000 :data {:entries (into {(mem/job-kind "j") [{:t 1000 :data m}]}
                                    (map (fn [[k ds]] [k (mapv (fn [d] {:t 1000 :data d}) ds)]))
                                    more)}}))

(defn ctx-of [sym world args & [m more]]
  {:primitives (tu/fake-on-floor world) :args args :view (fn [] (view-with (or m {}) more)) :root "j" :slots []
   :sym sym})

(defn verdict-of [export sym world args & [m]]
  (let [job (get registry/jobs sym)]
    (contract/verdict (ctx-of sym world args m) (select-keys job [export]))))

(defn kind+reason [[kind _ data]] [kind (:reason data)])

(deftest bad-args-are-never-at-the-start
  (are [sym args reason] (= [:never reason] (kind+reason (verdict-of :can-start? sym {} args)))
    'jobs.items.equip {} :bad-args
    'jobs.items.equip {:item "iron_pickaxx" :hand "main"} :unknown-item
    'jobs.items.craft {} :bad-args
    'jobs.items.interact {} :bad-args
    'jobs.items.get-kit {} :bad-args
    'jobs.items.smelt {:item "diamond"} :not-smeltable
    'jobs.memory.remember {} :bad-kind
    'jobs.memory.remember {:kind :hurt} :reserved-kind))

(deftest a-missing-chest-or-item-is-later-at-the-start
  (are [sym world args reason] (= [:later reason] (kind+reason (verdict-of :can-start? sym world args)))
    'jobs.storage.deposit {} {} :no-chest
    'jobs.storage.withdraw {} {} :no-chest
    'jobs.storage.haul {} {:from [[1 64 0]]} :no-chest
    'jobs.items.equip {:inventory [{:name "dirt" :count 1}]} {:item "iron_pickaxe" :hand "main" :fetch false} :no-item
    'jobs.items.wear {:inventory [{:name "dirt" :count 1}]} {:item "iron_helmet"} :no-item))

(deftest a-started-withdraw-waits-when-its-chest-is-gone
  (is (= [:later :no-chest] (kind+reason (verdict-of :can-run? 'jobs.storage.withdraw {} {:items {"bread" 1}})))))

(deftest a-known-chest-passes
  (is (= :ok (first (verdict-of :can-start? 'jobs.storage.deposit {} {:chest {:x 3 :y 64 :z 0}})))))

(deftest a-smelt-waits-cooking-with-the-count-it-loaded
  (let [owed {:owed {:count 4 :base 0} :ready-at 999999 :unlit-from 999999}
        status (verdict-of :can-run? 'jobs.items.smelt {} {:item "beef" :furnace {:x 1 :y 64 :z 0}} owed)]
    (is (= [:later :cooking] (kind+reason status)))
    (is (= 4 (:loaded (nth status 2))))
    (is (= :ok (first (verdict-of :can-run? 'jobs.items.smelt {} {:item "beef" :furnace {:x 1 :y 64 :z 0}} {}))))))

(def done-args
  {'jobs.items.craft {:item "oak_planks" :count 4} 'jobs.items.enchant {:item "iron_sword"}
   'jobs.items.equip {:item "stone_pickaxe"} 'jobs.items.get-kit {:items ["iron_pickaxe"]}
   'jobs.items.get-tool {:kind "pickaxe"} 'jobs.items.interact {:id 7} 'jobs.items.give {:player "Steve" :item "bread"}
   'jobs.items.kitchen {:chest {:x 10 :y 64 :z 0}} 'jobs.items.obtain {:item "oak_log"} 'jobs.items.pick-up-loose {}
   'jobs.items.smelt {:item "beef"} 'jobs.items.wear {} 'jobs.storage.deposit {} 'jobs.storage.haul {:from [[1 64 0]]}
   'jobs.storage.kit {:tools ["hoe"]} 'jobs.storage.make-room {} 'jobs.storage.withdraw {:items {"bread" 1}}
   'jobs.food.fish {} 'jobs.memory.forget-place {:name "home"} 'jobs.memory.remember {:kind :note}
   'jobs.memory.set-place {:name "home"}})

(defn done-of
  "What sym's done? says in world with args (over its defaults), its memory m and more memory entries."
  [sym world args & [m more]]
  ((:done? (get registry/jobs sym)) (ctx-of sym world (second (expr/leaf registry/jobs sym args)) m more)))

(deftest every-done-of-the-area-is-a-status
  (doseq [[sym args] done-args
          :let [s (done-of sym {} args)]]
    (is (contract/status? s) (str sym " done? gives a status"))
    (is (not (::contract/old (nth s 2))) (str sym " done? is no old map"))
    (is (some? (:units (nth s 2))) (str sym " done? counts its units"))))

(def pickaxe {:inventory [{:name "wooden_pickaxe" :count 1}]})

(deftest a-goal-that-holds-while-a-child-is-under-way-is-not-done
  (are [sym world args m reason] (= reason (tu/kind-reason (done-of sym world args m)))
    'jobs.items.get-tool pickaxe {:kind "pickaxe"} {} [:ok :carried]
    'jobs.items.get-tool pickaxe {:kind "pickaxe"} {:children {:obtain {}}} [:later :wrapping-up]
    'jobs.items.get-tool {} {:kind "pickaxe"} {} [:later :no-tool]
    'jobs.items.obtain {:inventory [{:name "oak_log" :count 2}]} {:item "oak_log"} {:start {:have 1 :target 2}} [:ok :carried]
    'jobs.items.obtain {:inventory [{:name "oak_log" :count 2}]} {:item "oak_log"} {:start {:have 1 :target 2} :children {:craft {}}} [:later :wrapping-up]
    'jobs.items.obtain {:inventory [{:name "oak_log" :count 2}]} {:item "oak_log"} {:start {:have 1 :target 2} :gather {:tree {}}} [:later :wrapping-up]
    'jobs.items.obtain {:inventory [{:name "oak_log" :count 2}]} {:item "oak_log"} {} [:later :short]
    'jobs.items.equip {:self {:held "iron_pickaxe"}} {:item "iron_pickaxe"} {} [:ok :held]
    'jobs.items.equip {:self {:held "iron_pickaxe"}} {:item "iron_pickaxe"} {:children {:fetch {}}} [:later :wrapping-up]
    'jobs.items.equip {} {:item "iron_pickaxe"} {} [:later :not-held]))

(deftest a-done-goal-carries-the-result-the-job-ends-with
  (are [sym world args m result] (= result (:result (nth (done-of sym world args m) 2)))
    'jobs.items.get-tool pickaxe {:kind "pickaxe"} {} {:tool "wooden_pickaxe"}
    'jobs.items.get-tool {} {:block "dirt"} {} {:needed false}
    'jobs.items.obtain {:inventory [{:name "oak_log" :count 2}]} {:item "oak_log"} {:start {:have 1 :target 2}} {:got 1 :item "oak_log"}
    'jobs.items.equip {:self {:held "iron_pickaxe"}} {:item "iron_pickaxe"} {} {:item "iron_pickaxe" :hand "main"}))

(deftest a-kitchen-is-done-when-the-chest-is-stocked-and-no-pass-is-under-way
  (let [stocked {:fetch/stock [{:pos [10 64 0] :items {"bread" 20}}]}
        short {:fetch/stock [{:pos [10 64 0] :items {"bread" 2}}]}
        args {:chest {:x 10 :y 64 :z 0} :stock {"bread" 16}}]
    (are [m more reason] (= reason (tu/kind-reason (done-of 'jobs.items.kitchen {} args m more)))
      {} stocked [:ok :stocked]
      {:todo [:deposit]} stocked [:later :pass-under-way]
      {} short [:later :short]
      {} {} [:later :short])))

(deftest wear-is-done-when-nothing-better-is-carried
  (are [world reason] (= reason (tu/kind-reason (done-of 'jobs.items.wear world {})))
    {:inventory [{:name "iron_helmet" :count 1}] :equipment {:head {:name "diamond_helmet" :count 1}}} [:ok :worn]
    {:inventory [{:name "iron_helmet" :count 1}]} [:later :to-wear]
    {:inventory [{:name "dirt" :count 1}]} [:ok :worn]))
