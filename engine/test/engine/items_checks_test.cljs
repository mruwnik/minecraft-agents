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
    jobs.storage.deposit jobs.storage.haul jobs.storage.make-room jobs.storage.withdraw
    jobs.food.fish jobs.memory.forget-place jobs.memory.remember jobs.memory.set-place])

(deftest every-job-of-the-area-has-a-check
  (doseq [sym area
          :let [job (get registry/jobs sym)]]
    (is (some some? ((juxt :can-start? :can-run?) job)) (str sym " has a check"))))

(defn view-with
  "A view whose job j holds memory m, with more memory entries {kind [data]}."
  ([m] (view-with m {}))
  ([m more]
   {:now 1000 :data {:entries (into {(mem/job-kind "j") [{:t 1000 :data m}]}
                                    (map (fn [[k ds]] [k (mapv (fn [d] {:t 1000 :data d}) ds)]))
                                    more)
                     :policies (zipmap (keys more) (repeat mem/place-policy))}}))

(defn ctx-of [sym world args & [m more]]
  {:primitives (tu/fake-on-floor world) :args args :view (fn [] (view-with (or m {}) more)) :root "j" :slots []
   :sym sym})

(defn verdict-of [export sym world args & [m]]
  (let [job (get registry/jobs sym)]
    (contract/verdict (ctx-of sym world args m) (select-keys job [export]))))

(defn kind+reason [[kind _ data]] [kind (:reason data)])
(defn kind+why [[kind _ data]] [kind (:why data)])

(deftest bad-args-are-never-at-the-start
  (are [sym args reason] (= [:never reason] (kind+reason (verdict-of :can-start? sym {} args)))
    'jobs.items.equip {} :bad-args
    'jobs.items.equip {:item "iron_pickaxx" :hand "main"} :unknown-item
    'jobs.items.craft {} :bad-args
    'jobs.items.interact {} :bad-args
    'jobs.items.get-kit {} :bad-args
    'jobs.items.smelt {:item "diamond"} :not-smeltable
    'jobs.memory.remember {} :bad-args))

(deftest remember-refuses-a-bad-or-reserved-kind-as-bad-args-with-the-why
  (are [args why] (= [:never why] (kind+why (verdict-of :can-start? 'jobs.memory.remember {} args)))
    {} :bad-kind
    {:kind :hurt} :reserved-kind))

(deftest a-missing-chest-or-item-is-later-at-the-start
  (are [sym world args reason] (= [:later reason] (kind+reason (verdict-of :can-start? sym world args)))
    'jobs.storage.deposit {} {} :no-chest
    'jobs.storage.withdraw {} {} :no-chest
    'jobs.storage.haul {} {:from [[1 64 0]]} :no-chest
    'jobs.items.equip {:inventory [{:name "dirt" :count 1}]} {:item "iron_pickaxe" :hand "main" :fetch false} :no-item
    'jobs.items.wear {:inventory [{:name "dirt" :count 1}]} {:item "iron_helmet"} :no-item))

(deftest a-started-withdraw-waits-when-its-chest-is-gone
  (is (= [:later :no-chest] (kind+reason (verdict-of :can-run? 'jobs.storage.withdraw {} {:items {"bread" 1}})))))

(deftest a-started-deposit-waits-when-its-chest-is-gone
  (is (= [:later :no-chest] (kind+reason (verdict-of :can-run? 'jobs.storage.deposit {} {})))))

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
   'jobs.storage.make-room {} 'jobs.storage.withdraw {:items {"bread" 1}}
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
    'jobs.items.obtain {:inventory [{:name "oak_log" :count 2}]} {:item "oak_log" :count 3} {} [:later :short]
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

(deftest wear-result-lists-the-pieces-worn-so-far
  (let [helmet {:inventory [{:name "iron_helmet" :count 1}]}
        put [{:item "iron_boots" :slot :feet}]]
    (are [world m status result] (= [status result] ((juxt first #(:result (nth % 2))) (done-of 'jobs.items.wear world {} m)))
      {} {} :ok {:worn []}
      {} {:worn put} :ok {:worn put}
      helmet {:worn put} :later {:worn put}
      helmet {} :later {:worn []})))

(deftest a-later-done-carries-a-result-map
  (doseq [sym '[jobs.items.enchant jobs.items.interact jobs.items.get-kit jobs.storage.haul
                jobs.items.pick-up-loose jobs.storage.withdraw jobs.memory.remember jobs.items.kitchen
                jobs.memory.set-place jobs.memory.forget-place]
          :let [s (done-of sym {} (done-args sym))]]
    (is (map? (:result (nth s 2))) (str sym " done? carries a :result"))))

(def kit-world {:inventory [{:name "iron_pickaxe" :count 1}]})
(def kit-args {:items ["iron_pickaxe"]})
(def cell [1 64 0])

(deftest a-whole-goal-that-can-be-read-from-state-is-ok
  (are [sym world args m reason] (= reason (tu/kind-reason (done-of sym world args m)))
    'jobs.items.enchant {} {:item "iron_sword"} {:enchanted true} [:ok :enchanted]
    'jobs.items.enchant {} {:item "iron_sword"} {} [:later :enchanting]
    'jobs.items.interact {} {:id 7} {:used true} [:ok :used]
    'jobs.items.interact {} {:id 7} {} [:later :not-used]
    'jobs.items.get-kit kit-world kit-args {} [:ok :kitted]
    'jobs.items.get-kit kit-world kit-args {:children {:obtain {}}} [:later :wrapping-up]
    'jobs.items.get-kit {} kit-args {} [:later :kitting]
    'jobs.items.get-kit (assoc kit-world :inventory [{:name "iron_pickaxe" :count 1} {:name "iron_helmet" :count 1}])
    {:items ["iron_pickaxe" "iron_helmet"] :wear true} {} [:later :kitting]
    'jobs.storage.haul {} {:from [cell]} {:empty [cell]} [:ok :hauled]
    'jobs.storage.haul {} {:from [cell]} {} [:later :hauling]
    'jobs.storage.haul {:inventory [{:name "oak_log" :count 5}]} {:from [cell]} {:empty [cell] :hauled {"oak_log" 5}} [:later :hauling]
    'jobs.storage.haul {} {:from [cell]} {:empty [cell] :children {:deposit {}}} [:later :wrapping-up]))

(deftest give-is-ok-once-the-toss-is-confirmed-taken
  (are [m reason] (= reason (tu/kind-reason (done-of 'jobs.items.give {} {:player "Steve" :item "bread" :count 2} m)))
    {:tossed 2 :had 2 :confirmed true} [:ok :given]
    {:tossed 2 :had 2} [:later :giving]
    {:tossed 2 :had 2 :confirmed true :collecting true} [:later :giving]
    {:tossed 1 :had 2 :confirmed true} [:later :giving]
    {:tossed 2 :had 2 :confirmed true :back 1} [:later :giving]))

(def full-bag {:inventory (vec (repeat 36 {:name "dirt" :count 64}))})
(def bread {:inventory [{:name "bread" :count 1}]})
(def home-cell {:x 3 :y 64 :z 5})

(deftest the-shape-only-jobs-are-ok-when-the-goal-holds
  (are [sym world args m more reason] (= reason (tu/kind-reason (done-of sym world args m more)))
    'jobs.storage.make-room {} {} {} {} [:ok :room]
    'jobs.storage.make-room {} {} {:acted true} {} [:ok :room]
    'jobs.storage.make-room full-bag {} {} {} [:later :short]
    'jobs.food.fish {:inventory [{:name "cod" :count 2}]} {:count 2} {} {} [:ok :caught]
    'jobs.food.fish {:inventory [{:name "cod" :count 1}]} {:count 2} {:base 0} {} [:later :fishing]
    'jobs.storage.withdraw bread {:items {"bread" 1}} {} {} [:ok :carried]
    'jobs.storage.withdraw {} {:items {"bread" 1}} {} {} [:later :short]
    'jobs.memory.set-place {} {:name :home :pos [3 64 5]} {} {:home [{:pos home-cell}]} [:ok :set]
    'jobs.memory.set-place {} {:name :home :pos [3 64 5]} {} {} [:later :not-set]
    'jobs.memory.forget-place {} {:name :home} {} {} [:ok :forgotten]
    'jobs.memory.forget-place {} {:name :home} {} {:home [{:pos home-cell}]} [:later :recorded]))
