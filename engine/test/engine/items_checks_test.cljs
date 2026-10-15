(ns engine.items-checks-test
  "The checks of the items, storage, food and memory jobs: can-start? and can-run? statuses, no old conditions."
  (:require [cljs.test :refer [deftest is are]]
            [engine.contract :as contract]
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
  "A view whose job j holds memory m."
  [m]
  {:now 1000 :data {:entries {(mem/job-kind "j") [{:t 1000 :data m}]}}})

(defn ctx-of [sym world args & [m]]
  {:primitives (tu/fake-on-floor world) :args args :view (fn [] (view-with (or m {}))) :root "j" :slots []
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

(deftest a-known-chest-passes
  (is (= :ok (first (verdict-of :can-start? 'jobs.storage.deposit {} {:chest {:x 3 :y 64 :z 0}})))))

(deftest a-smelt-waits-cooking-with-the-count-it-loaded
  (let [owed {:owed {:count 4 :base 0} :ready-at 999999 :unlit-from 999999}
        status (verdict-of :can-run? 'jobs.items.smelt {} {:item "beef" :furnace {:x 1 :y 64 :z 0}} owed)]
    (is (= [:later :cooking] (kind+reason status)))
    (is (= 4 (:loaded (nth status 2))))
    (is (= :ok (first (verdict-of :can-run? 'jobs.items.smelt {} {:item "beef" :furnace {:x 1 :y 64 :z 0}} {}))))))
