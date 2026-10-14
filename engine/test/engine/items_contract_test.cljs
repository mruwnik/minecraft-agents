(ns engine.items-contract-test
  "jobs.items.equip, interact, give and enchant as contract jobs: they export the contract and end with a kind and a
  cause (a failed fact is condition-failed, a failed try nothing-possible; 0 of N is never done)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.registry :as registry]
            [engine.test-util :as tu]))

(def jobs '[jobs.items.equip jobs.items.interact jobs.items.give jobs.items.enchant])

(deftest every-item-job-exports-the-contract-and-quotes-validly
  (let [p (tu/fake {})
        args {'jobs.items.equip {:item "stone_pickaxe" :hand "main"} 'jobs.items.interact {:id 7}
              'jobs.items.give {:player "Steve" :item "bread" :wait-s 6} 'jobs.items.enchant {:item "iron_sword"}}]
    (doseq [sym jobs
            :let [job (get registry/jobs sym)]]
      (is (vector? (:conditions job)) (str sym " has conditions"))
      (is (nil? (:check job)) (str sym " has no old check"))
      (is (vector? (:target job)) (str sym " has a target"))
      (is (fn? (:done? job)) (str sym " has done?"))
      (is (nil? (contract/quote-problem ((:quote-of job) {:primitives p :args (get args sym)}))) (str sym)))))

(defn ^:async ending-of
  "Run job with args as a ctx/call child of a recording parent in a fake world, at most 40 ticks: the child's
  {:kind :cause :data}."
  [world job args]
  (let [out (atom nil)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [e (await (ctx/call c :kid job args))]
                           (if (= :continue e) :continue (do (reset! out (select-keys e [:kind :cause :data])) :done))))}
        eng (core/create {:primitives (tu/seeing-all (tu/fake-on-floor world)) :jobs (assoc registry/jobs 'recording-parent parent)
                          :dir (tu/tmp-dir)})]
    (core/submit! eng '(recording-parent) {})
    (await (tu/run-until-empty eng 40))
    @out))

(defn kc [e] (select-keys e [:kind :cause]))
(defn stacks [& names] {:inventory (mapv (fn [n] {:name n :count 3}) names)})
(defn cow [id x] {:id id :uuid (str "u" id) :name "cow" :kind "passive" :pos {:x x :y 64 :z 0}})

(deftest equip-ends-done-or-with-the-fact-that-stopped-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:kind :done :data {:item "iron_pickaxe" :hand "main"}}
               (select-keys (await (ending-of (stacks "iron_pickaxe") 'jobs.items.equip {:item "iron_pickaxe"})) [:kind :data])))
        (is (= {:kind :done} (select-keys (await (ending-of (assoc (stacks "iron_pickaxe") :self {:held "iron_pickaxe"})
                                                             'jobs.items.equip {:item "iron_pickaxe"})) [:kind]))
            "held already")
        (is (= {:kind :condition-failed :cause :unknown-item}
               (kc (await (ending-of (stacks "dirt") 'jobs.items.equip {:item "iron_pickaxx"})))))
        (is (= {:kind :condition-failed :cause :no-item}
               (kc (await (ending-of (stacks "dirt") 'jobs.items.equip {:item "iron_pickaxe" :fetch false})))))))))

(deftest interact-ends-by-what-the-click-did
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (merge {:entities [(cow 7 2)]} (stacks "wheat"))]
          (is (= {:kind :done :data {:used true :id 7 :item "wheat"}}
                 (select-keys (await (ending-of world 'jobs.items.interact {:id 7 :item "wheat"})) [:kind :data])))
          (is (= {:kind :condition-failed :cause :gone} (kc (await (ending-of world 'jobs.items.interact {:id 99})))))
          (is (= {:kind :condition-failed :cause :no-item}
                 (kc (await (ending-of world 'jobs.items.interact {:id 7 :item "lead"}))))))))))

(deftest give-ends-before-the-toss-with-the-fact
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:kind :condition-failed :cause :no-item}
               (kc (await (ending-of {:entities [{:id 5 :name "Steve" :kind "player" :pos {:x 3 :y 64 :z 0}}]}
                                     'jobs.items.give {:player "Steve" :item "bread"})))))
        (is (= {:kind :condition-failed :cause {:reason :bad-args :why "needs a player name and an item name"}}
               (kc (await (ending-of {} 'jobs.items.give {:item "bread"})))))))))

(deftest enchant-ends-with-the-fact-that-stopped-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:kind :condition-failed :cause :no-table}
               (kc (await (ending-of (stacks "iron_sword") 'jobs.items.enchant {:item "iron_sword"})))))
        (let [e (await (ending-of (stacks "iron_sword") 'jobs.items.enchant {:item "iron_sword"}))]
          (is (= {:enchanted false :item "iron_sword" :levels-spent 0 :lapis-spent 0} (:data e))))))))
