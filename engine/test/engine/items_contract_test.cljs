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
              'jobs.items.give {:player "Steve" :item "bread"} 'jobs.items.enchant {:item "iron_sword"}}]
    (doseq [sym jobs
            :let [job (get registry/jobs sym)]]
      (is (fn? (:can-start? job)) (str sym " has can-start?"))
      (is (vector? (:target job)) (str sym " has a target"))
      (is (fn? (:done? job)) (str sym " has done?"))
      (is (nil? (contract/quote-problem ((:quote-of job) {:primitives p :args (get args sym)}))) (str sym)))))

(defn ^:async ending-of
  "Run job with args as a ctx/call child of a recording parent in a fake world, at most 40 ticks: the child's
  {:kind :cause :data}."
  [world job args]
  (let [out (atom nil)
        parent {:round (fn ^:async recording-round [c]
                         (let [e (await (ctx/call c :kid job args))]
                           (if (= :continue e) :continue (do (reset! out (select-keys e [:kind :cause :data])) (ctx/end c :done nil)))))}
        eng (core/create {:primitives (tu/seeing-all (tu/fake-on-floor world)) :jobs (assoc registry/jobs 'recording-parent parent)
                          :dir (tu/tmp-dir)})]
    (core/submit! eng '(recording-parent) {})
    (await (tu/run-until-empty eng 40))
    @out))

(defn kc [e] {:kind (:kind e) :cause (contract/cause-key e)})
(defn stacks [& names] {:inventory (mapv (fn [n] {:name n :count 3}) names)})
(defn cow [id x] {:id id :uuid (str "u" id) :name "cow" :kind "passive" :pos {:x x :y 64 :z 0}})

(deftest equip-ends-done-or-with-the-fact-that-stopped-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:kind :done :data {:item "iron_pickaxe" :hand "main" :text "holding iron_pickaxe"}}
               (select-keys (await (ending-of (stacks "iron_pickaxe") 'jobs.items.equip {:item "iron_pickaxe"})) [:kind :data])))
        (is (= {:kind :done} (select-keys (await (ending-of (assoc (stacks "iron_pickaxe") :self {:held "iron_pickaxe"})
                                                             'jobs.items.equip {:item "iron_pickaxe"})) [:kind]))
            "held already")
        (is (= {:kind :impossible :cause :unknown-item}
               (kc (await (ending-of (stacks "dirt") 'jobs.items.equip {:item "iron_pickaxx"})))))
        (is (= {:kind :condition-failed :cause :no-item}
               (kc (await (ending-of (stacks "dirt") 'jobs.items.equip {:item "iron_pickaxe" :hand "main" :fetch false})))))))))

(deftest interact-ends-by-what-the-click-did
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (merge {:entities [(cow 7 2)]} (stacks "wheat"))]
          (is (= {:kind :done :data {:used true :id 7 :item "wheat" :text "used wheat on entity 7"}}
                 (select-keys (await (ending-of world 'jobs.items.interact {:id 7 :item "wheat"})) [:kind :data])))
          (is (= {:kind :condition-failed :cause :gone} (kc (await (ending-of world 'jobs.items.interact {:id 99})))))
          (is (= {:kind :condition-failed :cause :no-item}
                 (kc (await (ending-of world 'jobs.items.interact {:id 7 :item "lead"}))))))))))

(deftest interact-walks-to-where-an-entity-dropped-out-of-sight
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (merge {:entities [(cow 7 6)]} (stacks "wheat"))
              reads (atom 0)
              wrap (fn [p] (let [raw (.-entities p)]
                             (aset p "entities" (fn [a] (if (and (= 64 (.-radius a)) (= 3 (swap! reads inc))) #js [] (raw a))))
                             p))
              out (atom nil)
              parent {:round (fn ^:async recording-round [c]
                               (let [e (await (ctx/call c :kid 'jobs.items.interact {:id 7 :item "wheat"}))]
                                 (if (= :continue e) :continue (do (reset! out (select-keys e [:kind :data])) (ctx/end c :done nil)))))}
              eng (core/create {:primitives (wrap (tu/seeing-all (tu/fake-on-floor world))) :jobs (assoc registry/jobs 'recording-parent parent)
                                :dir (tu/tmp-dir)})]
          (core/submit! eng '(recording-parent) {})
          (await (tu/run-until-empty eng 40))
          (is (= {:kind :done :data {:used true :id 7 :item "wheat" :text "used wheat on entity 7"}} @out)))))))

(deftest give-ends-before-the-toss-with-the-fact
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:kind :condition-failed :cause :no-item}
               (kc (await (ending-of {:entities [{:id 5 :name "Steve" :kind "player" :pos {:x 3 :y 64 :z 0}}]}
                                     'jobs.items.give {:player "Steve" :item "bread"})))))
        (is (= {:kind :impossible :cause :bad-args}
               (kc (await (ending-of {} 'jobs.items.give {:item "bread"})))))))))

(deftest enchant-ends-with-the-fact-that-stopped-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:kind :condition-failed :cause :no-table}
               (kc (await (ending-of (assoc (stacks "iron_sword" "lapis_lazuli") :self {:experience {:level 30}}) 'jobs.items.enchant {:item "iron_sword"})))))
        (let [e (await (ending-of (assoc (stacks "iron_sword" "lapis_lazuli") :self {:experience {:level 30}}) 'jobs.items.enchant {:item "iron_sword"}))]
          (is (= {:enchanted false :item "iron_sword" :levels-spent 0 :lapis-spent 0 :text "enchant gave up: no-table"} (:data e))))))))

(defn ^:async waits?
  "Whether job sym with args, submitted as a root job in world, is still listed (waiting on a condition) after 12 ticks."
  [world sym args]
  (let [eng (core/create {:primitives (tu/seeing-all (tu/fake-on-floor world)) :jobs registry/jobs :dir (tu/tmp-dir)})]
    (core/submit! eng (list sym args) {})
    (await (tu/run-until-empty eng 12))
    (boolean (seq (:list (core/state eng))))))

(deftest item-facts-are-conditions-so-a-root-job-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [xp {:self {:experience {:level 30}}}]
          (is (true? (await (waits? (stacks "dirt") 'jobs.items.equip {:item "iron_pickaxe" :fetch false}))))
          (is (true? (await (waits? (merge xp (stacks "lapis_lazuli")) 'jobs.items.enchant {:item "iron_sword" :fetch false}))))
          (is (true? (await (waits? (merge xp (stacks "iron_sword")) 'jobs.items.enchant {:item "iron_sword" :fetch false}))))
          (is (true? (await (waits? (stacks "iron_sword" "lapis_lazuli") 'jobs.items.enchant {:item "iron_sword"}))) "no level at all")
          (is (true? (await (waits? {} 'jobs.items.give {:player "Steve" :item "bread"})))))))))

(deftest enchant-fetches-the-lapis-and-the-item-by-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (assoc (stacks "iron_sword") :self {:experience {:level 30}})]
          (is (= :need (:cause (kc (await (ending-of w 'jobs.items.enchant {:item "iron_sword"}))))) "no lapis: a fetch for it fails as :need")
          (is (= :need (:cause (kc (await (ending-of (assoc w :inventory [{:name "lapis_lazuli" :count 3}]) 'jobs.items.enchant {:item "iron_sword"}))))) "no item")
          (is (= :no-lapis (:cause (kc (await (ending-of w 'jobs.items.enchant {:item "iron_sword" :fetch false}))))))
          (is (= :no-item (:cause (kc (await (ending-of (assoc w :inventory [{:name "lapis_lazuli" :count 3}]) 'jobs.items.enchant {:item "iron_sword" :fetch false})))))))))))

(deftest wear-and-pick-up-loose-facts-are-conditions-so-a-root-job-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (false? (await (waits? (stacks "dirt") 'jobs.items.wear {:item "dirt"}))) "no armour piece is impossible, not waiting")
        (is (true? (await (waits? (stacks "dirt") 'jobs.items.wear {:item "iron_helmet"}))) "not carried")
        (is (true? (await (waits? {} 'jobs.items.pick-up-loose {}))) "no loose item in sight")))))
