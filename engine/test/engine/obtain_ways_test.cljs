(ns engine.obtain-ways-test
  "items.obtain's ways beyond the chest and the craft chain (:loose, :hunt, :crops, :search) and its :bill."
  (:require [cljs.test :refer [deftest is async are]]
            [engine.blocks-dig-test :as bd]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.game :as game]
            [engine.hostile-test :as h]
            [engine.expr :as expr]
            [engine.farm-grow-test :as grow]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.items.obtain :as obtain]
            [jobs.items.obtain.base :as base]
            [jobs.items.obtain.bill :as bill]
            [jobs.items.obtain.table :as table]
            [jobs.items.recipes :as recipes]))

(defn cow [id x] {:id id :name "cow" :kind "passive" :pos {:x x :y 64 :z 0} :drops [{:name "beef" :count 1}]})

(defn ^:async scenario [args world n]
  (let [s (h/setup world 50)]
    (core/submit! (:eng s) (list 'jobs.items.obtain args) {})
    (await (tu/run-ticks (:eng s) (:clock s) n 700))
    s))

(defn attacked [{:keys [p]}] (mapv #(.-id (.-args %)) (h/calls p "attack")))

(def herd {:inventory h/sword :entities [(cow 1 3) (cow 2 6) (cow 3 8) (cow 4 10)]})

(deftest raw-meat-is-a-hunt-need-and-cooked-meat-gathers-it
  (is (= {"beef" 2} (:gather (recipes/plan game/default-version {} "cooked_beef" 2 (merge (table/source-opts) {:gather? true :table? true})))))
  (is (= [:hunt :hunt] (map (comp table/need-way first table/gather-needs) [{"beef" 1} {"porkchop" 1}]))))

(deftest a-field-crop-is-a-crops-need-and-a-block-is-a-gather-need
  (are [item way] (= way (table/need-way (first (table/gather-needs {item 1}))))
    "wheat" :crops "potato" :crops "cobblestone" :gather "leather" :hunt))

(deftest a-gather-in-the-ways-brings-the-field-ways
  (are [ways wanted] (= wanted (base/way? {:quote-call {:ways ways}} :hunt))
    #{:gather} true #{:hunt} true #{:chest :craft} false))

(deftest hunt-way-hunts-raw-meat
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:item "beef" :count 1 :ways #{:hunt}} herd 40))]
          (is (seq (attacked s)) "a cow was attacked")
          (is (= 'jobs.combat.hunt (:via (first (filter #(= :obtain.gathering (:kind %)) @(:seen s)))))))))))

(defn ^:async gathered-via [world]
  (let [s (grow/setup world)
        _ (core/submit! (:eng s) (list 'jobs.items.obtain {:item "wheat" :count 1 :ways #{:crops}}) {})
        _ (await (tu/run-ticks (:eng s) (:clock s) 6 700))]
    (:via (first (filter #(= :obtain.gathering (:kind %)) @(:seen s))))))

(defn young-or-ripe [age] {:blocks {"3,63,0" "farmland" "3,64,0" "wheat"} :ages {"3,64,0" age}})

(deftest the-crops-way-grows-wheat-when-no-ripe-crop-is-in-sight-and-cuts-one-that-is
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= 'jobs.farm.grow (await (gathered-via {}))) "nothing seen")
        (is (= 'jobs.farm.grow (await (gathered-via (young-or-ripe 3)))) "a crop still growing")
        (is (= 'jobs.farm.harvest (await (gathered-via (young-or-ripe 7)))) "a ripe crop")))))

(deftest a-ripe-crop-far-from-the-body-is-harvested
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (grow/setup {:blocks {"25,63,0" "farmland" "25,64,0" "wheat"} :ages {"25,64,0" 7}})
              _ (core/submit! (:eng s) (list 'jobs.items.obtain {:item "wheat" :count 1 :ways #{:crops}}) {})
              _ (await (tu/run-ticks (:eng s) (:clock s) 40 700))]
          (is (not-any? #(= :no-crop-seen (get-in % [:data :cause :cause])) @(:seen s))))))))

(deftest without-the-hunt-way-no-animal-is-hunted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:item "beef" :count 1 :ways #{:chest :craft :loose}} herd 40))]
          (is (empty? (attacked s))))))))

(deftest the-loose-way-picks-up-what-lies-in-sight
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (bd/setup {:self {:pos {:x 0 :y 64 :z 0}}})]
          (swap! (fake/state p) fake/spawn [4 64 0] "beef" 1)
          (let [res (await (tu/child-outcome eng 'jobs.items.obtain {:item "beef" :count 1 :ways #{:loose}} 30))]
            (is (= :done (:status res)) (pr-str res))
            (is (= 1 (bd/carried p "beef")))))))))

(deftest a-bill-gets-each-item-and-names-the-one-it-stops-at
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (bd/setup {:self {:pos {:x 0 :y 64 :z 0}}})]
          (swap! (fake/state p) fake/spawn [4 64 0] "beef" 1)
          (let [res (await (tu/child-outcome eng 'jobs.items.obtain {:bill {"beef" 1 "diamond" 1} :ways #{:loose}} 60))]
            (is (= :stopped (:status res)))
            (is (= "diamond" (:item res)))
            (is (= 1 (bd/carried p "beef")) "the first item was got")))))))

(deftest a-bill-sets-its-reserve-once-not-every-round
  (let [store (atom [])
        writes (atom 0)
        c {:id "j1/bill" :primitives (tu/fake {}) :args {:bill {"iron_pickaxe" 1}}
           :view (fn [] {:data {:entries {:reserve @store (mem/job-kind "j1") [{:data {}}]}} :now 0})
           :remember (fn [_ data _] (swap! writes inc) (swap! store conj {:data data}))
           :forget (fn [_ pred] (swap! store #(vec (remove (comp pred :data) %))))}]
    (bill/reserve! c ["iron_pickaxe"])
    (let [first-writes @writes]
      (is (pos? first-writes))
      (bill/reserve! c ["iron_pickaxe"])
      (bill/reserve! c ["iron_pickaxe"])
      (is (= first-writes @writes) "same entries: nothing written again")
      (bill/reserve! c [])
      (is (empty? @store)))))

(deftest a-bill-and-an-item-together-are-bad-args
  (are [args] (some? (base/args-error args))
    {:bill {"beef" 1} :item "beef" :count 1}
    {:bill {} :count 1}
    {:bill {"beef" 0} :count 1}))

(deftest a-carried-bill-is-done
  (let [c {:primitives (tu/fake {:inventory [{:name "beef" :count 2}]}) :args {:bill {"beef" 2}} :view (fn [] {:data {} :now 0})}]
    (is (= :ok (first (obtain/done? c))))))

(deftest a-worn-piece-of-the-bill-counts-as-had
  (let [c {:primitives (tu/fake {:inventory [{:name "beef" :count 1}] :equipment {:head {:name "iron_helmet" :count 1}}})
           :args {:bill {"beef" 1 "iron_helmet" 1}} :view (fn [] {:data {} :now 0})}]
    (is (= :ok (first (obtain/done? c))))))

(deftest every-gather-need-passes-only-args-its-job-has
  (let [items (concat (keys table/mob-drops) (keys table/crop-blocks) (keys table/grass-seeds) table/stalks
                      (keys table/hand-dug) table/ore-drops table/fish-items
                      ["log" "oak_log" "oak_sapling" "cobblestone" "obsidian" "bone_meal" "water_bucket" "white_wool"])]
    (doseq [item items
            need (table/gather-needs {item 1})]
      (is (nil? (expr/problem registry/jobs (list (:job need) (:args need))))
          (str item " via " (:job need))))))
