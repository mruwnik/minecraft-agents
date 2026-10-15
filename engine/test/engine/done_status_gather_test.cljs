(ns engine.done-status-gather-test
  "done? of the gather, farm, forestry and strip jobs answers a status (docs/contract-conditions-design.md 2.2)."
  (:require [cljs.test :refer [deftest is are]]
            [engine.contract :as contract]
            [engine.expr :as expr]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.lib.world-files :as world]))

(defn ctx-of
  "A ctx of job sym in a seen world, args over its defaults, its memory m."
  [sym world args m]
  {:primitives (tu/seeing-all (tu/fake world)) :args (second (expr/leaf registry/jobs sym args)) :root "j" :slots []
   :id "j" :engine {:warned (atom #{})} :emit (fn [& _]) :sym sym :view (fn [] {:now 1000 :data {:entries {(mem/job-kind "j") [{:t 1000 :data m}]}}})})

(defn done-of [sym world args & [m]]
  ((:done? (get registry/jobs sym)) (ctx-of sym world args (or m {}))))

(defn done-in
  "done-of with a world of plans {id plan} and no zones."
  [sym world args m plans]
  ((:done? (get registry/jobs sym)) (assoc-in (ctx-of sym world args m) [:engine :world] (world/of-data plans {} []))))

(def fresh-args
  {'jobs.gather.get-seeds {:count 2}
   'jobs.gather.make-obsidian {}
   'jobs.gather.mine {:block "stone" :count 2}
   'jobs.farm.till {:center [0 63 0] :radius 0}
   'jobs.farm.tidy {:plan "p"}
   'jobs.farm.harvest {}
   'jobs.farm.fertilize {}
   'jobs.farm.compost {}
   'jobs.farm.find-spot {}
   'jobs.farm.tend {:plan "p"}
   'jobs.forestry.collect-drops {}
   'jobs.forestry.fell-tree {}
   'jobs.forestry.fell-from-pillar {:at [0 64 0]}
   'jobs.forestry.harvest-wood {}
   'jobs.forestry.maintain {:plan "p"}
   'jobs.forestry.prepare {:plan "p"}
   'jobs.forestry.tidy {}
   'jobs.forestry.plant-sapling {:at [0 64 0]}
   'jobs.strip.seal {:at [0 64 0] :axis :x :side :from}
   'jobs.strip.corridor {:segment "s"}
   'jobs.strip.site {}
   'jobs.strip.loot-cart {}})

(deftest every-done-of-the-area-is-a-status-on-a-fresh-frame
  (doseq [[sym args] fresh-args
          :let [s (done-of sym {} args)]]
    (is (contract/status? s) (str sym " done? gives a status"))
    (is (not (::contract/old (nth s 2))) (str sym " done? is no old map"))
    (is (every? #(contains? (nth s 2) %) [:units :done]) (str sym " done? carries its progress"))
    (is (map? (:result (nth s 2))) (str sym " done? carries a result"))))

(def farmland {:blocks {"0,63,0" "farmland"}})

(deftest till-is-done-when-every-cell-is-farmland-and-no-child-is-under-way
  (are [world m reason] (= reason (tu/kind-reason (done-of 'jobs.farm.till world {:center [0 63 0] :radius 0} m)))
    farmland {} [:ok :tilled]
    farmland {:children {:cover {}}} [:later :wrapping-up]
    {:blocks {"0,63,0" "dirt"}} {} [:later :to-till])
  (is (= {:tilled 1 :skipped {}}
         (:result (nth (done-of 'jobs.farm.till farmland {:center [0 63 0] :radius 0} {:tilled #{{:x 0 :y 63 :z 0}}}) 2)))))

(def seeds {:inventory [{:name "wheat_seeds" :count 5}]})

(deftest get-seeds-is-done-when-the-goal-is-carried-and-no-child-is-under-way
  (are [m reason] (= reason (tu/kind-reason (done-of 'jobs.gather.get-seeds seeds {:count 2} m)))
    {:goal 5} [:ok :carried]
    {:goal 5 :children {:collect {}}} [:later :wrapping-up]
    {:goal 6} [:later :short]
    {} [:later :short])
  (is (= {:got 2 :reason :count} (:result (nth (done-of 'jobs.gather.get-seeds seeds {:count 2} {:goal 5}) 2)))))

(deftest make-obsidian-is-done-at-the-count-gained-and-no-child-under-way
  (are [m reason] (= reason (tu/kind-reason (done-of 'jobs.gather.make-obsidian {:inventory [{:name "obsidian" :count 1}]} {:count 1} m)))
    {:base 0} [:ok :made]
    {:base 0 :children {:mine {}}} [:later :wrapping-up]
    {:base 1} [:later :short]
    {} [:later :short])
  (is (= {:got 1} (:result (nth (done-of 'jobs.gather.make-obsidian {:inventory [{:name "obsidian" :count 1}]} {:count 1} {:base 0}) 2)))))

(def section (tu/box -1 64 0 -1 66 2 "stone"))

(deftest seal-is-done-when-the-section-is-shut-and-the-drops-picked-up
  (let [args {:at [-1 64 1] :axis :x :side :from}]
    (are [blocks m reason] (= reason (tu/kind-reason (done-of 'jobs.strip.seal {:blocks blocks} args m)))
      section {:placed 2} [:ok :sealed]
      section {:collect true} [:later :wrapping-up]
      (assoc section "-1,64,1" "air") {} [:later :open])
    (is (= {:placed 2} (:result (nth (done-of 'jobs.strip.seal {:blocks section} args {:placed 2}) 2))))))

(deftest plant-sapling-at-a-spot-is-done-when-its-sapling-stands-there
  (are [blocks args m reason] (= reason (tu/kind-reason (done-of 'jobs.forestry.plant-sapling {:blocks blocks} args m)))
    {"0,64,0" "oak_sapling"} {:at [0 64 0] :species "oak"} {} [:ok :planted]
    {"0,64,0" "birch_sapling"} {:at [0 64 0] :species "oak"} {} [:later :owed]
    {"0,64,0" "oak_sapling"} {:at [0 64 0] :species "oak" :bone-meal 2} {:meal {:pos {:x 0 :y 64 :z 0} :left 2}} [:later :owed]
    {"0,64,0" "oak_sapling"} {:at [0 64 0]} {:children {:walk {}}} [:later :wrapping-up]
    {} {:at [0 64 0]} {} [:later :owed]))

(deftest forestry-tidy-is-done-once-looked-round-with-no-trunk-and-no-child
  (are [m reason] (= reason (tu/kind-reason (done-of 'jobs.forestry.tidy {} {} m)))
    {:tree {:base {:x 0 :y 64 :z 0}}} [:later :tidying]
    {} [:later :tidying]
    {:looked {:x 0 :y 64 :z 0} :children {:fell {}}} [:later :wrapping-up]
    {:looked {:x 0 :y 64 :z 0}} [:ok :tidied]))

(def oak-plan {:id "forest" :parts [{:id "a" :cells [[3 64 0]] :want {:tree "oak"}}]})

(deftest forestry-prepare-is-done-when-every-cell-is-ready-and-no-child-is-under-way
  (are [blocks m reason] (= reason (tu/kind-reason (done-in 'jobs.forestry.prepare {:blocks blocks} {:plan "forest"} m {"forest" oak-plan})))
    {"3,63,0" "dirt" "3,64,0" "oak_sapling"} {} [:ok :prepared]
    {"3,63,0" "dirt" "3,64,0" "oak_sapling"} {:collect true} [:later :wrapping-up]
    {"3,63,0" "dirt" "3,64,0" "oak_sapling"} {:children {:dig {}}} [:later :wrapping-up]
    {"3,63,0" "dirt"} {} [:later :preparing]))

(deftest fertilize-and-compost-are-done-once-their-count-is-reached
  (are [sym args m reason] (= reason (tu/kind-reason (done-of sym {} args m)))
    'jobs.farm.fertilize {:max 3} {:used 3} [:ok :fertilized]
    'jobs.farm.fertilize {:max 3} {:used 1} [:later :fertilizing]
    'jobs.farm.compost {:times 3} {:taken 3} [:ok :composted]
    'jobs.farm.compost {:times 3} {:taken 1} [:later :composting]))

(def corridor-seg {:y 64 :axis :x :line 0 :from 0 :to 4 :open-ends #{}})

(deftest corridor-is-done-when-nothing-is-left-to-take-and-the-far-end-was-looked-at
  (are [world m reason] (= reason (tu/kind-reason (done-of 'jobs.strip.corridor {:blocks world} {:segment corridor-seg} m)))
    {} {:looked true} [:ok :stripped]
    {} {} [:later :digging]
    {"2,64,0" "rail"} {:looked true} [:later :digging]))

(deftest loot-cart-is-done-once-the-cart-is-broken-and-its-drops-collected
  (are [m reason] (= reason (tu/kind-reason (done-of 'jobs.strip.loot-cart {} {} m)))
    {:before {} :looted true} [:ok :looted]
    {:before {} :broken true} [:later :looting]
    {} [:later :looting]))
