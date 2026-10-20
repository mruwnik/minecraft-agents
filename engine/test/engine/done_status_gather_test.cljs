(ns engine.done-status-gather-test
  "done? of the gather, farm, forestry and strip jobs answers a status (docs/contract-conditions-design.md 2.2)."
  (:require [cljs.test :refer [deftest is are]]
            [engine.contract :as contract]
            [engine.expr :as expr]
            [engine.fake :as fake]
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
   'jobs.items.collect-drops {}
   'jobs.forestry.fell-tree {}
   'jobs.forestry.fell-from-pillar {:base {:x 0 :y 64 :z 0}}
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
  (are [args m reason] (= reason (tu/kind-reason (done-of 'jobs.gather.get-seeds seeds args m)))
    {:count 5} {} [:ok :carried]
    {:count 5} {:children {:collect {}}} [:later :wrapping-up]
    {:count 6} {} [:later :short])
  (is (= {:got 2 :reason :count} (:result (nth (done-of 'jobs.gather.get-seeds seeds {:count 5} {:had 3}) 2)))))

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

(def corridor-walls
  (into {} (for [x (range 5) z [-2 2] y [64 65]] [(str x "," y "," z) "stone"])))

(deftest corridor-is-done-when-nothing-is-left-to-take-and-the-far-end-was-looked-at
  (are [world m reason] (= reason (tu/kind-reason (done-of 'jobs.strip.corridor {:blocks (merge corridor-walls world)} {:segment corridor-seg} m)))
    {} {:looked true} [:ok :stripped]
    {} {} [:later :digging]
    {"2,64,0" "rail"} {:looked true} [:later :digging])
  (is (= [:later :digging] (tu/kind-reason (done-of 'jobs.strip.corridor {:blocks {}} {:segment corridor-seg} {:looked true})))
      "side walls never seen: a side corridor may hang there"))

(deftest loot-cart-is-done-once-the-cart-is-broken-and-its-drops-collected
  (are [m reason] (= reason (tu/kind-reason (done-of 'jobs.strip.loot-cart {} {} m)))
    {:before {} :looted true} [:ok :looted]
    {:before {} :broken true} [:later :looting]
    {} [:later :looting]))

(defn site-done [phase]
  (let [c (ctx-of 'jobs.strip.site {} {:site "s" :base "b" :at [0 64 0]} {})]
    ((:done? (get registry/jobs 'jobs.strip.site))
     (assoc c :view (fn [] {:now 1000 :data {:entries {:strip/sites [{:t 1000 :data {"s" {:phase phase :segments {"a" {:state :stripped}}}}}]}}})))))

(deftest strip-site-is-done-once-the-home-is-restored
  (are [phase reason] (= reason (tu/kind-reason (site-done phase)))
    :done [:ok :stripped]
    :restore [:later :stripping]
    :strip [:later :stripping]))

(defn field-of
  "Farmland at y 63 and a wheat crop of age over the xs of z 0."
  [age xs]
  {:blocks (into {} (mapcat (fn [x] [[(str x ",63,0") "farmland"] [(str x ",64,0") "wheat"]]) xs))
   :ages (into {} (map (fn [x] [(str x ",64,0") age])) xs)})

(def here {:x 0 :y 64 :z 0})

(deftest harvest-is-done-when-nothing-ripe-is-left-and-nothing-is-owed
  (are [world m reason] (= reason (tu/kind-reason (done-of 'jobs.farm.harvest world {:radius 6} m)))
    (field-of 7 [2 3]) {} [:later :ripe]
    (field-of 3 [2 3]) {} [:ok :harvested]
    (field-of 3 [2 3]) {:cut 2 :replanted 2} [:ok :harvested]
    (field-of 3 [2 3]) {:collect true} [:later :collecting]
    (field-of 3 [2 3]) {:replant [{:pos {:x 2 :y 64 :z 0} :seed "wheat_seeds"}]} [:later :replanting]
    (field-of 3 [2 3]) {:bare [{:x 2 :y 64 :z 0}]} [:later :bare]
    (field-of 3 [2 3]) {:children {:collect {}}} [:later :wrapping-up]
    {} {} [:later :looking]))

(def plant-box {:min {:x 0 :y 63 :z 0} :max {:x 1 :y 63 :z 0}})

(deftest plant-is-done-when-no-cell-is-bare-and-the-field-was-seen
  (are [blocks m reason] (= reason (tu/kind-reason (done-of 'jobs.farm.plant {:blocks blocks} {:box plant-box} m)))
    {"0,63,0" "farmland" "1,63,0" "farmland"} {} [:later :planting]
    {"0,63,0" "farmland" "1,63,0" "farmland" "0,64,0" "wheat" "1,64,0" "wheat"} {:looked here} [:ok :planted]
    {"0,63,0" "farmland" "1,63,0" "farmland" "0,64,0" "wheat" "1,64,0" "wheat"} {:started true :planted 2} [:ok :planted]
    {"0,63,0" "farmland" "1,63,0" "farmland" "0,64,0" "wheat" "1,64,0" "wheat"} {} [:later :looking]
    {"0,63,0" "farmland" "1,63,0" "dirt" "0,64,0" "wheat"} {:looked here} [:later :untilled]))

(deftest fertilize-is-also-done-when-a-look-found-nothing-to-fertilize
  (are [world m reason] (= reason (tu/kind-reason (done-of 'jobs.farm.fertilize world {:max 3} m)))
    (field-of 7 [2]) {:looked here} [:ok :fertilized]
    (field-of 3 [2]) {:looked here} [:later :fertilizing]
    (field-of 7 [2]) {} [:later :fertilizing]))

(deftest find-spot-is-done-when-spots-are-found
  (let [spots [{:pos {:x 4 :y 64 :z 4} :score 90}]]
    (are [args m reason] (= reason (tu/kind-reason (done-of 'jobs.farm.find-spot {} args m)))
      {} {} [:later :searching]
      {} {:spots spots} [:ok :found]
      {} {:spots []} [:later :searching])))

(def tidy-plan {:id "p" :parts [{:id "a" :cells [[0 65 0]] :want :clear}]})

(deftest farm-tidy-is-done-when-no-stray-is-left
  (are [blocks m reason] (= reason (tu/kind-reason (done-in 'jobs.farm.tidy {:blocks blocks} {:plan "p"} m {"p" tidy-plan})))
    {"0,65,0" "dirt"} {:looked here} [:later :tidying]
    {} {:looked here} [:ok :tidied]
    {} {:looked here :collect true} [:later :collecting]
    {} {:looked here :refused [{:pos {:x 0 :y 65 :z 0}}]} [:later :reporting]))

(deftest farm-tend-is-done-when-every-part-is-in-order
  (let [box {:min {:x 0 :y 63 :z 0} :max {:x 1 :y 64 :z 0}}]
    (are [world m reason] (= reason (tu/kind-reason (done-in 'jobs.farm.tend world {:box box} m {})))
      (field-of 3 [0 1]) {:results {"*" {:steps {:harvest {:cut 1}}}}} [:ok :in-order]
      (field-of 3 [0 1]) {} [:later :tending]
      (field-of 7 [0 1]) {} [:later :tending]
      (field-of 3 [0 1]) {:todo [:till]} [:later :tending]
      (field-of 3 [0 1]) {:limit {:reason :x}} [:later :tending])))

(def tree-at {:column {:x 5 :z 5} :base {:x 5 :y 64 :z 5}})

(deftest fell-tree-is-done-when-its-column-is-cut-and-the-body-is-down
  (are [world m reason] (= reason (tu/kind-reason (done-of 'jobs.forestry.fell-tree world {} (merge tree-at m))))
    {} {:dug 3} [:ok :felled]
    {} {:dug 0} [:later :felling]
    {} {:dug 3 :pillar true} [:later :felling]
    {} {:dug 3 :children {:down {}}} [:later :felling]
    {:blocks {"5,64,5" "oak_log" "5,65,5" "oak_log"}} {:dug 1} [:later :felling])
  (is (= [:later :felling] (tu/kind-reason (done-of 'jobs.forestry.fell-tree {} {} {:dug 3})))))

(def logs-2 {:inventory [{:name "oak_log" :count 2}]})

(deftest harvest-wood-is-done-when-the-planting-is-over-or-the-count-is-carried
  (are [world args m reason] (= reason (tu/kind-reason (done-of 'jobs.forestry.harvest-wood world args m)))
    logs-2 {:count 2} {} [:ok :harvested]
    logs-2 {:count 3} {} [:later :harvesting]
    logs-2 {:count 2} {:phase :collect :tree {:x 1 :y 64 :z 1}} [:later :harvesting]
    logs-2 {:count 2} {:children {:fell {}}} [:later :harvesting]
    logs-2 {} {} [:later :harvesting]
    logs-2 {} {:phase :done} [:ok :harvested]))

(deftest forestry-maintain-is-done-when-the-forest-is-in-order
  (are [blocks inv m reason] (= reason (tu/kind-reason (done-in 'jobs.forestry.maintain {:blocks blocks :inventory inv} {:plan "forest"} m {"forest" oak-plan})))
    {"3,63,0" "dirt" "3,64,0" "oak_sapling"} [] {:begun true} [:ok :in-order]
    {"3,63,0" "dirt" "3,64,0" "oak_sapling"} [] {:begun true :collect ["oak"]} [:later :working]
    {"3,63,0" "dirt"} [] {:begun true} [:later :planting]
    {"3,63,0" "dirt"} [{:name "oak_sapling" :count 1}] {:begun true} [:later :planting]))

(deftest collect-drops-is-done-when-no-wanted-drop-is-in-sight
  (are [args m reason] (= reason (tu/kind-reason (done-of 'jobs.items.collect-drops {} args m)))
    {} {} [:ok :collected]
    {} {:collected 2} [:ok :collected]
    {:search 4} {:collected 1} [:later :searching]
    {:search 4} {:collected 1 :wanted 1} [:ok :collected]
    {} {:collected 0 :skipped [7]} [:later :skipped]
    {} {:left [7]} [:later :left]
    {:ids [7]} {} [:later :ids-unseen]
    {:ids [7]} {:collected 1 :skipped [7]} [:ok :collected])
  (let [c (ctx-of 'jobs.items.collect-drops {} {} {})]
    (swap! (fake/state (:primitives c)) fake/spawn [1 64 1] "apple" 1)
    (is (= [:later :collecting] (tu/kind-reason ((:done? (get registry/jobs 'jobs.items.collect-drops)) c))))))

(deftest mine-is-done-once-finished-with-the-count-carried-and-home
  (are [count m reason] (= reason (tu/kind-reason (done-of 'jobs.gather.mine {:inventory [{:name "cobblestone" :count 5}]} {:block "stone" :count count} m)))
    5 {:goal 5 :had 3 :finished true} [:ok :mined]
    5 {:goal 5 :had 3} [:later :mining]
    8 {:goal 8 :had 3 :finished true} [:later :mining]
    5 {:goal 5 :had 3 :finished true :reason :not-home} [:later :mining]))