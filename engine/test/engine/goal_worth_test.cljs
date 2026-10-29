(ns engine.goal-worth-test
  "Goal jobs price their own goal at the root (their registry worth-of): what the goal yields, not ##Inf (arrive), and
  ##Inf only while a step would run that no price covers."
  (:require [cljs.test :refer [deftest is]]
            [engine.apiary-maintain-test :as am]
            [engine.harvest-test :as ht]
            [engine.library-test :as lt]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.lib.cost :as cost]
            [jobs.lib.world-files :as world]))

(defn ctx-of
  "A root ctx over primitives p with args (more: extra ctx keys)."
  [p args more]
  (merge {:primitives p :args args :id "j" :root "j" :slots [] :view (fn [] {:now 0 :data mem/empty-data})
          :engine {:world (world/of-data {} {} []) :warned (atom #{})}}
         more))

(defn worth
  "The root worth job's worth-of gives (ctx-of p args more)."
  ([job p args] (worth job p args {}))
  ([job p args more] ((:worth-of (get registry/jobs job)) (ctx-of p args more))))

(defn priced? [w] (and (pos? w) (js/isFinite w)))

(defn item [n k] {:name n :count k})

(deftest fish-trade-breed-bring-and-hunt-are-worth-what-they-still-want
  (let [fish #(worth 'jobs.food.fish (tu/fake {:inventory %2}) {:count %1})
        trade #(worth 'jobs.village.trade (tu/fake {}) {:buy "bread" :count %})
        breed #(worth 'jobs.animals.breed (tu/fake {}) {:mob "cow" :pairs %})]
    (is (priced? (fish 2 [])))
    (is (= (fish 2 []) (fish 3 [(item "cod" 1)])) "a fish carried is caught already")
    (is (priced? (trade 1)))
    (is (= (* 4 (trade 1)) (trade 4)))
    (is (= (* 2 (breed 1)) (breed 2)) "a calf a pair")
    (is (priced? (worth 'jobs.animals.bring (tu/fake {}) {:mob "cow"})))
    (is (= (worth 'jobs.combat.hunt (tu/fake {}) {:item "beef" :count 1})
           (worth 'jobs.combat.hunt (tu/fake {:inventory [(item "beef" 1)]}) {:item "beef" :count 2})) "a hunt: the items still to carry")))

(def oak-world {:blocks (merge {"3,63,0" "dirt"} (lt/tree 3 0 "oak" 4))})

(deftest harvest-wood-is-worth-the-logs-it-still-wants-or-the-tree-it-fells
  (let [wood #(worth 'jobs.forestry.harvest-wood (tu/seeing-all (tu/fake-on-floor (assoc oak-world :inventory %2))) (merge {:radius 16} %1))]
    (is (priced? (wood {:count 2 :species "oak"} [])))
    (is (= (wood {:count 2 :species "oak"} []) (wood {:count 3 :species "oak"} [(item "oak_log" 1)])))
    (is (= (wood {:count 4 :species "oak"} []) (wood {} [])) "one tree: its four logs")))

(deftest set-spawn-is-worth-the-respawn-at-its-bed
  (let [p (tu/seeing-all (tu/fake-on-floor {:blocks {"6,64,0" "red_bed"}}))]
    (is (priced? (worth 'jobs.survival.set-spawn p {:bed {:x 6 :y 64 :z 0}})))
    (is (zero? (worth 'jobs.survival.set-spawn (tu/fake-on-floor {}) {})) "no bed")))

(deftest a-harvest-is-worth-its-ripe-crops-and-their-replanting
  (let [seen (tu/seeing-all (tu/fake-on-floor ht/wheat-world))
        harvest #(worth 'jobs.farm.harvest %1 (ht/plain-args %2))]
    (is (= (* 9 (cost/crop-worth "wheat")) (harvest seen {:replant false})) "nine ripe wheat")
    (is (< (harvest seen {:replant false}) (harvest seen {})) "replanting adds the sown cells")
    (is (= ##Inf (harvest (tu/fake-on-floor ht/wheat-world) {})) "nothing seen yet: a look first")))

(def field {:min {:x 2 :y 63 :z 2} :max {:x 4 :y 64 :z 4}})

(deftest a-farm-pass-is-worth-its-tend-and-the-stock-it-stores
  (let [p (tu/seeing-all (tu/fake-on-floor (assoc ht/wheat-world :inventory [(item "wheat" 30)])))
        args {:area field :till false :irrigate false :fertilize false :carry {}}]
    (is (priced? (worth 'jobs.farm.maintain p args)))
    (is (= (+ (worth 'jobs.farm.maintain p args) (:value (cost/goal-value [(item "wheat" 28)])))
           (worth 'jobs.farm.maintain p (assoc args :chest {:x 8 :y 64 :z 0}))) "the wheat above the 2 kept as bait, stored at its goal-value")))

(deftest an-apiary-pass-is-worth-the-honeycomb-of-its-ripe-hive
  (let [args {:box am/box :with :either :carry {} :ways #{:carried}}
        apiary #(worth 'jobs.apiary.maintain % args)
        seen #(tu/seeing-all (tu/fake-on-floor (assoc-in (am/world %) [:blocks "2,65,0"] "white_carpet")))
        p (seen {:inventory (am/inv "shears" 1)})]
    (is (= (:value (cost/goal-value [(item "honeycomb" 3)])) (apiary p)) "a carpeted fire, a ripe hive")
    (is (zero? (apiary (seen {:inventory (am/inv "shears" 1) :level 0}))) "no hive ripe")
    (is (= ##Inf (apiary (doto (tu/blind (tu/fake-on-floor (am/world {:inventory (am/inv "shears" 1)}))) (aset "sensedAt" (fn [_] #js {:unknown true}))))) "an area never seen")))

(def forest {:id "forest" :parts [{:id "a" :cells [[3 64 0]] :want {:tree "oak"}}]})

(deftest a-forest-pass-is-worth-the-logs-of-its-grown-trees
  (let [forestry #(worth 'jobs.forestry.maintain (tu/seeing-all (tu/fake-on-floor %)) {:plan "forest" :accept #{:fluid-adjacent}}
                         {:engine {:world (world/of-data {"forest" forest} {} []) :warned (atom #{})}})]
    (is (= (:value (cost/goal-value [(item "oak_log" 4)])) (forestry oak-world)))
    (is (= ##Inf (forestry {:blocks {"3,63,0" "dirt"} :inventory [(item "oak_sapling" 1)]})) "planting has no price")))
