(ns engine.cost-test
  "jobs.lib.cost: one armour formula (vanilla's) under every calculator: fight cost, mob threat and the planner's rate."
  (:require [cljs.test :refer [deftest is]]
            [jobs.lib.cost :as cost]))

(defn round2 [x] (/ (js/Math.round (* 100 x)) 100))

(def iron-set {:head {:name "iron_helmet"} :torso {:name "iron_chestplate"}
               :legs {:name "iron_leggings"} :feet {:name "iron_boots"}})

(def iron-8 "Eight points of iron, no toughness." {:head {:name "iron_helmet"} :torso {:name "iron_chestplate"}})
(def diamond-8 "Eight points of diamond, toughness 2." {:torso {:name "diamond_chestplate"}})

(defn enchanted [eq ench] (assoc-in eq [:torso :enchants] [{:name ench :lvl 4}]))

(deftest armour-stats-read-points-toughness-and-enchants-from-js-or-cljs
  (is (= {:points 0 :toughness 0 :enchants []} (cost/armour-stats nil)))
  (is (= 15 (:points (cost/armour-stats iron-set))))
  (is (= 15 (:points (cost/armour-stats (clj->js iron-set)))))
  (is (= {:points 8 :toughness 2} (select-keys (cost/armour-stats diamond-8) [:points :toughness])))
  (is (= 5 (:points (cost/armour-stats (clj->js {:torso {:name "golden_chestplate"} :mainHand {:name "iron_sword"}}))))
      "only worn slots count"))

(deftest diamond-and-enchanted-armour-cost-less-than-plain-armour-of-the-same-points
  (let [armed {:health 20 :weapon "diamond_sword"}
        rate #(cost/danger-rate (assoc armed :equipment %) "zombie")]
    (is (< (rate diamond-8) (rate iron-8)) "toughness")
    (is (< (rate (enchanted iron-8 "protection")) (rate iron-8)) "protection")
    (is (< (cost/fight-damage {:weapon "iron_sword" :equipment diamond-8 :mobs [{:name "zombie" :distance 0}]})
           (cost/fight-damage {:weapon "iron_sword" :equipment iron-8 :mobs [{:name "zombie" :distance 0}]})))))

(deftest a-creeper-uses-blast-damage
  (let [plain (cost/armour-stats iron-set)
        blast (cost/armour-stats (enchanted iron-set "blast_protection"))]
    (is (< (cost/mob-hurt blast "creeper" 1) (cost/mob-hurt plain "creeper" 1)))
    (is (= (cost/mob-hurt blast "zombie" 1) (cost/mob-hurt plain "zombie" 1)) "blast protection does nothing for a zombie")
    (is (= (cost/mob-hurt plain "creeper" 1) (cost/mob-hurt plain "creeper" 10)) "one blast, however long the exposure")
    (is (= 43 (cost/mob-hurt (cost/armour-stats nil) "creeper" 1)))))

(deftest fight-damage-with-real-equipment
  (is (= 7.5 (cost/fight-damage {:weapon "iron_sword" :equipment nil :mobs [{:name "zombie" :distance 3}]})))
  (is (= 3.45 (round2 (cost/fight-damage {:weapon "iron_sword" :equipment iron-set :mobs [{:name "zombie" :distance 3}]})))
      "4 swings, 2.5 s of 3 hp hits: vanilla's 13.5 effective points take 54% off"))

(deftest fire-bypasses-armour-points-but-not-fire-protection
  (let [fire #(cost/after-armour (cost/armour-stats %) :fire 10 1)]
    (is (= (fire nil) (fire iron-set)) "armour points do nothing against fire")
    (is (= (fire nil) (fire diamond-8)) "nor toughness")
    (is (< (fire (enchanted iron-set "fire_protection")) (fire nil)) "fire protection still counts")
    (is (< (cost/after-armour (cost/armour-stats iron-set) :melee 10 1) (cost/after-armour (cost/armour-stats nil) :melee 10 1)))))

(deftest an-enchanted-worn-chestplate-lowers-the-danger-rate
  (let [body {:health 20 :weapon "diamond_sword"}
        rate #(cost/danger-rate (assoc body :equipment {:torso %}) "zombie")
        plain {:name "iron_chestplate"}]
    (is (< (rate (assoc plain :enchants [{:name "protection" :level 4}])) (rate plain)))))

(defn stack [& pairs] (mapv (fn [[n c]] {:name n :count c}) (partition 2 pairs)))

(deftest food-reserve-keeps-three-days-of-food-best-first
  (is (= 60 cost/food-reserve-points) "3 days at 20 hunger points a day")
  (is (= {"bread" 12} (cost/food-reserve (stack "bread" 20))))
  (is (= {"bread" 3} (cost/food-reserve (stack "bread" 3))))
  (is (= {"bread" 2 "apple" 9} (cost/food-reserve (stack "apple" 9 "bread" 2))))
  (is (= {"bread" 12} (cost/food-reserve (stack "bread" 5 "bread" 15))))
  (is (= {"carrot" 20} (cost/food-reserve (stack "carrot" 30 "dirt" 64))))
  (is (= {} (cost/food-reserve (stack "rotten_flesh" 9 "dirt" 3 "iron_hoe" 1))))
  (is (= {"bread" 12} (cost/food-reserve (stack "golden_apple" 3 "bread" 20))) "golden apples are not reserve food")
  (is (= {} (cost/food-reserve (stack "golden_apple" 3 "enchanted_golden_apple" 1))))
  (is (= {} (cost/food-reserve []))))

(deftest food-short-is-the-points-missing-from-the-reserve
  (is (= 60 (cost/food-short [])))
  (is (= 35 (cost/food-short (stack "bread" 5))))
  (is (= 0 (cost/food-short (stack "bread" 20))))
  (is (= 60 (cost/food-short (stack "golden_apple" 3 "rotten_flesh" 9)))))
