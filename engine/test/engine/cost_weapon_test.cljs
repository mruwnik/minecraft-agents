(ns engine.cost-weapon-test
  "jobs.lib.cost.weapon: the fight inputs (weapon damage, attack gap, mob health) and the one tool-material table."
  (:require [cljs.test :refer [deftest is]]
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.cost.weapon :as weapon]))

(deftest attack-gap-ms-is-the-held-weapons-cooldown
  (doseq [[item ms] [["wooden_sword" 625] ["diamond_sword" 625] ["netherite_sword" 625]
                     ["wooden_axe" 1250] ["stone_axe" 1250] ["iron_axe" 1112]
                     ["copper_sword" 625] ["copper_axe" 1250] ["golden_axe" 1000] ["diamond_axe" 1000] ["netherite_axe" 1000]
                     ["trident" 910] ["mace" 1667] ["iron_pickaxe" 500] ["stick" 500] [nil 500]]]
    (is (= ms (weapon/attack-gap-ms item)) (str item))))

(deftest every-skeleton-variant-is-ranged-and-priced
  (doseq [mob ["skeleton" "stray" "parched" "bogged"]]
    (is (contains? weapon/ranged-mobs mob) mob)
    (is (contains? threat/bow-mobs mob) mob)))

(deftest an-arrow-blows-vanillas-mean-for-the-difficulty
  (doseq [[d blow] [[1 3.877] [2 4.052] [3 4.226]]]
    (is (< (js/Math.abs (- blow (threat/arrow-blow d))) 1e-3) (str "difficulty " d))))

(deftest an-arrow-hits-at-the-posterior-mean-of-the-counts
  (is (< (js/Math.abs (- (/ 191 353) (threat/arrow-hit-chance))) 1e-9))
  (settings/with-settings {::threat/arrow-hits 0 ::threat/arrow-shots 0}
    (fn [] (is (= 0.5 (threat/arrow-hit-chance)) "no counts: uniform prior"))))

(deftest a-skeleton-shoots-vanillas-cadence-and-the-arrows-that-hit-blow-the-difficultys-mean
  (doseq [[d shots scale] [[1 (/ 1 3) #(min (+ (/ % 2) 1) %)] [2 (/ 1 3) identity] [3 0.5 #(* 1.5 %)]]]
    (settings/with-settings {::threat/difficulty d}
      (fn []
        (let [rate (* shots (/ 191 353))]
          (is (< (js/Math.abs (- rate (threat/lone-hit-rate "skeleton"))) 1e-9) (str "hits a second at difficulty " d))
          (is (< (js/Math.abs (- (* 10 rate (scale (threat/arrow-blow d))) (:threat (threat/base-threat "skeleton" 10)))) 1e-9)
              (str "10 s of arrows at difficulty " d)))))))

(deftest remaining-health-estimates-what-is-left-of-a-mob
  (is (= 8 (weapon/remaining-health {:name "zombie" :hits 2 :damage 6})) "20 less two iron sword hits")
  (is (= 3 (weapon/remaining-health {:name "zombie" :hits 2 :damage 6 :health 3})) "a reported health wins")
  (is (= 16 (weapon/remaining-health {:name "spider" :hits 0 :damage 6})))
  (is (= 20 (weapon/remaining-health {:name "unknown_mob" :hits 0 :damage 6})) "unknown mobs count as 20"))

(deftest weapon-damage-by-item
  (is (= 6 (weapon/weapon-damage "iron_sword")))
  (is (= 5 (weapon/weapon-damage "copper_sword")))
  (is (= 9 (weapon/weapon-damage "copper_axe")))
  (is (= 1 (weapon/weapon-damage nil)) "a fist")
  (is (= 1 (weapon/weapon-damage "stick")) "an unknown item is a fist"))

(deftest a-copper-sword-is-costed-as-a-weapon-not-a-fist
  (let [mobs [{:name "zombie" :distance 3}]]
    (is (< (cost/fight-damage {:weapon "copper_sword" :mobs mobs})
           (cost/fight-damage {:weapon nil :mobs mobs})))))

(deftest one-material-table-gives-tier-rank-and-cheapness
  (is (= [1 2 3 4 4] (mapv weapon/tool-tier ["wooden_pickaxe" "copper_pickaxe" "iron_axe" "diamond_sword" "netherite_axe"])))
  (is (= [2 4 6 8 9 12] (mapv weapon/tool-speed ["wooden_axe" "stone_pickaxe" "iron_pickaxe" "diamond_pickaxe" "netherite_pickaxe" "golden_pickaxe"])))
  (is (= 1 (weapon/tool-tier "stick")) "an unknown material counts as tier 1")
  (is (= [0 1 2 3 4 5] (mapv weapon/weapon-rank ["wooden_sword" "golden_sword" "copper_sword" "iron_sword" "diamond_sword" "netherite_sword"])))
  (is (= 0 (weapon/weapon-rank "stick")))
  (is (= ["wooden" "stone" "copper" "iron" "diamond" "netherite" "golden"] weapon/cheapness)))
