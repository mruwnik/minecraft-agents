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
                     ["iron_pickaxe" 500] ["stick" 500] [nil 500]]]
    (is (= ms (weapon/attack-gap-ms item)) (str item))))

(deftest every-skeleton-variant-is-ranged-and-priced
  (doseq [mob ["skeleton" "stray" "parched" "bogged"]]
    (is (contains? weapon/ranged-mobs mob) mob)
    (is (= threat/arrow-blow (get threat/mob-hit mob)) mob)))

(deftest a-skeleton-shoots-vanillas-cadence-for-the-difficulty-and-each-arrow-blows-the-measured-mean
  (doseq [[d rate blow] [[1 (/ 1 3) 2.9] [2 (/ 1 3) 3.8] [3 0.5 5.7]]]
    (settings/with-settings {::threat/difficulty d}
      (fn []
        (is (= rate (threat/lone-hit-rate "skeleton")) (str "shots a second at difficulty " d))
        (is (< (js/Math.abs (- (* 10 rate blow) (:threat (threat/base-threat "skeleton" 10 rate)))) 1e-9) (str "10 s of arrows at difficulty " d))))))

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
  (is (= 1 (weapon/tool-tier "stick")) "an unknown material counts as tier 1")
  (is (= [0 1 2 3 4 5] (mapv weapon/weapon-rank ["wooden_sword" "golden_sword" "copper_sword" "iron_sword" "diamond_sword" "netherite_sword"])))
  (is (= 0 (weapon/weapon-rank "stick")))
  (is (= ["wooden" "stone" "copper" "iron" "diamond" "netherite" "golden"] weapon/cheapness)))
