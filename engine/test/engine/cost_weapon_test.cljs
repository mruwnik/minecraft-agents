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
                     ["trident" 910] ["mace" 1667] ["iron_pickaxe" 834] ["stone_shovel" 1000] ["stick" 500] [nil 500]]]
    (is (= ms (weapon/attack-gap-ms item)) (str item))))

(deftest every-skeleton-variant-is-ranged-and-priced
  (doseq [mob ["skeleton" "stray" "parched" "bogged"]]
    (is (contains? weapon/ranged-mobs mob) mob)
    (is (contains? threat/bow-mobs mob) mob)))

(deftest an-arrow-blows-vanillas-mean-for-the-difficulty
  (doseq [[d blow] [[1 3.877] [2 4.052] [3 4.226]]]
    (is (< (js/Math.abs (- blow (threat/arrow-blow d))) 1e-3) (str "difficulty " d))))

(deftest a-pillagers-bolt-blows-as-an-arrow-does
  (doseq [d [1 2 3]]
    (settings/with-settings {::threat/difficulty d}
      (fn [] (is (= (threat/arrow-blow d) (threat/mob-blow "pillager")) (str "difficulty " d))))))

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
  (is (= [3 4.5] (mapv weapon/weapon-damage ["stone_pickaxe" "iron_shovel"])) "a digging tool hits harder than a fist")
  (is (= 1 (weapon/weapon-damage "wooden_hoe")) "a hoe hits as a fist")
  (is (= 1 (weapon/weapon-damage "stick")) "an unknown item is a fist"))

(deftest weapons-rank-by-the-damage-their-hits-deal-a-second
  (is (= ["iron_sword" "iron_axe" "stone_sword" "stone_axe" "stone_pickaxe" "stone_shovel" nil]
         (sort-by weapon/weapon-score > [nil "stone_pickaxe" "stone_axe" "iron_axe" "stone_shovel" "stone_sword" "iron_sword"]))))

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

(deftest a-bow-mob-holding-a-weapon-blows-two-plus-the-weapons-bonus
  (doseq [[item hp] [["stone_sword" 6] ["iron_sword" 7] ["diamond_sword" 8]]]
    (is (= hp (threat/mob-blow (threat/kind {:name "skeleton" :held item}))) item)))

(deftest an-armed-melee-mob-blows-its-base-plus-the-weapons-bonus
  (doseq [[mob item hp] [["zombie" "iron_sword" 8] ["husk" "stone_sword" 7] ["drowned" "diamond_sword" 9]
                         ["zombified_piglin" "golden_sword" 8] ["piglin" "golden_sword" 8] ["vindicator" "iron_axe" 13]]]
    (is (= hp (threat/mob-blow (threat/kind {:name mob :held item}))) (str mob " " item))))

(deftest an-armed-kind-is-priced-as-its-mob-in-the-name-keyed-tables
  (let [k (threat/kind {:name "zombie" :held "iron_sword"})]
    (is (= (threat/lone-hit-rate "zombie") (threat/lone-hit-rate k)))
    (is (= (threat/hit-rate-shape "zombie") (threat/hit-rate-shape k)))))

(deftest a-seen-empty-hand-or-a-non-weapon-keeps-the-plain-kind
  (is (= "zombie" (threat/kind {:name "zombie" :held "none"})))
  (is (= "zombie" (threat/kind {:name "zombie" :held nil})))
  (is (= 3 (threat/mob-blow (threat/kind {:name "zombie" :held "dirt"})))))

(deftest a-bow-mob-seen-with-an-empty-hand-is-a-melee-kind-blowing-two
  (is (= "skeleton/none" (threat/kind {:name "skeleton" :held "none"})))
  (is (= 2 (threat/mob-blow (threat/kind {:name "skeleton" :held "none"}))))
  (is (threat/strikes? {:name "skeleton" :held "none"}))
  (is (not (threat/strikes? {:name "skeleton" :held nil}))))

(deftest a-melee-bow-kind-reads-its-mob-in-the-name-keyed-tables
  (let [k (threat/kind {:name "bogged" :held "stone_sword"})]
    (is (= "bogged/stone_sword" k))
    (is (= (cost/chase-speed "bogged") (cost/chase-speed k)))
    (is (= 16 (weapon/remaining-health {:name k})))
    (is (= 6 (threat/mob-blow k)))
    (is (not (threat/ranged? {:name "bogged" :held "stone_sword"})))
    (is (threat/ranged? {:name "bogged" :held "bow"}))))

(deftest tables-keyed-by-mob-name-read-an-armed-kind-by-its-mob
  (is (= (weapon/remaining-health {:name "vindicator"}) (weapon/remaining-health {:name "vindicator/iron_axe"})))
  (is (= 24 (weapon/remaining-health {:name "vindicator/iron_axe"})))
  (is (= (threat/follow-range "vindicator") (threat/follow-range "vindicator/iron_axe")))
  (is (= 12 (threat/follow-range "vindicator/iron_axe")))
  (is (= (cost/chase-speed "vindicator") (cost/chase-speed "vindicator/iron_axe")))
  (is (= (cost/chase-speed "zombie") (cost/chase-speed "zombie/iron_sword"))))
