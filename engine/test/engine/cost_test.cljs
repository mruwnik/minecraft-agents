(ns engine.cost-test
  "jobs.lib.cost: one armour formula (vanilla's) under every calculator: fight cost, mob threat and the planner's rate."
  (:require [cljs.test :refer [deftest is are]]
            [engine.settings :as settings]
            [engine.shelter-support :as st]
            [engine.sky-perception :as sky]
            [engine.test-util :as tu]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger]
            [jobs.lib.look :as look]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]))

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

(deftest a-witchs-potions-ignore-armour
  (let [bare (cost/armour-stats nil)
        iron (cost/armour-stats iron-set)]
    (is (= (cost/mob-hurt bare "witch" 5) (cost/mob-hurt iron "witch" 5)) "poison and harming bypass armour")
    (is (< (cost/mob-hurt iron "skeleton" 5) (cost/mob-hurt bare "skeleton" 5)) "arrows are still reduced")))

(deftest fight-damage-with-real-equipment
  (is (= 7.5 (cost/fight-damage {:weapon "iron_sword" :equipment nil :mobs [{:name "zombie" :distance 3}]})))
  (is (= 3.45 (round2 (cost/fight-damage {:weapon "iron_sword" :equipment iron-set :mobs [{:name "zombie" :distance 3}]})))
      "4 swings, 2.5 s of 3 hp hits: vanilla's 13.5 effective points take 54% off"))

(deftest an-enderman-costs-a-full-iron-body-about-twelve-health
  (let [cost-of #(cost/fight-damage {:weapon "iron_sword" :equipment iron-set :mobs [{:name "enderman" :distance 3 :health %}]})]
    (is (< 10 (cost-of 40) 13) "7 swings of 6, 5 s of an enderman's 5 hp/s (2.2 per hit in iron measured), 46% through the armour")
    (is (cost/leaves-reserve? {:health 20 :reserve 4 :damage (cost-of 40)}) "a full-health body wins")
    (is (not (cost/leaves-reserve? {:health 12 :reserve 4 :damage (cost-of 40)})) "at 12 it would end near 0")))

(deftest burning-bypasses-armour-points-but-not-fire-protection
  (let [burn #(cost/after-armour (cost/armour-stats %) :burning 10 1)]
    (is (= (burn nil) (burn iron-set)) "armour points do nothing against burning")
    (is (= (burn nil) (burn diamond-8)) "nor toughness")
    (is (< (burn (enchanted iron-set "fire_protection")) (burn nil)) "fire protection still counts")
    (is (< (cost/after-armour (cost/armour-stats iron-set) :melee 10 1) (cost/after-armour (cost/armour-stats nil) :melee 10 1)))))

(deftest standing-in-fire-or-lava-is-reduced-by-armour
  (let [fire #(cost/after-armour (cost/armour-stats %) :fire 10 1)]
    (is (< (fire iron-set) (fire nil)) "in_fire and lava damage is not in the bypasses_armor tag")
    (is (< (fire (enchanted iron-set "fire_protection")) (fire iron-set)) "fire protection counts too")))

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

(def ff-boots (fn [lvl] {:feet {:name "iron_boots" :enchants [{:name "feather_falling" :lvl lvl}]}}))

(deftest fall-damage-is-vanillas-and-armour-points-do-not-reduce-it
  (is (= [0 0 1 2 4] (mapv #(cost/fall-damage nil %) [2 3 4 5 7])))
  (is (= 2 (cost/fall-damage iron-set 5)) "points do not count")
  (is (= 0.52 (round2 (cost/fall-damage (ff-boots 4) 4))) "feather falling IV: 12 of 25 off")
  (is (< (cost/fall-damage (enchanted iron-8 "protection") 5) 2) "protection counts"))

(deftest fall-profile-takes-the-longest-drop-the-budget-pays-for
  (is (= {:max-drop 3 :fall-factor 1} (cost/fall-profile {:health 20 :equipment iron-set :damage-budget 0})))
  (is (= {:max-drop 10 :fall-factor 1} (cost/fall-profile {:health 20 :equipment iron-set :damage-budget 7})))
  (is (= 16 (:max-drop (cost/fall-profile {:health 20 :equipment iron-set :damage-budget 99}))) "never over 16")
  (is (= {:max-drop 12 :fall-factor 0.52} (update (cost/fall-profile {:health 20 :equipment (ff-boots 4) :damage-budget 5}) :fall-factor round2))
      "feather falling stretches the same budget"))

(deftest the-fight-reserve-has-one-default
  (is (= cost/default-reserve (danger/fight-reserve)) "the ::fight-reserve setting's default is the cost's"))

;; ---------------------------------------------------------------- cooking against eating now

(def four-beef [{:name "beef" :count 4}])

(deftest cook-gain-counts-the-points-cooking-adds
  (is (= 20 (cost/cook-gain four-beef)))
  (is (= 0 (cost/cook-gain [{:name "bread" :count 3}]))))

(def costs {:furnace-cost 8 :place-cost 2 :walk-cost 0.2})

(deftest cooking-is-worth-it-by-gain-against-walk-furnace-and-urgency
  (is (cost/cook-worth? four-beef 5 4 false costs) "a furnace near")
  (is (cost/cook-worth? four-beef 5 nil false costs) "no furnace: one is made, four beef pay for it")
  (is (not (cost/cook-worth? [{:name "beef" :count 1}] 5 nil false costs)) "one beef does not pay for a furnace")
  (is (not (cost/cook-worth? four-beef 5 200 false costs)) "a furnace too far")
  (is (cost/cook-worth? four-beef 3 4 false costs) "low food still cooks at a furnace near"))

(def meat-opts {:pen-adults 2 :wild-premium 3 :wild-relief 4})

(deftest meat-is-replaced-dearly-unless-a-working-pen-breeds-it
  (are [seen expected] (= expected (cost/meat-factor seen meat-opts))
    {:wild 0 :penned 0} 4
    {:wild 4 :penned 0} 2.5
    {:wild 0 :penned 1} 4
    {:wild 0 :penned 2} 1
    {:wild 9 :penned 3} 1))

(deftest meat-is-priced-by-its-animal
  (are [raw animal] (= animal (cost/meat-animal raw))
    "beef" "cow" "porkchop" "pig" "mutton" "sheep" "chicken" "chicken" "rabbit" "rabbit" "cod" nil))

(deftest raw-meat-from-a-hunt-pays-for-a-furnace-a-penned-herd-does-not
  (let [two-beef [{:name "beef" :count 2}]
        factor {:hunted (constantly 4) :penned (constantly 1)}]
    (are [kind furnace? food expected]
         (= expected (cost/cook-worth? two-beef food nil furnace? (assoc costs :meat-factor (factor kind))))
      :hunted false 5 true
      :penned false 5 false
      :penned true 5 true
      :hunted false 20 true)))

(deftest a-carried-furnace-makes-small-batches-worth-cooking
  (let [one-mutton [{:name "mutton" :count 1}]]
    (is (not (cost/cook-worth? one-mutton 12 nil false costs)) "no furnace at all: one mutton does not pay for making one")
    (is (cost/cook-worth? one-mutton 12 nil true costs) "a furnace in the pack only needs putting down")))

(deftest a-body-outruns-only-mobs-well-slower-than-itself
  (are [food mobs expected] (= expected (cost/outruns? food mobs))
    20 [] true
    20 ["zombie"] true
    6 ["zombie"] true
    20 ["creeper"] true
    20 ["spider"] false
    6 ["spider"] false
    20 ["cave_spider"] false
    20 ["zombie" "spider"] false)
  (is (true? (settings/with-settings {:jobs.lib.cost.danger/outrun-factor 1} #(cost/outruns? 20 ["spider"])))))

(deftest the-race-to-a-refuge-counts-each-chaser-at-its-own-speed
  (are [names expected] (= expected (cost/reach-first? 5 [5] 20 names))
    ["zombie"] true
    ["spider"] false
    ["baby"] false))

(deftest a-baby-zombie-is-priced-as-a-baby-not-as-an-adult
  (are [e kind] (= kind (cost/mob-kind e))
    #js {:name "zombie" :baby true} "baby"
    #js {:name "zombie" :baby false} "zombie"
    #js {:name "skeleton"} "skeleton")
  (is (false? (cost/outruns? 20 [(cost/mob-kind #js {:name "zombie" :baby true})])))
  (is (true? (cost/outruns? 20 [(cost/mob-kind #js {:name "zombie"})]))))

(deftest a-frontier-tour-costs-its-blocks-walked-and-more-with-each-walk
  (let [tour #(:cost (cost/tour-cost {:explored %1 :walks %2}))]
    (is (= (:cost (cost/walk-cost {:distance 100})) (tour 100 0)) "no walk spent: its blocks at the walk price")
    (is (< (tour 50 3) (tour 100 3)) "the farther it walked away")
    (is (< (tour 100 1) (tour 100 4)) "the more walks it took")))

(deftest a-goal-is-worth-a-trip-and-more-the-farther-it-lies
  (is (= (cost/trip) (settings/with-settings {:jobs.lib.cost.value/tour-floor-blocks 0} #(cost/tour-worth 0))))
  (is (< (cost/tour-worth 40) (cost/tour-worth 80))))

(deftest a-near-goal-is-worth-a-tour-floor-and-the-prices-are-settings
  (is (= (cost/tour-worth 32) (cost/tour-worth 2)) "under the floor, the floor's worth")
  (is (< (settings/with-settings {:jobs.lib.cost.value/tour-floor-blocks 0} #(cost/tour-worth 2)) (cost/tour-worth 2)))
  (is (= (+ (cost/trip) (:cost (cost/walk-cost {:distance 10})))
         (settings/with-settings {:jobs.lib.cost.value/tour-floor-blocks 0 :jobs.lib.cost.value/tour-ratio 1} #(cost/tour-worth 10))))
  (is (= 7 (:cost (settings/with-settings {:jobs.lib.cost.value/per-frontier-walk 1} #(cost/tour-cost {:explored 0 :walks 7}))))))

(deftest dig-cost-is-the-item-base-s-dig-term
  (is (< (cost/dig-cost "dirt") (cost/dig-cost "stone") (cost/dig-cost "obsidian")))
  (is (nil? (cost/dig-cost "bedrock")) "never dug"))

(deftest dark-factor-grows-with-the-danger-to-the-kit
  (let [floor (cost/dark-factor)
        unarmed (cost/dark-factor {:weapon nil :equipment nil})
        sword (cost/dark-factor {:weapon "iron_sword" :equipment nil})
        armed-armoured (cost/dark-factor {:weapon "iron_sword" :equipment iron-set})]
    (is (zero? floor) "the bare factor is the flat floor")
    (is (< (js/Math.abs (- 1 unarmed)) 1e-9) "an unarmed body pays a dark cell 2x a lit one")
    (is (> unarmed sword armed-armoured floor) "a bare body pays most, armour and a sword less, never below the floor")))

(deftest spawn-chance-is-zero-where-a-mob-cannot-spawn
  (let [lit (fn [block sky] (fn [_] {:block block :sky sky}))
        dark (cost/spawn-chance [0 0 0] {:light (lit 0 0)})]
    (is (pos? dark))
    (is (= dark (cost/spawn-chance [0 0 0])) "a cell the body has not seen counts as dark")
    (is (= 0 (cost/spawn-chance [0 0 0] {:light (lit 7 0)})) "a torch's light keeps mobs off")
    (is (= 0 (cost/spawn-chance [0 0 0] {:light (lit 0 15)})) "open sky lit")
    (is (= dark (cost/spawn-chance [0 0 0] {:light (lit 0 0)})) "sky light darkened away")))

(deftest spawn-chance-of-an-area-is-the-mean-of-its-cells
  (let [light (fn [[x]] (if (= x 0) {:block 0 :sky 0} {:block 14 :sky 0}))
        one (cost/spawn-chance [0 0 0])]
    (is (= (/ one 2) (cost/spawn-chance #{[0 0 0] [1 0 0]} {:light light})))
    (is (= (/ one 2) (cost/spawn-chance {:min [0 0 0] :max [1 0 0]} {:light light})))))

(def open-night-chance (cost/spawn-chance [0 0 0]))

(def pit
  "A 1x1 pit with a roof: dirt round the feet and head cells, a roof over the head."
  (merge (into {} (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]] y [64 65]] [(str dx "," y "," dz) "dirt"]))
         {"0,66,0" "dirt"}))

(deftest exposure-of-a-walk-out-of-shelter-is-the-open-sky-night-figure-lowered-by-dawn
  (let [p (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks pit}))]
    (let [e (cost/exposure p {:x 15 :y 64 :z 0})]
      (is (= {:chance open-night-chance :lowered-by #{:light}} (dissoc e :light-in)))
      (is (pos? (:light-in e)) "and the seconds till light"))))

(deftest exposure-of-a-walk-out-of-shelter-judges-an-unseen-cell-only-where-it-could-be-seen
  (let [p (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks pit}))
        exposure (fn [glimpse? to] (with-redefs [sh-sense/seen-cell? (constantly false) sh-sense/block-light-at (constantly 14) look/glimpse? (constantly glimpse?)]
                                     (cost/exposure p to)))]
    (is (= {:chance 0.0 :lowered-by #{}} (exposure true {:x 3 :y 64 :z 0})) "a lit cell in clear line is made out")
    (is (pos? (:chance (exposure false {:x 3 :y 64 :z 0}))) "a lit cell behind a wall stays unknown, so dark")))

(deftest exposure-of-a-walk-in-the-open-is-the-spawn-chance-at-its-end
  (let [night (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/night :self {:pos {:x 0 :y 64 :z 0}}}))
        day (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/noon :self {:pos {:x 0 :y 64 :z 0}}}))
        seen (fn [p block sky] (with-redefs [sh-sense/seen-cell? (constantly true) sh-sense/block-light-at (constantly block) sh-sense/sky-light-at (constantly sky) sh-inside/walk-leaves-shelter? (constantly nil)]
                                 (cost/exposure p {:x 15 :y 64 :z 0})))]
    (is (= {:chance (cost/spawn-chance [15 64 0]) :lowered-by #{:look} :look-at [[15 64 0]]}
           (cost/exposure night {:x 15 :y 64 :z 0}))
        "an unseen cell is guessed dark, and looking is what lowers that")
    (is (= {:chance 0.0 :lowered-by #{}} (seen night 8 15)) "a seen lit cell spawns nothing")
    (is (= #{} (:lowered-by (seen day 0 0))) "a seen dark cave cell by day: nothing foreseeable lowers it")))

(deftest exposure-takes-the-shelter-entry-of-the-body
  (let [p (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks {"0,67,0" "stone"}}))]
    (with-redefs [sh-inside/walk-leaves-shelter? (fn [_ shelter _ _] (when (= :entry shelter) :dug-in))]
      (is (= #{:light} (:lowered-by (cost/exposure p {:x 15 :y 64 :z 0} :entry))))
      (is (= #{:look} (:lowered-by (cost/exposure p {:x 15 :y 64 :z 0})))))))

(deftest exposure-floors-the-goal-and-takes-a-column-at-the-feet-height
  (let [p (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/night :self {:pos {:x 0 :y 64 :z 0}}}))]
    (is (= {:chance (cost/spawn-chance [15 64 0]) :lowered-by #{:look} :look-at [[15 64 0]]}
           (cost/exposure p {:x 15.5 :z 0.2})))))

(deftest exposure-of-a-solid-seen-goal-is-judged-by-the-air-over-it
  (let [p (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks {"15,64,0" "grass_block"}}))
        sky (fn [_ at] (if (= 64 (:y at)) 0 15))]
    (with-redefs [sh-sense/seen-cell? (constantly true) sh-sense/block-light-at (constantly 0)
                  sh-sense/sky-light-at sky sh-inside/walk-leaves-shelter? (constantly nil)]
      (is (= #{:light} (:lowered-by (cost/exposure p {:x 15 :y 64 :z 0}))) "the grass cell is dark by day too; the air over it lowers at dawn")
      (is (= (cost/exposure p {:x 15 :y 65 :z 0}) (cost/exposure p {:x 15 :y 64 :z 0}))))))

(deftest exposure-of-a-dark-cell-is-lowered-by-dawn-where-the-sky-reaches-down-its-column
  (let [at (fn [sky blocks] (let [p (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks blocks}))]
                              (with-redefs [sh-sense/seen-cell? (constantly true) sh-sense/block-light-at (constantly 0)
                                            sh-sense/sky-light-at (constantly sky) sh-inside/walk-leaves-shelter? (constantly nil)]
                                (:lowered-by (cost/exposure p {:x 15 :y 64 :z 0})))))]
    (is (= #{:light} (at 15 {})) "open sky: the night is lifted at dawn")
    (is (= #{:light} (at 13 {"15,70,0" "oak_leaves"})) "under a canopy the sky is dimmed but dawn still lights the floor")
    (is (= #{} (at 12 {"15,67,0" "stone"})) "a stair cell the sky reaches only round a corner: dawn is no cure, only threats count")
    (is (= #{} (at 0 {})))))

(def far {:x 100 :y 64 :z 0})

(deftest death-costs-more-the-farther-from-respawn-and-the-more-levels
  (let [body {:respawn {:x 0 :y 64 :z 0} :level 0}]
    (is (> (cost/death-cost body far) (cost/death-cost body {:x 10 :y 64 :z 0})))
    (is (> (cost/death-cost (assoc body :level 10) far) (cost/death-cost body far)))
    (is (= (cost/experience-cost {:level 10}) (- (cost/death-cost (assoc body :level 10) far) (cost/death-cost body far))))))

(deftest a-route-with-a-death-chance-costs-more-than-the-same-route-without
  (let [body {:respawn {:x 0 :y 64 :z 0} :level 3}
        night (cost/route-death-cost body {:night? true} far)]
    (is (pos? night))
    (is (> night (cost/route-death-cost body {:night? false} far)) "night is the likelier death")))
