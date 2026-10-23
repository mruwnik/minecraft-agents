(ns engine.cost-test
  "jobs.lib.cost: one armour formula (vanilla's) under every calculator: fight cost, mob threat and the planner's rate."
  (:require [cljs.test :refer [deftest is are use-fixtures]]
            [engine.constants :as constants]
            [engine.fake :as fake]
            [engine.game :as game]
            [engine.settings :as settings]
            [engine.shelter-support :as st]
            [engine.sky-perception :as sky]
            [engine.test-util :as tu]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.look :as look]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]))

(defn round2 [x] (/ (js/Math.round (* 100 x)) 100))

(use-fixtures :each {:before #(game/select! (fake/create {}))})

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

(deftest exposure-is-the-hurt-a-second-at-the-hp-price-of-the-health-left
  (let [price #(cost/exposure-price {:health %1 :equipment %2} %3)]
    (is (zero? (price 9.7 {} [])) "no danger, no price")
    (is (= (round2 (* (cost/hurt-rate {} "skeleton") (cost/hp-price 9.7))) (round2 (price 9.7 {} ["skeleton"]))))
    (is (< (price 20 {} ["skeleton"]) (price 9.7 {} ["skeleton"])) "an hp is dearer at low health")
    (is (< (price 9.7 iron-set ["skeleton"]) (price 9.7 {} ["skeleton"])) "armour")
    (is (< (price 9.7 {} ["skeleton"]) (price 9.7 {} ["skeleton" "zombie"])) "each danger adds its own")))

(deftest a-creeper-uses-blast-damage
  (let [plain (cost/armour-stats iron-set)
        blast (cost/armour-stats (enchanted iron-set "blast_protection"))]
    (is (< (cost/mob-hurt blast "creeper" 1) (cost/mob-hurt plain "creeper" 1)))
    (is (= (cost/mob-hurt blast "zombie" 1) (cost/mob-hurt plain "zombie" 1)) "blast protection does nothing for a zombie")
    (is (= (cost/mob-hurt plain "creeper" 1) (cost/mob-hurt plain "creeper" 10)) "one blast, however long the exposure")
    (are [difficulty hurt] (= hurt (settings/with-settings {:jobs.lib.cost.threat/difficulty difficulty} #(cost/mob-hurt (cost/armour-stats nil) "creeper" 1)))
      1 22.5
      2 43
      3 64.5)))

(deftest a-witchs-potions-ignore-armour
  (let [bare (cost/armour-stats nil)
        iron (cost/armour-stats iron-set)]
    (is (= (cost/mob-hurt bare "witch" 5) (cost/mob-hurt iron "witch" 5)) "poison and harming bypass armour")
    (is (< (cost/mob-hurt iron "skeleton" 5) (cost/mob-hurt bare "skeleton" 5)) "arrows are still reduced")))

(deftest fight-damage-with-real-equipment
  (is (= 3.75 (cost/fight-damage {:weapon "iron_sword" :equipment nil :mobs [{:name "zombie" :distance 3}]})))
  (is (= 1.69 (round2 (cost/fight-damage {:weapon "iron_sword" :equipment iron-set :mobs [{:name "zombie" :distance 3}]})))
      "4 swings, 2.5 s of 0.6 hits a second at 2.5 hp (easy): vanilla's 13.5 effective points take off more"))

(deftest mob-hits-are-priced-at-the-difficulty-a-hit-a-second
  (let [bare (cost/armour-stats nil)
        zombie-s #(settings/with-settings % (fn [] (cost/mob-hurt bare "zombie" 10)))]
    (is (= 25 (zombie-s {})) "easy: a hit of 2.5 a second")
    (is (= 30 (zombie-s {:jobs.lib.cost.threat/difficulty 2})) "normal: 3 a hit")
    (is (= 45 (zombie-s {:jobs.lib.cost.threat/difficulty 3})) "hard: 4.5 a hit")
    (is (= 0 (zombie-s {:jobs.lib.cost.threat/difficulty 0})))
    (is (= 20 (cost/mob-hurt bare "spider" 10)) "the lone-fight rate (0.35 for a spider) is fight-damage's, not mob-hurt's")
    (is (= 10 (cost/mob-hurt bare "silverfish" 10)))))

(deftest an-enderman-costs-a-full-iron-body-about-seven-health
  (let [cost-of #(cost/fight-damage {:weapon "iron_sword" :equipment iron-set :mobs [{:name "enderman" :distance 3 :health %}]})]
    (is (< 6 (cost-of 40) 9) "7 swings of 6, 5 s of an enderman's 3.5 hp hits on easy, one a second, through the armour")
    (is (cost/leaves-reserve? {:health 20 :reserve 4 :damage (cost-of 40)}) "a full-health body wins")
    (is (not (cost/leaves-reserve? {:health 10 :reserve 4 :damage (cost-of 40)})) "at 10 it would end near 3")))

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

(deftest a-chaser-is-within-its-blow-of-a-flight-only-while-it-wins-the-race-there
  (let [route (mapv (fn [x] {:x x :y 64 :z 0 :t (/ x 3)}) (range 11))
        reach #(cost/reach-s {:chaser "zombie" :pos {:x % :y 64 :z 0} :route route})]
    (is (< 0 (reach -0.5) 1) "beside the start: its blows until the body pulls away, not the whole flight")
    (is (zero? (reach -4)) "left behind")
    (is (> (reach 11) 1) "ahead on the way: the body walks into it")
    (let [swum (mapv #(assoc % :t (* 2 (:t %)) :wet? true) route)
          drowned #(cost/reach-s {:chaser "drowned" :pos {:x -0.5 :y 64 :z 0} :route %})]
      (is (< (drowned swum) (drowned (mapv #(dissoc % :wet?) swum))) "a drowned swims after a swimmer at its swim speed, not its walk"))))

(deftest a-baby-zombie-is-priced-as-a-baby-not-as-an-adult
  (are [e kind] (= kind (cost/mob-kind e))
    #js {:name "zombie" :baby true} "baby"
    #js {:name "zombie" :baby false} "zombie"
    #js {:name "skeleton"} "skeleton")
  (is (false? (cost/outruns? 20 [(cost/mob-kind #js {:name "zombie" :baby true})])))
  (is (true? (cost/outruns? 20 [(cost/mob-kind #js {:name "zombie"})]))))

(deftest chase-speeds-follow-the-game-s-walk-formula-that-gives-a-player-its-speeds
  (are [blocks-s speed] (= blocks-s (round2 speed))
    4.32 (constants/ground-speed 0.98 0.1)
    5.61 (constants/ground-speed 0.98 0.13)
    2.28 (cost/chase-speed "zombie")
    3.89 (cost/chase-speed "spider")
    2.7 (cost/chase-speed "skeleton")
    5.14 (cost/chase-speed "baby")))

(deftest a-frontier-tour-costs-its-blocks-walked-and-more-with-each-walk
  (let [tour #(:cost (cost/tour-cost {:explored %1 :walks %2}))]
    (is (= (:cost (cost/walk-cost {:distance 100})) (tour 100 0)) "no walk spent: its blocks at the walk price")
    (is (< (tour 50 3) (tour 100 3)) "the farther it walked away")
    (is (< (tour 100 1) (tour 100 4)) "the more walks it took")))

(deftest a-cover-tour-walks-only-as-far-as-brings-each-point-within-reach
  (is (zero? (cost/cover-tour-s [0 0 0] [[3 0 0] [0 4 0]] 4)) "all in reach: no walk")
  (is (= (:cost (cost/walk-cost {:distance 6})) (cost/cover-tour-s [0 0 0] [[3 0 0] [10 0 0]] 4)) "one walk to reach of the far point")
  (is (= (+ (:cost (cost/walk-cost {:distance 2})) (:cost (cost/walk-cost {:distance 8})))
         (cost/cover-tour-s [0 0 0] [[10 0 0] [-6 0 0]] 4))
      "the nearest point first, then on from where that walk stopped"))

(deftest a-goal-is-worth-a-trip-and-more-the-farther-it-lies
  (is (= (cost/trip) (settings/with-settings {:jobs.lib.cost.value/tour-floor-blocks 0} #(cost/tour-worth 0))))
  (is (< (cost/tour-worth 40) (cost/tour-worth 80))))

(deftest a-near-goal-is-worth-a-tour-floor-and-the-prices-are-settings
  (is (= (cost/tour-worth 32) (cost/tour-worth 2)) "under the floor, the floor's worth")
  (is (< (settings/with-settings {:jobs.lib.cost.value/tour-floor-blocks 0} #(cost/tour-worth 2)) (cost/tour-worth 2)))
  (is (= (+ (cost/trip) (:cost (cost/walk-cost {:distance 10})))
         (settings/with-settings {:jobs.lib.cost.value/tour-floor-blocks 0 :jobs.lib.cost.value/tour-ratio 1} #(cost/tour-worth 10))))
  (is (= 7 (:cost (settings/with-settings {:jobs.lib.cost.value/per-frontier-walk 1} #(cost/tour-cost {:explored 0 :walks 7}))))))

(deftest a-goal-worth-the-caller-says-budgets-the-tour-less-the-trip-and-the-walk
  (let [walk (:cost (cost/walk-cost {:distance 50}))]
    (is (= 100 (cost/tour-worth 50 (+ 100 (cost/trip) walk))))
    (is (= (cost/tour-worth 50) (cost/tour-worth 50 0) (cost/tour-worth 50 nil)) "no worth said (0 or nil): the default")
    (is (zero? (cost/tour-worth 50 1)) "worth less than the trip and walk: no tour")
    (is (= (+ (cost/trip) (* 3 (:cost (cost/walk-cost {:distance 50})))) (cost/tour-worth 50)) "unsaid: a trip and 3 times the walk")))

(deftest food-is-worth-more-reaching-the-lower-the-bar
  (is (zero? (cost/food-worth 10)))
  (is (< 0 (cost/food-worth 8) (cost/food-worth 2) (cost/food-worth 0))))

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

(deftest a-dark-way-pays-its-own-seconds-once-and-the-cheaper-of-its-torches-and-the-dark
  (let [bare {:weapon nil :equipment nil}
        armed {:weapon "iron_sword" :equipment iron-set}
        way (fn [s kit] (settings/with-settings {:jobs.lib.cost.value/per-dark 0.3} #(cost/dark-way-s s cost/torch-gap 4 kit)))]
    (is (= 100 (- (way 100 bare) (way 0 bare))) "the seconds a way takes to dig bear no dark price")
    (is (<= (way 0 bare) (:cost (cost/light-cost {:carried 4}))) "never dearer than a torch to light it")
    (is (< (way 0 armed) (way 0 bare)) "an armed, armoured body walks it dark for less than a bare one lights it")))

(deftest an-unseen-way-is-priced-by-how-often-the-seen-ways-near-it-go-on
  (let [kit {:weapon nil :equipment nil}
        open {:k 98 :n 100}
        closed {:k 50 :n 100}]
    (is (= 0.5 (cost/way-on-chance {:k 0 :n 0})) "nothing seen: the uniform prior")
    (is (= 0.75 (cost/way-on-chance {:k 2 :n 2})))
    (is (> (cost/unseen-share closed kit) (cost/unseen-share open kit) (cost/dark-factor kit))
        "seen dead ends round it make an unseen way dearer, open ground cheaper, never below a dark cell's price")
    (is (< (cost/unseen-share open {:weapon "iron_sword" :equipment iron-set}) (cost/unseen-share open kit)) "an armed, armoured body fears the dark less")
    (is (< 0 (cost/dead-end-cost open 30) (cost/dead-end-cost closed 30) 30) "the chasers met on the way back, at the dead end's chance")))

(deftest a-flight-is-caught-by-the-way-it-takes
  (let [prior 0.15
        open {:k 98 :n 100}
        closed {:k 50 :n 100}
        unseen #(cost/flight-caught-chance {:way? true :unseen? true :counts %})]
    (is (= 1 (cost/flight-caught-chance {:way? false})) "no way left: caught")
    (is (< (js/Math.abs (- prior (cost/flight-caught-chance {:way? true :counts closed}))) 1e-9) "a way in sight: the measured prior")
    (is (< prior (unseen open) (unseen closed) 1) "a way into unseen cells may end there, the likelier where the seen ways end")))

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

(deftest exposure-of-a-seen-log-under-unseen-ones-is-judged-by-the-seen-air-over-the-pile
  (let [p (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/night :self {:pos {:x 0 :y 64 :z 0}}}))]
    (is (= [[3 67 0]] (with-redefs [sh-sense/solid-at? (fn [_ c] (= 64 (:y c)))
                                    sh-sense/seen-cell? (fn [_ c] (or (= 64 (:y c)) (= 67 (:y c))))
                                    look/glimpse? (constantly false)]
                        (:look-at (cost/exposure p {:x 3 :y 64 :z 0})))))))

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

(deftest a-hold-costs-its-seconds-and-hp-lost-and-a-way-out-its-hurt-and-death-risk
  (let [body {:respawn {:x 0 :y 64 :z 0} :level 0 :health 3}
        route {:underground? true}]
    (is (= 100 (cost/hold-price {:held-s 100 :hp-lost 0 :health 3})))
    (is (= (+ 100 (* 2 (cost/hp-price 3))) (cost/hold-price {:held-s 100 :hp-lost 2 :health 3})) "hunger's hp at the price of the health left")
    (is (= (cost/route-death-cost body route far) (cost/opening-price body 0 route far)) "no hurt: the death risk alone")
    (is (< (cost/opening-price (assoc body :health 20) 2 route far) (cost/opening-price body 2 route far)) "the same hurt is dearer to a weaker body")
    (is (= (cost/opening-price body 3 route far) (cost/opening-price body 300 route far)) "no more hp than the body has")))

(deftest a-death-by-terrain-costs-plans-near-its-spot-fading-with-age
  (let [at {:x 1 :y 64 :z 2}
        danger #(cost/terrain-danger {:pos at :cause "fall" :weight (cost/death-weight %)})]
    (is (= {:x 1 :y 64 :z 2 :close 1 :radius 4 :rate 4 :mob "fall" :block false} (danger 0)))
    (is (= 2 (:rate (danger 3600000))))
    (is (zero? (:rate (danger 8000000))))))

(deftest a-respawn-point-here-is-worth-the-deaths-it-brings-nearer
  (let [here {:x 0 :y 64 :z 0}
        lost (cost/spawn-worth {:carried 100} here)]
    (is (> lost (cost/spawn-worth {:carried 0} here)) "with no respawn the carried items are lost too")
    (is (> lost (cost/spawn-worth {:respawn far} here) 0) "a far respawn bed is worth less to replace")
    (is (zero? (cost/spawn-worth {:respawn here} here)))))

(deftest a-search-leg-costs-go-tos-price-and-the-death-it-risks
  (let [found {:uses {:job-time [4 4 6]} :route {:found true :seconds 4 :priced 9}}]
    (is (= 12 (cost/leg-price found 3)) "a found route: the planner's price, darkness and dangers in it")
    (is (= 10 (cost/leg-price {:uses {:job-time [4 7 ##Inf]} :route {:found false :reason :partial}} 3)) "else the quote's mean")))

(deftest a-find-is-worth-its-value-a-bed-its-respawn-too-and-no-names-nothing
  (let [here {:x 0 :y 64 :z 0}]
    (is (> (cost/worth-to {} ["bed"] here) (cost/worth-to {} ["oak_planks"] here)) "a plan's bed block counts as a bed")
    (is (= (cost/worth-to {} ["white_bed"] here) (cost/worth-to {} ["red_bed" "white_bed"] here)))
    (is (zero? (cost/worth-to {} [] here)))))

(deftest waiting-costs-the-share-of-the-time-until-needed-all-of-it-from-then
  (is (= 25 (cost/wait-price 100 30 120)))
  (is (= 100 (cost/wait-price 100 300 120)) "a wait past the need loses all of it")
  (is (= 100 (cost/wait-price 100 ##Inf)) "a wait with no date"))

(deftest a-search-radius-grows-with-the-worth
  (is (> (cost/search-radius 600 16) (cost/search-radius 100 16) 0))
  (is (zero? (cost/search-radius 0 16))))

(deftest a-route-with-a-death-chance-costs-more-than-the-same-route-without
  (let [body {:respawn {:x 0 :y 64 :z 0} :level 3}
        night (cost/route-death-cost body {:night? true} far)]
    (is (pos? night))
    (is (> night (cost/route-death-cost body {:night? false} far)) "night is the likelier death")))

(deftest a-known-threat-the-route-meets-costs-the-death-of-the-answer-the-body-would-take
  (let [fist {:health 20}
        route (fn [n] {:cells (for [x (range n)] {:x x :y 64 :z 0})})
        near-x (fn [x0] (fn [x _ _] (when (<= x0 x) 1)))
        chance (fn [body route & threats] (cost/death-chance body (assoc route :threats threats)))
        zombie (fn [& kvs] (apply assoc {:mob "zombie" :weight 1 :reach (near-x 0)} kvs))
        base (cost/death-chance fist {})]
    (is (= base (chance fist (route 10))) "no threat: the chance of the route's class")
    (is (= base (chance fist (route 10) {:mob "zombie" :weight 1 :reach (constantly nil)})) "a threat whose reach the route misses adds nothing")
    (is (> (chance fist (route 40) (zombie)) (chance fist (route 40) (zombie :weight 0.2)) base) "a fading memory weighs less")
    (is (> (chance {:health 6} (route 40) (zombie)) (chance fist (route 40) (zombie))) "the same walk away is likelier death at low health")
    (is (< 0.2 (- (chance fist (route 40) (zombie)) base) 0.35) "a fist walks away now and again when it meets the mob later, each caught at the prior")
    (is (< (- (chance {:health 20 :weapon "diamond_sword"} (route 40) (zombie)) base) 0.01) "a body that would fight it takes only the fight")
    (is (< (- (chance fist (route 40) (zombie :caught 0)) base) 0.01) "a flight with an open way out hardly kills")
    (is (< 0.9 (chance fist (route 40) (zombie :caught 1))) "no way out, no blocks: the fist fight")
    (is (< (- (chance (assoc fist :covers {:walls {:open-s 1 :seconds 5}}) (route 40) (zombie :caught 1)) base) 0.01) "no way out, blocks carried: the body walls in")))

(deftest a-find-is-worth-its-count-less-a-trip-and-an-animal-its-meat
  (let [body {:level 0 :carried 0}]
    (is (= (- (* 3 (cost/worth-to body ["oak_log"] nil)) (cost/trip)) (cost/find-worth body ["oak_log"] nil 3)))
    (is (< (cost/find-worth body ["oak_log"] nil 1) (cost/find-worth body ["oak_log"] nil 3)))
    (is (< (cost/animal-worth "horse") (cost/animal-worth "cow")) "a cow is worth its beef, a kind with no meat the unknown item")
    (is (= (cost/animal-worth "cow") (cost/animal-worth "sheep")) "mutton as dear as beef")))

(deftest a-look-for-animals-pays-less-the-more-ground-round-it-showed-none
  (let [note (fn [x z] {:kind :searched :what ["sheep"] :pos [x 64 z] :r 24})
        first-look (cost/look-chance [] ["sheep"] [0 0] 24)
        after (fn [ns] (cost/look-chance ns ["sheep"] [0 0] 24))]
    (is (< 0.05 first-look 0.6) "nothing looked over yet: the base rate's chance")
    (is (= (after [(note 0 0) (note 0 0)]) (after [(note 0 0)])) "the same ground counts once")
    (is (> first-look (after [(note 0 0)]) (after (for [x (range -96 97 16) z (range -96 97 16)] (note x z)))))
    (is (= first-look (after [(note 600 0)])) "ground past the region tells nothing here")
    (is (= first-look (after [(assoc (note 0 0) :saw ["sheep"])])) "a look that saw sheep, kept or barred ones too, searched in vain for none")
    (is (= (cost/look-chance [(note 0 0)] ["sheep"] [0 0] 24)
           (cost/look-chance [{:kind :searched :what ["sheep" "cow"] :saw ["cow"] :pos [0 64 0] :r 24}] ["sheep"] [0 0] 24))
        "a look that saw only cows searched in vain for sheep")
    (is (< (after (for [x (range -128 129 16) z (range -128 129 16)] (note x z))) 0.001) "a region searched in vain")
    (is (= 1 (cost/look-chance [(note 0 0)] ["oak_log"] [0 0] 24)) "a block has no base rate: no chance is priced")))

(deftest a-search-costs-more-a-find-the-more-ground-round-it-showed-none
  (let [a (* js/Math.PI 16 16)
        sheep (fn [looked] (cost/leg-chance ["sheep"] {:looked looked :a a}))
        per-find (fn [looked] (cost/search-s (sheep looked) 6 2000))]
    (is (< 6 (per-find 0) (per-find 20000)) "a find takes more than a leg; each empty look round lowers the chance of the next")
    (is (infinite? (per-find 1e7)) "a region searched in vain pays for no leg")
    (is (infinite? (cost/search-s (sheep 0) 6 5)) "a worth under a leg's price walks none")
    (is (infinite? (cost/search-s (sheep 0) 6 js/Infinity)) "a worth with no bound sets no stop")
    (is (nil? (cost/leg-chance ["oak_log"] {:looked 0 :a a})) "logs have no base rate: no chance is priced")
    (is (infinite? (cost/search-s nil 6 2000)))))

(deftest the-price-stop-is-a-leg-dearer-than-the-worth-left-times-its-chance
  (is (not (cost/leg-stops? 5 0.5 10)))
  (is (cost/leg-stops? 6 0.5 10))
  (is (cost/leg-stops? 1 0 js/Infinity) "nothing to find stops even an unbounded worth"))

(defn faces [block y n] (repeat n {:name block :pos {:x 0 :y y :z 0}}))

(deftest an-ore-look-is-priced-by-the-rock-it-sees-at-its-height-and-a-tunnel-by-the-rock-it-cuts
  (let [chance (fn [seen] ((cost/leg-chance ["coal_ore" "deepslate_coal_ore"] {:seen seen}) 0))]
    (is (= "coal" (cost/ore-of ["coal_ore" "deepslate_coal_ore"])))
    (is (nil? (cost/ore-of ["coal_ore" "iron_ore"])))
    (is (< 0.3 (chance (faces "stone" 60 800)) 0.9) "a cave of stone at y 60 shows coal more often than not")
    (is (< (chance (faces "stone" 60 100)) (chance (faces "stone" 60 800))) "less rock seen, less chance")
    (is (zero? (chance (faces "dirt" 60 800))) "dirt holds no ore")
    (is (zero? (chance (faces "deepslate" -30 800))) "coal lies above y 0")
    (is (= (cost/seen-ore "coal" (faces "stone" 60 800)) (cost/seen-ore "coal" (faces "andesite" 60 800))) "every rock the veins replace alike")
    (is (< (cost/seen-ore "coal" (faces "stone" 60 6)) (cost/tunnel-ore "coal" {:y 60} 1))
        "a cave wall was open to air when the lower coal was placed: half its vein blocks are left out; a cut lays bare buried rock")
    (is (= (cost/seen-ore "iron" (faces "stone" 16 6)) (cost/tunnel-ore "iron" {:y 16} 1)) "iron drops none beside air")))

(deftest a-kill-is-priced-by-the-fight-and-drops-by-the-loot-table
  (let [body {:health 20 :level 0 :carried 0 :weapon "stone_sword" :equipment {}}]
    (is (< (cost/kill-price body "spider" {:x 0 :y 64 :z 0}) (cost/kill-price (assoc body :health 4) "spider" {:x 0 :y 64 :z 0}))
        "the same blows cost a hurt body more")
    (is (< (cost/kill-price body "spider" {:x 0 :y 64 :z 0}) (cost/kill-price (dissoc body :weapon) "spider" {:x 0 :y 64 :z 0}))
        "a fist fight lasts longer")
    (is (= 1 (cost/kill-drops "spider" "string")) "0 to 2 string")
    (is (nil? (cost/kill-drops "spider" "bone")))))

(deftest a-mob-outlives-the-body-going-far-only-when-it-could-pick-up-a-drop-it-equips-or-is-kept-by-its-state
  ;; a zombie is a chicken jockey 0.0049 of the time, a skeleton a spider's rider 0.0099
  (game/select! (fake/create {:server {:difficulty "hard" :simulationDistance 10}}))
  (are [mob drops clock kept] (= (js/Math.round (* 1000 kept)) (js/Math.round (* 1000 (threat/left-alive mob 200 0 drops clock))))
    "zombie" ["dirt"] nil 0.5522
    "zombie" :unknown nil 0.5522
    "skeleton" ["dirt" "stick"] nil 0.0099
    "skeleton" ["iron_helmet"] nil 0.5544
    "skeleton" ["shield"] nil 0.5544
    "skeleton" ["carved_pumpkin"] nil 0.5544
    "skeleton" ["zombie_head"] nil 0.5544
    "skeleton" ["bow"] nil 0.5544
    "skeleton" ["stone_sword" "iron_pickaxe"] nil 0.0099
    "wither_skeleton" ["iron_sword"] nil 0.55
    "wither_skeleton" ["wooden_sword" "bow"] nil 0
    "zombified_piglin" ["iron_axe"] nil 0.5522
    "zombified_piglin" ["bow" "dirt"] nil 0.0049
    "zombie" [] nil 0.0049
    "zombie" nil nil 0.0049
    "creeper" :unknown nil 0
    "enderman" nil nil 1
    "zombie" ["dirt"] {:day-time 0 :moon-phase 4 :world-age 0} (- 1 (* (- 1 (* 0.55 0.125)) (- 1 0.0049)))))

(deftest costs-follow-the-difficulty-and-simulation-distance-the-server-announced
  (let [zombie-s #(cost/mob-hurt (cost/armour-stats nil) "zombie" 10)]
    (game/select! (fake/create {:server {:difficulty "hard" :simulationDistance 6}}))
    (is (= 45 (zombie-s)) "hard prices a hit at 4.5")
    (is (pos? (threat/door-break-share "zombie")) "a zombie breaks doors on hard")
    (is (= 6 (threat/simulation-distance)))
    (is (< 0 (threat/left-alive "creeper" 200 600 nil nil) 1) "6 chunks is 96 blocks: no tick reaches the instant-despawn zone, only the idle odds apply")
    (is (= 25 (settings/with-settings {::threat/difficulty 1} zombie-s)) "the setting overrides what the server said")
    (is (= 0.0125 (threat/door-break-share "zombie" {:day-time 0 :moon-phase 4 :world-age 0}))
        "a fresh world: the regional difficulty 2.25, a zombie a breaker by 0.1 x 0.125")
    (is (= 0.1 (threat/door-break-share "zombie")) "a clock not known: the cap")
    (is (< (threat/door-break-share "zombie" {:day-time 400000 :moon-phase 4 :world-age 400000})
           (threat/door-break-share "zombie" {:day-time 400000 :moon-phase 0 :world-age 400000}))
        "the full moon raises it once the day time is past its first three days")
    (is (= 0 (threat/door-break-share "drowned" nil)))
    (game/select! (fake/create {:server {:difficulty nil :simulationDistance nil}}))
    (is (= 45 (zombie-s)) "unknown: hard")
    (is (pos? (threat/door-break-share "zombie")) "unknown: a zombie breaks doors (hard)")
    (is (nil? (threat/simulation-distance)))
    (is (< 0 (threat/left-alive "creeper" 200 0 nil nil)) "unknown simulation distance: a far mob is not assumed despawned")
    (game/select! (fake/create {:server {:simulationDistance 10}}))
    (is (= 0 (threat/door-break-share "zombie")) "a zombie breaks no door below hard")
    (is (= 0 (threat/left-alive "creeper" 200 0 nil nil)))))
