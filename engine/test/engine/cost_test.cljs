(ns engine.cost-test
  "jobs.lib.cost: one armour formula (vanilla's) under every calculator: fight cost, mob threat and the planner's rate."
  (:require [cljs.test :refer [deftest is are use-fixtures]]
            [engine.constants :as constants]
            [engine.fake :as fake]
            [engine.game :as game]
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.answer :as answer]
            [jobs.lib.cost.caught :as caught]
            [jobs.lib.cost.death :as death]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.cost.ways :as ways]))

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
  (is (false? (settings/with-settings {:jobs.lib.cost.danger/closing-rates {"zombie" {:sprint {:blocks -20 :s 40}}}} #(cost/outruns? 20 ["zombie"])))
      "flights from zombies measured closing: a fed body no longer outruns one")
  (is (true? (settings/with-settings {:jobs.lib.cost.danger/closing-rates {"zombie" {:sprint {:blocks -20 :s 40}}}} #(cost/outruns? 20 ["creeper"])))
      "the count is per kind")
  (is (true? (settings/with-settings {:jobs.lib.cost.danger/closing-rates {"spider" {:walk {:blocks 40 :s 40}}}} #(cost/outruns? 6 ["spider"])))
      "measured gains outweigh the weak prior"))

(deftest a-flights-closing-prior-is-its-pace-with-the-stops-between-steps
  (is (< (cost/flee-pace 20) (cost/flee-walk-speed 20)) "closing-rates count step start to step start, stops included")
  (is (= (/ (cost/flee-pace 20) (cost/flee-pace 6)) (/ (cost/flee-walk-speed 20) (cost/flee-walk-speed 6))))
  (is (< 0 (- (cost/flee-pace 20) (cost/chase-speed "unlisted")) 0.05)
      "a fed body's 3.72 b/s steps of 1.02 s with 0.23 s stops make 3.04 b/s: it barely outruns a mob at the 3 b/s ::chase-speed default"))

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
      (is (< (drowned swum) (drowned (mapv #(dissoc % :wet?) swum))) "a drowned swims after a swimmer at its swim speed, not its walk")
      (let [catches (fn [route x] (cost/caught-on? {:chaser "drowned" :pos {:x x :y 64 :z 0} :route route}))
            dry (mapv #(dissoc % :wet?) swum)
            offsets (range -8 0 0.25)]
        (is (seq (filter #(not= (catches swum %) (catches dry %)) offsets)) "whether a drowned catches a swimmer is raced at its swim speed")))))

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
    5.14 (cost/chase-speed "baby")
    8.74 (cost/chase-speed "enderman")
    3.38 (cost/chase-speed "zombified_piglin")
    3.89 (cost/chase-speed "wither_skeleton")
    3.89 (cost/chase-speed "zoglin")))

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

(deftest an-unseen-floor-is-absent-at-the-counted-rate-of-its-kind-and-costs-its-look-the-walk-back-and-the-next-way
  (is (= 0.5 (cost/counted-chance {:k 0 :n 0})) "nothing counted: the uniform prior")
  (is (= 0.25 (cost/counted-chance {:k 0 :n 0 :prior [1 3]})) "a measured prior's pseudo-counts")
  (settings/with-settings {:jobs.lib.cost.value/unseen-floor-counts {:level {:absent 1 :present 3} :down {:absent 3 :present 1}
                                                                      :up {:absent 0 :present 1} :drop {:absent 1 :present 1}}}
    (fn []
      (is (= 0.25 (cost/floor-absent-chance :level nil)) "the measured prior of the step's kind, not the uniform one")
      (is (= 0.75 (cost/floor-absent-chance :down nil)))
      (is (= 0.5 (cost/floor-absent-chance :level {:absent 2})) "a call's floors of the kind seen not there raise it")
      (is (= (* 0.75 0.5 0.5) (cost/floor-dead-end-chance :level nil)) "a level floor not there: no step 1, 2 or 3 lower either")
      (is (= (* 0.8 0.5 0.5) (cost/floor-dead-end-chance :level {:down {:absent 1}})) "the call's lower floors not there raise it")))
  (let [walk #(:cost (cost/walk-cost {:distance %}))]
    (is (= (+ 0.7 (* 0.5 (walk 10))) (:cost (cost/turn-back-cost [{:look 0.7 :back 10 :p 0.5}]))))
    (is (= (+ 0.7 (* 0.5 (+ (walk 10) 4))) (:cost (cost/turn-back-cost [{:look 0.7 :back 10 :p 0.5 :extra-s 4}]))) "the next-best way's extra seconds on top")
    (is (< (:cost (cost/turn-back-cost [{:look 0.1 :back 2 :p 0.5}])) (:cost (cost/turn-back-cost [{:look 0.1 :back 40 :p 0.5}])))
        "the farther the walk back, the dearer")
    (is (< (:cost (cost/turn-back-cost [{:look 0.1 :back 10 :p 0.1}])) (:cost (cost/turn-back-cost [{:look 0.1 :back 10 :p 0.9}])))
        "the likelier not there, the dearer")
    (is (> 1e-9 (js/Math.abs (- (+ 0.1 (* 0.5 (walk 2)) (* 0.5 (+ 0.1 (* 0.25 (walk 3)))))
                                (:cost (cost/turn-back-cost [{:look 0.1 :back 2 :p 0.5} {:look 0.1 :back 3 :p 0.25}])))))
        "a later floor is reached only while those before it held")))

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
    (is (= unarmed (* (cost/spawns-hurt {:weapon nil :equipment nil} 1 nil) (cost/spawn-chance [0 0 0]) (cost/hp-seconds)))
        "an unarmed body pays the hurt of the spawn mix, the one spawn price walk-risk uses")
    (is (> unarmed sword armed-armoured floor) "a bare body pays most, armour and a sword less, never below the floor")))

(deftest a-kit-s-dark-factor-is-worked-out-once-while-the-settings-stay
  (let [kit {:weapon "stone_sword" :equipment nil :health 13}
        asked (atom 0)
        hurt answer/spawns-hurt]
    (with-redefs [answer/spawns-hurt (fn [& args] (swap! asked inc) (apply hurt args))]
      (let [once (cost/dark-factor kit)]
        (is (= once (cost/dark-factor kit)))
        (is (= 1 @asked) "the second costed world reads the kept factor")
        (let [rarer (settings/with-settings {:jobs.lib.cost.value/spawn-chance-per-s 0.0001} #(cost/dark-factor kit))]
          (is (= 2 @asked) "other settings are priced anew")
          (is (< rarer once) "by them"))))))

(deftest spawns-hurt-is-at-most-the-life
  (let [body {:weapon nil :equipment nil :health 6}]
    (is (< (cost/spawns-hurt body 0.1 nil) 6))
    (is (= 6 (cost/spawns-hurt body 50 nil)) "fifty spawns take the life, no more")))

(deftest a-lit-way-pays-its-seconds-once-and-its-torches-and-a-dark-one-its-spawns-over-its-seconds
  (let [bare {:weapon nil :equipment nil}
        armed {:weapon "iron_sword" :equipment iron-set}
        n cost/torch-gap
        dark (fn [s kit] (settings/with-settings {:jobs.lib.cost.value/per-dark 0.3} #(cost/dark-way-s s n kit)))]
    (is (= 100 (- (cost/lit-way-s 100 n) (cost/lit-way-s 0 n))) "the seconds a lit way takes to dig bear no dark price")
    (is (= (* 2 (:cost (cost/light-cost {:carried 1}))) (cost/lit-way-s 0 (inc n))) "a torch every torch-gap blocks")
    (is (> (- (dark 70 bare) 70) (- (dark 8 bare) 8)) "a dark way dug in 70 s meets more spawns than one walked in 8")
    (is (< (dark 70 armed) (dark 70 bare)) "an armed, armoured body fears the dark less")))

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

(deftest a-dead-end-costs-the-walk-back-out-priced-smoothly-by-the-race-on-it
  ;; card 1ecaa07d: the reverted pricing paid nothing for the walk back to the fork, and the whole trap once a chaser
  ;; was within its blow at all
  (let [none {:k 0 :n 0}
        back #(cost/dead-end-cost none 30 [{:s 4 :caught-s % :unseen? false}])]
    (is (= 2 (back 0)) "a free way back: its seconds, at the dead end's chance")
    (is (< (back 0) (back 0.01) (+ (back 0) 0.5)) "a blow barely in reach adds a little")
    (is (< (back 0.5) (back 2) 15) "the longer in reach, the nearer the trap")
    (is (= 15 (cost/dead-end-cost none 30 [{:s 40 :caught-s 0}])) "never more than the trap")))

(deftest a-flight-is-caught-by-the-way-it-takes
  (let [open {:k 98 :n 100}
        closed {:k 50 :n 100}
        unseen #(cost/flight-caught-chance {:way? true :unseen? true :counts %})]
    (is (= 1 (cost/flight-caught-chance {:way? false})) "no way left: caught")
    (is (zero? (cost/flight-caught-chance {:way? true :counts closed})) "a way in sight adds nothing to the prior")
    (is (< 0 (unseen open) (unseen closed) 1) "a way into unseen cells may end there, the likelier where the seen ways end")))

(deftest a-flight-is-caught-at-a-chance-smooth-in-the-health-it-starts-at
  ;; the zombie family's trial flights (cost.caught/::flights, easy): under 10 hp 18 of 29 died, at 20 hp 3 of 27
  (settings/with-settings {::threat/difficulty 1}
    (fn []
      (let [flee #(cost/flee-answer {:health % :food 20 :mobs [{:name "zombie" :distance 3 :follow 35}]})
            chance #(get-in (flee %) [:caught :chance])
            dies #(answer/blows-chance {:health % :food 20} (flee %))]
        (is (< (js/Math.abs (- (chance 9.9) (chance 10))) 0.01) "no step at a band edge")
        (is (< (chance 20) (chance 10) (chance 5)) "caught more often from low health")
        (is (< 0.45 (dies 5) 0.8) "its own blows kill about the measured 18 of 29 under 10 hp")
        (is (< 0.05 (dies 20) 0.2) "and 3 of 27 at 20 hp: a caught flight takes some blows, not the whole flight's")))))

(deftest a-flight-from-a-shooter-is-caught-at-the-bow-mobs-own-prior
  ;; the trial flights from bow mobs: 2 deaths in 45, most of the arrows on the walk out of range
  (let [bow (caught/prior "stray" 20) zombie (caught/prior "zombie" 20)]
    (is (= bow (caught/prior "skeleton" 20)) "one group")
    (is (< (:chance bow) (:chance zombie)) "caught less often than by a zombie")
    (is (< (:blows bow) (:blows zombie)) "and hit fewer times when it is")
    (is (= zombie (caught/prior "spider" 20)) "a group not counted is priced as the zombie family")
    (is (= (caught/prior "zombie" 13) (caught/prior "zombie" 12.6)) "health read in whole hp, so its memo stays a few keys")
    (let [at (fn [d mob] (settings/with-settings {:jobs.lib.cost.threat/difficulty d} #(:blows (caught/prior mob 20))))]
      (is (< (js/Math.abs (- (* 1.5 (at 1 "skeleton")) (at 3 "skeleton"))) 1e-9) "on hard a skeleton shoots 1.5x as often as on easy, where the arrows were counted")
      (is (= (at 1 "zombie") (at 3 "zombie")) "a zombie's blow count does not change with difficulty"))))

(deftest the-posterior-keeps-its-points-when-a-row-term-is-quite-zero
  ;; row 1 (80 blows) is 1e-144 to 1e-32 over the lambda grid, row 2 (400 blows) 0 for every mu and lambda: without the floor
  ;; on a term each product underflows to 0, every log is -Infinity and no point is left
  (let [{:keys [blows points]} (caught/posterior {:lived [[10 80 1] [10 400 1]] :died []})]
    (is (seq points))
    (is (pos? blows))))

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

(def far {:x 100 :y 64 :z 0})

(deftest death-costs-more-the-farther-from-respawn-and-the-more-levels
  (let [body {:respawn {:x 0 :y 64 :z 0} :level 0}]
    (is (> (cost/death-cost body far) (cost/death-cost body {:x 10 :y 64 :z 0})))
    (is (> (cost/death-cost (assoc body :level 10) far) (cost/death-cost body far)))
    (is (= (cost/experience-cost {:level 10}) (- (cost/death-cost (assoc body :level 10) far) (cost/death-cost body far))))))

(deftest a-death-loses-its-load-unless-the-walk-back-beats-the-despawn
  (let [lost #(- (cost/death-cost (assoc % :carried 3000) far) (cost/death-cost % far))]
    (is (zero? (lost {:respawn {:x 0 :y 64 :z 0}})) "a bed 100 blocks off: the walk back gets it")
    (is (= 3000 (lost {:respawn {:x 3000 :y 64 :z 0}})) "a bed too far for the despawn window")
    (is (= 3000 (lost {})) "no bed: world spawn")))

(deftest a-hold-costs-its-seconds-and-hp-lost-and-a-way-out-its-hurt-and-death-risk
  (let [body {:respawn {:x 0 :y 64 :z 0} :level 0 :health 3}
        route {:spawns 0.5}]
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
        dark (cost/route-death-cost body {:spawns 0.1} far)]
    (is (pos? dark))
    (is (zero? (cost/route-death-cost body {} far)) "no mob met spawning, no known threat: no death")))

(deftest a-known-threat-the-route-meets-costs-the-death-of-the-answer-the-body-would-take
  (let [fist {:health 20}
        route (fn [n] {:cells (for [x (range n)] {:x x :y 64 :z 0})})
        near-x (fn [x0] (fn [x _ _] (when (<= x0 x) 1)))
        chance (fn [body route & threats] (cost/death-chance body (assoc route :threats threats)))
        zombie (fn [& kvs] (apply assoc {:mob "zombie" :weight 1 :reach (near-x 0)} kvs))]
    (is (zero? (chance fist (route 10))) "no threat, no spawns: no death")
    (is (zero? (chance fist (route 10) {:mob "zombie" :weight 1 :reach (constantly nil)})) "a threat whose reach the route misses adds nothing")
    (is (> 1e-12 (js/Math.abs (- (cost/spawns-chance fist 0.5 {:x 0 :y 64 :z 0}) (cost/death-chance fist (assoc (route 10) :spawns 0.5)))))
        "the mobs met spawning on it: the stay price's chance, no flat prior for the route's kind")
    (is (> 1e-12 (js/Math.abs (- (- 1 (* (- 1 (cost/spawns-chance fist 0.5 {:x 0 :y 64 :z 0})) (- 1 (chance fist (route 40) (zombie)))))
                                 (cost/death-chance fist (assoc (route 40) :spawns 0.5 :threats [(zombie)])))))
        "spawns and known threats: either kills")
    (is (> (chance fist (route 40) (zombie)) (chance fist (route 40) (zombie :weight 0.2)) 0) "a fading memory weighs less")
    (is (> (chance {:health 6} (route 40) (zombie)) (chance fist (route 40) (zombie))) "the same walk away is likelier death at low health")
    (is (< 0.25 (chance fist (route 40) (zombie)) 0.36) "a fist walks away now and again when it meets the mob later, that one a return, each caught at the prior of full health (cost.caught/prior); ~0.35, as the flight's pace with its stops gains only 0.75 b/s on a zombie")
    (is (< 0.02 (chance {:health 20 :weapon "diamond_sword"} (route 40) (zombie)) 0.1) "a body that would fight it takes only the fight, which goes bad now and again (cost.caught ::fights: 4 of 51 sword fights begun at 20 hp died)")
    (is (= (chance fist (route 40) (zombie)) (chance fist (route 40) (zombie :caught 0))) "a way the race calls free adds nothing to the prior")
    (is (< 0.85 (chance fist (route 40) (zombie :caught 1))) "no way out, no blocks: the fist fight, nearly sure death (the 40 cells are timed at fight/walk-speed, so ~0.89)")
    (is (< (chance (assoc fist :covers {:walls {:open-s 1 :seconds 5}}) (route 40) (zombie :caught 1)) 0.01) "no way out, blocks carried: the body walls in")))

(deftest threats-met-at-once-are-one-meeting-and-threats-met-apart-are-each-their-own
  (let [fist {:health 20}
        armed {:health 20 :weapon "iron_sword" :equipment {:head {:name "iron_helmet"} :torso {:name "iron_chestplate"} :legs {:name "iron_leggings"} :feet {:name "iron_boots"}}}
        cells (for [x (range 120)] {:x x :y 64 :z 0})
        on (fn [mob lo hi] {:mob mob :weight 1 :reach (fn [x _ _] (when (<= lo x hi) 1))})
        parts (fn [body ts] (cost/route-parts body {:cells cells :threats ts}))
        chance #(:chance (parts %1 %2))
        hurt #(:hurt (parts %1 %2))
        apart (fn [mob] [(on mob 0 10) (on mob 50 60) (on mob 100 110)])
        at-once (fn [mob] (repeat 3 (on mob 0 10)))
        exposure (:exposure (death/met-stretch "zombie" (:reach (on "zombie" 0 10)) cells))
        dies (:chance (answer/group-parts fist ["zombie"] nil (first cells)))
        one-rate (- 1 (reduce + (map (fn [[r w]] (* w (js/Math.pow (- 1 (* dies (- 1 (js/Math.exp (- (* r exposure)))))) 3)))
                                     (death/rate-nodes (death/notice-prior "zombie")))))]
    (is (< (js/Math.abs (- (chance fist (apart "zombie")) one-rate)) 1e-9)
        "three zombies a walk passes one by one: three answers, each its own flight and catch, the zombies' one notice rate shared")
    (is (> (chance armed (at-once "skeleton")) (chance armed (apart "skeleton")))
        "three skeletons shooting at once: their arrows add up in one fight")
    (is (> (chance fist (at-once "skeleton")) (chance fist (apart "skeleton")))
        "and in one flight")
    (is (> (hurt armed (at-once "zombie")) (* 3.3 (hurt armed [(on "zombie" 0 10)])))
        "three zombies fought at once: each hits while the others are fought")
    (is (< (js/Math.abs (- (hurt armed (apart "zombie")) (* 3 (hurt armed [(on "zombie" 0 10)])))) 1e-9)
        "three fought one by one: three fights' hurt")))

(deftest zombies-twenty-blocks-apart-are-partly-met-in-one-answer
  (let [armed {:health 20 :weapon "iron_sword" :equipment iron-set}
        cells (for [x (range 140)] {:x x :y 64 :z 0})
        field (fn [x0] {:mob "zombie" :weight 1 :reach (fn [x _ _] (let [d (js/Math.abs (- x x0))] (when (<= d (threat/follow-range "zombie")) (threat/way-weight d))))})
        hurt #(:hurt (cost/route-parts armed {:cells cells :threats %}))]
    (is (< (* 2 (hurt [(field 35)])) (hurt [(field 35) (field 55)]) (hurt [(field 35) (field 35)]))
        "the second may come while the first is fought, hitting meanwhile, as one at the same spot comes more surely")))

(deftest a-pack-is-priced-no-surer-than-its-shared-notice-rate-allows
  (let [fist {:health 20}
        cells (for [x (range 40)] {:x x :y 64 :z 0})
        on (fn [mob] {:mob mob :weight 1 :reach (fn [x _ _] (when (<= 0 x 10) 1))})
        chance #(:chance (cost/route-parts fist {:cells cells :threats %}))
        exposure (:exposure (death/met-stretch "skeleton" (:reach (on "skeleton")) cells))
        none-notices (death/missed (death/notice-prior "skeleton") (* 6 exposure))
        counted-often {:jobs.lib.cost.death/notice-rate {:other {:k 849 :n-s 4237.5} :ranged {:k 37 :n-s 162.8}}}
        dies (:chance (answer/group-parts fist ["skeleton"] nil (first cells)))
        apart (for [x0 (range 0 120 20)] {:mob "skeleton" :weight 1 :reach (fn [x _ _] (when (<= x0 x (+ x0 10)) 1))})
        long-way (for [x (range 130)] {:x x :y 64 :z 0})]
    (is (< (js/Math.pow (death/missed (death/notice-prior "skeleton") exposure) 6) none-notices)
        "six skeletons share one rate: they may all miss the body together, likelier than six apart")
    (is (< 0.9 (chance (repeat 6 (on "skeleton"))) (- 1 none-notices)) "the pack kills only when one of them gets to the body")
    (is (< (js/Math.abs (- (:chance (cost/route-parts fist {:cells long-way :threats apart}))
                           (- 1 (reduce + (map (fn [[r w]] (* w (js/Math.pow (- 1 (* dies (- 1 (js/Math.exp (- (* r exposure)))))) 6)))
                                               (death/rate-nodes (death/notice-prior "skeleton")))))))
           1e-9)
        "met one by one the six still share the one rate: all may miss the body together")
    (is (< (js/Math.abs (- 1 (/ (chance [(on "zombie")]) (settings/with-settings counted-often #(chance [(on "zombie")]))))) 0.05)
        "a zombie, its class counted 8 times, prices within 5% of a class counted a hundred times as often at the same mean")))

(deftest a-find-is-worth-its-count-less-a-trip-and-an-animal-its-meat
  (let [body {:level 0 :carried 0}]
    (is (= (- (* 3 (cost/worth-to body ["oak_log"] nil)) (cost/trip)) (cost/find-worth body ["oak_log"] nil 3)))
    (is (< (cost/find-worth body ["oak_log"] nil 1) (cost/find-worth body ["oak_log"] nil 3)))
    (is (< (cost/animal-worth "horse") (cost/animal-worth "cow")) "a cow is worth its beef, a kind with no meat the unknown item")
    (is (= (cost/animal-worth "cow") (cost/animal-worth "sheep")) "mutton as dear as beef")
    (is (= (:value (cost/item-value [{:name "chicken" :count (cost/kill-drops "chicken" "chicken")}])) (cost/animal-worth "chicken"))
        "a kill counts the drops the game data lists, not a flat count")))

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

(deftest a-search-whose-legs-all-find-alike-costs-a-leg-over-the-chance-at-any-worth
  (is (= 24 (cost/search-s 0.25 6 1e15)) "a worth that pays for 1e14 legs is priced without walking them")
  (is (> 1e-9 (js/Math.abs (- (cost/search-s 0.25 6 1e3) (cost/search-s (constantly 0.25) 6 1e3)))) "the same as each leg walked")
  (is (infinite? (cost/search-s 0.25 6 20)) "a worth whose first leg does not pay walks none")
  (is (infinite? (cost/search-s 0 6 1e15)))
  (is (number? (cost/tunnel-chance "iron" 16 cost/torch-gap)) "a tunnel's legs all cut rock alike"))

(deftest the-price-stop-is-a-leg-dearer-than-the-worth-left-times-its-chance
  (is (not (cost/leg-stops? 5 0.5 10)))
  (is (cost/leg-stops? 6 0.5 10))
  (is (cost/leg-stops? 1 0 js/Infinity) "nothing to find stops even an unbounded worth"))

(defn faces [block y n] (repeat n {:name block :pos {:x 0 :y y :z 0}}))

(deftest an-ore-look-is-priced-by-the-rock-it-sees-at-its-height-and-a-tunnel-by-the-rock-it-cuts
  (let [chance (fn [seen] (cost/leg-chance ["coal_ore" "deepslate_coal_ore"] {:seen seen}))]
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

(deftest a-tunnel-is-priced-by-the-ore-it-lays-bare-a-second
  (let [line (fn [y names] (map #(hash-map :name % :y y) names))
        rate #(cost/tunnel-rate "iron" #{"iron_ore" "deepslate_iron_ore"} %)]
    (is (< (rate (line 16 (repeat 8 "air"))) (rate (line 16 (repeat 8 "stone")))) "a cut through air lays bare nothing")
    (is (< (rate (line -8 (repeat 8 "deepslate"))) (rate (line -8 (repeat 8 "stone")))) "harder rock, less a second")
    (is (< (rate (line 16 (repeat 8 "stone"))) (rate (line 16 (cons "iron_ore" (repeat 7 "stone"))))) "seen ore counts whole")
    (is (zero? (rate (line 16 (repeat 8 "dirt")))) "dirt holds no ore")
    (is (< (rate (line 16 (repeat 8 nil))) (rate (line 16 (repeat 8 "stone")))) "unseen rock at the dearest host's dig")
    (is (nil? (rate (line 16 ["stone" "water" "stone"]))) "a flooded heading is refused")
    (is (nil? (rate (line -40 ["lava"]))))
    (is (= (rate (line 16 ["stone"])) (rate (line 16 ["stone" "bedrock" "iron_ore"]))) "the cut ends at a block never dug")))

(deftest an-ore-look-takes-no-evidence-from-the-walls-of-the-bodys-own-tunnel
  (let [walls (for [x (range 32) z [-1 1] y [16 17]] #js {:name "stone" :pos #js {:x x :y y :z z}})
        p #js {:seenBlocks (fn [_] (clj->js walls))}
        tunnel (set (for [x (range 32) y [16 17]] [x y 0]))
        chance #(cost/look-leg-chance p [] ["iron_ore" "deepslate_iron_ore"] [0 0] 16 %)]
    (is (pos? (chance #{})) "a cave's walls: a look elsewhere shows rock alike")
    (is (zero? (chance tunnel)) "walls the body cut itself tell nothing of the rock a search leg sees")))

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

(deftest heights-nobody-recorded-keep-a-pseudo-count-in-the-surface-prior
  (let [rise #(settings/with-settings % (fn [] (cost/expected-rise 60 0 nil)))]
    (is (= 2 (rise {:jobs.lib.cost.rise/surface-prior [[62 1000]] :jobs.lib.cost.rise/surface-floor 0})))
    (is (< 2 (rise {:jobs.lib.cost.rise/surface-prior [[62 1000]] :jobs.lib.cost.rise/surface-floor 1}))
        "the unseen bins above weigh in")))

(deftest the-caught-posterior-of-a-count-is-its-grid-posterior
  ;; values of the per-row-log grid sum (a 25 x 17 x 30 x 9 grid), kept to 1e-9 of them
  (let [zombie {:lived [[1 0 1] [3 0 2] [4 0 2] [4 1 1] [5 1 1] [6 0 1] [7 0 1] [8 1 1] [9 2 1] [10 0 1] [10 3 1] [11 0 2] [11 4 1]
                        [12 0 1] [12 6 1] [13 1 1] [14 7 1] [19 0 1] [20 0 18] [20 1 2] [20 2 2] [20 3 1] [20 4 1]]
                :died [[1 1 2] [2 1 2] [3 2 3] [4 2 2] [5 2 1] [6 3 3] [7 3 1] [8 4 3] [9 4 1] [11 5 1] [15 6 1] [17 7 1] [19 8 1] [20 8 3]]}
        bow {:lived [[1 0 2] [2 0 3] [4 0 1] [5 2 1] [6 1 1] [6 3 1] [7 0 2] [8 0 2] [8 1 3] [9 0 1] [11 0 1] [11 1 1] [12 2 1]
                     [13 0 2] [16 1 1] [17 2 1] [17 4 1] [17 7 1] [18 1 1] [19 0 2] [19 1 1] [20 0 11] [20 1 2]]
             :died [[1 1 1] [6 3 1]]}
        near? (fn [want got] (< (js/Math.abs (- want got)) 1e-9))]
    (doseq [[counts blows shape c20 c5] [[zombie 6.769544125120731 43.566697206644704 0.27302431494366297 0.6249289429830022]
                                         [bow 3.5373950759349806 4.439186783014941 0.16042298631766747 0.2448134451737375]]
            :let [post (caught/posterior counts)]]
      (is (near? blows (:blows post)))
      (is (near? shape (:shape post)))
      (is (near? c20 (caught/chance counts 20)))
      (is (near? c5 (caught/chance counts 5))))))


(deftest a-walls-unseen-row-is-wall-by-the-worse-of-the-counted-rows-and-its-own
  (are [counts rows q] (= q (settings/with-settings {:jobs.lib.cost.door/wall-rows counts} #(cost/wall-on-chance rows)))
    {:k 0 :n 0} 0 (/ 1 2)
    {:k 0 :n 0} 3 (/ 4 5)
    {:k 0 :n 98} 3 (/ 4 5)
    {:k 98 :n 98} 3 (/ 99 100)))

(deftest a-door-past-an-unseen-far-side-prices-the-chance-it-is-a-dead-end
  (let [price #(cost/door-s {:sure-s 2 :row-s 2 :most 6 :q % :back-s (fn [k] k)})]
    (is (= 2 (price 0)) "a far side surely open costs only the rows seen")
    (is (= (+ 2 (* 2 (+ 0.5 0.25)) (* 0.125 (+ 2 4 2))) (price 0.5)) "two rows the budget leaves, then a dead end: its digs again and the walk back")
    (is (< 6 (price 0.9)) "a wall likely thicker than the budget is dearer than it")))

(deftest a-door-with-no-bound-on-its-budget-prices-in-finite-time
  (let [price #(cost/door-s {:sure-s 2 :row-s 2 :most ##Inf :q % :back-s (fn [k] k)})]
    (is (= 2 (price 0)))
    (is (= (+ 2 (* 2 (/ 0.5 0.5))) (price 0.5)) "every row dug while the wall holds: no dead end is ever reached")
    (is (= ##Inf (price 1)))))

(deftest a-steps-price-adds-its-unseen-seconds-after-the-stair-floor
  (are [s0 stair? unseen expected] (= expected (round2 (ways/step-price s0 0.3 1 stair? unseen)))
    0.3 false 0.5 0.8
    2.3 false 0.5 2.8
    0.3 true 0.5 1.5
    2.3 true 0.5 3.5
    2.3 true 0 3))
