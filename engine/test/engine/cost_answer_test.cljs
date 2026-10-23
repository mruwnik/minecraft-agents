(ns engine.cost-answer-test
  "jobs.lib.cost.answer: one price for each answer to hostiles (fight, walk away, build a refuge) and the cheapest."
  (:require [cljs.test :refer [deftest is are use-fixtures]]
            [engine.fake :as fake]
            [engine.game :as game]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.answer :as answer]
            [jobs.lib.cost.death :as death]
            [jobs.lib.cost.threat :as threat]
            [engine.settings :as settings]))

(use-fixtures :each {:before #(game/select! (fake/create {}))})

(def here {:x 0 :y 64 :z 0})

(defn body
  "A fed body with health and no bed: a death walks it back from world spawn."
  [health]
  {:health health :food 20 :level 0 :carried 0})

(def sword "iron_sword")

(defn fight [weapon & mobs] {:weapon weapon :mobs (vec mobs)})

(defn mob
  ([name distance] {:name name :distance distance :follow 16})
  ([name distance follow] {:name name :distance distance :follow follow}))

(defn choice
  "What decide makes of fighting mobs with weapon or walking away from them, for a body at health with returns flights from them lately."
  ([health weapon mobs] (choice health weapon mobs 0))
  ([health weapon mobs returns]
   (let [f (cost/fight-answer (apply fight weapon mobs))]
     (:answer (cost/decide (body health)
                           {:fight f :flee (cost/flee-answer {:food 20 :mobs mobs :returns returns :later f})}
                           here)))))

(deftest blows-that-may-take-the-health-are-the-death-risk
  (is (zero? (death/blow-chance {:health 20} 0 3)))
  (is (< (death/blow-chance {:health 20} 4 2.5) 0.01) "a fight of 4 hp at full health hardly kills")
  (is (< 0.4 (death/blow-chance {:health 5} 4 2.5) 0.6) "at 5 hp it may well")
  (is (< (death/blow-chance {:health 5 :absorption 4} 4 2.5) (death/blow-chance {:health 5} 4 2.5)) "absorption is health")
  (is (= (death/blow-chance (body 5) 4 2.5) (cost/death-chance (body 5) {:damage 4 :hit 2.5})) "the death chance of an answer's blows")
  (is (= 1 (cost/death-chance (body 20) {:damage 21 :hit 3 :blasts [20]})) "a blast that takes the health kills")
  (is (> (cost/death-chance (body 20) {:damage 21 :hit 3 :blasts [16]}) (cost/death-chance (body 20) {:damage 21 :hit 16}))
      "a blast of 16 leaves 4 hp against 5 hp of zombie blows, not 5/16 more of a blast"))

(deftest the-cheapest-open-answer-is-taken
  (is (= {:answer :b :prices {:a 10 :b 5}}
         (cost/decide (body 20) {:a {:damage 0 :seconds 10} :b {:damage 0 :seconds 5} :c nil} here)))
  (is (nil? (cost/decide (body 20) {:a nil} here)) "none open")
  (is (> (cost/answer-price (body 10) {:damage 4 :hit 2 :seconds 0} here) (cost/answer-price (body 20) {:damage 4 :hit 2 :seconds 0} here))
      "hp are dearer near the floor")
  (is (> (cost/answer-price (body 20) {:damage 0 :seconds 5 :then [{:damage 0 :seconds 7}]} here)
         (cost/answer-price (body 20) {:damage 0 :seconds 5} here))
      "what follows is paid too")
  (is (< (- (cost/answer-price (body 5) {:damage 100 :hit 5 :seconds 1 :then [{:damage 0 :seconds 50}]} here)
            (cost/answer-price (body 5) {:damage 100 :hit 5 :seconds 1} here))
         0.01)
      "a death all but sure ends the meet: what would follow it is not paid"))

(deftest a-fight-is-taken-when-it-is-cheaper-than-walking-away-and-meeting-the-mob-again
  (are [health weapon mobs expected] (= expected (choice health weapon mobs))
    20 sword [(mob "zombie" 3 35)] :fight
    5 sword [(mob "zombie" 3 35)] :flee
    20 nil [(mob "zombie" 3 35)] :flee
    20 sword [(mob "zombie" 3 35) (mob "zombie" 3 35) (mob "zombie" 3 35)] :flee
    20 sword [(mob "skeleton" 10)] :fight))

(deftest a-walk-away-is-caught-at-the-prior-and-so-loses-to-a-fight-or-a-wall-it-would-beat-on-an-open-way
  (let [zombie (mob "zombie" 3 35)
        f (cost/fight-answer (fight sword zombie))
        flee #(cost/flee-answer {:food 20 :mobs [zombie] :later f :caught %})
        wall (assoc (cost/build-answer {:mobs [zombie] :seconds 2}) :seconds 120)
        pick #(:answer (cost/decide (body 8) %1 here))]
    (is (= :flee (pick {:fight f :flee (flee 0)})) "at 8 hp an open way beats the fight")
    (is (= :fight (pick {:fight f :flee (flee nil)})))
    (is (= :flee (pick {:wall wall :flee (flee 0)})) "a wall that holds the body long costs more than an open way")
    (is (= :wall (pick {:wall wall :flee (flee nil)})))
    (is (> (cost/death-chance (body 20) (flee 1)) 0.9) "caught, the zombie lands its blows all the flight")
    (is (< (cost/death-chance (body 20) (cost/flee-answer {:food 20 :mobs [zombie] :later f :caught 1 :caught-s 0.5})) 0.1)
        "caught for the half second the race gives it, only those blows")))

(deftest a-walk-caught-beside-a-creeper-takes-its-blast-as-a-sure-part-of-the-life
  (let [walk (cost/flee-answer {:food 20 :mobs [(mob "creeper" 1) (mob "zombie" 3 35)] :caught 1})
        {:keys [damage hit blasts]} (:caught walk)
        blast (cost/blast-hurt (cost/armour-stats nil) answer/swell-start)]
    (is (= [blast] blasts))
    (is (= (death/blow-chance (body 8) damage hit blast) (cost/death-chance (body 8) walk)))))

(deftest a-walk-from-a-mob-that-keeps-up-is-no-answer
  (is (nil? (cost/flee-answer {:food 20 :mobs [(mob "spider" 4)]})) "a spider outruns the body")
  (is (some? (cost/flee-answer {:food 20 :mobs [(mob "zombie" 4 35)]})))
  (is (pos? (:damage (cost/flee-answer {:food 20 :mobs [(mob "creeper" 2)]}))) "a creeper 2 off blows before the walk is out of its reach")
  (is (zero? (:damage (cost/flee-answer {:food 20 :mobs [(mob "creeper" 16)]}))) "far off, even the slowest start of the measured ones gets out of its reach"))

(deftest a-creeper-that-blows-on-the-walk-is-not-met-again
  (let [zombie (mob "zombie" 4 35)]
    (is (nil? (:then (cost/flee-answer {:food 20 :mobs [(mob "creeper" 1)]}))) "a lone creeper blown is gone")
    (is (some? (:then (cost/flee-answer {:food 20 :mobs [(mob "creeper" 3)]}))) "one 3 off mostly does not reach the walk")
    (is (= (cost/flee-answer {:food 20 :mobs [zombie]}) (first (:then (cost/flee-answer {:food 20 :mobs [(mob "creeper" 1) zombie]}))))
        "after the blast only the zombie is walked from again")))

(deftest a-body-that-can-neither-outrun-nor-outfight-a-spider-digs-in
  (let [spider (mob "spider" 2)
        answers {:fight (cost/fight-answer (fight nil spider)) :flee (cost/flee-answer {:food 20 :mobs [spider]})}]
    (is (= :fight (:answer (cost/decide (body 20) answers here))) "with no cover the fist is all that is left")
    (is (= :pit (:answer (cost/decide (body 20) (assoc answers :pit (cost/cover-answer {:mobs [spider]} {:open-s 4 :seconds 40})) here)))
        "a pit dug in 4 s under the spider's blows beats a fist fight it hardly survives")
    (is (nil? (cost/cover-answer {:mobs [(mob "creeper" 4)]} {:open-s 4 :seconds 40})) "a blast takes the cover")))

(deftest a-threats-cover-is-priced-as-respond-prices-it
  (let [cover {:open-s 2 :seconds 50}
        zombie (mob "zombie" 0 35)]
    (is (= (cost/cover-answer {:mobs [zombie]} cover)
           (:walls (answer/threat-answers {:food 20 :covers {:walls cover}} "zombie" nil)))
        "one cover-answer over shut-in/covers' seconds, the hide-out's quiet in them, for death-chance and respond alike")))

(deftest a-mob-that-came-back-before-makes-a-walk-dearer
  (let [walk #(cost/answer-price (body 20) (cost/flee-answer {:food 20 :mobs [(mob "skeleton" 10)] :returns %}) here)]
    (is (> (walk 2) (walk 1) (walk 0)))))

(deftest a-lone-creeper-is-fought-hit-and-back-at-the-measured-exchange
  (let [price #(cost/answer-price (body %) (cost/fight-answer (fight sword (mob "creeper" 3))) here)]
    (is (> (price 10) (* 2 (price 20)))))
  (are [health weapon distance returns expected] (= expected (choice health weapon [(mob "creeper" distance)] returns))
    20 "stone_sword" 4 0 :fight
    20 "stone_sword" 9 0 :fight
    20 "stone_sword" 9 3 :fight))

(deftest a-creepers-blast-falls-off-to-nothing-at-twice-its-power
  (let [bare (cost/armour-stats nil)]
    (is (> (cost/blast-hurt bare 1) (cost/blast-hurt bare 3) (cost/blast-hurt bare 5) 0))
    (is (zero? (cost/blast-hurt bare 6)))
    (is (< (cost/blast-hurt (cost/armour-stats {:torso {:name "diamond_chestplate"}}) 3) (cost/blast-hurt bare 3)) "armour takes some")))

(deftest a-refuge-costs-the-blows-of-the-mobs-that-arrive-before-it-is-built
  (let [build #(:damage (cost/build-answer {:mobs [(mob "zombie" %)] :seconds 2}))]
    (is (> (build 1) (build 4) (build 8)))
    (is (zero? (build 8)) "a zombie 8 off is not there within 2 s"))
  (is (pos? (:damage (cost/build-answer {:mobs [(mob "skeleton" 10)] :seconds 2}))) "a shooter in bow range shoots meanwhile")
  (is (zero? (:damage (cost/build-answer {:mobs [(mob "skeleton" 20)] :seconds 2}))) "one beyond it does not"))




(deftest a-long-fist-stay-in-the-dark-open-risks-a-death-worth-more-than-half-the-health
  ;; 120 s at the dark spawn chance; no bed: a death walks the body back from world spawn
  (let [spawns (* 120 (cost/spawn-chance [0 0 0]))]
    (is (> (cost/lethal-hp (body 20) (cost/spawns-chance (body 20) spawns here) here) 10))
    (is (< (cost/spawns-chance (assoc (body 20) :weapon "diamond_sword") spawns here) (cost/spawns-chance (body 20) spawns here)) "a sword dies less")
    (is (zero? (cost/spawns-chance (body 20) 0 here)))))

(deftest a-fight-out-of-reach-takes-the-mobs-blows-and-kills-nothing-so-the-swim-out-wins
  (let [deep (assoc (mob "drowned" 3 35) :reach? false)
        unreached (cost/fight-answer (fight "stone_sword" deep))
        fought (cost/fight-answer (fight "stone_sword" (dissoc deep :reach?)))
        swim (cost/flee-answer {:food 20 :mobs [deep] :swim 4})]
    (is (= [deep] (:left unreached)) "the mob is left alive")
    (is (> (:damage unreached) (:damage fought)) "its blows land the whole fight")
    (is (= :fight (:answer (cost/decide (body 13) {:fight fought :flee (cost/flee-answer {:food 20 :mobs [deep] :swim 4 :later fought})} here)))
        "in reach, the fight at 13 hp beats the walk")
    (is (= :flee (:answer (cost/decide (body 13) {:fight (cost/followed unreached swim) :flee swim} here)))
        "out of reach, the fight is the blows and then the same swim out")))

(deftest a-swim-out-gains-on-a-drowned-and-a-swim-with-no-shore-from-a-faster-swimmer-is-no-answer
  (are [m swim expected] (= expected (some-> (cost/flee-answer {:food 20 :mobs [m] :swim swim}) :damage zero?))
    (mob "drowned" 3 35) 4 true
    (mob "drowned" 3 35) ##Inf true
    (mob "drowned" 0.5 35) 4 false
    (mob "guardian" 3 35) ##Inf nil)
  (is (> (:seconds (cost/flee-answer {:food 20 :mobs [(mob "drowned" 3 35)] :swim 4}))
         (:seconds (cost/flee-answer {:food 20 :mobs [(mob "drowned" 3 35)]})))
      "a swim is slower than a run")
  (is (pos? (:damage (cost/flee-answer {:food 20 :mobs [(assoc (mob "drowned" 8 35) :held "trident")] :swim 4})))
      "a trident is thrown from 10 blocks"))

(deftest a-trident-drowned-is-priced-by-its-throws-not-a-melee-blow
  (let [trident (assoc (mob "drowned" 8 35) :held "trident")
        melee (mob "drowned" 8 35)
        hit #(:hit (cost/flee-answer {:food 20 :mobs [%] :swim 4}))]
    (is (= (threat/difficulty-scale 8) (hit trident)) "a throw is 8 hp, before the server difficulty")
    (is (> (hit trident) (hit melee)))
    (is (> (:damage (cost/fight-answer (fight "stone_sword" trident)))
           (:damage (cost/fight-answer (fight "stone_sword" melee))))
        "the fight prices its throws too")))

(deftest a-swim-to-a-ledge-with-no-land-to-walk-on-is-no-escape-from-a-drowned
  (let [drowned (mob "drowned" 3 35)
        flee #(cost/flee-answer {:food 20 :mobs [drowned] :swim 4 :land %})]
    (is (nil? (flee 0)) "the drowned keeps up on a ledge")
    (is (some? (flee ##Inf)) "land that goes on, or not known, is the walk away")))

(deftest a-lethal-threat-costs-the-body-its-life-not-more
  ;; a fist fight with a witch is 26 punches 1.3 s apart under her potions: 84 hp expected
  (let [route (fn [weight & mobs] {:cells [(assoc here :s 60)] :threats (for [m mobs] {:mob m :weight weight :reach (constantly 1)})})]
    (is (= 20 (cost/route-hurt (body 20) (route 1 "witch"))))
    (is (= 20 (cost/route-hurt (body 20) (route 1 "witch" "witch"))) "nor do two")
    (is (< 5.99 (cost/route-hurt (body 20) (route 0.3 "witch")) 6.01) "a faded one its share of the life")))

(deftest a-second-in-water-costs-the-drowned-meet-rate-times-the-cheapest-answer-to-one
  (let [kit (assoc (body 20) :weapon "stone_sword")
        price (apply min (vals (:prices (cost/decide kit (answer/threat-answers (assoc kit :swim ##Inf) "drowned" nil) nil))))
        standing (apply min (vals (:prices (cost/decide kit (answer/threat-answers kit "drowned" nil) nil))))
        [deep other] (cost/water-factors kit)]
    (is (< (js/Math.abs (- deep (* 0.0021 price))) 1e-9) "water a drowned can spawn in: its measured meet rate")
    (is (< (js/Math.abs (- other (* 0.00043 standing))) 1e-9) "other water, mostly a wade: the meet answered standing")
    (is (< deep (first (cost/water-factors (body 20)))) "a bare body pays more than one with a sword")
    (is (< deep (first (cost/water-factors (assoc kit :health 8)))) "and a hurt one more than a whole one")))

(deftest a-phantom-floor-beside-seen-water-pays-its-chance-of-the-swim-there
  (settings/with-settings {:jobs.lib.cost.answer/unseen-water-lags [{:k 8 :n 8} {:k 2 :n 8}]}
    (fn []
      (let [kit (assoc (body 20) :weapon "stone_sword")
            [deep] (cost/water-factors kit)
            swim (dec (/ (* (/ 0.2 0.392) (inc deep)) 0.23164234422052352))]
        (is (every? #(< (js/Math.abs %) 1e-9) (map - [(* 0.9 swim) (* 0.3 swim)] (cost/unseen-water-factors kit))) "Beta(1,1) of each lag's count times the swim over the walk")))))

(deftest a-look-is-worth-what-it-may-change-less-what-it-costs
  (let [walls (cost/cover-answer {:mobs [(mob "skeleton" 8)]} {:open-s 9 :seconds 39})
        door (cost/cover-answer {:mobs [(mob "skeleton" 8)]} {:open-s 3 :seconds 33})
        now {:walls walls}
        look (fn [mobs chance] (cost/look-answer (body 5) {:mobs mobs :looks 4 :outcomes [[chance {:walls walls :door door}] [(- 1 chance) now]]} here))
        value (fn [mobs chance] (cost/look-value (body 5) now (look mobs chance) here))]
    (is (zero? (:value (cost/look-value (body 5) now (cost/look-answer (body 5) {:mobs [] :looks 4 :outcomes [[1 now]]} here) here)))
        "a look that can show nothing new changes nothing")
    (is (< (:cost (value [] 0.9)) (:value (value [] 0.9))) "likely closed: the cheaper door is worth the look")
    (is (= :look (:answer (cost/decide (body 5) (assoc now :look (look [] 0.9)) here))) "decide takes it")
    (is (> (:value (value [] 0.9)) (:value (value [] 0.2))) "the less likely the door, the less the look is worth")
    (is (= 1 (cost/closed-chance 11 0)) "all seen: closed")
    (is (= (/ 12 13) (cost/closed-chance 11 1)) "one unseen of twelve: the rule of succession")
    (is (> (cost/closed-chance 11 1) (cost/closed-chance 2 1) (cost/closed-chance 2 3)))))

(deftest a-swim-to-a-shore-whose-land-may-go-on-is-caught-at-the-chance-it-ends
  (let [m (mob "drowned" 3 35)
        known (cost/flee-answer {:food 20 :mobs [m] :swim 4 :land 0})
        open (cost/flee-answer {:food 20 :mobs [m] :swim 4 :land 0 :open 0.25})
        seen-land (cost/flee-answer {:food 20 :mobs [m] :swim 4 :land ##Inf})]
    (is (nil? known) "the land seen ends at once and the mob keeps up")
    (is (< (js/Math.abs (- 0.7875 (-> open :caught :chance))) 1e-9) "caught at 1 - (1 - 0.15) * 0.25")
    (is (< (-> open :caught :damage) (-> (cost/flee-answer {:food 20 :mobs [m] :swim 4 :land 0 :open 0.25 :later {:damage 100 :hit 3 :seconds 5}}) :caught :damage)) "a later fight is what the ending flight takes")
    (is (> (-> open :caught :chance) (-> seen-land :caught :chance)))
    (is (= (:caught seen-land) (:caught (cost/flee-answer {:food 20 :mobs [m] :swim 4 :land ##Inf :open 0.25}))) "land enough already: the chance is no matter")))

(deftest a-drowned-fought-in-water-lands-its-measured-blows-for-each-swing-its-kill-needs
  (let [kit (assoc (body 20) :weapon "stone_sword")
        wet (:fight (answer/threat-answers (assoc kit :swim ##Inf) "drowned" nil))
        dry (:fight (answer/threat-answers kit "drowned" nil))
        blows (* 4 1.51)]
    (is (< (js/Math.abs (- (:damage wet) (threat/mob-hurt (cost/armour-stats nil) "drowned" blows identity 1))) 1e-9) "4 stone sword swings kill it")
    (is (> (:damage wet) (* 1.5 (:damage dry))) "a land fight lands its swings")))

(deftest a-drowned-met-swimming-kills-by-the-answer-taken-its-weapon-and-the-shore
  (let [chance #(answer/threat-chance %1 "drowned" nil here)
        iron (assoc (body 20) :weapon "iron_sword")
        fist (body 20)]
    (is (< (chance iron) 0.01) "standing, as in a wade, an iron sword wins")
    (is (> (chance (assoc fist :swim ##Inf)) 0.1) "a bare body far from shore dies often whatever it answers")
    (let [answers (answer/threat-answers (assoc fist :swim 1) "drowned" nil)]
      (is (= :flee (:answer (cost/decide fist answers here))) "a bare body a block from shore swims out")
      (is (< (* 2 (answer/answer-death-chance fist (:flee answers) here)) (answer/answer-death-chance fist (:fight answers) here))
          "and that swim risks far less than a fight with it"))))
